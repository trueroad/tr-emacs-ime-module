;;; tr-ime.el --- Emulator of IME patch for Windows -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Masamichi Hosoda

;; Author: Masamichi Hosoda <trueroad@trueroad.jp>
;; URL: https://github.com/trueroad/tr-emacs-ime-module
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1") (w32-ime "0.0.1"))

;; Emulator of GNU Emacs IME patch for Windows (tr-ime)
;; is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Emulator of GNU Emacs IME patch for Windows (tr-ime)
;; is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with tr-ime.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emulator of GNU Emacs IME patch for Windows (tr-ime) emulates the C
;; implementation part of an IME patch for GNU Emacs that allows to input
;; Japanese using the Windows IME (Input Method Editor).  On Emacs 26.2 or
;; later, Japanese input using the IME is now possible even without the IME
;; patch.  However, some of the features of the IME patch are not implemented
;; in current Emacs, so it is possible to type Japanese on Emacs without the
;; IME patch, but it is not convenient.
;;
;; w32-ime.el in the w32-ime package manages the upper layer of the IME
;; patch and provides the convenient features but it could not be used on
;; emacs.exe without the IME patch applied.  tr-ime emulates the lower
;; layers of the IME patches and can interact with w32-ime.el.  So, using
;; tr-ime, w32-ime.el can be used on emacs.exe without the IME patch applied.
;;
;; By using tr-ime and w32-ime.el together, emacs.exe without the IME patch
;; can use the same convenient features as the IME patched emacs.exe.  They
;; can display the on/off state of the IME in the mode line as UI/UX features.
;; They also have hooks to call when the IME state is changed.  With these
;; hooks, you can change the color and shape of the cursor depending on the
;; IME on/off status to be visually known to the IME state.

;; To use standard features (stable but less functionality) of the tr-ime
;; package, add the following code to your init.el or .emacs
;; It loads the tr-ime-mod DLL module if you use Emacs 27.
;;
;;   (tr-ime-standard-install)
;;   (setq default-input-method "W32-IME")
;;   (w32-ime-initialize)
;;
;; To use advanced features (experimental but more functionality) of the
;; tr-ime package, add the following code to your init.el or .emacs
;; It loads the tr-ime-modadv DLL module.
;;
;;   (tr-ime-advanced-install)
;;   (setq default-input-method "W32-IME")
;;   (w32-ime-initialize)

;;; Code:

(defgroup tr-ime nil
  "Emulator of GNU Emacs IME patch for Windows (tr-ime)."
  :group 'emacs)

(defconst tr-ime--mod-abi-version 1
  "ABI version number of tr-ime-mod DLL.")
(defconst tr-ime--mod-name (concat "tr-ime-mod-"
                                   (int-to-string tr-ime--mod-abi-version)
                                   "-"
                                   system-configuration)
  "Module name of tr-ime-mod (standard).")

(defconst tr-ime--modadv-abi-version 2
  "ABI version number of tr-ime-modadv DLL.")
(defconst tr-ime--modadv-name (concat
                               "tr-ime-modadv-"
                               (int-to-string tr-ime--modadv-abi-version)
                               "-"
                               system-configuration)
  "Module name of tr-imeadv-mod (advanced).")

(defvar tr-ime-enabled-features nil
  "Enabled features in tr-ime (standard/advanced).

If the standard features are enabled, it is set to 'standard.
If the advanced features are enabled, it is set to 'advanced.
If any features are not enabled, it is set to nil.")

(declare-function tr-ime-download-mod-file "tr-ime-download"
                  (name &optional no-confirm))

(defun tr-ime--c-function-p (symb)
  "Return non-nil if SYMB is a function that is defined in a C source."
  (and (fboundp symb)
       (subrp (symbol-function symb))))

(defun tr-ime--c-variable-p (symb)
  "Return non-nil if SYMB is a variable that is defined in a C source."
  (and (boundp symb)
       (not (symbol-file symb 'defvar))))

;;;###autoload
(defun tr-ime-detect-ime-patch-p ()
  "Return non-nil if an IME patch seems to be applied to Emacs."
  (or (tr-ime--c-function-p 'ime-get-mode)
      (tr-ime--c-function-p 'ime-force-on)
      (tr-ime--c-function-p 'ime-force-off)
      (tr-ime--c-variable-p 'set-selected-window-buffer-functions)
      (tr-ime--c-variable-p 'select-window-functions)))

;;;###autoload
(defun tr-ime-standard-install (&optional no-confirm)
  "Install tr-ime standard (stable but less functionality) DLL.

If NO-CONFIRM is non-nil, download the necessary module DLL without
confirming the user."
  (tr-ime-uninitialize)
  (cond
   ((not (eq window-system 'w32))
    (display-warning 'tr-ime
                     (concat
                      "window-system is not w32. "
                      "tr-ime cannot install standard DLL on it.")
                     :warning))
   ((tr-ime-detect-ime-patch-p)
    (display-warning 'tr-ime
                     (concat
                      "Emacs seems to have an IME patch applied. "
                      "tr-ime cannot work on it.")
                     :warning))
   ((fboundp 'w32-get-ime-open-status)
    (display-warning 'tr-ime
                     (concat
                      "Function w32-get-ime-open-status exists. "
                      "tr-ime standard DLL is not necessary.")
                     :debug))
   ((not (string= module-file-suffix ".dll"))
    (display-warning 'tr-ime
                     (concat
                      "module-file-suffix is not \".dll\". "
                      "Emacs cannot load tr-ime module DLLs.")
                     :warning))
   ((locate-library tr-ime--mod-name)
    (display-warning 'tr-ime
                     "tr-ime standard DLL already exists."
                     :debug))
   (t
    (require 'tr-ime-download)
    (tr-ime-download-mod-file tr-ime--mod-name no-confirm)))
  (tr-ime-standard-initialize))

;;;###autoload
(defun tr-ime-standard-initialize ()
  "Initialize tr-ime standard (stable but less functionality) features."
  (cond
   ((not (eq window-system 'w32))
    (display-warning 'tr-ime
                     (concat
                      "window-system is not w32. "
                      "tr-ime cannot initialize on it.")
                     :warning))
   ((tr-ime-detect-ime-patch-p)
    (display-warning 'tr-ime
                     (concat
                      "Emacs seems to have an IME patch applied. "
                      "tr-ime cannot work on it.")
                     :warning))
   ((and (not (fboundp 'w32-get-ime-open-status))
         (not (string= module-file-suffix ".dll")))
    (display-warning 'tr-ime
                     (concat
                      "module-file-suffix is not \".dll\". "
                      "Emacs cannot load tr-ime module DLLs.")
                     :warning))
   (t
    (setq tr-ime-enabled-features 'standard)
    (unless (fboundp 'w32-get-ime-open-status)
      (require 'tr-ime-mod tr-ime--mod-name))
    (require 'tr-ime-openstatus)
    (require 'tr-ime-hook)
    (require 'tr-ime-workaround-prefix-key)
    (require 'tr-ime-workaround-inconsistent)
    (define-key global-map [M-kanji] 'toggle-input-method)
    (define-key isearch-mode-map [M-kanji] 'isearch-toggle-input-method)
    (require 'w32-ime))))

;;;###autoload
(defun tr-ime-advanced-install (&optional no-confirm)
  "Install tr-ime advanced (experimental but more functionality) DLL.

If NO-CONFIRM is non-nil, download the necessary module DLL without
confirming the user."
  (tr-ime-uninitialize)
  (cond
   ((not (eq window-system 'w32))
    (display-warning 'tr-ime
                     (concat
                      "window-system is not w32. "
                      "tr-ime cannot install advanced DLL on it.")
                     :warning))
   ((tr-ime-detect-ime-patch-p)
    (display-warning 'tr-ime
                     (concat
                      "Emacs seems to have an IME patch applied. "
                      "tr-ime cannot work on it.")
                     :warning))
   ((not (string= module-file-suffix ".dll"))
    (display-warning 'tr-ime
                     (concat
                      "module-file-suffix is not \".dll\". "
                      "Emacs cannot load tr-ime module DLLs.")
                     :warning))
   ((locate-library tr-ime--modadv-name)
    (display-warning 'tr-ime
                     "tr-ime advanced DLL already exists."
                     :debug))
   (t
    (require 'tr-ime-download)
    (tr-ime-download-mod-file tr-ime--modadv-name no-confirm)))
  (tr-ime-advanced-initialize))

;;;###autoload
(defun tr-ime-advanced-initialize ()
  "Initialize tr-ime advanced (experimental but more functionality) features."
  (cond
   ((not (eq window-system 'w32))
    (display-warning 'tr-ime
                     (concat
                      "window-system is not w32. "
                      "tr-ime cannot initialize on it.")
                     :warning))
   ((tr-ime-detect-ime-patch-p)
    (display-warning 'tr-ime
                     (concat
                      "Emacs seems to have an IME patch applied. "
                      "tr-ime cannot work on it.")
                     :warning))
   ((not (string= module-file-suffix ".dll"))
    (display-warning 'tr-ime
                     (concat
                      "module-file-suffix is not \".dll\". "
                      "Emacs cannot load tr-ime module DLLs.")
                     :warning))
   (t
    (setq tr-ime-enabled-features 'advanced)
    (require 'tr-ime-modadv tr-ime--modadv-name)
    (require 'tr-ime-openstatus)
    (require 'tr-ime-hook)
    (require 'tr-ime-subclassify)
    (require 'tr-ime-thread-message)
    (require 'tr-ime-recv-notify)
    (require 'tr-ime-font)
    (require 'tr-ime-isearch)
    (require 'tr-ime-workaround-isearch)
    (require 'tr-ime-prefix-key)
    (require 'tr-ime-sync)
    (require 'tr-ime-reconversion)
    (require 'tr-ime-documentfeed)
    (require 'tr-ime-debug)
    (define-key global-map [M-kanji] 'ignore)
    (require 'w32-ime))))

;;;###autoload
(defun tr-ime-uninitialize ()
  "Uninitialize tr-ime features."
  (when (eq tr-ime-enabled-features 'standard)
    (define-key global-map [M-kanji] 'ignore)
    (define-key isearch-mode-map [M-kanji] 'ignore))
  (when (featurep 'tr-ime-debug)
    (unload-feature 'tr-ime-debug t))
  (when (featurep 'tr-ime-documentfeed)
    (unload-feature 'tr-ime-documentfeed t))
  (when (featurep 'tr-ime-reconversion)
    (unload-feature 'tr-ime-reconversion t))
  (when (featurep 'tr-ime-sync)
    (unload-feature 'tr-ime-sync t))
  (when (featurep 'tr-ime-prefix-key)
    (unload-feature 'tr-ime-prefix-key t))
  (when (featurep 'tr-ime-workaround-isearch)
    (unload-feature 'tr-ime-workaround-isearch t))
  (when (featurep 'tr-ime-isearch)
    (unload-feature 'tr-ime-isearch t))
  (when (featurep 'tr-ime-font)
    (unload-feature 'tr-ime-font t))
  (when (featurep 'tr-ime-recv-notify)
    (unload-feature 'tr-ime-recv-notify t))
  (when (featurep 'tr-ime-thread-message)
    (unload-feature 'tr-ime-thread-message t))
  (when (featurep 'tr-ime-subclassify)
    (unload-feature 'tr-ime-subclassify t))
  (when (featurep 'tr-ime-workaround-inconsistent)
    (unload-feature 'tr-ime-workaround-inconsistent t))
  (when (featurep 'tr-ime-workaround-prefix-key)
    (unload-feature 'tr-ime-workaround-prefix-key t))
  (when (featurep 'tr-ime-hook)
    (unload-feature 'tr-ime-hook t))
  (when (featurep 'tr-ime-openstatus)
    (unload-feature 'tr-ime-openstatus t))
  (when (featurep 'tr-ime-modadv)
    (unload-feature 'tr-ime-modadv t))
  (when (featurep 'tr-ime-mod)
    (unload-feature 'tr-ime-mod t))
  (setq tr-ime-enabled-features nil))

(defun tr-ime-unload-feature ()
  "Prepare unloading tr-ime features."
  (tr-ime-uninitialize))

(provide 'tr-ime)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime.el ends here
