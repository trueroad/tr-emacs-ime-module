;;; tr-ime.el --- Emulator of IME patch for Windows -*- lexical-binding: t -*-

;; Copyright (C) 2020 Masamichi Hosoda

;; Author: Masamichi Hosoda <trueroad@trueroad.jp>
;; URL: https://github.com/trueroad/tr-emacs-ime-module
;; Version: 0.3.50
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
;;
;; To use advanced features (experimental but more functionality) of the
;; tr-ime package, add the following code to your init.el or .emacs
;; It loads the tr-ime-modadv DLL module.
;;
;;   (tr-ime-advanced-install)

;;; Code:

(defconst tr-ime-mod-abi-version 1
  "ABI version number of tr-ime-mod DLL.")
(defconst tr-ime-modadv-abi-version 1
  "ABI version number of tr-ime-modadv DLL.")

;;;###autoload
(defun tr-ime-standard-install ()
  "Install tr-ime standard features (stable but less functionality)."
  (when (and (eq window-system 'w32)
             (not (fboundp 'ime-get-mode))
             (string= module-file-suffix ".dll"))
    (unless (fboundp 'w32-get-ime-open-status)
      (require 'tr-ime-mod (concat "tr-ime-mod-"
                                   (int-to-string tr-ime-mod-abi-version)
                                   "-"
                                   system-configuration)))
    (require 'tr-ime-openstatus)
    (require 'tr-ime-hook)
    (require 'tr-ime-workaround-prefix-key)
    (require 'tr-ime-workaround-inconsistent)
    (define-key global-map [M-kanji] 'toggle-input-method)
    (define-key isearch-mode-map [M-kanji] 'isearch-toggle-input-method)
    (require 'w32-ime)))

;;;###autoload
(defun tr-ime-advanced-install ()
  "Install tr-ime advanced features (experimental but more functionality)."
  (when (and (eq window-system 'w32)
             (not (fboundp 'ime-get-mode))
             (string= module-file-suffix ".dll"))
    (require 'tr-ime-modadv (concat "tr-ime-modadv-"
                                    (int-to-string tr-ime-modadv-abi-version)
                                    "-"
                                    system-configuration))
    (require 'tr-ime-openstatus)
    (require 'tr-ime-hook)
    (require 'tr-ime-subclassify)
    (require 'tr-ime-thread-message)
    (require 'tr-ime-recv-notify)
    (require 'tr-ime-font)
    (require 'tr-ime-isearch)
    (require 'tr-ime-module2-helper)
    (require 'w32-ime)))

(provide 'tr-ime)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime.el ends here
