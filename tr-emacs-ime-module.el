;;; tr-emacs-ime-module.el --- Simple IME module -*- lexical-binding: t -*-

;; Copyright (C) 2020 Masamichi Hosoda

;; Author: Masamichi Hosoda <trueroad@trueroad.jp>
;; URL: https://github.com/trueroad/tr-emacs-ime-module
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))

;; Simple IME module for GNU Emacs (tr-emacs-ime-module)
;; is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Simple IME module for GNU Emacs (tr-emacs-ime-module)
;; is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with tr-emacs-ime-module.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple IME module for GNU Emacs (tr-emacs-ime-module) is an attempt in
;; GNU Emacs for Windows (MinGW/Cygwin) to make Japanese input with IME
;; easier to use by using the dynamic module in Emacs, e.g. for GNU binary
;; distributions/Cygwin packages that are not applied IME patches.
;;
;; To use Module1 (stable but less functionality) of this package,
;; add the following code to your init.el or .emacs
;;
;;   (tr-emacs-ime-module-install)
;;
;; To use Module2 (experimental but more functionality) of this package,
;; add the following code to your init.el or .emacs
;;
;;   (tr-emacs-ime-module2-install)

;;; Code:

;;;###autoload
(defun tr-emacs-ime-module-install ()
  "Install tr-emacs-ime-module Module1 (stable but less functionality)"
  (when (and (eq window-system 'w32)
             (not (fboundp 'ime-get-mode))
             (string= module-file-suffix ".dll")
             (locate-library "tr-ime-module-helper"))
    (require 'tr-ime-module-helper)
    (require 'w32-ime "w32-ime-for-tr-ime-module")))

;;;###autoload
(defun tr-emacs-ime-module2-install ()
  "Install tr-emacs-ime-module Module2 (experimental but more functionality)"
  (when (and (eq window-system 'w32)
             (not (fboundp 'ime-get-mode))
             (string= module-file-suffix ".dll")
             (locate-library "tr-ime-module2-helper"))
    (require 'tr-ime-module2-helper)
    (require 'w32-ime "w32-ime-for-tr-ime-module")))

(provide 'tr-emacs-ime-module)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-emacs-ime-module.el ends here
