;;; tr-emacs-ime-module.el --- Simple IME module for GNU Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2020 Masamichi Hosoda

;; Author: Masamichi Hosoda <trueroad@trueroad.jp>
;; URL: https://github.com/trueroad/tr-emacs-ime-module
;; Version: 0.1.0
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

;;; Commentary

;; Simple IME module for GNU Emacs (tr-emacs-ime-module) is an attempt in
;; GNU Emacs for Windows (MinGW/Cygwin) to make Japanese input with IME
;; easier to use by using the dynamic module in Emacs, e.g. for GNU binary
;; distributions/Cygwin packages that are not applied IME patches.

;;; Code:

;;;###autoload(when (and (eq window-system 'w32)
;;;###autoload           (not (fboundp 'ime-get-mode))
;;;###autoload           (string= module-file-suffix ".dll")
;;;###autoload           (locate-library "tr-ime-module2-helper"))
;;;###autoload  (require 'tr-ime-module2-helper)
;;;###autoload  (require 'w32-ime "w32-ime-for-tr-ime-module"))

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-emacs-ime-module.el ends here
