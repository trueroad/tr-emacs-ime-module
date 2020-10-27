;;; tr-ime-download.el --- Download files -*- lexical-binding: t -*-

;; Copyright (C) 2020 Masamichi Hosoda

;; Author: Masamichi Hosoda <trueroad@trueroad.jp>
;; URL: https://github.com/trueroad/tr-emacs-ime-module
;; Version: 0.3.50
;; Package-Requires: ((emacs "27.1"))

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

;; This file is part of
;; Emulator of GNU Emacs IME patch for Windows (tr-ime).
;; See tr-ime.el

;;; Code:

;;
;; モジュール DLL のダウンロードを案内
;;

(defun tr-ime-download-mod-file (name)
  "モジュール DLL のダウンロードを案内する.

NAME にモジュール DLL のファイル名拡張子なしを指定する。
現在はダウンロードの案内を出すだけだが、
将来的には実際にダウンロードしてロードするところまで実装する予定。"
  (error "%s.dll is not found.

Download or build it and put it in the directory specified in \"load-path.\"
See https://github.com/trueroad/tr-emacs-ime-module" name))

;;
;; provide
;;

(provide 'tr-ime-download)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-download.el ends here
