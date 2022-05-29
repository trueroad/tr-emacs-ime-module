;;; tr-ime-debug.el --- Debug settings -*- lexical-binding: t -*-

;; Copyright (C) 2020, 2022 Masamichi Hosoda

;; Author: Masamichi Hosoda <trueroad@trueroad.jp>
;; URL: https://github.com/trueroad/tr-emacs-ime-module

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
;; ユーザ設定用
;;

(defgroup tr-ime-debug nil
  "デバッグ設定 (advanced)."
  :group 'tr-ime)

;;
;; デバッグ出力レベル
;;

(declare-function tr-ime-modadv--set-verbose-level "tr-ime-modadv"
                  (arg1))

(defun tr-ime-debug--verbose-level-set (symb level)
  "デバッグ出力レベルを設定する.

SYMB には tr-ime-debug-verbose-level をしている。
LEVEL はデバッグ出力レベルを 0 (none) から 6 (trace) の整数で指定するか、
nil を指定した場合は設定（変更）しない。"
  (when level
    (tr-ime-modadv--set-verbose-level level))
  (set-default symb level))

(defcustom tr-ime-debug-verbose-level nil
  "デバッグ出力レベル.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

Win32 API の OutputDebugString を使った、
デバッグメッセージの出力レベルを 0 (none) から 6 (trace) の整数で指定するか、
nil を指定した場合は設定（変更）しない。"
  :type '(choice (const :tag "none" 0)
                 (const :tag "fatal" 1)
                 (const :tag "error" 2)
                 (const :tag "warn" 3)
                 (const :tag "info" 4)
                 (const :tag "debug" 5)
                 (const :tag "trace" 6)
                 (const :tag "no set" nil))
  :set #'tr-ime-debug--verbose-level-set
  :group 'tr-ime-debug)

;;
;; provide
;;

(provide 'tr-ime-debug)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-debug.el ends here
