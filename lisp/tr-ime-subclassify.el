;;; tr-ime-subclassify.el --- Subclassify frame -*- lexical-binding: t -*-

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

(defgroup tr-ime-core nil
  "コア機能設定.

コア機能の設定です。通常は設定変更しないでください。"
  :group 'tr-ime)

(defgroup tr-ime-subclassify nil
  "メッセージフックとサブクラス化 (advanced)."
  :group 'tr-ime-core)

;;
;; メッセージフックとサブクラス化
;;

(declare-function tr-ime-modadv--install-message-hook-hwnd "tr-ime-modadv"
                  (arg1))
(declare-function tr-ime-modadv--uninstall-message-hook-hwnd "tr-ime-modadv"
                  (arg1))
(declare-function tr-ime-modadv--subclassify-hwnd "tr-ime-modadv"
                  (arg1 &optional arg2))
(declare-function tr-ime-modadv--unsubclassify-hwnd "tr-ime-modadv"
                  (&optional arg1 arg2))
(declare-function tr-ime-modadv--exists-subclassified "tr-ime-modadv")

(defun tr-ime-subclassify--set (symb bool)
  "IME 制御のためメッセージフックしてサブクラス化するか否か設定する.

SYMB は tr-ime-subclassify-p を指定する。
BOOL が non-nil ならメッセージフックしてサブクラス化する。
そうでなければサブクラス解除してメッセージフックを停止する。

注意：advanced のほとんどの機能は
メッセージフックとサブクラス化を前提としており、
これらが有効でなければ機能しないだけではなく、
設定変更すらできないものも存在する。"
  (if bool
      (progn
        (tr-ime-modadv--install-message-hook-hwnd
         (string-to-number (frame-parameter nil 'window-id)))
        (tr-ime-modadv--subclassify-hwnd
         (string-to-number (frame-parameter nil 'window-id)) nil))
    (tr-ime-modadv--unsubclassify-hwnd
     (string-to-number (frame-parameter nil 'window-id)) nil)
    ;; サブクラス解除は非同期に実施されるが、
    ;; 解除前にメッセージフック停止すると解除できなくなるので
    ;; 存在確認し待機する。
    (let ((counter 0))
      (while (and (< counter 10)
                  (tr-ime-modadv--exists-subclassified))
        (thread-yield)
        (setq counter (1+ counter))))
    (tr-ime-modadv--uninstall-message-hook-hwnd
     (string-to-number (frame-parameter nil 'window-id))))
  (set-default symb bool))

(defcustom tr-ime-subclassify-p t
  "IME 制御のためメッセージフックしてサブクラス化するか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

注意：advanced のほとんどの機能は
メッセージフックとサブクラス化を前提としており、
これらが有効でなければ機能しないだけではなく、
設定変更すらできないものが存在する。
特別な目的が無い限りは non-nil (Enable) にしておくこと。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-subclassify--set
  :group 'tr-ime-subclassify)

(defun tr-ime-subclassify-unload-function ()
  "アンロードするためメッセージフックとサブクラス化を無効にする."
  (let (_dummy)
    (tr-ime-subclassify--set '_dummy nil)))

;;
;; provide
;;

(provide 'tr-ime-subclassify)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-subclassify.el ends here
