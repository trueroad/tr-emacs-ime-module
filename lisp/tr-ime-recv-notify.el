;;; tr-ime-recv-notify.el --- Receive notify -*- lexical-binding: t -*-

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

(defgroup tr-ime-recv-notify nil
  "UI スレッドからの通知 (advanced)."
  :group 'tr-ime-core)

;;
;; UI スレッドからの通知を Lisp で受け取る
;;

(declare-function tr-ime-modadv--language-change-handler "tr-ime-modadv")

(defun tr-ime-recv-notify--set (symb bool)
  "UI スレッドからの通知を Lisp で受け取るか否か設定する.

SYMB は tr-ime-recv-notify-p を指定する。
BOOL が non-nil なら UI スレッドからの通知を Lisp で受け取る。
そうでなければ受け取らない。"
  (if bool
      (define-key special-event-map [language-change]
        (lambda ()
          (interactive)
          (tr-ime-modadv--language-change-handler)))
    (define-key special-event-map [language-change] 'ignore))
  (set-default symb bool))

(defcustom tr-ime-recv-notify-p t
  "UI スレッドからの通知を Lisp で受け取るか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

注意：advanced の一部の機能は
UI スレッドからの通知を Lisp で受け取り、
Lisp での処理結果が UI スレッドへ通知されるまで待つものがある。
これらの機能が有効なまま本設定を無効にしてしまうと
Lisp が通知を受け取れなくなり処理もされず、
UI スレッドは返ってこない通知を待つため（一時的に）
ロックしてしまうことがある。
特別な目的が無い限りは non-nil (Enable) にしておくこと。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-recv-notify--set
  :group 'tr-ime-recv-notify)

(defun tr-ime-recv-notify-unload-function ()
  "アンロードするため UI スレッドからの通知を無効にする."
  (let (_dummy)
    (tr-ime-recv-notify--set '_dummy nil)))

;;
;; provide
;;

(provide 'tr-ime-recv-notify)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-recv-notify.el ends here
