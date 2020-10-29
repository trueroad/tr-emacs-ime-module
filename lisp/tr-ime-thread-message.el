;;; tr-ime-thread-message.el --- Thread message -*- lexical-binding: t -*-

;; Copyright (C) 2020 Masamichi Hosoda

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

(defgroup tr-ime nil
  "Emulator of GNU Emacs IME patch for Windows (tr-ime)"
  :group 'emacs)

(defgroup tr-ime-core nil
  "コア機能設定

コア機能の設定です。通常は設定変更しないでください。"
  :group 'tr-ime)

(defgroup tr-ime-thread-message nil
  "スレッドメッセージ (advanced)"
  :group 'tr-ime-core)

;;
;; スレッドメッセージのディスパッチ
;;

(declare-function tr-ime-modadv--set-dispatch-thread-message "tr-ime-modadv"
                  (arg1))

(defun tr-ime-thread-message--dispatch-set (symb bool)
  "スレッドメッセージをディスパッチするか否か設定する.

SYMB は tr-ime-thread-message-dispatch-p を設定する。
BOOL が non-nil ならスレッドメッセージをディスパッチする。
そうでなければスレッドメッセージをディスパッチしない。

GNU Emacs 27 や 28 の UI スレッドは、
スレッドメッセージをディスパッチしないため IME の動作に不具合が発生する
（タスクトレイの IME 状態表示アイコンが変わらない等）。
そこで、本設定によって Emacs の代わりにメッセージフックが
スレッドメッセージをディスパッチするようにできる。

ただし、将来の Emacs でスレッドメッセージをディスパッチするように修正されたら
本設定を nil にすること。
さもなければひとつのスレッドメッセージを
二重にディスパッチしてしまうことになり、
Emacs の動作がおかしくなってしまう。"
  (tr-ime-modadv--set-dispatch-thread-message bool)
  (set-default symb bool))

(defcustom tr-ime-thread-message-dispatch-p t
  "スレッドメッセージをディスパッチするか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

GNU Emacs 27 や 28 の UI スレッドは、
スレッドメッセージがディスパッチされない。
これによって IME の動作に不具合が発生する
（タスクトレイの IME 状態表示アイコンが変わらない等）。
そこで、本設定によってメッセージフックが
スレッドメッセージをディスパッチするようにできる。

ただし、将来の Emacs で
スレッドメッセージをディスパッチするようになったら
本設定を nil (Disable) にすること。
さもなければひとつのスレッドメッセージを
二重にディスパッチしてしまうことになり、
Emacs の動作がおかしくなってしまう。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-thread-message--dispatch-set
  :group 'tr-ime-thread-message)

;;
;; provide
;;

(provide 'tr-ime-thread-message)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-thread-message.el ends here
