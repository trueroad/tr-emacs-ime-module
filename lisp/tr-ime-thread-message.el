;;; tr-ime-thread-message.el --- Thread message -*- lexical-binding: t -*-

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

(defgroup tr-ime-thread-message nil
  "スレッドメッセージ (advanced)."
  :group 'tr-ime-core)

;;
;; スレッドメッセージのディスパッチ
;;

(declare-function tr-ime-modadv--set-dispatch-thread-message "tr-ime-modadv"
                  (arg1))
(declare-function tr-ime-modadv--set-dispatch-thread-wm-timer "tr-ime-modadv"
                  (arg1))
(defvar tr-ime-thread-message-dispatch-p)
(defvar tr-ime-thread-message-dispatch-wm-timer-p)

(defun tr-ime-thread-message--dispatch-set (symb bool)
  "すべてのスレッドメッセージをディスパッチするか否か設定する.

SYMB は tr-ime-thread-message-dispatch-p を設定する。
BOOL が non-nil ならすべてのスレッドメッセージをディスパッチする。
そうでなければディスパッチしない。

本関数の設定より
tr-ime-thread-message--dispatch-wm-timer-set 関数の設定が優先される。
本関数による設定を有効にしたい場合は
tr-ime-thread-message--dispatch-wm-timer-set の BOOL を
nil にする必要がある。

GNU Emacs 27 や 28 の UI スレッドは、
スレッドメッセージをディスパッチしないため IME の動作に不具合が発生する
（タスクトレイの IME 状態表示アイコンが変わらない等）。
そこで、本設定によって Emacs の代わりにメッセージフックが
スレッドメッセージをディスパッチするようにできる。

大抵の場合はすべてのスレッドメッセージではなく
WM_TIMER だけをディスパッチすればよいので、本関数ではなく
tr-ime-thread-message--dispatch-wm-timer-set を使った方が良い。
WM_TIMER だけではうまくいかないときに本関数を使う。

ただし、将来の Emacs でスレッドメッセージをディスパッチするように修正されたら
本設定を nil にすること。
さもなければひとつのスレッドメッセージを
二重にディスパッチしてしまうことになり、
Emacs の動作がおかしくなってしまう。"
  (if (and (boundp 'tr-ime-thread-message-dispatch-wm-timer-p)
           (not tr-ime-thread-message-dispatch-wm-timer-p))
      (tr-ime-modadv--set-dispatch-thread-message bool)
    (tr-ime-modadv--set-dispatch-thread-message nil))
  (set-default symb bool))

(defun tr-ime-thread-message--dispatch-wm-timer-set (symb bool)
  "スレッドメッセージ WM_TIMER をディスパッチするか否か設定する.

SYMB は tr-ime-thread-message-dispatch-wm-timer-p を設定する。
BOOL が non-nil ならスレッドメッセージ WM_TIMER をディスパッチして
WM_NULL にすり替えて握りつぶす。そうでなければしない。

本関数の設定は tr-ime-thread-message--dispatch-set よりも優先する。
tr-ime-thread-message--dispatch-set 関数による設定を有効にしたい場合は
本関数の BOOL を nil にする必要がある。

GNU Emacs 27 や 28 の UI スレッドは、
スレッドメッセージをディスパッチしないため IME の動作に不具合が発生する
（タスクトレイの IME 状態表示アイコンが変わらない等）。
そこで、本設定によって Emacs の代わりにメッセージフックが
スレッドメッセージ WM_TIMER をディスパッチするようにできる。

大抵の場合はすべてのスレッドメッセージではなく
WM_TIMER だけをディスパッチすればよい。
また、WM_NULL にすり替えて握りつぶすことにより
将来の Emacs でスレッドメッセージをディスパッチするように
修正されても二重ディスパッチにならないようにしている。

WM_TIMER だけではうまくいかない時には本関数の BOOL を nil にして
tr-ime-thread-message--dispatch-set を使って
すべてのスレッドメッセージをディスパッチするとよいかもしれない。"
  (let (_dummy)
    (if bool
        (tr-ime-thread-message--dispatch-set '_dummy nil)
      (if (boundp 'tr-ime-thread-message-dispatch-p)
          (tr-ime-thread-message--dispatch-set
           '_dummy tr-ime-thread-message-dispatch-p))))
  (tr-ime-modadv--set-dispatch-thread-wm-timer bool)
  (set-default symb bool))

(defcustom tr-ime-thread-message-dispatch-p nil
  "すべてのスレッドメッセージをディスパッチするか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

本設定より
tr-ime-thread-message--dispatch-wm-timer-p 設定が優先される。
本設定を有効にしたい場合は
tr-ime-thread-message--dispatch-wm-timer-p を nil にする必要がある。

GNU Emacs 27 や 28 の UI スレッドは、
スレッドメッセージがディスパッチされない。
これによって IME の動作に不具合が発生する
（タスクトレイの IME 状態表示アイコンが変わらない等）。
そこで、本設定によってメッセージフックが
スレッドメッセージをディスパッチするようにできる。

大抵の場合はすべてのスレッドメッセージではなく
WM_TIMER だけをディスパッチすればよいので、本設定ではなく
tr-ime-thread-message-dispatch-wm-timer-p を使った方が良い。
WM_TIMER だけではうまくいかないときに本設定を使う。

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

(defcustom tr-ime-thread-message-dispatch-wm-timer-p t
  "スレッドメッセージ WM_TIMER をディスパッチするか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

本設定は tr-ime-thread-message--dispatch-p よりも優先する。
tr-ime-thread-message--dispatch-p の設定を有効にしたい場合は
本設定 を nil にする必要がある。

GNU Emacs 27 や 28 の UI スレッドは、
スレッドメッセージがディスパッチされない。
これによって IME の動作に不具合が発生する
（タスクトレイの IME 状態表示アイコンが変わらない等）。
そこで、本設定によってメッセージフックが
スレッドメッセージ WM_TIMER をディスパッチするようにできる。

大抵の場合はすべてのスレッドメッセージではなく
WM_TIMER だけをディスパッチすればよい。
また、WM_NULL にすり替えて握りつぶすことにより
将来の Emacs でスレッドメッセージをディスパッチするように
修正されても二重ディスパッチにならないようにしている。

WM_TIMER だけではうまくいかない時には本設定を nil にして
tr-ime-thread-message-dispatch-p を使って
すべてのスレッドメッセージをディスパッチするとよいかもしれない。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-thread-message--dispatch-wm-timer-set
  :group 'tr-ime-thread-message)

(defun tr-ime-thread-message-unload-function ()
  "アンロードするためスレッドメッセージのディスパッチを無効にする."
  (let (_dummy)
    (tr-ime-thread-message--dispatch-set '_dummy nil)
    (tr-ime-thread-message--dispatch-wm-timer-set '_dummy nil)))

;;
;; provide
;;

(provide 'tr-ime-thread-message)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-thread-message.el ends here
