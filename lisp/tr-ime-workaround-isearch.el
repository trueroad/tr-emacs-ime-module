;;; tr-ime-workaround-isearch.el --- Workaround isearch-mode -*- lexical-binding: t -*-

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

(defgroup tr-ime-workaround nil
  "ワークアラウンド設定."
  :group 'tr-ime)

(defgroup tr-ime-workaround-isearch nil
  "\"isearch-mode\" (advanced)."
  :group 'tr-ime-workaround)

;;
;; isearch-mode 時の Alt + 半角/全角ワークアラウンド
;;

(defcustom tr-ime-workaround-isearch-delayed-update-time 0.0001
  "Alt + 半角/全角ワークアラウンドで使うタイマの待ち時間（秒）."
  :type 'float
  :group 'tr-ime-workaround-isearch)

(defun tr-ime-workaround-isearch--delayed-update ()
  "アイドル状態になったら \"isearch-mode\" のエコーエリアを再表示する.

advanced で \"isearch-mode\" 時に Alt + 半角/全角キー操作をすると、
なぜかエコーエリアが消えてしまう。
キー操作時に再表示させても効果が無い
（恐らくキー操作後にくるイベントか何かで消されている）ので、
Emacs がアイドル状態になったら動作するタイマで再表示させる。"
  (interactive)
  (run-with-idle-timer tr-ime-workaround-isearch-delayed-update-time
                       nil #'isearch-update))

(defun tr-ime-workaround-isearch--delayed-update-set (symb bool)
  "Alt + 半角/全角ワークアラウンドを動作させるか否か設定.

advanced で \"isearch-mode\" 時に Alt + 半角/全角キー操作をすると、
なぜかエコーエリアが消えてしまう対策のワークアラウンドを動作させるか否か
設定する。

SYMB には tr-ime-workaround-isearch-delayed-update-p を指定する。
BOOL が non-nil なら動作させる。それ以外なら停止させる。"
  (if bool
      (define-key isearch-mode-map [M-kanji]
        'tr-ime-workaround-isearch--delayed-update)
    (define-key isearch-mode-map [M-kanji] 'ignore))
  (set-default symb bool))

(defcustom tr-ime-workaround-isearch-delayed-update-p t
  "Alt + 半角/全角ワークアラウンドを動作させるか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

advanced で \"isearch-mode\" 時に Alt + 半角/全角キー操作をすると、
なぜかエコーエリアが消えてしまう。
キー操作時に再表示させても効果が無い
（恐らくキー操作後にくるイベントか何かで消されている）ので、
Emacs がアイドル状態になったら動作するタイマで再表示させるワークアラウンド。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-workaround-isearch--delayed-update-set
  :group 'tr-ime-workaround-isearch)

(defun tr-ime-workaround-isearch-unload-function ()
  "アンロードするため \"isearch-mode\" ワークアラウンドを無効にする."
  (let (_dummy)
    (tr-ime-workaround-isearch--delayed-update-set '_dummy nil)))

;;
;; provide
;;

(provide 'tr-ime-workaround-isearch)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-workaround-isearch.el ends here
