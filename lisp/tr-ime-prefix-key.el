;;; tr-ime-prefix-key.el --- Handle prefix key -*- lexical-binding: t -*-

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

(defgroup tr-ime-prefix-key nil
  "プレフィックスキー検出 (advanced)."
  :group 'tr-ime)

;;
;; 検出対象リスト
;;

(declare-function tr-ime-modadv--set-prefix-keys "tr-ime-modadv"
                  (arg1 arg2))

(defvar tr-ime-prefix-key-p t)

(defun tr-ime-prefix-key--list-set (symb settings)
  "プレフィックスキー検出対象リストを設定する.

SYMB は tr-ime-prefix-key-list を指定する。
SETTINGS はプレフィックスキーとして検出したいコードのリスト。"
  (set-default symb settings)
  (when tr-ime-prefix-key-p
    (tr-ime-modadv--set-prefix-keys
     (string-to-number (frame-parameter nil 'window-id))
     settings)))

(defcustom tr-ime-prefix-key-list '(#x20058 #x20048 #x20043 #x1b)
  "プレフィックスキー検出対象リスト.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

プレフィックスキーとして検出したいコードのリスト。
コードは上位 16 bit が修飾キー、下位 16 bit が修飾されるキーの
バーチャルキーコードを指定する。
修飾キーは Shift (#x10000), Ctrl (#x20000), Alt (#x40000) の
ビット論理和で指定する。バーチャルキーコードは Windows のものを指定する。

例えば Ctrl-x は Ctrl の修飾キー #x20000 と、
X キーのバーチャルキーコード #x58 のビット論理和なので #x20058 を指定する。
Ctrl-Alt-x であれば、さらに Alt の修飾キーを含めて #x60058 を指定する。"
  :type '(repeat integer)
  :set #'tr-ime-prefix-key--list-set
  :group 'tr-ime-prefix-key)

;;
;; プレフィックスキー（C-x など）を検出して自動的に IME off する
;;

(declare-function tr-ime-modadv--resume-prefix-key "tr-ime-modadv")

(defun tr-ime-prefix-key--set (symb bool)
  "プレフィックスキーを検出して自動的に IME off するか否か設定する.

SYMB には tr-ime-prefix-key-p を指定する。
BOOL が non-nil ならプレフィックスキーを検出して IME off する。
あわせて standard で同様な機能を持つワークアラウンドを無効にする。
BOOL が nil なら停止する。"
  (if bool
      (when (and (boundp 'tr-ime-enabled-features)
                 (eq tr-ime-enabled-features 'advanced))
        ;; ここで custom-set-variables を使うと init.el に
        ;; 設定が書き込まれてしまうので直接 setter を使って無効に設定する
        (when (fboundp 'tr-ime-workaround-prefix-key--set)
          (let (_dummy)
            (tr-ime-workaround-prefix-key--set '_dummy nil)))
        (tr-ime-modadv--set-prefix-keys
         (string-to-number (frame-parameter nil 'window-id))
         tr-ime-prefix-key-list)
        (add-hook 'pre-command-hook #'tr-ime-modadv--resume-prefix-key))
    (when (fboundp 'tr-ime-modadv--set-prefix-keys)
      (tr-ime-modadv--set-prefix-keys
       (string-to-number (frame-parameter nil 'window-id)) nil))
    (remove-hook 'pre-command-hook #'tr-ime-modadv--resume-prefix-key))
  (set-default symb bool))

(defcustom tr-ime-prefix-key-p t
  "プレフィックスキーを検出して自動的に IME off するか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

コマンドのキーシーケンスになる最初のキーである
プレフィックスキーを検出すると、
自動的に IME off にして、コマンド終了後に IME 状態を戻す機能。

本機能を有効にすると standard 用の
プレフィックスキー検出ワークアラウンドが無効になる。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-prefix-key--set
  :group 'tr-ime-prefix-key)

(defun tr-ime-prefix-key-unload-function ()
  "アンロードするためプレフィックスキー検出を無効にする."
  (let (_dummy)
    (tr-ime-prefix-key--set '_dummy nil)))

;;
;; provide
;;

(provide 'tr-ime-prefix-key)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-prefix-key.el ends here
