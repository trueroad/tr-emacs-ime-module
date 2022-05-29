;;; tr-ime-workaround-prefix-key.el --- Prefix key workaround -*- lexical-binding: t -*-

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
;; require
;;

(require 'tr-ime-openstatus)

;;
;; ユーザ設定用
;;

(defgroup tr-ime-workaround nil
  "ワークアラウンド設定."
  :group 'tr-ime)

(defgroup tr-ime-workaround-prefix-key nil
  "プレフィックスキー検出 (standard)

advanced では、このワークアラウンドではなく、
advanced のプレフィックスキー検出を使うこと。"
  :group 'tr-ime-workaround)

;;
;; プレフィックスキー（C-x など）を検出して IME OFF するワークアラウンド
;;

(defcustom tr-ime-workaround-prefix-key-polling-time 0.1
  "ポーリング時間（秒）."
  :type 'float
  :group 'tr-ime-workaround-prefix-key)

(defcustom tr-ime-workaround-prefix-key-list '(?\C-x ?\C-h ?\C-c ?\e)
  "検出対象リスト.

プレフィックスキーとして検出したいキーのリスト。"
  :type '(repeat integer)
  :group 'tr-ime-workaround-prefix-key)

(defvar tr-ime-workaround-prefix-key--detected-p nil
  "プレフィックスキー検出フラグ.")
(defvar tr-ime-workaround-prefix-key--before-ime-mode-p nil
  "プレフィックスキー検出時の IME 状態保存用.")
(defvar tr-ime-workaround-prefix-key--timer nil
  "プレフィックスキー検出用タイマ.")

(defun tr-ime-workaround-prefix-key--polling-handler ()
  "プレフィックスキー検出のためのポーリングで呼ばれる関数.

未検出かつ最後に押されたキーが検出対象リストのいずれかだったら、
IME 状態を保存してから IME off にし、フラグを検出済にする。"
  (unless tr-ime-workaround-prefix-key--detected-p
    (let ((keys (this-single-command-keys)))
      (when (and (not (equal [] keys))
                 (member (aref keys 0) tr-ime-workaround-prefix-key-list))
        (setq tr-ime-workaround-prefix-key--before-ime-mode-p
              (tr-ime-openstatus-get-mode))
        (tr-ime-openstatus-force-off)
        (setq tr-ime-workaround-prefix-key--detected-p t)))))

(defun tr-ime-workaround-prefix-key--restore-ime-mode ()
  "自動 IME off から IME 状態を復帰させる関数.

Emacs の標準的なフックの一つ \"pre-command-hook\" に登録する。
\"pre-command-hook\" によって、ほとんどのコマンドの動作後に呼ばれる。

この関数の動作は、
プレフィックスキー検出済であったら未検出に変え、
検出時の IME 状態が on であれば IME on に復帰する。
未検出であったら何もしない。"
  (when tr-ime-workaround-prefix-key--detected-p
    (setq tr-ime-workaround-prefix-key--detected-p nil)
    (when tr-ime-workaround-prefix-key--before-ime-mode-p
      (tr-ime-openstatus-force-on))))

;;
;; 設定用
;;

(defun tr-ime-workaround-prefix-key--set (symb bool)
  "プレフィックスキー検出ワークアラウンドを動作させるか否か設定する.

SYMB は tr-ime-workaround-prefix-key-p を指定すること。
BOOL が non-nil ならワークアラウンド有効で動作する。
そうでなければワークアラウンド無効で動作しない。"
  (if bool
      (when (and (boundp 'tr-ime-enabled-features)
                 (eq tr-ime-enabled-features 'standard))
        (setq tr-ime-workaround-prefix-key--detected-p nil)
        (add-hook 'pre-command-hook
                  #'tr-ime-workaround-prefix-key--restore-ime-mode)
        (when tr-ime-workaround-prefix-key--timer
          (cancel-timer tr-ime-workaround-prefix-key--timer))
        (setq tr-ime-workaround-prefix-key--timer
              (run-with-idle-timer
               tr-ime-workaround-prefix-key-polling-time t
               #'tr-ime-workaround-prefix-key--polling-handler)))
    (remove-hook 'pre-command-hook
                 #'tr-ime-workaround-prefix-key--restore-ime-mode)
    (when tr-ime-workaround-prefix-key--timer
      (cancel-timer tr-ime-workaround-prefix-key--timer))
    (setq tr-ime-workaround-prefix-key--timer nil))
  (set-default symb bool))

(defcustom tr-ime-workaround-prefix-key-p t
  "プレフィックスキー検出ワークアラウンドを動作させるか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

コマンドのキーシーケンスになる最初のキーである
プレフィックスキーをタイマによるポーリングで検出すると、
自動的に IME off にして、コマンド開始前に IME 状態を戻す機能。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-workaround-prefix-key--set
  :group 'tr-ime-workaround-prefix-key)

(defun tr-ime-workaround-prefix-key-unload-function ()
  "アンロードするためプレフィックスキー検出ワークアラウンドを無効にする."
  (let (_dummy)
    (tr-ime-workaround-prefix-key--set '_dummy nil)))

;;
;; provide
;;

(provide 'tr-ime-workaround-prefix-key)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-workaround-prefix-key.el ends here
