;;; tr-ime-workaround-inconsistent.el --- Fix inconsistent workaround -*- lexical-binding: t -*-

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
;; requires
;;

(require 'tr-ime-openstatus)
(require 'tr-ime-hook)

;;
;; ユーザ設定用
;;

(defgroup tr-ime-workaround nil
  "ワークアラウンド設定."
  :group 'tr-ime)

(defgroup tr-ime-workaround-inconsistent nil
  "IME 状態食い違い検出 (standard)

advanced では、このワークアラウンドではなく、
advanced の IME 状態変更通知による IM 状態同期を使うこと。"
  :group 'tr-ime-workaround)

;;
;; IME 状態の食い違いを検出して修正するワークアラウンド
;;

(defcustom tr-ime-workaround-inconsistentent-polling-time 1.0
  "IME 状態食い違い検出修正ワークアラウンド用ポーリング時間（秒）."
  :type 'float
  :group 'tr-ime-workaround-inconsistent)

(defvar tr-ime-workaround-inconsistent--timer nil
  "IME 状態食い違い検出修正ワークアラウンド用タイマ.")

(defun tr-ime-workaround-inconsistent--polling-handler ()
  "IME 状態食い違い検出修正ワークアラウンドのためのポーリングで呼ばれる関数.

まずフックエミュレーション関数を呼ぶ。
これによってウィンドウやバッファの切り替え未検出があったら、
アブノーマルフックが呼ばれて、IME/IM 状態が整えられる。

その上で IME 状態と IM 状態が食い違ったら IM 状態を反転して一致させる。
これにより、IME 側トリガの状態変更を IM に反映させる。"
  (tr-ime-hook-check)
  (let ((ime-status (tr-ime-openstatus-get-mode)))
    (cond ((and ime-status
                (not current-input-method))
           (activate-input-method "W32-IME"))
          ((and (not ime-status)
                current-input-method)
           (deactivate-input-method)))))

;;
;; 設定用
;;

(defun tr-ime-workaround-inconsistent--set (symb bool)
  "IME 状態食い違い検出修正ワークアラウンドを動作させるか否か設定する.

SYMB は tr-ime-workaround-inconsistent-p を指定すること。
BOOL が non-nil ならワークアラウンド有効で動作する。
そうでなければワークアラウンド無効で動作しない。"
  (when tr-ime-workaround-inconsistent--timer
    (cancel-timer tr-ime-workaround-inconsistent--timer)
    (setq tr-ime-workaround-inconsistent--timer nil))
  (when (and bool
             (boundp 'tr-ime-enabled-features)
             (eq tr-ime-enabled-features 'standard))
    (setq tr-ime-workaround-inconsistent--timer
          (run-at-time
           t tr-ime-workaround-inconsistentent-polling-time
           #'tr-ime-workaround-inconsistent--polling-handler)))
  (set-default symb bool))

(defcustom tr-ime-workaround-inconsistent-p t
  "IME 状態食い違い検出修正ワークアラウンドを動作させるか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

IME 側トリガの状態変更（半角/全角キーやマウスでの切り替え）を
定期的なタイマによるポーリングで検出して IM 側を同期させるための機構。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-workaround-inconsistent--set
  :group 'tr-ime-workaround-inconsistent)

(defun tr-ime-workaround-inconsistent-unload-function ()
  "アンロードするため IME 状態食い違い検出修正ワークアラウンドを無効にする."
  (let (_dummy)
    (tr-ime-workaround-inconsistent--set '_dummy nil)))

;;
;; provide
;;

(provide 'tr-ime-workaround-inconsistent)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-workaround-inconsistent.el ends here
