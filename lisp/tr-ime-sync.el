;;; tr-ime-sync.el --- Sync IME openstatus -*- lexical-binding: t -*-

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

(require 'tr-ime-hook)
(require 'tr-ime-openstatus)

;;
;; ユーザ設定用
;;

(defgroup tr-ime-sync nil
  "IME 状態変更通知による IME/IM 状態同期 (advanced)."
  :group 'tr-ime)

;;
;; 状態同期
;;

(defun tr-ime-sync--setopenstatus ()
  "IME 状態変更通知時に呼ばれる関数.

まずフックエミュレーション関数を呼び、
これによってウィンドウやバッファの切り替え未検出があったら、
アブノーマルフックが呼ばれて、IME/IM 状態が整える。

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

(defun tr-ime-sync--set (symb bool)
  "IME 状態変更通知による IM 状態同期をするか否か設定する.

SYMB には tr-ime-sync-p を指定する。
BOOL が non-nil なら IME 状態変更通知による IM 状態同期をする。
そうでなければ同期しない。"
  (if bool
      (progn
        ;; ここで custom-set-variables を使うと init.el に
        ;; 設定が書き込まれてしまうので直接 setter を使って無効に設定する
        (when (fboundp 'tr-ime-workaround-inconsistent--set)
          (let (_dummy)
            (tr-ime-workaround-inconsistent--set '_dummy nil)))
        (add-hook 'tr-ime-modadv--setopenstatus-hook
                  #'tr-ime-sync--setopenstatus))
    (remove-hook 'tr-ime-modadv--setopenstatus-hook
                 #'tr-ime-sync--setopenstatus))
  (set-default symb bool))

(defcustom tr-ime-sync-p t
  "IME 状態変更通知による IM 状態同期をするか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

Emacs 側トリガ（C-\\ やウィンドウ・バッファの切り替えなど）だけでなく、
IME 側トリガ（半角/全角キーやマウスでの切り替えなど）も含め、
IME 状態変更通知がきた時に、IME/IM 状態同期をする機能。

本機能を有効にすると standard 用の
IME 状態食い違い検出ワークアラウンドが無効になる。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-sync--set
  :group 'tr-ime-sync)

(defun tr-ime-sync-unload-function ()
  "アンロードするため IME 状態変更通知による IM 状態同期を無効にする."
  (let (_dummy)
    (tr-ime-sync--set '_dummy nil)))

;;
;; provide
;;

(provide 'tr-ime-sync)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-sync.el ends here
