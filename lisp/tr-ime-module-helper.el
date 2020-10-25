;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

;; This file is part of
;; Simple IME module for GNU Emacs (tr-emacs-ime-module)
;; https://github.com/trueroad/tr-emacs-ime-module
;;
;; Copyright (C) 2020 Masamichi Hosoda
;;
;; Simple IME module for GNU Emacs (tr-emacs-ime-module)
;; is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Simple IME module for GNU Emacs (tr-emacs-ime-module)
;; is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with tr-emacs-ime-module.
;; If not, see <https://www.gnu.org/licenses/>.

;;
;; ユーザ設定用
;;

(defgroup w32-tr-ime-module nil
  "Simple IME module for GNU Emacs (tr-emacs-ime-module)"
  :group 'W32-IME)

(defgroup w32-tr-ime-module-core nil
  "コア機能設定

モジュールを使用する際のコア機能の設定。
通常は設定変更しないこと。"
  :group 'w32-tr-ime-module)

(defgroup w32-tr-ime-module-core-emacs28 nil
  "Emacs 28 以降向け設定

Emacs 28 以降で Module2 を使わない場合のコア機能の設定。
通常は設定変更しないこと。"
  :group 'w32-tr-ime-module-core)

(defgroup w32-tr-ime-module-workaround nil
  "ワークアラウンド設定"
  :group 'w32-tr-ime-module)

(defgroup w32-tr-ime-module-workaround-prefix-key nil
  "プレフィックスキー検出

Module2 を使用するなら、このワークアラウンドではなく
Module2 のプレフィックスキー検出を使うこと。"
  :group 'w32-tr-ime-module-workaround)

(defgroup w32-tr-ime-module-workaround-inconsist-ime nil
  "IME 状態食い違い検出

Module2 を使用するなら、このワークアラウンドではなく
Module2 の IME 状態変更通知による IM 状態同期が利用できる。"
  :group 'w32-tr-ime-module-workaround)

;;
;; IME 状態変更・状態取得関数のエミュレーション
;;

;;(load "tr-ime-openstatus")

(declare-function ime-force-on "tr-ime-openstatus.el" &optional _dummy)
(declare-function ime-force-off "tr-ime-openstatus.el" &optional _dummy)
(declare-function ime-get-mode "tr-ime-openstatus.el")

;;
;; ウィンドウやバッファ状態の変更を通知するフックのエミュレーション
;;

;;(load "tr-ime-hook")

(declare-function tr-ime-hook-check "tr-ime-hook.el")

;;
;; プレフィックスキー（C-x など）を検出して IME OFF するワークアラウンド
;;

;;(load "tr-ime-workaround-prefix-key")

;;
;; IME 状態の食い違いを検出して修正するワークアラウンド
;;

;;(load "tr-ime-workaround-inconsistent")

;;
;; キー設定
;;

;; Alt + 半角全角で IME だけでなく IM も切り替わるようにする
(define-key global-map [M-kanji] 'toggle-input-method)

;; C-s (isearch-forward) などでも Alt + 半角全角が効くようにする
(define-key isearch-mode-map [M-kanji] 'isearch-toggle-input-method)

;;
;; provide
;;

(provide 'tr-ime-module-helper)
