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
;; 他のライブラリ
;;

(autoload 'seq-drop-while "seq")

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

(defgroup w32-tr-ime-module-core-module2 nil
  "Module2 設定

Module2 を使用する際のコア機能の設定。
通常は設定変更しないこと。"
  :group 'w32-tr-ime-module-core)

(defgroup w32-tr-ime-module-ime-font nil
  "IME フォント (Module2)"
  :group 'w32-tr-ime-module)

(defgroup w32-tr-ime-module-isearch-mode nil
  "isearch-mode (Module2)"
  :group 'w32-tr-ime-module)

(defgroup w32-tr-ime-module-prefix-key nil
  "プレフィックスキー検出 (Module2)"
  :group 'w32-tr-ime-module)

(defgroup w32-tr-ime-module-setopenstatus nil
  "IME 状態変更通知による IM 状態同期 (Module2)"
  :group 'w32-tr-ime-module)

(defgroup w32-tr-ime-module-reconversion nil
  "RECONVERSION (Module2)"
  :group 'w32-tr-ime-module)

(defgroup w32-tr-ime-module-documentfeed nil
  "DOCUMENTFEED (Module2)"
  :group 'w32-tr-ime-module)

(defgroup w32-tr-ime-module-workaround nil
  "ワークアラウンド設定"
  :group 'w32-tr-ime-module)

(defgroup w32-tr-ime-module-workaround-isearch-mode nil
  "isearch-mode (Module2)"
  :group 'w32-tr-ime-module-workaround)

(defgroup w32-tr-ime-module-debug nil
  "デバッグ設定 (Module2)"
  :group 'w32-tr-ime-module)

;;
;; C++ 実装による DLL
;;

(declare-function tr-ime-modadv--install-message-hook-hwnd "tr-ime-modadv"
                  arg1)
(declare-function tr-ime-modadv--uninstall-message-hook-hwnd "tr-ime-modadv"
                  arg1)
(declare-function tr-ime-modadv--subclassify-hwnd "tr-ime-modadv"
                  arg1 &optional arg2)
(declare-function tr-ime-modadv--unsubclassify-hwnd "tr-ime-modadv"
                  arg1 &optional arg2)
(declare-function tr-ime-modadv--set-dispatch-thread-message "tr-ime-modadv"
                  arg1)
(declare-function tr-ime-modadv--set-font "tr-ime-modadv"
                  arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)
(declare-function tr-ime-modadv--set-composition-window "tr-ime-modadv"
                  arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10
                  arg11 arg12 arg13 arg14 arg15)
(declare-function tr-ime-modadv--set-startcomposition-defsubclassproc
                  "tr-ime-modadv"
                  arg1 arg2)
(declare-function tr-ime-modadv--set-prefix-keys "tr-ime-modadv"
                  arg1 arg2)
(declare-function tr-ime-modadv--resume-prefix-key "tr-ime-modadv")
(declare-function tr-ime-modadv--language-change-handler "tr-ime-modadv")
(declare-function tr-ime-modadv--notify-reconvert-string "tr-ime-modadv"
                  arg1 arg2 arg3)
(declare-function tr-ime-modadv--set-reconversion "tr-ime-modadv"
                  arg1 arg2)
(declare-function tr-ime-modadv--set-documentfeed "tr-ime-modadv"
                  arg1 arg2)
(declare-function tr-ime-modadv--get-dpi "tr-ime-modadv")
(declare-function tr-ime-modadv--set-verbose-level "tr-ime-modadv"
                  arg1)

;;
;; ウィンドウやバッファ状態の変更を通知するフックのエミュレーション
;;

;;
;; メッセージフックとサブクラス化
;;

;;
;; スレッドメッセージのディスパッチ
;;

;;
;; UI スレッドからの通知を Lisp で受け取る
;;

;;
;; IME 状態変更・状態取得関数のエミュレーション
;;

;;
;; IME フォント設定（未定義文字列のフォント）
;;

;;
;; isearch-mode 時の未確定文字列ウィンドウ位置設定
;;

;;
;; isearch-mode 時の Alt + 半角/全角ワークアラウンド
;;

;;
;; プレフィックスキー（C-x など）を検出して自動的に IME OFF する
;;

;;
;; IME 状態変更通知による IME/IM 状態同期
;;

;;
;; 再変換 (RECONVERSION)
;;

;;
;; 前後の確定済文字列を参照した変換 (DOCUMENTFEED)
;;

;;
;; デバッグ出力レベル
;;

;;
;; キー設定
;;

;; Alt + 半角全角の設定
(define-key global-map [M-kanji] 'ignore)

;; C-s (isearch-forward) などでの Alt + 半角全角の設定は
;; ワークアラウンドの中で実施済

;;
;; provide
;;

(provide 'tr-ime-module2-helper)
