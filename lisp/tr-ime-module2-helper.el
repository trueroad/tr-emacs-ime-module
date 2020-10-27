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

(defun w32-tr-ime-module-notify-reconvert-string ()
  "RECONVERTSTRING 構造体用の材料を収集して UI スレッドへ通知する

point のある行全体の文字列と、文字列中の point 位置を収集し、
Module2 の C++ 実装である tr-ime-modadv--notify-reconvert-string 関数を呼び、
UI スレッドへ通知する。
ノーマルフック tr-ime-modadv--reconvertstring-hook および
tr-ime-modadv--documentfeed-hook に登録して使う。"
  (tr-ime-modadv--notify-reconvert-string
   (string-to-number (frame-parameter nil 'window-id))
   (buffer-substring-no-properties
    (line-beginning-position) (line-end-position))
   (- (point) (line-beginning-position))))

(defun w32-tr-ime-module-reconversion-p-set (symb bool)
  "再変換 (RECONVERSION) 動作を行うか否か設定する"
  (if bool
      (progn
        (add-hook 'tr-ime-modadv--reconvertstring-hook
                  #'w32-tr-ime-module-notify-reconvert-string)
        (tr-ime-modadv--set-reconversion
         (string-to-number (frame-parameter nil 'window-id)) t))
    (tr-ime-modadv--set-reconversion
     (string-to-number (frame-parameter nil 'window-id)) nil)
    (remove-hook 'tr-ime-modadv--reconvertstring-hook
                 #'w32-tr-ime-module-notify-reconvert-string))
  (set-default symb bool))

(defcustom w32-tr-ime-module-reconversion-p t
  "再変換 (RECONVERSION) 動作を行うか否か

この設定を変更する場合には custom-set-variables を使うこと。

確定済文字列にカーソルを置いて変換キーを押すと、
カーソルのあった場所の確定済文字列が未確定文字列になって、
再変換できるようになる機能。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-reconversion-p-set
  :group 'w32-tr-ime-module-reconversion)

;;
;; 前後の確定済文字列を参照した変換 (DOCUMENTFEED)
;;

(defun w32-tr-ime-module-documentfeed-p-set (symb bool)
  "前後の確定済文字列を参照した変換 (DOCUMENTFEED) 動作を行うか否か設定する"
  (if bool
      (progn
        (add-hook 'tr-ime-modadv--documentfeed-hook
                  #'w32-tr-ime-module-notify-reconvert-string)
        (tr-ime-modadv--set-documentfeed
         (string-to-number (frame-parameter nil 'window-id)) t))
    (tr-ime-modadv--set-documentfeed
     (string-to-number (frame-parameter nil 'window-id)) nil)
    (remove-hook 'tr-ime-modadv--documentfeed-hook
                 #'w32-tr-ime-module-notify-reconvert-string))
  (set-default symb bool))

(defcustom w32-tr-ime-module-documentfeed-p t
  "前後の確定済文字列を参照した変換 (DOCUMENTFEED) 動作を行うか否か

この設定を変更する場合には custom-set-variables を使うこと。

確定済文字列のあるところにカーソルを置いて文字を入力・変換すると、
カーソルのあった場所の確定済文字列によって変換候補が変わる機能。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-documentfeed-p-set
  :group 'w32-tr-ime-module-documentfeed)

;;
;; デバッグ出力レベル
;;

(defun w32-tr-ime-module-verbose-level-set (symb level)
  "Module2 のデバッグ出力レベルを設定する"
  (when level
    (tr-ime-modadv--set-verbose-level level))
  (set-default symb level))

(defcustom w32-tr-ime-module-verbose-level nil
  "Module2 のデバッグ出力レベル

この設定を変更する場合には custom-set-variables を使うこと。

Win32 API の OutputDebugString を使った、
デバッグメッセージの出力レベル。"
  :type '(choice (const :tag "none" 0)
                 (const :tag "fatal" 1)
                 (const :tag "error" 2)
                 (const :tag "warn" 3)
                 (const :tag "info" 4)
                 (const :tag "debug" 5)
                 (const :tag "trace" 6)
                 (const :tag "no set" nil))
  :set #'w32-tr-ime-module-verbose-level-set
  :group 'w32-tr-ime-module-debug)

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
