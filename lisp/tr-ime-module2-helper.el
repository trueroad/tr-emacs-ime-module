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

;;
;; C++ 実装による DLL をロードする
;;

(unless (featurep 'tr-ime-module2)
  (load (concat "tr-ime-module2-" system-configuration) t))

(declare-function w32-tr-ime-install-message-hook-hwnd "tr-ime-module2"
                  arg1)
(declare-function w32-tr-ime-uninstall-message-hook-hwnd "tr-ime-module2"
                  arg1)
(declare-function w32-tr-ime-subclassify-hwnd "tr-ime-module2"
                  arg1 &optional arg2)
(declare-function w32-tr-ime-unsubclassify-hwnd "tr-ime-module2"
                  arg1 &optional arg2)
(declare-function w32-tr-ime-set-dispatch-thread-message "tr-ime-module2"
                  arg1)
(declare-function w32-tr-ime-setopenstatus2 "tr-ime-module2"
                  arg1 arg2)
(declare-function w32-tr-ime-getopenstatus2 "tr-ime-module2"
                  arg1)
(declare-function w32-tr-ime-set-font "tr-ime-module2"
                  arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)
(declare-function w32-tr-ime-set-composition-window "tr-ime-module2"
                  arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10
                  arg11 arg12 arg13 arg14 arg15)
(declare-function w32-tr-ime-set-startcomposition-defsubclassproc
                  "tr-ime-module2"
                  arg1 arg2)
(declare-function w32-tr-ime-set-prefix-keys "tr-ime-module2"
                  arg1 arg2)
(declare-function w32-tr-ime-resume-prefix-key "tr-ime-module2")
(declare-function w32-tr-ime-language-change-handler "tr-ime-module2")
(declare-function w32-tr-ime-notify-reconvert-string "tr-ime-module2"
                  arg1 arg2 arg3)
(declare-function w32-tr-ime-set-reconversion "tr-ime-module2"
                  arg1 arg2)
(declare-function w32-tr-ime-set-documentfeed "tr-ime-module2"
                  arg1 arg2)
(declare-function w32-tr-ime-get-dpi "tr-ime-module2")

;;
;; Module1 がロードされていなければロードする
;;

;; Module1 ヘルパのみロードして Module1 DLL はロードしない
;; （Module1 ヘルパは Module2 DLL があると Module1 DLL をロードしない）
(require 'tr-ime-module-helper)

;;
;; メッセージフックとサブクラス化
;;

(defun w32-tr-ime-module-message-hook-and-subclassify-p-set (symb bool)
  "IME 制御のためメッセージフックしてフレームをサブクラス化するか否か設定

BOOL が non-nil ならメッセージフックしてサブクラス化する。
BOOL が nil ならサブクラス解除してメッセージフックを停止する。

注意：Module2 のほとんどの機能は
メッセージフックとサブクラス化を前提としており、
これらが有効でなければ機能しないだけではなく、
設定変更すらできないものも存在する。"
  (if bool
      (progn
        (w32-tr-ime-install-message-hook-hwnd
         (string-to-number (frame-parameter nil 'window-id)))
        (w32-tr-ime-subclassify-hwnd
         (string-to-number (frame-parameter nil 'window-id)) nil))
    (w32-tr-ime-unsubclassify-hwnd
     (string-to-number (frame-parameter nil 'window-id)) nil)
    ;; サブクラス解除は非同期に実施されるが、
    ;; 解除前にメッセージフック停止すると解除できなくなるので少し待機する。
    (sleep-for 1)
    (w32-tr-ime-uninstall-message-hook-hwnd
     (string-to-number (frame-parameter nil 'window-id))))
  (set-default symb bool))

(defcustom w32-tr-ime-module-message-hook-and-subclassify-p t
  "IME 制御のためメッセージフックしてフレームをサブクラス化するか否か

この設定を変更する場合には custom-set-variables を使うこと。

注意：Module2 のほとんどの機能は
メッセージフックとサブクラス化を前提としており、
これらが有効でなければ機能しないだけではなく、
設定変更すらできないものが存在する。
特別な目的が無い限りは non-nil にしておくこと。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-message-hook-and-subclassify-p-set
  :group 'w32-tr-ime-module-core-module2)

;;
;; スレッドメッセージのディスパッチ
;;

(defun w32-tr-ime-module-dispatch-thread-message-p-set (symb bool)
  "スレッドメッセージをディスパッチするか否か設定

BOOL が non-nil ならスレッドメッセージをディスパッチする。
BOOL が nil ならスレッドメッセージをディスパッチしない。

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
  (if bool
      (w32-tr-ime-set-dispatch-thread-message t)
    (w32-tr-ime-set-dispatch-thread-message nil))
  (set-default symb bool))

(defcustom w32-tr-ime-module-dispatch-thread-message-p t
  "スレッドメッセージをディスパッチするか否か

この設定を変更する場合には custom-set-variables を使うこと。

GNU Emacs 27 や 28 の UI スレッドは、
スレッドメッセージがディスパッチされない。
これによって IME の動作に不具合が発生する
（タスクトレイの IME 状態表示アイコンが変わらない等）。
そこで、本設定によってメッセージフックが
スレッドメッセージをディスパッチするようにできる。

ただし、将来の Emacs で
スレッドメッセージをディスパッチするようになったら本設定を nil にすること。
さもなければひとつのスレッドメッセージを
二重にディスパッチしてしまうことになり、
Emacs の動作がおかしくなってしまう。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-dispatch-thread-message-p-set
  :group 'w32-tr-ime-module-core-module2)

;;
;; UI スレッドからの通知を Lisp で受け取る
;;

(defun w32-tr-ime-module-recv-from-ui-thread-p-set (symb bool)
  "UI スレッドからの通知を Lisp で受け取るか否か設定する"
  (if bool
      (progn
        (define-key special-event-map [language-change]
          (lambda ()
            (interactive)
            (w32-tr-ime-language-change-handler))))
    (define-key special-event-map [language-change] 'ignore))
  (set-default symb bool))

(defcustom w32-tr-ime-module-recv-from-ui-thread-p t
  "UI スレッドからの通知を Lisp で受け取るか否か

この設定を変更する場合には custom-set-variables を使うこと。

注意：Module2 の一部の機能は
UI スレッドからの通知を Lisp で受け取り、
Lisp での処理結果が UI スレッドへ通知されるまで待つものがある。
これらの機能が有効なまま本設定を無効にしてしまうと
Lisp が通知を受け取れなくなり処理もされず、
UI スレッドは返ってこない通知を待つため（一時的に）
ロックしてしまうことがある。
特別な目的が無い限りは non-nil にしておくこと。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-recv-from-ui-thread-p-set
  :group 'w32-tr-ime-module-core-module2)

;;
;; IME 状態変更・状態取得関数のエミュレーション
;;

(defun ime-force-on (&optional _dummy)
  "IME を ON にする関数

Module2 で IME パッチの ime-force-on をエミュレーションする。"
  (w32-tr-ime-setopenstatus2
   (string-to-number (frame-parameter nil 'window-id)) t))

(defun ime-force-off (&optional _dummy)
  "IME を OFF にする関数

Module2 で IME パッチの ime-force-off をエミュレーションする。"
  (w32-tr-ime-setopenstatus2
   (string-to-number (frame-parameter nil 'window-id)) nil))

(defun ime-get-mode ()
  "IME 状態を返す関数

Module2 で IME パッチの ime-get-mode をエミュレーションする。
IME が OFF なら nil を、ON ならそれ以外を返す。"
  (w32-tr-ime-getopenstatus2
   (string-to-number (frame-parameter nil 'window-id))))

;;
;; IME フォント設定（未定義文字列のフォント）
;;

(defun w32-tr-ime-font-encode-weight (symb)
  "フェイス属性の weight から LOGFONT 構造体の lfWeight へ変換する"
  (let* ((result
	  (seq-drop-while
	   (lambda (x)
	     (eq (seq-drop-while (lambda (y) (not (eq y symb))) x) [] ))
	   font-weight-table))
         (weight
	  (if (eq result []) 100 (aref (aref result 0) 0))))
    (cond ((>= weight 210) 900) ;; FW_HEAVY
	  ((>= weight 205) 800) ;; FW_EXTRABOLD
	  ((>= weight 200) 700) ;; FW_BOLD
	  ((>= weight 180) 600) ;; FW_SEMIBOLD
	  ((>= weight 100) 400) ;; FW_NORMAL
	  ((>= weight 50) 300)  ;; FW_LIGHT
	  ((>= weight 40) 200)  ;; FW_EXTRALIGHT
	  ((>= weight 20) 100)  ;; FW_THIN
	  (t 0))))

(defun w32-tr-ime-font-encode-slant (symb)
  "フェイス属性の slant から LOGFONT 構造体の lfItalic へ変換する"
  (let* ((result
	  (seq-drop-while
	   (lambda (x)
	     (eq (seq-drop-while (lambda (y) (not (eq y symb))) x) [] ))
	   font-slant-table))
         (slant
	  (if (eq result []) 100 (aref (aref result 0) 0))))
    (if (> slant 150) t nil)))

(defun w32-tr-ime-reflect-frame-parameter-ime-font (&optional frame)
  "フレームの ime-font 設定をモジュールのフォント設定に反映させる

FRAME の frame-parameter から ime-font 設定を読み取り、
モジュールで C++ 実装されている低レベルフォント設定関数
w32-tr-ime-set-font を使って未確定文字列のフォントを設定する。
FRAME が nil または省略された場合は選択されているフレームが対象となる。
family に generic family を指定することはできない。

IME パッチではフレームの ime-font 設定を変更すると即座に反映されるが、
モジュール環境では、フレームの ime-font 設定と、
モジュール内部の未確定文字列フォント設定は独立であり、
フレームの ime-font 設定を変更しても、本関数を呼ぶまで反映されない。
よって、設定後に必ず本関数を呼ぶようにするか、何らかのフックなどで
フレームの ime-font 設定変更を検出して本関数を呼ぶようなどすれば、
IME パッチと同様の設定で使うことができる。

なお、低レベルフォント設定関数 w32-tr-ime-set-font で設定される、
モジュール内部の設定はスレッド毎の設定となっており、
一度設定すると他のフレームでも同じ設定が使われる。
そのため、全フレームで同一の設定にしたい場合は、

(modify-all-frames-parameters '((ime-font . \"原ノ味明朝-24\")))
(w32-tr-ime-reflect-frame-parameter-ime-font)

のようにすることによって、全フレームおよび default-frame-alist の
ime-font 設定が変更された上で、モジュール内部の設定にも反映される。

逆にフレーム毎の設定をしたい場合は、
各フレームの ime-font 設定を別々に設定しておき、
focus-in-hook などで ime-font 設定が変わったことを検出して
本関数を呼び出すなどすれば、フレームが切り替わった際に、
そのフレームの ime-font 設定を反映させることもできる。"
  (interactive)
  (let ((ime-font (frame-parameter frame 'ime-font)))
    (when ime-font
      (let* ((attributes (font-face-attributes ime-font))
             (family (plist-get attributes :family))
             (height (plist-get attributes :height)))
        (when (and family height)
          (let ((h (round (/ (* height
                                (cdr (w32-tr-ime-get-dpi)))
                             -720.0))))
            (w32-tr-ime-set-font
             (string-to-number
              (frame-parameter frame 'window-id))
             h 0 0 0
             (w32-tr-ime-font-encode-weight (plist-get attributes :weight))
             (w32-tr-ime-font-encode-slant (plist-get attributes :slant))
             nil nil 1 0 0 0 0 family)))))))

(defvar w32-tr-ime-module-last-ime-font nil
  "未確定文字列フォント変更検出用")

(defun w32-tr-ime-module-ime-font-emulator ()
  "フレームパラメータ ime-font 設定をエミュレーションする関数

Emacs の標準的なフックである post-command-hook に登録するか、
focus-in-hook （GNU Emacs 26.3 まで）に登録／
after-focus-change-function （GNU Emacs 27.1 以降）経由で呼び出す。

IME パッチは、フレームパラメータの ime-font を設定すると、
即座に未確定文字列フォントに反映されるが、モジュール環境では反映できない。
本関数は ime-font が変更されているか確認し、変更されていたら
変更を反映する w32-tr-ime-reflect-frame-parameter-ime-font
関数を呼び出すことによって未確定文字列フォントを設定する。

focus-in-hook へ登録／
after-focus-change-function 経由で呼び出した場合は、
フレームを変更した際に呼ばれ、
フレームへ設定されたパラメータに応じて未確定文字列フォントが
設定されるようになる。

post-command-hook に登録した場合は、ほとんどのコマンドの動作後に呼ばれ、
フレームパラメータの変更後すぐに未確定文字列フォントが
設定されるようになる。"
  (unless (string= w32-tr-ime-module-last-ime-font
                   (frame-parameter nil 'ime-font))
    (w32-tr-ime-reflect-frame-parameter-ime-font)
    (setq w32-tr-ime-module-last-ime-font (frame-parameter nil 'ime-font))))

(defun w32-tr-ime-module-ime-font-after-focus-change-function ()
  "フォーカス変更を検知して ime-font 設定エミュレーション関数を呼ぶ

after-focus-change-function は呼び出された時点では
まだ selected-frame が変わっていないことがあるので、
全フレームに対して frame-focus-state　でフォーカスを得ているか否かを判定し、
フォーカスを得ていたフレームで w32-tr-ime-module-ime-font-emulator
を呼び出して未確定文字列フォントを設定する。

after-focus-change-function （GNU Emacs 27.1 以降）
に登録して使う。"
  (dolist (f (frame-list))
    (when (frame-focus-state f)
      (with-selected-frame f (w32-tr-ime-module-ime-font-emulator)))))

(defun w32-tr-ime-module-ime-font-focus-p-set (symb bool)
  "フォーカス変更時に ime-font 設定エミュレーションを呼ぶか否か設定

BOOL が non-nil なら設定する。
これにより after-focus-change-function
にフォーカス検知関数を追加することで、
フレームへ設定されたパラメータに応じて未確定文字列フォントが
設定されるようになる。
BOOL が nil ならフックから削除して設定を停止する。"
  (if bool
      (add-function :before
                    after-focus-change-function
                    #'w32-tr-ime-module-ime-font-after-focus-change-function)
    (remove-function
     after-focus-change-function
     #'w32-tr-ime-module-ime-font-after-focus-change-function))
  (set-default symb bool))

(defun w32-tr-ime-module-ime-font-post-command-p-set (symb bool)
  "コマンド実行後に ime-font 設定エミュレーションを呼ぶか否か設定

BOOL が non-nil なら設定する。
これにより post-command-hook にエミュレーション関数を追加することで、
ime-font 変更後すぐに未確定文字列フォントが設定されるようになる。
BOOL が nil ならフックから削除して設定を停止する。"
  (if bool (add-hook 'post-command-hook
                     #'w32-tr-ime-module-ime-font-emulator)
    (remove-hook 'post-command-hook
                 #'w32-tr-ime-module-ime-font-emulator))
  (set-default symb bool))

(defcustom w32-tr-ime-module-ime-font-focus-p t
  "フォーカス変更時に ime-font 設定エミュレーションを呼ぶか否か

この設定を変更する場合には custom-set-variables を使うこと。

本設定を non-nil にすると、フォーカス変更時（フレーム変更時）に
フレームパラメータの ime-font 設定が、
モジュール環境の未確定文字列フォントに反映される。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-ime-font-focus-p-set
  :group 'w32-tr-ime-module-ime-font)

(defcustom w32-tr-ime-module-ime-font-post-command-p nil
  "コマンド実行後に ime-font 設定エミュレーションを呼ぶか否か

この設定を変更する場合には custom-set-variables を使うこと。

本設定を non-nil にすると、ほとんどのコマンド実行後に
フレームパラメータの ime-font 設定が、
モジュール環境の未確定文字列フォントに反映される。
つまり、ime-font 設定を変更することで IME パッチ環境と同様、
ほぼ即座に未確定文字列フォントが設定できる。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-ime-font-post-command-p-set
  :group 'w32-tr-ime-module-ime-font)

;;
;; isearch-mode 時の未確定文字列ウィンドウ位置設定
;;

(defvar w32-tr-ime-module-isearch-last-echo-area-0-point nil
  "エコーエリア 0 の point 移動検出用")

(defvar w32-tr-ime-module-isearch-last-echo-area-1-point nil
  "エコーエリア 1 の point 移動検出用")

(defvar w32-tr-ime-module-isearch-last-echo-area-buffer nil
  "最後に使用されたエコーエリアバッファ")

(defun w32-tr-ime-module-isearch-start ()
  "エコーエリアのバッファのうちどちらが使われているか検出を開始する

isearch-mode-hook に登録することにより、isearch-mode 中に
どのエコーエリアが使われているか検出できるようにする。"
  (setq w32-tr-ime-module-isearch-last-echo-area-0-point
        (with-current-buffer (get-buffer " *Echo Area 0*")
          (point)))
  (setq w32-tr-ime-module-isearch-last-echo-area-1-point
        (with-current-buffer (get-buffer " *Echo Area 1*")
          (point))))

(defun w32-tr-ime-module-isearch-detect-echo-area-buffer ()
  "isearch-mode で使用されているエコーエリアバッファを検出して返す"
  (let* ((point0 (with-current-buffer
                     (get-buffer " *Echo Area 0*") (point)))
         (point1 (with-current-buffer
                     (get-buffer " *Echo Area 1*") (point)))
         (buff (cond ((/= point0
                          w32-tr-ime-module-isearch-last-echo-area-0-point)
                      (get-buffer " *Echo Area 0*"))
                     ((/= point1
                          w32-tr-ime-module-isearch-last-echo-area-1-point)
                      (get-buffer " *Echo Area 1*"))
                     (t
                      (if w32-tr-ime-module-isearch-last-echo-area-buffer
                          w32-tr-ime-module-isearch-last-echo-area-buffer
                        (get-buffer " *Echo Area 0*"))))))
    ;; (w32-tr-ime-debug-output
    ;;  (format-message "last point0 %s, point1 %s, buff %s"
    ;;                  w32-tr-ime-module-isearch-last-echo-area-0-point
    ;;                  w32-tr-ime-module-isearch-last-echo-area-1-point
    ;;                  w32-tr-ime-module-isearch-last-echo-area-buffer))
    ;; (w32-tr-ime-debug-output
    ;;  (format-message " now point0 %s, point1 %s, buff %s"
    ;;                  point0 point1 buff))
    (setq w32-tr-ime-module-isearch-last-echo-area-0-point point0)
    (setq w32-tr-ime-module-isearch-last-echo-area-1-point point1)
    (setq w32-tr-ime-module-isearch-last-echo-area-buffer buff)))

(defun w32-tr-ime-module-isearch-update ()
  "isearch-mode 中の未確定文字列の表示位置を設定する

isearch-update-post-hook に登録することにより、
isearch-mode 中の未確定文字列の表示位置を
ミニバッファ上の文字入力位置に設定する。"
  (with-current-buffer (w32-tr-ime-module-isearch-detect-echo-area-buffer)
    (let* ((sw
            (string-width
             (buffer-substring-no-properties (point-min) (point))))
           (wc (window-width (minibuffer-window)))
           (x (* (% sw wc) (frame-char-width)))
           (y (* (/ sw wc) (with-selected-window (minibuffer-window)
                             (line-pixel-height))))
           (edges (window-inside-pixel-edges (minibuffer-window)))
           (left (nth 0 edges))
           (top (nth 1 edges))
           (right (nth 2 edges))
           (bottom (nth 3 edges))
           (px (+ x left))
           (py (+ y top)))
      ;; (w32-tr-ime-debug-output
      ;;  (format-message
      ;;   "sw %s, wc %s, x %s, y %s, edges %s"
      ;;   sw wc x y edges))
      ;; (w32-tr-ime-debug-output
      ;;  (format-message
      ;;   "left %s, top %s, right %s, bottom %s, px %s, py %s"
      ;;   left top right bottom px py))
      (w32-tr-ime-set-composition-window
       (string-to-number (frame-parameter nil 'window-id))
       1 px py left top right bottom))))

(defun w32-tr-ime-module-isearch-end ()
  "未確定文字列の表示位置を元に戻す

isearch-mode-end-hook に登録することにより、isearch-mode 終了後に
未確定文字列の表示位置を通常のバッファ内のカーソル位置に戻す。"
  (w32-tr-ime-set-composition-window
   (string-to-number (frame-parameter nil 'window-id))
   0 0 0 0 0 0 0))

(defun w32-tr-ime-module-isearch-p-set (symb bool)
  "isearch-mode 中の未確定文字列表示位置を文字入力位置にするか否か設定する

BOOL が non-nil なら設定する。
これにより isearch のフックに追加して文字入力位置を検出し設定する。
BOOL が nil ならフックから削除して設定を停止する。"
  (if bool (progn
             (add-hook 'isearch-mode-hook
                       #'w32-tr-ime-module-isearch-start)
             (add-hook 'isearch-update-post-hook
                       #'w32-tr-ime-module-isearch-update)
             (add-hook 'isearch-mode-end-hook
                       #'w32-tr-ime-module-isearch-end))
    (remove-hook 'isearch-mode-hook
                 #'w32-tr-ime-module-isearch-start)
    (remove-hook 'isearch-update-post-hook
                 #'w32-tr-ime-module-isearch-update)
    (remove-hook 'isearch-mode-end-hook
                 #'w32-tr-ime-module-isearch-end))
  (set-default symb bool))

(defcustom w32-tr-ime-module-isearch-p t
  "isearch-mode 中の未確定文字列表示位置を文字入力位置にするか否か

この設定を変更する場合には custom-set-variables を使うこと。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-isearch-p-set
  :group 'w32-tr-ime-module-isearch-mode)

(defun w32-tr-ime-module-isearch-defsubclassproc-p-set (symb bool)
  "WM_IME_STARTCOMPOSITION で常に DefSubclassProc を呼ぶか否か設定する"
  (w32-tr-ime-set-startcomposition-defsubclassproc
   (string-to-number (frame-parameter nil 'window-id)) bool)
  (set-default symb bool))

(defcustom w32-tr-ime-module-isearch-defsubclassproc-p nil
  "WM_IME_STARTCOMPOSITION で常に DefSubclassProc を呼ぶか否か

この設定を変更する場合には custom-set-variables を使うこと。

WM_IME_STARTCOMPOSITION ハンドラにおいて、
isearch-mode 中（未確定文字列ウィンドウの位置設定中）は
DefSubcalssProc を呼ばず Emacs のメッセージ処理をスキップしている。
これは Emacs で未確定文字列ウィンドウの位置を isearch-mode
に入る前の文字入力位置に設定してしまうからで、
この設定後に位置を上書きしても未確定文字列ウィンドウがチラつくからである。
しかし、何らかの理由で元の Emacs の処理に戻さなければならない時は、
本設定を non-nil にすることで isearch-mode 中であっても、
DefSubcalssProc により Emacs のメッセージ処理が必ず呼ばれるようになる。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-isearch-defsubclassproc-p-set
  :group 'w32-tr-ime-module-isearch-mode)

;;
;; isearch-mode 時の Alt + 半角/全角ワークアラウンド
;;

(defun w32-tr-ime-module-workaround-isearch-mode-delayed-update ()
  "アイドル状態になったら isearch-mode のエコーエリアを再表示する

Module2 で isearch-mode 時に Alt + 半角/全角キー操作をすると、
なぜかエコーエリアが消えてしまう。
キー操作時に再表示させても効果が無い
（恐らくキー操作後にくるイベントか何かで消されている）ので、
Emacs がアイドル状態になったら動作するタイマで再表示させる。"
  (interactive)
  (run-with-idle-timer 0.0001 nil #'isearch-update))

(defun w32-tr-ime-module-workaround-isearch-mode-delayed-update-p-set
    (symb bool)
  "isearch-mode 時の Alt + 半角/全角ワークアラウンドを動作させるか否か設定

Module2 で isearch-mode 時に Alt + 半角/全角キー操作をすると、
なぜかエコーエリアが消えてしまう対策のワークアラウンドを動作させるか否か
設定する。
bool が non-nil なら動作させる。それ以外なら停止させる。"
  (if bool
      (define-key isearch-mode-map [M-kanji]
        'w32-tr-ime-module-workaround-isearch-mode-delayed-update)
    (define-key isearch-mode-map [M-kanji] 'ignore))
  (set-default symb bool))

(defcustom w32-tr-ime-module-workaround-isearch-mode-delayed-update-p t
  "isearch-mode 時の Alt + 半角/全角ワークアラウンドを動作させるか否か

この設定を変更する場合には custom-set-variables を使うこと。

Module2 で isearch-mode 時に Alt + 半角/全角キー操作をすると、
なぜかエコーエリアが消えてしまう。
キー操作時に再表示させても効果が無い
（恐らくキー操作後にくるイベントか何かで消されている）ので、
Emacs がアイドル状態になったら動作するタイマで再表示させるワークアラウンド。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-workaround-isearch-mode-delayed-update-p-set
  :group 'w32-tr-ime-module-workaround-isearch-mode)

;;
;; プレフィックスキー（C-x など）を検出して自動的に IME OFF する
;;

(defvar w32-tr-ime-module-prefix-key-p t)

(defun w32-tr-ime-module-prefix-key-list-set (symb settings)
  "プレフィックスキー検出対象リストを設定する

SETTINGS はプレフィックスキーとして検出したいコードのリスト。"
  (set-default symb settings)
  (if w32-tr-ime-module-prefix-key-p
      (w32-tr-ime-set-prefix-keys
       (string-to-number (frame-parameter nil 'window-id))
       settings)))

(defcustom w32-tr-ime-module-prefix-key-list
  '(#x20058 #x20048 #x20043 #x1b)
  "プレフィックスキー検出対象リスト

この設定を変更する場合には custom-set-variables を使うこと。

プレフィックスキーとして検出したいコードのリスト。
コードは上位 16 bit が修飾キー、下位 16 bit が修飾されるキーの
バーチャルキーコードを指定する。
修飾キーは Shift (#x10000), Ctrl (#x20000), Alt (#x40000) の
ビット論理和で指定する。バーチャルキーコードは Windows のものを指定する。

例えば C-x は Ctrl の修飾キー #x20000 と、
X キーのバーチャルキーコード #x58 のビット論理和なので #x20058 を指定する。
C-M-x であれば、さらに Alt の修飾キーを含めて #x60058 を指定する。"
  :type '(repeat integer)
  :set #'w32-tr-ime-module-prefix-key-list-set
  :group 'w32-tr-ime-module-prefix-key)

(defun w32-tr-ime-module-prefix-key-p-set (symb bool)
  "プレフィックスキーを検出して自動的に IME OFF するか否か設定する

BOOL が non-nil ならプレフィックスキーを検出して IME OFF する。
あわせて module1 で同様な機能を持つワークアラウンドを無効にする。
BOOL が nil ならフックから削除して停止する。"
  (if bool
      (progn
        (custom-set-variables
         '(w32-tr-ime-module-workaround-prefix-key-p nil))
        (w32-tr-ime-set-prefix-keys
         (string-to-number (frame-parameter nil 'window-id))
         w32-tr-ime-module-prefix-key-list)
        (add-hook 'post-command-hook #'w32-tr-ime-resume-prefix-key))
    (w32-tr-ime-set-prefix-keys
     (string-to-number (frame-parameter nil 'window-id)) nil)
    (remove-hook 'post-command-hook #'w32-tr-ime-resume-prefix-key))
  (set-default symb bool))

(defcustom w32-tr-ime-module-prefix-key-p t
  "プレフィックスキーを検出して自動的に IME OFF するか否か

この設定を変更する場合には custom-set-variables を使うこと。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-prefix-key-p-set
  :group 'w32-tr-ime-module-prefix-key)

;;
;; IME 状態変更通知による IME/IM 状態同期
;;

(defvar w32-tr-ime-module-setopenstatus-hook nil
  "IME 状態変更通知があったときに呼ばれるノーマルフック

Module2 の C++ 実装である
w32-tr-ime-language-change-handler 関数から呼ばれる。")

(defcustom
  w32-tr-ime-module-setopenstatus-call-hook-emulator-p t
  "IME 状態変更通知時にフックエミュレーション関数を呼ぶか否か"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :group 'w32-tr-ime-module-setopenstatus)

(defun w32-tr-ime-module-setopenstatus-sync ()
  "IME 状態変更通知時に呼ばれる関数

w32-tr-ime-module-setopenstatus-call-hook-emulator-p
が non-nil であれば、まずフックエミュレーション関数を呼ぶ。
これによってウィンドウやバッファの切り替え未検出があったら、
アブノーマルフックが呼ばれて、IME/IM 状態が整えられる。

その上で IME 状態と IM 状態が食い違ったら IM 状態を反転して一致させる。
これにより、IME 側トリガの状態変更を IM に反映させる。"
  (when w32-tr-ime-module-setopenstatus-call-hook-emulator-p
    (w32-tr-ime-module-hook-emulator))
  (let ((ime-status (ime-get-mode)))
    (cond ((and ime-status
                (not current-input-method))
           (activate-input-method "W32-IME"))
          ((and (not ime-status)
                current-input-method)
           (deactivate-input-method)))))

(defun w32-tr-ime-module-setopenstatus-sync-p-set (symb bool)
  "IME 状態変更通知による IM 状態同期をするか否か設定する"
  (if bool
      (progn
        (custom-set-variables
         '(w32-tr-ime-module-workaround-inconsistent-ime-p nil))
        (add-hook 'w32-tr-ime-module-setopenstatus-hook
                  #'w32-tr-ime-module-setopenstatus-sync))
    (remove-hook 'w32-tr-ime-module-setopenstatus-hook
                 #'w32-tr-ime-module-setopenstatus-sync))
  (set-default symb bool))

(defcustom w32-tr-ime-module-setopenstatus-sync-p t
  "IME 状態変更通知による IM 状態同期をするか否か

この設定を変更する場合には custom-set-variables を使うこと。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-setopenstatus-sync-p-set
  :group 'w32-tr-ime-module-setopenstatus)

;;
;; 再変換 (RECONVERSION)
;;

(defvar w32-tr-ime-module-reconvertstring-hook nil
  "WM_IME_REQUEST IMR_RECONVERTSTRING が来た時に呼ばれるノーマルフック

Module2 の C++ 実装である
w32-tr-ime-language-change-handler 関数から呼ばれる。")

(defun w32-tr-ime-module-notify-reconvert-string ()
  "RECONVERTSTRING 構造体用の材料を収集して UI スレッドへ通知する

point のある行全体の文字列と、文字列中の point 位置を収集し、
Module2 の C++ 実装である w32-tr-ime-notify-reconvert-string 関数を呼び、
UI スレッドへ通知する。
ノーマルフック w32-tr-ime-module-reconvertstring-hook および
w32-tr-ime-module-documentfeed-hook に登録して使う。"
  (w32-tr-ime-notify-reconvert-string
   (string-to-number (frame-parameter nil 'window-id))
   (buffer-substring-no-properties
    (line-beginning-position) (line-end-position))
   (- (point) (line-beginning-position))))

(defun w32-tr-ime-module-reconversion-p-set (symb bool)
  "再変換 (RECONVERSION) 動作を行うか否か設定する"
  (if bool
      (progn
        (add-hook 'w32-tr-ime-module-reconvertstring-hook
                  #'w32-tr-ime-module-notify-reconvert-string)
        (w32-tr-ime-set-reconversion
         (string-to-number (frame-parameter nil 'window-id)) t))
    (w32-tr-ime-set-reconversion
     (string-to-number (frame-parameter nil 'window-id)) nil)
    (remove-hook 'w32-tr-ime-module-reconvertstring-hook
                 #'w32-tr-ime-module-notify-reconvert-string))
  (set-default symb bool))

(defcustom w32-tr-ime-module-reconversion-p nil
  "再変換 (RECONVERSION) 動作を行うか否か

この設定を変更する場合には custom-set-variables を使うこと。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-reconversion-p-set
  :group 'w32-tr-ime-module-reconversion)

;;
;; 前後の確定済文字列を参照した変換 (DOCUMENTFEED)
;;

(defvar w32-tr-ime-module-documentfeed-hook nil
  "WM_IME_REQUEST IMR_DOCUMENTFEED が来た時に呼ばれるノーマルフック

Module2 の C++ 実装である
w32-tr-ime-language-change-handler 関数から呼ばれる。")

(defun w32-tr-ime-module-documentfeed-p-set (symb bool)
  "前後の確定済文字列を参照した変換 (DOCUMENTFEED) 動作を行うか否か設定する"
  (if bool
      (progn
        (add-hook 'w32-tr-ime-module-documentfeed-hook
                  #'w32-tr-ime-module-notify-reconvert-string)
        (w32-tr-ime-set-documentfeed
         (string-to-number (frame-parameter nil 'window-id)) t))
    (w32-tr-ime-set-documentfeed
     (string-to-number (frame-parameter nil 'window-id)) nil)
    (remove-hook 'w32-tr-ime-module-documentfeed-hook
                 #'w32-tr-ime-module-notify-reconvert-string))
  (set-default symb bool))

(defcustom w32-tr-ime-module-documentfeed-p nil
  "前後の確定済文字列を参照した変換 (DOCUMENTFEED) 動作を行うか否か

この設定を変更する場合には custom-set-variables を使うこと。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-documentfeed-p-set
  :group 'w32-tr-ime-module-documentfeed)

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
