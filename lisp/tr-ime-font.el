;;; tr-ime-font.el --- Font handling -*- lexical-binding: t -*-

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

(require 'seq)

;;
;; ユーザ設定用
;;

(defgroup tr-ime-font nil
  "IME フォント（未確定文字列フォント）設定 (advanced)."
  :group 'tr-ime)

;;
;; 属性変換
;;

(defun tr-ime-font--encode-weight (symb)
  "フェイス属性の weight から LOGFONT 構造体の lfWeight へ変換する.

SYMB に weight を指定する。返り値は lfWeight。"
  (let* ((result
          (seq-drop-while
           (lambda (x)
             (eq (seq-drop-while (lambda (y) (not (eq y symb))) x) []))
           font-weight-table))
         (weight
          (if (eq result []) 100 (aref (aref result 0) 0))))
    (cond ((>= weight 210) 900) ; FW_HEAVY
          ((>= weight 205) 800) ; FW_EXTRABOLD
          ((>= weight 200) 700) ; FW_BOLD
          ((>= weight 180) 600) ; FW_SEMIBOLD
          ((>= weight 100) 400) ; FW_NORMAL
          ((>= weight 50) 300)  ; FW_LIGHT
          ((>= weight 40) 200)  ; FW_EXTRALIGHT
          ((>= weight 20) 100)  ; FW_THIN
          (t 0))))

(defun tr-ime-font--encode-slant (symb)
  "フェイス属性の slant から LOGFONT 構造体の lfItalic へ変換する.

SYMB に slant を指定する。返り値は lfItalic。"
  (let* ((result
          (seq-drop-while
           (lambda (x)
             (eq (seq-drop-while (lambda (y) (not (eq y symb))) x) []))
           font-slant-table))
         (slant
          (if (eq result []) 100 (aref (aref result 0) 0))))
    (> slant 150)))

;;
;; フレームの ime-font 設定を反映
;;

(declare-function tr-ime-modadv--set-font "tr-ime-modadv"
                  (arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8
                        arg9 arg10 arg11 arg12 arg13 arg14 arg15))
(declare-function tr-ime-modadv--get-dpi "tr-ime-modadv")

(defun tr-ime-font-reflect-frame-parameter (&optional frame)
  "フレームの ime-font 設定を反映する.

FRAME の フレームパラーメータから ime-font 設定を読み取り、
モジュールで C++ 実装されている低レベルフォント設定関数
tr-ime-modadv--set-font を使って未確定文字列のフォントを設定する。
FRAME が nil または省略された場合は選択されているフレームが対象となる。
family に generic family を指定することはできない。

IME パッチではフレームの ime-font 設定を変更すると即座に反映されるが、
tr-ime 環境では、フレームの ime-font 設定と、
モジュール内部の未確定文字列フォント設定は独立であり、
フレームの ime-font 設定を変更しても、本関数を呼ぶまで反映されない。
よって、設定後に必ず本関数を呼ぶようにするか、何らかのフックなどで
フレームの ime-font 設定変更を検出して本関数を呼ぶようなどすれば、
IME パッチと同様の設定で使うことができる。

なお、低レベルフォント設定関数 tr-ime-modadv--set-font で設定される、
モジュール内部の設定はスレッド毎の設定となっており、
一度設定すると他のフレームでも同じ設定が使われる。
そのため、全フレームで同一の設定にしたい場合は、

  (modify-all-frames-parameters '((ime-font . \"原ノ味明朝-24\")))
  (w32-tr-ime-reflect-frame-parameter-ime-font)

のようにすることによって、全フレームおよび \"default-frame-alist\" の
ime-font 設定が変更された上で、モジュール内部の設定にも反映される。

逆にフレーム毎の設定をしたい場合は、
各フレームの ime-font 設定を別々に設定しておき、
\"focus-in-hook\" などで ime-font 設定が変わったことを検出して
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
                                (cdr (tr-ime-modadv--get-dpi)))
                             -720.0))))
            (tr-ime-modadv--set-font
             (string-to-number
              (frame-parameter frame 'window-id))
             h 0 0 0
             (tr-ime-font--encode-weight (plist-get attributes :weight))
             (tr-ime-font--encode-slant (plist-get attributes :slant))
             nil nil 1 0 0 0 0 family)))))))

;;
;; 未確定文字列フォント変更検出
;;

(defvar tr-ime-font--last-ime-font nil
  "未確定文字列フォント変更検出用.")

(defun tr-ime-font-check ()
  "フレームパラメータ ime-font が変更されていたら反映する.

Emacs の標準的なフックである \"post-command-hook\" に登録するか、
\"after-focus-change-function\" 経由で呼び出す。

IME パッチは、フレームパラメータの ime-font を設定すると、
即座に未確定文字列フォントに反映されるが、モジュール環境では反映できない。
本関数は ime-font が変更されているか確認し、変更されていたら
変更を反映する tr-ime-font-reflect-frame-parameter
関数を呼び出すことによって未確定文字列フォントを設定する。

\"after-focus-change-function\" 経由で呼び出した場合は、
フレームを変更した際に呼ばれ、
フレームへ設定されたパラメータに応じて未確定文字列フォントが
設定されるようになる。

\"post-command-hook\" に登録した場合は、ほとんどのコマンドの動作後に呼ばれ、
フレームパラメータの変更後すぐに未確定文字列フォントが
設定されるようになる。"
  (let ((parameter (frame-parameter nil 'ime-font)))
    (unless (string= parameter tr-ime-font--last-ime-font)
      (tr-ime-font-reflect-frame-parameter)
      (setq tr-ime-font--last-ime-font parameter))))

;;
;; フォーカス変更（フレーム変更）時
;;

(defun tr-ime-font--after-focus-change-function ()
  "フォーカス変更を確認して ime-font 設定を反映する.

\"after-focus-change-function\" は呼び出された時点では
まだ \"selected-frame\" が変わっていないことがあるので、
全フレームに対して \"frame-focus-state\" でフォーカスを得ているか否か判定し、
フォーカスを得ていたフレームで w32-tr-ime-font-check
を呼び出して未確定文字列フォントを設定する。

\"after-focus-change-function\" に登録して使う。"
  (dolist (f (frame-list))
    (when (frame-focus-state f)
      (with-selected-frame f (tr-ime-font-check)))))

(defun tr-ime-font--focus-set (symb bool)
  "フォーカス変更時に ime-font 設定を反映するか否か設定する.

SYMB は tr-ime-font-focus-p を指定する。
BOOL が non-nil ならフォーカス変更時に ime-font 設定を反映する。
そうでなければフォーカス変更時に ime-font 設定を反映しない。"
  (if bool
      (add-function :before
                    after-focus-change-function
                    #'tr-ime-font--after-focus-change-function)
    (remove-function
     after-focus-change-function
     #'tr-ime-font--after-focus-change-function))
  (set-default symb bool))

(defcustom tr-ime-font-focus-p t
  "フォーカス変更時に ime-font 設定を反映するか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

本設定を non-nil (Enable) にすると、フォーカス変更（フレーム変更）時に
フレームパラメータの ime-font 設定が、
モジュールの未確定文字列フォントに反映される。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-font--focus-set
  :group 'tr-ime-font)

;;
;; コマンド実行後
;;

(defun tr-ime-font--post-command-set (symb bool)
  "コマンド実行後に ime-font 設定を反映するか否か設定する.

SYMB は tr-ime-font-post-command-p を指定する。
BOOL が non-nil ならコマンド実行後に設定を反映する。
そうでなければコマンド実行後に設定を反映しない。"
  (if bool
      (add-hook 'post-command-hook
                #'tr-ime-font-check)
    (remove-hook 'post-command-hook
                 #'tr-ime-font-check))
  (set-default symb bool))

(defcustom tr-ime-font-post-command-p nil
  "コマンド実行後に ime-font 設定を反映するか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

本設定を non-nil (Enable) にすると、ほとんどのコマンド実行後に
フレームパラメータの ime-font 設定が、
モジュールの未確定文字列フォントに反映される。
つまり、ime-font 設定を変更することで IME パッチ環境と同様、
ほぼ即座に未確定文字列フォントが設定できる。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-font--post-command-set
  :group 'tr-ime-font)

;;
;; アンロード
;;

(defun tr-ime-font-unload-function ()
  "アンロードするため IME フォント（未確定文字列フォント）設定を無効にする."
  (let (_dummy)
    (tr-ime-font--focus-set '_dummy nil)
    (tr-ime-font--post-command-set '_dummy nil)))

;;
;; provide
;;

(provide 'tr-ime-font)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-font.el ends here
