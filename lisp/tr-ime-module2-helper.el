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

;;
;; C++ 実装による DLL をロードする
;;

(unless (featurep 'tr-ime-module2)
  (load (concat "tr-ime-module2-" system-configuration) t))

(declare-function w32-tr-ime-subclassify-hwnd "tr-ime-module2"
                  arg1 &optional arg2)
(declare-function w32-tr-ime-set-dispatch-thread-message "tr-ime-module2"
                  arg1)
(declare-function w32-tr-ime-set-font "tr-ime-module2"
                  arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)
(declare-function w32-tr-ime-set-composition-window "tr-ime-module2"
                  arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10
                  arg11 arg12 arg13 arg14 arg15)
(declare-function w32-tr-ime-get-dpi "tr-ime-module2")

;;
;; IME フォント設定（未定義文字列のフォント）
;;

(defun w32-tr-ime-reflect-frame-parameter-ime-font (&optional frame)
  "フレームの ime-font 設定をモジュールのフォント設定に反映させる

FRAME の frame-parameter から ime-font 設定を読み取り、
モジュールで C++ 実装されている低レベルフォント設定関数
w32-tr-ime-set-font を使って未確定文字列のフォントを設定する。
FRAME が nil または省略された場合は選択されているフレームが対象となる。
現状では family と height のみが設定され、他の属性は無視される。

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
                                (nth 1 (w32-tr-ime-get-dpi)))
                             -720.0))))
            (w32-tr-ime-set-font
             (string-to-number
              (frame-parameter frame 'window-id))
             h 0 0 0 0 nil nil nil 0 0 0 0 0 family)))))))

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

(defcustom w32-tr-ime-module-ime-font-focus-p nil
  "フォーカス変更時に ime-font 設定エミュレーションを呼ぶか否か

この設定を変更する場合には custom-set-variables を使うこと。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-ime-font-focus-p-set
  :group 'w32-tr-ime-module)

(defcustom w32-tr-ime-module-ime-font-post-command-p nil
  "コマンド実行後に ime-font 設定エミュレーションを呼ぶか否か

この設定を変更する場合には custom-set-variables を使うこと。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-ime-font-post-command-p-set
  :group 'w32-tr-ime-module)

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
  :group 'w32-tr-ime-module)

;;
;; provide
;;

(provide 'tr-ime-module2-helper)