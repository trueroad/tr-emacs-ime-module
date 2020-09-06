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
;; C++ 実装による DLL をロードする
;;

(unless (featurep 'tr-ime-module2)
  (load (concat "tr-ime-module2-" system-configuration) t))

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

;;
;; provide
;;

(provide 'tr-ime-module2-helper)
