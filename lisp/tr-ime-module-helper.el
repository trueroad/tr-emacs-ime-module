;; -*- mode: emacs-lisp; coding: utf-8 -*-

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

;; C 実装による DLL をロードする
(load "tr-ime-module" t)

(defun ime-force-on (&rest dummy)
  "IME を ON にする関数

IME パッチの ime-force-on をエミュレーションする。"
  (w32-tr-ime-setopenstatus
   (string-to-number (frame-parameter (selected-frame) 'window-id)) t))

(defun ime-force-off (&rest dummy)
  "IME を OFF にする関数

IME パッチの ime-force-off をエミュレーションする。"
  (w32-tr-ime-setopenstatus
   (string-to-number (frame-parameter (selected-frame) 'window-id)) nil))

(defun ime-get-mode ()
  "IME 状態を返す関数

IME パッチの ime-get-mode をエミュレーションする。
IME が OFF なら nil を、ON ならそれ以外を返す。"
  (w32-tr-ime-getopenstatus
   (string-to-number (frame-parameter (selected-frame) 'window-id))))

(defvar select-window-functions nil
  "ウィンドウが変更されると呼ばれるアブノーマルフック

IME パッチ特有のフックで、 IME パッチでは C 実装されているが、
Lisp でエミュレーションする。")

(defvar set-selected-window-buffer-functions nil
  "ウィンドウに紐づいたバッファが変更されると呼ばれるアブノーマルフック

IME パッチ特有のフックで、 IME パッチでは C 実装されているが、
Lisp でエミュレーションする。")

(defvar w32-tr-ime-module-last-selected-window nil
  "選択ウィンドウの変更検出用変数")
(defvar w32-tr-ime-module-last-current-buffer nil
  "カレントバッファの変更検出用変数")

(defun w32-tr-ime-module-post-command-hook ()
  "IME パッチ特有のアブノーマルフックをエミュレーションする関数

Emacs の標準的なフックの一つ post-command-hook に登録する。
post-command-hook によって、ほとんどのコマンドの動作後に呼ばれる。

この関数の動作は、
選択ウィンドウが変更されていたら select-window-functions を呼び、
ウィンドウが変わらずカレントバッファが変更されていたら
set-selected-window-buffer-functions を呼ぶ。
どちらも変わっていなければ何もしない。"
  (cond
   ((not (eq (selected-window) w32-tr-ime-module-last-selected-window))
    (run-hook-with-args 'select-window-functions
                        w32-tr-ime-module-last-selected-window
                        (selected-window))
    (setq w32-tr-ime-module-last-selected-window (selected-window)))
   ((not (eq (current-buffer) w32-tr-ime-module-last-current-buffer))
    (run-hook-with-args 'set-selected-window-current-buffer-functions
                        w32-tr-ime-module-last-current-buffer
                        (selected-window)
                        (current-buffer))
    (setq w32-tr-ime-module-last-current-buffer (current-buffer)))))

;; フックのエミュレーション用関数を post-command-hook に登録
;; ほとんどのコマンドの動作後に関数が呼ばれるようになる。
(add-hook 'post-command-hook 'w32-tr-ime-module-post-command-hook)

(defvar w32-tr-ime-module-check-prefix-key-flag t)
(defvar w32-tr-ime-module-check-prefix-key-before-ime-mode nil)
(defvar w32-tr-ime-module-check-prefix-key-timer nil)
(defvar w32-tr-ime-module-check-prefix-key-polling-time 0.1)

(defun w32-tr-ime-module-check-prefix-key ()
  (when w32-tr-ime-module-check-prefix-key-flag
    (let ((key (this-single-command-keys)))
      (when (or (equal key [24])
                (equal key [8])
                (equal key [3]))
        (setq w32-tr-ime-module-check-prefix-key-before-ime-mode
              (ime-get-mode))
        (ime-force-off)
        (setq w32-tr-ime-module-check-prefix-key-flag nil)))))

(defun w32-tr-ime-module-check-prefix-key-post-command-hook ()
  (when (not w32-tr-ime-module-check-prefix-key-flag)
    (setq w32-tr-ime-module-check-prefix-key-flag t)
    (when w32-tr-ime-module-check-prefix-key-before-ime-mode
      (ime-force-on))))

(defun w32-tr-ime-module-check-prefix-key-on ()
  (setq w32-tr-ime-module-check-prefix-key-flag t)
  (add-hook 'post-command-hook
            'w32-tr-ime-module-check-prefix-key-post-command-hook)
  (if w32-tr-ime-module-check-prefix-key-timer
      (cancel-timer w32-tr-ime-module-check-prefix-key-timer))
  (setq w32-tr-ime-module-check-prefix-key-timer
        (run-at-time t w32-tr-ime-module-check-prefix-key-polling-time
                     'w32-tr-ime-module-check-prefix-key)))

(defun w32-tr-ime-module-check-prefix-key-off ()
  (remove-hook 'post-command-hook
               'w32-tr-ime-module-check-prefix-key-post-command-hook)
  (if w32-tr-ime-module-check-prefix-key-timer
      (cancel-timer w32-tr-ime-module-check-prefix-key-timer))
  (setq w32-tr-ime-module-check-prefix-key-timer nil))

;; Alt + 半角全角で IME だけでなく IM も切り替わるようにする
(define-key global-map [M-kanji] 'toggle-input-method)

(provide 'tr-ime-module-helper)