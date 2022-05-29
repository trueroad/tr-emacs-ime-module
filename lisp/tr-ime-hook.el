;;; tr-ime-hook.el --- Hook emulation of IME patch -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Masamichi Hosoda

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
;; ユーザ設定用
;;

(defgroup tr-ime-core nil
  "コア機能設定.

コア機能の設定です。通常は設定変更しないでください。"
  :group 'tr-ime)

(defgroup tr-ime-hook nil
  "IME パッチ特有のアブノーマルフック."
  :group 'tr-ime-core)

;;
;; Hook
;;

;; IME パッチ適用 Emacs ではコンフリクトを未然に防ぐため
;; 明示的にエラーを出して動作を停止させる
(require 'tr-ime)
(when (tr-ime-detect-ime-patch-p)
  (error "%s" (concat "Emacs seems to have an IME patch applied. "
                      "tr-ime cannot work on it.")))

;; IME パッチ適用 Emacs だと変数 select-window-functions が
;; C 実装なのでエイリアスにしようとするとエラーが発生する
(define-obsolete-variable-alias 'select-window-functions
  'tr-ime-hook-select-window-functions "2020")
(defvar tr-ime-hook-select-window-functions nil
  "選択されたウィンドウが変更されると呼ばれるアブノーマルフック.

IME パッチ特有のフックで、 IME パッチでは C 実装されているが、
Lisp でエミュレーションする。")

;; IME パッチ適用 Emacs だと変数 set-selected-window-buffer-functions が
;; C 実装なのでエイリアスにしようとするとエラーが発生する
(define-obsolete-variable-alias 'set-selected-window-buffer-functions
  'tr-ime-hook-set-selected-window-buffer-functions "2020")
(defvar tr-ime-hook-set-selected-window-buffer-functions nil
  "選択ウィンドウに紐づいたバッファが変更されると呼ばれるアブノーマルフック.

IME パッチ特有のフックで、 IME パッチでは C 実装されているが、
Lisp でエミュレーションする。")

;;
;; 選択ウィンドウに紐づいたバッファの変更を検出する
;;

(defvar tr-ime-hook--last-selected-window-buffer nil
  "選択ウィンドウに紐づいたバッファの変更検出用変数.")

(defun tr-ime-hook-check-selected-window-buffer (_frame)
  "選択ウィンドウに紐づいたバッファが変更されていたらフックを呼ぶ."
  (let* ((window (selected-window))
         (buffer (window-buffer window)))
    (unless (eq buffer tr-ime-hook--last-selected-window-buffer)
      (run-hook-with-args 'tr-ime-hook-set-selected-window-buffer-functions
                          tr-ime-hook--last-selected-window-buffer
                          window
                          buffer)
      (setq tr-ime-hook--last-selected-window-buffer buffer))))

;;
;; 選択ウィンドウの変更を検出する
;;

(defvar tr-ime-hook--last-selected-window nil
  "選択ウィンドウの変更検出用変数.")

(defun tr-ime-hook-check-selected-window (_frame)
  "選択ウィンドウが変更されていたらフックを呼ぶ."
  (let* ((window (selected-window))
         (buffer (window-buffer window)))
    (unless (eq window tr-ime-hook--last-selected-window)
      (run-hook-with-args 'tr-ime-hook-select-window-functions
                          tr-ime-hook--last-selected-window
                          window)
      (setq tr-ime-hook--last-selected-window window)
      (setq tr-ime-hook--last-selected-window-buffer buffer))))

;;
;; 選択ウィンドウとバッファの両方の変更を検出する
;;

(defun tr-ime-hook-check ()
  "選択ウィンドウとバッファの両方を確認してフックを呼ぶ."
  (let ((frame (selected-frame)))
    (tr-ime-hook-check-selected-window frame)
    (tr-ime-hook-check-selected-window-buffer frame)))

;;
;; 設定用
;;

(defun tr-ime-hook--set (symb bool)
  "IME パッチ特有のアブノーマルフックをエミュレーションするか否か設定する.

SYMB は tr-ime-hook-p を指定すること。
BOOL が non-nil ならエミュレーション有効でフックが呼ばれる。
そうでなければエミュレーション無効でフックは呼ばれない。"
  (if bool
      (progn
        (add-hook 'window-selection-change-functions
                  #'tr-ime-hook-check-selected-window)
        (add-hook 'window-buffer-change-functions
                  #'tr-ime-hook-check-selected-window-buffer))
    (remove-hook 'window-selection-change-functions
                 #'tr-ime-hook-check-selected-window)
    (remove-hook 'window-buffer-change-functions
                 #'tr-ime-hook-check-selected-window-buffer))
  (set-default symb bool))

(defcustom tr-ime-hook-p t
  "IME パッチ特有のアブノーマルフックをエミュレーションするか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

注意：w32-ime.el はこれらのアブノーマルフックを使って
ウィンドウやバッファの切り替えを認識して
IME/IM の同期や切り替えなどを行っている。
本設定を無効にすると、ウィンドウやバッファ切り替え時に
IME/IM が同期しなくなるなどの問題が発生する。
特別な目的が無い限りは non-nil (Enable) にしておくこと。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-hook--set
  :group 'tr-ime-hook)

(defun tr-ime-hook-unload-function ()
  "アンロードするため IME パッチ特有のアブノーマルフックを無効にする."
  (let (_dummy)
    (tr-ime-hook--set '_dummy nil)))

;;
;; provide
;;

(provide 'tr-ime-hook)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-hook.el ends here
