;;; tr-ime-isearch.el --- Handle isearch-mode -*- lexical-binding: t -*-

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

(defgroup tr-ime-isearch nil
  "\"isearch-mode\" 設定 (advanced)."
  :group 'tr-ime)

;;
;; エコーエリア検出
;;

(defvar tr-ime-isearch--last-echo-area-0-point nil
  "エコーエリア 0 の point 移動検出用.")

(defvar tr-ime-isearch--last-echo-area-1-point nil
  "エコーエリア 1 の point 移動検出用.")

(defvar tr-ime-isearch--last-echo-area-buffer nil
  "最後に使用されたエコーエリアバッファ.")

(defun tr-ime-isearch--start ()
  "エコーエリアのバッファのうちどちらが使われているか検出を開始する.

\"isearch-mode-hook\" に登録することにより、\"isearch-mode\" 中に
どのエコーエリアが使われているか検出できるようにする。"
  (setq tr-ime-isearch--last-echo-area-0-point
        (with-current-buffer " *Echo Area 0*"
          (point)))
  (setq tr-ime-isearch--last-echo-area-1-point
        (with-current-buffer " *Echo Area 1*"
          (point))))

(defun tr-ime-isearch--detect-echo-area-buffer ()
  "使用中のエコーエリアバッファを検出して返す.

\"isearch-mode\" 中に使用しているエコーエリアバッファを返す。"
  (let* ((point0 (with-current-buffer " *Echo Area 0*"
                   (point)))
         (point1 (with-current-buffer " *Echo Area 1*"
                   (point)))
         (buff (cond ((and tr-ime-isearch--last-echo-area-0-point
                           (/= point0
                               tr-ime-isearch--last-echo-area-0-point))
                      (get-buffer " *Echo Area 0*"))
                     ((and tr-ime-isearch--last-echo-area-1-point
                           (/= point1
                               tr-ime-isearch--last-echo-area-1-point))
                      (get-buffer " *Echo Area 1*"))
                     (tr-ime-isearch--last-echo-area-buffer
                      tr-ime-isearch--last-echo-area-buffer)
                     (t
                      (get-buffer " *Echo Area 0*")))))
    ;; (tr-ime-modadv--debug-output
    ;;  (format-message "last point0 %s, point1 %s, buff %s"
    ;;                  tr-ime-isearch--last-echo-area-0-point
    ;;                  tr-ime-isearch--last-echo-area-1-point
    ;;                  tr-ime-isearch--last-echo-area-buffer))
    ;; (tr-ime-modadv--debug-output
    ;;  (format-message " now point0 %s, point1 %s, buff %s"
    ;;                  point0 point1 buff))
    (setq tr-ime-isearch--last-echo-area-0-point point0)
    (setq tr-ime-isearch--last-echo-area-1-point point1)
    (setq tr-ime-isearch--last-echo-area-buffer buff)))

;;
;; 未確定文字列の表示位置
;;

(declare-function tr-ime-modadv--set-composition-window "tr-ime-modadv"
                  (arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8))

(defun tr-ime-isearch--update ()
  "未確定文字列の表示位置を更新・設定する.

\"isearch-update-post-hook\" に登録することにより、
\"isearch-mode\" 中の未確定文字列の表示位置を
ミニバッファ上の文字入力位置を更新・設定する。"
  (with-current-buffer (tr-ime-isearch--detect-echo-area-buffer)
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
      ;; (tr-ime-modadv--debug-output
      ;;  (format-message
      ;;   "sw %s, wc %s, x %s, y %s, edges %s"
      ;;   sw wc x y edges))
      ;; (tr-ime-modadv--debug-output
      ;;  (format-message
      ;;   "left %s, top %s, right %s, bottom %s, px %s, py %s"
      ;;   left top right bottom px py))
      (tr-ime-modadv--set-composition-window
       (string-to-number (frame-parameter nil 'window-id))
       1 px py left top right bottom))))

(defun tr-ime-isearch--end ()
  "未確定文字列の表示位置を元に戻す.

\"isearch-mode-end-hook\" に登録することにより、\"isearch-mode\" 終了後に
未確定文字列の表示位置を通常のバッファ内のカーソル位置に戻す。"
  (tr-ime-modadv--set-composition-window
   (string-to-number (frame-parameter nil 'window-id))
   0 0 0 0 0 0 0))

;;
;; 未確定文字列表示位置を文字入力位置にするか否か設定
;;

(defun tr-ime-isearch--set (symb bool)
  "未確定文字列表示位置を文字入力位置にするか否か設定する.

SYMB には tr-ime-isearch-p を指定する。
BOOL が non-nil なら \"isearch-mode\" 中の
未確定文字列表示位置を文字入力位置に設定する。
そうでなければ設定しない。"
  (if bool
      (progn
        (add-hook 'isearch-mode-hook #'tr-ime-isearch--start)
        (add-hook 'isearch-update-post-hook #'tr-ime-isearch--update)
        (add-hook 'isearch-mode-end-hook #'tr-ime-isearch--end))
    (remove-hook 'isearch-mode-hook #'tr-ime-isearch--start)
    (remove-hook 'isearch-update-post-hook #'tr-ime-isearch--update)
    (remove-hook 'isearch-mode-end-hook #'tr-ime-isearch--end))
  (set-default symb bool))

(defcustom tr-ime-isearch-p t
  "未確定文字列表示位置を文字入力位置にするか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

\"isearch-mode\" 中に未確定文字列をエコーエリア（ミニバッファ）
に表示する機能。本機能が無効の場合、
未確定文字列は \"isearch-mode\" に入る前の入力位置に表示される。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-isearch--set
  :group 'tr-ime-isearch)

;;
;; WM_IME_STARTCOMPOSITION で常に DefSubclassProc を呼ぶか
;;

(declare-function tr-ime-modadv--set-startcomposition-defsubclassproc
                  "tr-ime-modadv"
                  (arg1 arg2))

(defun tr-ime-isearch--defsubclassproc-set (symb bool)
  "WM_IME_STARTCOMPOSITION で常に DefSubclassProc を呼ぶか否か設定する.

SYMB には tr-ime-isearch-defsubclassproc-p を指定する。
BOOL が non-nil なら常に呼ぶようになる。
そうでなければ未確定文字列ウィンドウの位置設定中は呼ばなくなる。"
  (tr-ime-modadv--set-startcomposition-defsubclassproc
   (string-to-number (frame-parameter nil 'window-id)) bool)
  (set-default symb bool))

(defcustom tr-ime-isearch-defsubclassproc-p nil
  "WM_IME_STARTCOMPOSITION で常に DefSubclassProc を呼ぶか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

WM_IME_STARTCOMPOSITION ハンドラにおいて、
\"isearch-mode\" 中（未確定文字列ウィンドウの位置設定中）は
DefSubcalssProc を呼ばず Emacs のメッセージ処理をスキップしている。
これは Emacs で未確定文字列ウィンドウの位置を \"isearch-mode\"
に入る前の文字入力位置に設定してしまうからで、
この設定後に位置を上書きしても未確定文字列ウィンドウがチラつくからである。
しかし、何らかの理由で元の Emacs の処理に戻さなければならない時は、
本設定を non-nil (Enable) にすることで \"isearch-mode\" 中であっても、
DefSubcalssProc により Emacs のメッセージ処理が必ず呼ばれるようになる。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-isearch--defsubclassproc-set
  :group 'tr-ime-isearch-mode)

;;
;; アンロード
;;

(defun tr-ime-isearch-unload-function ()
  "アンロードするため \"isearch-mode\" 設定を無効にする."
  (let (_dummy)
    (tr-ime-isearch--set '_dummy nil)
    (tr-ime-isearch--defsubclassproc-set '_dummy nil)))

;;
;; provide
;;

(provide 'tr-ime-isearch)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-isearch.el ends here
