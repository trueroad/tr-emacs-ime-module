;;; tr-ime-reconversion.el --- Reconversion -*- lexical-binding: t -*-

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
;; ユーザ設定用
;;

(defgroup tr-ime-reconversion nil
  "再変換 (advanced)."
  :group 'tr-ime)

;;
;; 再変換 (RECONVERSION)
;;

(declare-function tr-ime-modadv--notify-reconvert-string "tr-ime-modadv"
                  (arg1 arg2 arg3))

(defun tr-ime-reconversion--notify-reconvert-string ()
  "RECONVERTSTRING 構造体用の材料を収集して UI スレッドへ通知する.

point のある行全体の文字列と、文字列中の point 位置を収集し、
advanced の C++ 実装である tr-ime-modadv--notify-reconvert-string 関数を呼び、
UI スレッドへ通知する。
ノーマルフック tr-ime-modadv--reconvertstring-hook および
tr-ime-modadv--documentfeed-hook に登録して使う。"
  (tr-ime-modadv--notify-reconvert-string
   (string-to-number (frame-parameter nil 'window-id))
   (buffer-substring-no-properties
    (line-beginning-position) (line-end-position))
   (- (point) (line-beginning-position))))

;;
;; 設定用
;;

(declare-function tr-ime-modadv--set-reconversion "tr-ime-modadv"
                  (arg1 arg2))

(defun tr-ime-reconversion--set (symb bool)
  "再変換 (RECONVERSION) 動作を行うか否か設定する.

SYMB には tr-ime-reconversion-p を指定する。
BOOL が non-nil なら再変換動作を行う。
そうでなければ行わない。"
  (if bool
      (when (and (boundp 'tr-ime-enabled-features)
                 (eq tr-ime-enabled-features 'advanced))
        (add-hook 'tr-ime-modadv--reconvertstring-hook
                  #'tr-ime-reconversion--notify-reconvert-string)
        (tr-ime-modadv--set-reconversion
         (string-to-number (frame-parameter nil 'window-id)) t))
    (when (fboundp 'tr-ime-modadv--set-reconversion)
      (tr-ime-modadv--set-reconversion
       (string-to-number (frame-parameter nil 'window-id)) nil))
    (remove-hook 'tr-ime-modadv--reconvertstring-hook
                 #'tr-ime-reconversion--notify-reconvert-string))
  (set-default symb bool))

(defcustom tr-ime-reconversion-p t
  "再変換 (RECONVERSION) 動作を行うか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

確定済文字列にカーソルを置いて変換キーを押すと、
カーソルのあった場所の確定済文字列が未確定文字列になって、
再変換できるようになる機能。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-reconversion--set
  :group 'tr-ime-reconversion)

(defun tr-ime-reconversion-unload-function ()
  "アンロードするため再変換 (RECONVERSION) を無効にする."
  (let (_dummy)
    (tr-ime-reconversion--set '_dummy nil)))

;;
;; provide
;;

(provide 'tr-ime-reconversion)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-reconversion.el ends here
