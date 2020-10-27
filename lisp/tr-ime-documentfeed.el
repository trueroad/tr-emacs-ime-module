;;; tr-ime-documentfeed.el --- Documentfeed -*- lexical-binding: t -*-

;; Copyright (C) 2020 Masamichi Hosoda

;; Author: Masamichi Hosoda <trueroad@trueroad.jp>
;; URL: https://github.com/trueroad/tr-emacs-ime-module
;; Version: 0.3.50
;; Package-Requires: ((emacs "27.1"))

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

(defgroup tr-ime nil
  "Emulator of GNU Emacs IME patch for Windows (tr-ime)"
  :group 'emacs)

(defgroup tr-ime-documentfeed nil
  "前後の確定済文字列を参照した変換"
  :group 'tr-ime)

;;
;; 前後の確定済文字列を参照した変換 (DOCUMENTFEED)
;;

(declare-function tr-ime-modadv--set-documentfeed "tr-ime-modadv"
                  arg1 arg2)

(autoload 'tr-ime-reconversion--notify-reconvert-string
  "tr-ime-reconversion")

(defun tr-ime-documentfeed--set (symb bool)
  "前後の確定済文字列を参照した変換 (DOCUMENTFEED) 動作を行うか否か設定する.

SYMB には tr-ime-documentfeed-p を指定する。
BOOL が non-nil なら DOCUMENTFEED 動作を行う。
それ以外なら行わない。"
  (if bool
      (progn
        (add-hook 'tr-ime-modadv--documentfeed-hook
                  #'tr-ime-reconversion--notify-reconvert-string)
        (tr-ime-modadv--set-documentfeed
         (string-to-number (frame-parameter nil 'window-id)) t))
    (tr-ime-modadv--set-documentfeed
     (string-to-number (frame-parameter nil 'window-id)) nil)
    (remove-hook 'tr-ime-modadv--documentfeed-hook
                 #'tr-ime-reconversion--notify-reconvert-string))
  (set-default symb bool))

(defcustom tr-ime-documentfeed-p t
  "前後の確定済文字列を参照した変換 (DOCUMENTFEED) 動作を行うか否か.

この設定を変更する場合には \"custom-set-variables\" を使うこと。

確定済文字列のあるところにカーソルを置いて文字を入力・変換すると、
カーソルのあった場所の確定済文字列によって変換候補が変わる機能。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'tr-ime-documentfeed--set
  :group 'tr-ime-documentfeed)

;;
;; provide
;;

(provide 'tr-ime-documentfeed)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-documentfeed.el ends here
