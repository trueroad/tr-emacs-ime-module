;;; tr-ime-download.el --- Download files -*- lexical-binding: t -*-

;; Copyright (C) 2020 Masamichi Hosoda

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
;; 定数設定
;;

(defconst tr-ime-download--url-prefix
  "https://github.com/trueroad/tr-ime-dist/raw/v0.4.0/"
  "ダウンロード先 URL のプレフィックス.")

(defconst tr-ime-download--url-suffix ".gz"
  "ダウンロード先 URL のサフィックス.")

(defconst tr-ime-download--file-sum-alist
  '(("tr-ime-mod-1-i686-pc-cygwin.dll" .
     "caf4a4984c9096bad185238e7db048b79387940eef9018d1d9effd56b25c8aa0")
    ("tr-ime-mod-1-i686-w64-mingw32.dll" .
     "ae2bd610a148ea8f8fbe7b799753ed5f70b293b3b0776712ba279a6efd8157ea")
    ("tr-ime-mod-1-x86_64-pc-cygwin.dll" .
     "1026535d39ee64644700bb8bc73dc04c1be38559ed25a08d3c52ffa0e384a892")
    ("tr-ime-mod-1-x86_64-w64-mingw32.dll" .
     "7cd7d8229d997d914eba0fbbf966013eea7d32519e6c7e8cb69e9aaeba308ebe")
    ("tr-ime-modadv-1-i686-pc-cygwin.dll" .
     "5a427a3b742c2c529e1ebc302c822f78bdee7ab702dbf714b7228779f596d0ff")
    ("tr-ime-modadv-1-i686-w64-mingw32.dll" .
     "42f3b26cc4c4b22c063a9f349a20eed992ab26a6dce3b009c2a457b89fa5115b")
    ("tr-ime-modadv-1-x86_64-pc-cygwin.dll" .
     "70d00e1b2a4854093e150fb3e52666c0db82051dcd87320afd7f1ac9422834f3")
    ("tr-ime-modadv-1-x86_64-w64-mingw32.dll" .
     "728a59ddf4cf8d621a8f262bf61bf4a6973b68e342b1219f0c43f79a12d7d65d"))
  "ダウンロードする DLL の sha256sum.")

(defconst tr-ime-download--dir (file-name-directory load-file-name)
  "モジュール DLL のダウンロード先ディレクトリ.")

;;
;; ユーザ設定用
;;

(defgroup tr-ime-download nil
  "モジュール DLL 自動ダウンロード"
  :group 'tr-ime)

(defcustom tr-ime-download-settings 'query
  "モジュール DLL 自動ダウンロード設定.

モジュール DLL が \"load-path\" 上で見つからなかったときの動作を指定する。
'query なら自動ダウンロードするか否かユーザに尋ねる。
'yes ならばユーザに尋ねずに自動ダウンロードして使う。
'no ならばユーザに尋ねずに DLL が無い旨のエラーにする。"
  :type '(choice (const :tag "Query" 'query)
                 (const :tag "Yes" 'yes)
                 (const :tag "No" 'no))
  :group 'tr-ime-download)

;;
;; モジュール DLL のダウンロード
;;

(defun tr-ime-download--download-and-unzip (url filename)
  "モジュール DLL をダウンロードして解凍する.

URL からダウンロードして解凍したファイルを、
ディレクトリ DIR のファイル名 FILENAME に置く。"
  (let ((tmpfilename (make-temp-file (concat tr-ime-download--dir "tmp"))))
    (url-copy-file url tmpfilename t)
    (with-temp-file (concat tr-ime-download--dir filename)
      (set-buffer-multibyte nil)
      (insert-file-contents-literally tmpfilename)
      (delete-file tmpfilename)
      (zlib-decompress-region (point-min) (point-max)))))

(defun tr-ime-download--good-file-p (filename)
  "ファイルのハッシュを確認する.

ディレクトリ DIR のファイル名 FILENAME について sha256sum を確認する。
一致すれば non-nil を返す。そうでなければ nil を返す。"
  (let ((sum (cdr (assoc filename tr-ime-download--file-sum-alist)))
        (path (concat tr-ime-download--dir filename)))
    (if (file-exists-p path)
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally path)
          (string= (secure-hash 'sha256 (current-buffer)) sum))
      nil)))

(defun tr-ime-download--request-file (filename)
  "モジュール DLL をダウンロードしてロードする.

FILENAME のモジュール DLL をダウンロードしてロードする。"
  (tr-ime-download--download-and-unzip
   (concat tr-ime-download--url-prefix
           filename
           tr-ime-download--url-suffix)
   filename)
  (if (tr-ime-download--good-file-p filename)
      (progn
        (set-file-modes (concat tr-ime-download--dir filename) #o755)
        (load filename))
    (delete-file (concat tr-ime-download--dir filename))
    (error "Download failed: %s" filename)))

(defun tr-ime-download-mod-file (name)
  "モジュール DLL をダウンロードするか否か確認してダウンロードする.

NAME にモジュール DLL のファイル名拡張子なしを指定する。"
  (if (cond
       ((eq tr-ime-download-settings 'query)
        (y-or-n-p (format "Download %s.dll? " name)))
       ((eq tr-ime-download-settings 'yes)
        t)
       ((eq tr-ime-download-settings 'no)
        nil)
       (t
        (error "Variable tr-ime-download-settings value %S is unknown"
               tr-ime-download-settings)))
      (tr-ime-download--request-file (concat name ".dll"))
    (error "%s.dll is not found.

Download or build it and put it in the directory specified in \"load-path.\"
See https://github.com/trueroad/tr-emacs-ime-module" name)))

;;
;; provide
;;

(provide 'tr-ime-download)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tr-ime-download.el ends here
