;;; tr-ime-download.el --- Download files -*- lexical-binding: t -*-

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
;; モジュール DLL のダウンロード
;;

(defconst tr-ime-download--url-prefix
  "https://github.com/trueroad/tr-ime-dist/raw/v0.5.0/"
  "ダウンロード先 URL のプレフィックス.")

(defconst tr-ime-download--url-suffix ".gz"
  "ダウンロード先 URL のサフィックス.")

(defconst tr-ime-download--file-sum-alist
  '(("tr-ime-mod-1-i686-pc-cygwin.dll" .
     "68c734d8b1c56abb089b05f0ece0fe1b2c47ed818ea118be1cadd678d7a2ceb1")
    ("tr-ime-mod-1-i686-w64-mingw32.dll" .
     "ba162d3f927d779fa42f6906b3c8ac1252dd3f7332938b28c41b0b1cfee646dd")
    ("tr-ime-mod-1-x86_64-pc-cygwin.dll" .
     "f8362c23faa9835c6071de62967125a3683304718703f39296baec70bbdb0981")
    ("tr-ime-mod-1-x86_64-w64-mingw32.dll" .
     "92159ebcf946ec6d5a498d9bc473ec3788aaee0caea51f34435da3535f70f2bf")
    ("tr-ime-modadv-2-i686-pc-cygwin.dll" .
     "3a1dd0e3b114ee3ae1d9bc24f0723a11f4ac57dceaf0de6967262c611e80fe9f")
    ("tr-ime-modadv-2-i686-w64-mingw32.dll" .
     "9fc539e63601763f7e94fd82523c8b98ac5f2e49fb5ecc7040454488a1e77152")
    ("tr-ime-modadv-2-x86_64-pc-cygwin.dll" .
     "c3f6ec346293800268af2004053bf6ebde1fe513554e9755d1fa3b6662bd619b")
    ("tr-ime-modadv-2-x86_64-w64-mingw32.dll" .
     "d3492a55d6342351f38c3cb7c1c3afdc6cba36c58934786e7d2bc7ac999bc0c5"))
  "ダウンロードする DLL の sha256sum.")

(defconst tr-ime-download--dir (file-name-directory load-file-name)
  "モジュール DLL のダウンロード先ディレクトリ.")

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

(defun tr-ime-download-mod-file (name &optional no-confirm)
  "モジュール DLL をダウンロードするか否か確認してダウンロードする.

NAME にモジュール DLL のファイル名拡張子なしを指定する。
NO-CONFIRM が non-nil なら確認せずにダウンロードする。"
  (if (or no-confirm
          (y-or-n-p (format "Download %s.dll? " name)))
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
