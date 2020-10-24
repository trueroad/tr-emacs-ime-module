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

(defgroup w32-tr-ime-module-core nil
  "コア機能設定

モジュールを使用する際のコア機能の設定。
通常は設定変更しないこと。"
  :group 'w32-tr-ime-module)

(defgroup w32-tr-ime-module-core-emacs28 nil
  "Emacs 28 以降向け設定

Emacs 28 以降で Module2 を使わない場合のコア機能の設定。
通常は設定変更しないこと。"
  :group 'w32-tr-ime-module-core)

(defgroup w32-tr-ime-module-workaround nil
  "ワークアラウンド設定"
  :group 'w32-tr-ime-module)

(defgroup w32-tr-ime-module-workaround-prefix-key nil
  "プレフィックスキー検出

Module2 を使用するなら、このワークアラウンドではなく
Module2 のプレフィックスキー検出を使うこと。"
  :group 'w32-tr-ime-module-workaround)

(defgroup w32-tr-ime-module-workaround-inconsist-ime nil
  "IME 状態食い違い検出

Module2 を使用するなら、このワークアラウンドではなく
Module2 の IME 状態変更通知による IM 状態同期が利用できる。"
  :group 'w32-tr-ime-module-workaround)

;;
;; C 実装による DLL をロードする
;;

;; Emacs 28 で導入された関数がある場合、
;; もしくは既に DLL モジュールがある場合はロードしない
(unless (or (fboundp #'w32-get-ime-open-status)
            (featurep 'tr-ime-module)
            (featurep 'tr-ime-module2))
  (load (concat "tr-ime-mod-" system-configuration)))

(declare-function w32-tr-ime-setopenstatus "tr-ime-module"
                  arg1 arg2)
(declare-function w32-tr-ime-getopenstatus "tr-ime-module"
                  arg1)
(declare-function w32-set-ime-open-status "w32fns.c" status) ; Emacs 28
(declare-function w32-get-ime-open-status "w32fns.c") ; Emacs 28

(declare-function ime-force-on "tr-ime-module-helper.el" &optional _dummy)
(declare-function ime-force-off "tr-ime-module-helper.el" &optional _dummy)
(declare-function ime-get-mode "tr-ime-module-helper.el")

;;
;; IME 状態変更・状態取得関数のエミュレーション
;;

(defcustom w32-tr-ime-module-set-ime-open-check-counter 3
  "GNU Emacs 28 の IME 状態変更関数使用後の状態確認回数上限

GNU Emacs 28 で Module 2 を使わない場合は
IME 状態変更関数 w32-set-ime-open-status を使うが、
これを呼んだ直後に IME 状態確認関数 w32-get-ime-open-status を呼んでも、
状態変更前を示す返り値が得られることがある。
そこで、ここで設定した回数を上限として状態変更が完了するまで確認する。
デフォルト値の 3 であれば最大で 3 回確認するが、
1 回目や 2 回目で完了していたらそこで打ち切る。
0 を設定した場合は一切完了確認しない。"
  :type 'integer
  :group 'w32-tr-ime-module-core-emacs28)

(if (fboundp #'w32-set-ime-open-status)
    (progn
      (defun ime-force-on (&optional _dummy)
        "IME を ON にする関数

GNU Emacs 28 の w32-set-ime-open-status で
IME パッチの ime-force-on をエミュレーションする。"
        (w32-set-ime-open-status t)
        (let ((counter 0))
          (while
              (and (< counter w32-tr-ime-module-set-ime-open-check-counter)
                   (not (ime-get-mode)))
            (setq counter (1+ counter)))))

      (defun ime-force-off (&optional _dummy)
        "IME を OFF にする関数

GNU Emacs 28 の w32-set-ime-open-status で
IME パッチの ime-force-off をエミュレーションする。"
        (w32-set-ime-open-status nil)
        (let ((counter 0))
          (while
              (and (< counter w32-tr-ime-module-set-ime-open-check-counter)
                   (ime-get-mode))
            (setq counter (1+ counter))))))

  (defun ime-force-on (&optional _dummy)
    "IME を ON にする関数

モジュールで IME パッチの ime-force-on をエミュレーションする。"
    (w32-tr-ime-setopenstatus
     (string-to-number (frame-parameter (selected-frame) 'window-id)) t))

  (defun ime-force-off (&optional _dummy)
    "IME を OFF にする関数

IME パッチの ime-force-off をエミュレーションする。"
    (w32-tr-ime-setopenstatus
     (string-to-number (frame-parameter (selected-frame) 'window-id)) nil)))

(if (fboundp #'w32-get-ime-open-status)
    (defalias #'ime-get-mode #'w32-get-ime-open-status
      "IME 状態を返す関数

GNU Emacs 28 の w32-set-ime-open-status のエイリアスとして
IME パッチの ime-get-mode を割り当てることでエミュレーションする。")

  (defun ime-get-mode ()
    "IME 状態を返す関数

IME パッチの ime-get-mode をエミュレーションする。
IME が OFF なら nil を、ON ならそれ以外を返す。"
    (w32-tr-ime-getopenstatus
     (string-to-number (frame-parameter (selected-frame) 'window-id)))))

;;
;; ウィンドウやバッファ状態の変更を通知するフックのエミュレーション
;;

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
(defvar w32-tr-ime-module-last-selected-window-buffer nil
  "選択ウィンドウのバッファ変更検出用変数")

(defun w32-tr-ime-module-hook-emulator ()
  "IME パッチ特有のアブノーマルフックをエミュレーションする関数

Emacs の標準的なフックの一つ post-command-hook に登録する。
post-command-hook によって、ほとんどのコマンドの動作後に呼ばれる。

この関数の動作は、
選択ウィンドウが変更されていたら select-window-functions を呼び、
ウィンドウが変わらずバッファが変更されていたら
set-selected-window-buffer-functions を呼ぶ。
どちらも変わっていなければ何もしない。"
  (let* ((window (selected-window))
         (buffer (window-buffer window)))
    (cond
     ((not (eq window w32-tr-ime-module-last-selected-window))
      (run-hook-with-args 'select-window-functions
                          w32-tr-ime-module-last-selected-window
                          window)
      (setq w32-tr-ime-module-last-selected-window window)
      (setq w32-tr-ime-module-last-selected-window-buffer buffer))
     ((not (eq buffer w32-tr-ime-module-last-selected-window-buffer))
      (run-hook-with-args 'set-selected-window-buffer-functions
                          w32-tr-ime-module-last-selected-window-buffer
                          window
                          buffer)
      (setq w32-tr-ime-module-last-selected-window-buffer buffer)))))

(defun w32-tr-ime-module-hook-emulator-p-set (symb bool)
  "コマンド実行後に IME パッチ特有のフックエミュレーションを呼ぶか否か設定

bool が non-nil ならエミュレーションさせる。
これにより post-command-hook にエミュレーション関数を追加することで、
ほとんどのコマンドの動作後に関数が呼ばれるようになる。
bool が nil なら停止させる（post-command-hook から削除する）。"
  (if bool (add-hook 'post-command-hook #'w32-tr-ime-module-hook-emulator)
    (remove-hook 'post-command-hook #'w32-tr-ime-module-hook-emulator))
  (set-default symb bool))

(defcustom w32-tr-ime-module-hook-emulator-p t
  "コマンド実行後に IME パッチ特有のフックエミュレーションを呼ぶか否か

この設定を変更する場合には custom-set-variables を使うこと。

post-command-hook を使って
IME パッチ特有のアブノーマルフックをエミュレーションする機能。

注意：w32-ime.el はこれらのアブノーマルフックを使って
ウィンドウやバッファの切り替えを認識して
IME/IM の同期や切り替えなどを行っている。
本設定を無効にすると、ウィンドウやバッファ切り替え時に
IME/IM が同期しなくなるなどの問題が発生する。
特別な目的が無い限りは non-nil (Enable) にしておくこと。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-hook-emulator-p-set
  :group 'w32-tr-ime-module-core)

(defun w32-tr-ime-module-hook-emulator-focus-change ()
  "フォーカス変更を検知してIME パッチ特有のフックエミュレーションを呼ぶ

after-focus-change-function は呼び出された時点では
まだ selected-frame が変わっていないことがあるので、
全フレームに対して frame-focus-state　でフォーカスを得ているか否かを判定し、
フォーカスを得ていたフレームで w32-tr-ime-module-hook-emulator
を呼び出してウィンドウやバッファの変更を検知する。

after-focus-change-function （GNU Emacs 27.1 以降）
に登録して使う。"
  (dolist (f (frame-list))
    (when (frame-focus-state f)
      (with-selected-frame f (w32-tr-ime-module-hook-emulator)))))

(defun w32-tr-ime-module-hook-emulator-focus-change-p-set (symb bool)
  "フォーカス変更時に IME パッチ特有のフックエミュレーションを呼ぶか否か設定

BOOL が non-nil なら設定する。
BOOL が nil ならフックから削除して設定を停止する。"
  (if bool
      (add-function :before
                    after-focus-change-function
                    #'w32-tr-ime-module-hook-emulator-focus-change)
    (remove-function
     after-focus-change-function
     #'w32-tr-ime-module-hook-emulator-focus-change))
  (set-default symb bool))

(defcustom w32-tr-ime-module-hook-emulator-focus-change-p t
  "フォーカス変更時に IME パッチ特有のフックエミュレーションを呼ぶか否か

この設定を変更する場合には custom-set-variables を使うこと。

after-focus-change-function を使って
IME パッチ特有のアブノーマルフックをエミュレーションする機能。

本設定を non-nil (Enable) にすると、
フォーカス変更時（フレーム変更時）にも
IME パッチ特有のフックエミュレーションを呼ぶようになる。
emacsclient などでバッファが変更になった場合は
post-command-hook が呼ばれずに検知漏れが発生するが、
フォーカス変更もトリガに加えることで漏れを防ぐことができる。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-hook-emulator-focus-change-p-set
  :group 'w32-tr-ime-module-core)

;;
;; プレフィックスキー（C-x など）を検出して IME OFF するワークアラウンド
;;

(defcustom w32-tr-ime-module-workaround-prefix-key-polling-time 0.1
  "プレフィックスキー検出ワークアラウンド用ポーリング時間（秒）"
  :type 'float
  :group 'w32-tr-ime-module-workaround-prefix-key)
(defcustom w32-tr-ime-module-workaround-prefix-key-list
  '(?\C-x ?\C-h ?\C-c ?\e)
  "プレフィックスキー検出ワークアラウンド用検出対象リスト

プレフィックスキーとして検出したいキーのリスト。"
  :type '(repeat integer)
  :group 'w32-tr-ime-module-workaround-prefix-key)

(defvar w32-tr-ime-module-workaround-prefix-key-undetected-flag t
  "プレフィックスキー未検出フラグ")
(defvar w32-tr-ime-module-workaround-prefix-key-before-ime-mode nil
  "プレフィックスキー検出時の IME 状態保存用")
(defvar w32-tr-ime-module-workaround-prefix-key-timer nil
  "プレフィックスキー検出用タイマ")

(defun w32-tr-ime-module-workaround-prefix-key ()
  "プレフィックスキー検出のためのポーリングで呼ばれる関数

未検出かつ最後に押されたキーが検出対象リストのいずれかだったら、
IME 状態を保存してから IME OFF にし、フラグを検出済にする。"
  (when w32-tr-ime-module-workaround-prefix-key-undetected-flag
    (let ((key (car (append (this-single-command-keys) nil))))
      (when (member key w32-tr-ime-module-workaround-prefix-key-list)
        (setq w32-tr-ime-module-workaround-prefix-key-before-ime-mode
              (ime-get-mode))
        (ime-force-off)
        (setq w32-tr-ime-module-workaround-prefix-key-undetected-flag
              nil)))))

(defun w32-tr-ime-module-workaround-prefix-key-restore-ime-mode ()
  "プレフィックスキー検出による自動 IME OFF から IME 状態を復帰させる関数

Emacs の標準的なフックの一つ pre-command-hook に登録する。
pre-command-hook によって、ほとんどのコマンドの動作後に呼ばれる。

この関数の動作は、
プレフィックスキー検出済であったら未検出に変え、
検出時の IME 状態が ON であれば IME ON に復帰する。
未検出であったら何もしない。"
  (when (not w32-tr-ime-module-workaround-prefix-key-undetected-flag)
    (setq w32-tr-ime-module-workaround-prefix-key-undetected-flag t)
    (when w32-tr-ime-module-workaround-prefix-key-before-ime-mode
      (ime-force-on))))

(defun w32-tr-ime-module-workaround-prefix-key-on (&optional periodic)
  "プレフィックスキーで自動 IME OFF するワークアラウンドを動作させる

periodic が nil ならアイドル状態検出タイマ、non-nil なら周期的タイマで、
プレフィックスキーを検出し自動的に IME を OFF にする。
あわせて pre-command-hook をフックしてコマンドの終了を検知し
IME 状態を復帰させる。"
  (setq w32-tr-ime-module-workaround-prefix-key-undetected-flag t)
  (add-hook 'pre-command-hook
            #'w32-tr-ime-module-workaround-prefix-key-restore-ime-mode)
  (if w32-tr-ime-module-workaround-prefix-key-timer
      (cancel-timer w32-tr-ime-module-workaround-prefix-key-timer))
  (setq w32-tr-ime-module-workaround-prefix-key-timer
        (if periodic
            (run-at-time t
                         w32-tr-ime-module-workaround-prefix-key-polling-time
                         #'w32-tr-ime-module-workaround-prefix-key)
          (run-with-idle-timer
           w32-tr-ime-module-workaround-prefix-key-polling-time t
           #'w32-tr-ime-module-workaround-prefix-key))))

(defun w32-tr-ime-module-workaround-prefix-key-off ()
  "プレフィックスキーで自動 IME OFF するワークアラウンドを停止させる

タイマを停止させ、フックも削除することでワークアラウンドを停止させる。"
  (remove-hook 'pre-command-hook
               #'w32-tr-ime-module-workaround-prefix-key-restore-ime-mode)
  (if w32-tr-ime-module-workaround-prefix-key-timer
      (cancel-timer w32-tr-ime-module-workaround-prefix-key-timer))
  (setq w32-tr-ime-module-workaround-prefix-key-timer nil))

(defun w32-tr-ime-module-workaround-prefix-key-p-set (symb bool)
  "プレフィックスキー検出ワークアラウンドを動作させるか否か設定する

bool が non-nil なら動作させる。nil なら停止させる。"
  (if bool (w32-tr-ime-module-workaround-prefix-key-on nil)
    (w32-tr-ime-module-workaround-prefix-key-off))
  (set-default symb bool))

(defcustom w32-tr-ime-module-workaround-prefix-key-p t
  "プレフィックスキー検出ワークアラウンドを動作させるか否か

この設定を変更する場合には custom-set-variables を使うこと。

コマンドのキーシーケンスになる最初のキーである
プレフィックスキー（C-x など）をタイマによるポーリングで検出すると、
自動的に IME OFF にして、コマンド終了後に IME 状態を戻す機能。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-workaround-prefix-key-p-set
  :group 'w32-tr-ime-module-workaround-prefix-key)

;;
;; IME 状態の食い違いを検出して修正するワークアラウンド
;;

(defcustom w32-tr-ime-module-workaround-inconsistent-ime-polling-time 1.0
  "IME 状態食い違い検出修正用ポーリング時間（秒）"
  :type 'float
  :group 'w32-tr-ime-module-workaround-inconsist-ime)

(defcustom
  w32-tr-ime-module-workaround-inconsistent-ime-call-hook-emulator-p t
  "IME 状態食い違い検出修正前にフックエミュレーション関数を呼ぶか否か

ポーリング時の食い違い検出の前にフックエミュレーション関数を呼ぶ機能。
これにより未検出のウィンドウ変更やバッファ変更を検知し、
IME パッチ特有のアブノーマルフックが呼び IME/IM 状態が整えられる。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :group 'w32-tr-ime-module-workaround-inconsist-ime)

(defvar w32-tr-ime-module-workaround-inconsistent-ime-timer nil
  "IME 状態食い違い検出修正用タイマ")

(defun w32-tr-ime-module-workaround-inconsistent-ime ()
  "IME 状態食い違い検出修正のためのポーリングで呼ばれる関数

w32-tr-ime-module-workaround-inconsistent-ime-call-hook-emulator-p
が non-nil であれば、まずフックエミュレーション関数を呼ぶ。
これによってウィンドウやバッファの切り替え未検出があったら、
アブノーマルフックが呼ばれて、IME/IM 状態が整えられる。

その上で IME 状態と IM 状態が食い違ったら IM 状態を反転して一致させる。
これにより、IME 側トリガの状態変更を IM に反映させる。"
  (when w32-tr-ime-module-workaround-inconsistent-ime-call-hook-emulator-p
    (w32-tr-ime-module-hook-emulator))
  (let ((ime-status (ime-get-mode)))
    (cond ((and ime-status
                (not current-input-method))
           (activate-input-method "W32-IME"))
          ((and (not ime-status)
                current-input-method)
           (deactivate-input-method)))))

(defun w32-tr-ime-module-workaround-inconsistent-ime-p-set (symb bool)
  "IME 状態食い違い検出修正ワークアラウンドを動作させるか否か設定する

bool が non-nil ならポーリングさせる。
bool が nil なら停止させる。"
  (when w32-tr-ime-module-workaround-inconsistent-ime-timer
    (cancel-timer w32-tr-ime-module-workaround-inconsistent-ime-timer)
    (setq w32-tr-ime-module-workaround-inconsistent-ime-timer nil))
  (when bool
    (setq w32-tr-ime-module-workaround-inconsistent-ime-timer
          (run-at-time
           t w32-tr-ime-module-workaround-inconsistent-ime-polling-time
           #'w32-tr-ime-module-workaround-inconsistent-ime)))
  (set-default symb bool))

(defcustom w32-tr-ime-module-workaround-inconsistent-ime-p nil
  "IME 状態食い違い検出修正ワークアラウンドを動作させるか否か

この設定を変更する場合には custom-set-variables を使うこと。

IME 側トリガの状態変更（半角/全角キーやマウスでの切り替え）を
定期的なタイマによるポーリングで検出して IM 側を同期させるための機構。"
  :type '(choice (const :tag "Enable" t)
                 (const :tag "Disable" nil))
  :set #'w32-tr-ime-module-workaround-inconsistent-ime-p-set
  :group 'w32-tr-ime-module-workaround-inconsist-ime)

;;
;; キー設定
;;

;; Alt + 半角全角で IME だけでなく IM も切り替わるようにする
(define-key global-map [M-kanji] 'toggle-input-method)

;; C-s (isearch-forward) などでも Alt + 半角全角が効くようにする
(define-key isearch-mode-map [M-kanji] 'isearch-toggle-input-method)

;;
;; provide
;;

(provide 'tr-ime-module-helper)
