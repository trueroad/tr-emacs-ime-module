[![MELPA](https://melpa.org/packages/tr-ime-badge.svg)](https://melpa.org/#/tr-ime)

[日本語 / [English](./README.en.md) ]

# Emulator of GNU Emacs IME patch for Windows (tr-ime)

※注意：バージョン 0.3.0 (2020-09-25) から 0.4.1 (2020-11-28) へかけて、
パッケージ名、ファイル名、関数名、変数名、設定方法などを整理して変更しました。

Windows 用 (MinGW/Cygwin) GNU Emacs でダイナミックモジュールの機構を利用し、
IME パッチ無しの公式バイナリなどでも、
IME による日本語入力を使いやすくする試みです。

## はじめに

GNU Emacs 26.2 から、
IME パッチがなくても MS-IME などの IME (input method editor)
による日本語入力が「とりあえず」できるようになりました。
しかし IME パッチ無しだと、Emacs 自前の
かな漢字変換 (IM: input method) と IME が連動しないため、

* IME を on/off してもモードラインのかな漢字変換状態表示が変わらない
* IME on で使っているとき、
  状況に応じて自動的に IME off してくれる機能が無く、
  直接入力したいキーが IME に吸われて未確定文字になってしまう
    * ミニバッファでの y/n 入力
    * M-x によるミニバッファでのコマンド名入力
    * など
* C-\\ (toggle-input-method) すると IME ではなくて、
  IM による Emacs 独自の日本語入力モードになってしまい、
  他の Windows アプリと操作感や変換辞書が異なるため使いにくい

という問題があり使いにくくなってしまいます
（他にもあると思いますが、個人的に困った部分のみ列挙しています）。
IME パッチの方が便利なのは間違いありませんが、
Emacs リリースのたびに煩雑なことをしなければならなかったり、
場合によっては IME パッチによって Emacs が不安定になってしまうこともあります。

そこで、全世界のユーザが使っている IME パッチ無しで安定した
Emacs バイナリを使い、上記のような問題を解消できる最小限を目指します。
幸い、GNU Emacs 27.1 からダイナミックモジュールが
デフォルトで有効になったので、これを使って IME 関連の実装を追加します。

同様の試みに
[w32-imeadv](https://github.com/maildrop/w32-imeadv)
がありますが、
これは IME パッチの機能を完全に再現して置き換えることを目指しているようで、
C++ 実装のモジュールによって、それなりに複雑で大がかりな機構
（外部プロセスを経由したスレッド間通信など）を備えています。
一方で IME パッチで UI や設定をつかさどる要の部分ともいえる Lisp 実装
[w32-ime.el](https://github.com/trueroad/w32-ime.el)
が使えるわけでは無いので、こうした部分は異なってしまっています。

本プロジェクトでは、安定性を重視するため、
モジュールの実装は必要最小限として複雑な実装をしない
standard 版を用意しています。
standard 版は IME パッチの全機能を再現することはできません。
逆に UI や設定など従来の IME パッチと同じようにしたいため、
[w32-ime.el](https://github.com/trueroad/w32-ime.el)
を、ほとんどそのまま使えるようにします。
さらに、standard 版だとやっぱり使いにくいところもあったので、
機能を増やしていった advanced 版も用意しています。

## 動作環境

* Windows 用 (MinGW/Cygwin) GNU Emacs
    * IME パッチは不要です
    * GNU Emacs 27.1 以降が必要です
    * ダイナミックモジュールが有効になっている必要があります
        * GNU Emacs 27.1 からデフォルトで有効です
    * Cygwin 64 bit
      [emacs-w32](https://cygwin.com/packages/summary/emacs-w32.html)
      27.1-2 で動作確認しています
    * MinGW では
      GNU 公式バイナリの 27.1 (64 bit) や、GNU が配布する pretest の
      28.0.50-snapshot-2020-07-05-x86_64 バイナリで動作しました
        * MinGW は常用していないので
          気が付いていない不具合があるかもしれません

## インストール・設定

[MELPA](https://melpa.org/)
に収録されたため、Emacs からインターネットにアクセスできるなら
MELPA を使う方法が簡単です。

### MELPA 設定

[MELPA](https://melpa.org/)
の設定をしていないなら、まずは MELPA の設定をしてください。
とりあえず私は以下を `~/.emacs/init.el` に書いています。

```el
(customize-set-variable 'package-archives
                        `(,@package-archives
                          ("melpa" . "https://melpa.org/packages/")))
```

### インストール

MELPA が使えるようになっていれば、
`M-x package-install` して `tr-ime` を入力すればインストールできます。
あるいは、以下のようにしてもインストールできます。

```el
(package-install 'tr-ime)
```

### tr-ime 設定

tr-ime には、最低限の単純で基本的な機能のみを実装した standard と、
メッセージフックやサブクラス化といった多少複雑な機構で、
より高度な機能を実装した実験的な advanced の 2 つがあります。
安定性を取りたい場合は standard を、
実験的でもいいから高度な機能が欲しい場合は advanced を選んでください。
両方を同時に使うことはできません。

advanced の機能は以下の通りです。

* 再変換 (RECONVERSION) および
  前後の確定済文字列を参照した変換 (DOCUMENTFEED)に対応
    * standard ではどちらもできません
    * もし不安定になるようなら advanced でも設定で無効にすることができます
* すべての IME on/off 方法にタイムラグなく対応
  （IME 状態変更通知による IME/IM 状態同期）
    * standard では Alt + 半角/全角キー（もしくは C-\\）による
      IME on/off なら問題ありませんが、
      半角/全角キー単独やマウスで切り替えた場合には、
      タイマによるワークアラウンドで IM 状態との食い違いを解消しており、
      状態同期にタイムラグが発生するほか、
      ワークアラウンドによる負荷がかかります
* C-s など isearch-mode の検索中に未確定文字列をミニバッファの
  文字入力位置に表示できる
    * standard は、
      isearch-mode へ入る前に元々入力していた位置へ表示されます
* 未確定文字列のフォントが設定できる
    * standard は設定できないため、
      変換中は☃や🍣のような文字がおかしな表示（いわゆるトーフ）になります
* IME on の状態で C-x, C-c, C-h など、
  コマンドのキーシーケンスになる最初の文字（以下、プレフィックスキー）
  を押すと自動的に IME off になる
    * standard はタイマ動作によるワークアラウンドで
      なんとか同じような動作を実現していますが、
      タイマ動作なのでタイミングによっては自動 off にならない、
      負荷が問題となる、などの可能性があります
* IME on/off 制御や状態取得に Microsoft
  の公式なドキュメントに記載されている方法を使用
    * standard は（Emacs 27 の場合）
      公式ドキュメントに記載されていない方法を使用しているので、
      最悪の場合は Windows のアップデートなどにより、
      いきなり動かなくなる可能性もあります
    * Emacs 28 なら standard でも問題ありません
* IME on/off に連動してタスクバーの IME 状態表示アイコンが切り替わる
    * standard はもちろん IME パッチでも
      Windows 10 1909 などの MS-IME で使っているとアイコン表示が変わりません
    * Windows 10 20H2 などの MS-IME だと standard でも問題ないようです

どちらの設定であっても、必要なモジュール DLL が見つからない場合は
ダウンロードするか否か尋ねてきますので、よければ y を選んでください。
（自動ダウンロードが嫌であれば n を選び、
必要なモジュール DLL のファイル名を確認して、
そのファイルを load-path 上に置いてから再度実施してください。）
Cygwin の場合は、後述の「autorebase 設定（Cygwin のみ）」もしてください。

また、ここでは MEPLA でインストールした時の、
必要最低限の設定のみを示しています。
より使いやすくするためには後述の「w32-ime.el 設定」や
「tr-ime 詳細設定」をご覧ください。
MELPA 以外でインストールした場合は
後述の「MELPA を使わない」をご覧ください。

#### standard

```el
(tr-ime-standard-install)
(setq default-input-method "W32-IME")
(w32-ime-initialize)
```

#### advanced

```el
(tr-ime-advanced-install)
(setq default-input-method "W32-IME")
(w32-ime-initialize)
```

### 簡単な使い方

ここまでに示した必要最低限の設定をしたら、
モードラインの左端に `[O]` または `[|]` が表示されるようになります。
`[O]` は IME が off であることを示し、
`[|]` は IME が on であることを示します。

IME の on/off トグルは、Emacs 流の C-\\ でもできますし、
Windows 流の「半角/全角」キーでも、
古い Windows 流の「Alt + 半角/全角」キーでもできます。

IME on の時は、MS-IME など Windows の IME を使った日本語入力が可能です。

## w32-ime.el 設定

tr-ime は従来の IME パッチで UI や設定をつかさどっていた、
[w32-ime.el](https://github.com/trueroad/w32-ime.el)
を、ほとんどそのまま使用しています。
ですので、従来の IME パッチの設定の多くを使うことができます。

ただし、standard で使用する場合は
`(global-set-key [M-kanji] 'ignore)` をしないでください
（advanced なら問題ありません）。

### 基本設定

w32-ime.el の必要最小限の設定は、

```el
(setq default-input-method "W32-IME")
(w32-ime-initialize)
```

ですが、これを以下のような、
よく見かける IME パッチの設定に置き換えると、
より使いやすくなります。

```el
;; IM のデフォルトを IME に設定
(setq default-input-method "W32-IME")
;; IME のモードライン表示設定
(setq-default w32-ime-mode-line-state-indicator "[--]")
(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
;; IME 初期化
(w32-ime-initialize)
;; IME 制御（yes/no などの入力の時に IME を off にする）
(wrap-function-to-control-ime 'universal-argument t nil)
(wrap-function-to-control-ime 'read-string nil nil)
(wrap-function-to-control-ime 'read-char nil nil)
(wrap-function-to-control-ime 'read-from-minibuffer nil nil)
(wrap-function-to-control-ime 'y-or-n-p nil nil)
(wrap-function-to-control-ime 'yes-or-no-p nil nil)
(wrap-function-to-control-ime 'map-y-or-n-p nil nil)
(wrap-function-to-control-ime 'register-read-with-preview nil nil)
```

必要最小限の設定では、モードラインが
IME off のときは `[O]`、IME on のときは `[|]` となりますが、
上記設定をすると、それぞれ `[--]` `[あ]` になります。
また、ミニバッファでの yes/no 入力待ちなどのときには、
自動的に IME off にして、抜けたら IME 状態を復帰するようになります。

なお、tr-ime が使っている
[w32-ime.el](https://github.com/trueroad/w32-ime.el)
の
[MELPA 掲載版](https://melpa.org/#/w32-ime)
では、`wrap-function-to-control-ime` 関数について
[コーディング規約に従うため名前変更](https://github.com/trueroad/w32-ime.el/commit/4265355ad0ac07c8723a0db8276b5c9340c6f2b0)
して、`w32-ime-wrap-function-to-control-ime` 関数として、さらに
[実装方法や引数も変更](https://github.com/trueroad/w32-ime.el/pull/2)
しています。
後方互換性確保のため、従来版の名前・引数でも動作するように配慮しているので、
IME パッチ環境など従来版の w32-ime.el が存在する環境と同じ設定を使うには、
上記のような従来版と同じ使い方で良いと思います。
従来版とは関係ない独立した設定なのであれば、
下記のように新しい名前・引数で設定してもよいと思います。

```el
;; IME 制御（yes/no などの入力の時に IME を off にする）MELPA 掲載版用
(w32-ime-wrap-function-to-control-ime 'universal-argument)
(w32-ime-wrap-function-to-control-ime 'read-string)
(w32-ime-wrap-function-to-control-ime 'read-char)
(w32-ime-wrap-function-to-control-ime 'read-from-minibuffer)
(w32-ime-wrap-function-to-control-ime 'y-or-n-p)
(w32-ime-wrap-function-to-control-ime 'yes-or-no-p)
(w32-ime-wrap-function-to-control-ime 'map-y-or-n-p)
(w32-ime-wrap-function-to-control-ime 'register-read-with-preview)
```

### isearch-mode 設定

C-s (isearch-forward) などの IME パッチ向けの設定についてです。
よくある設定の、

```el
(defun w32-isearch-update ()
  (interactive)
  (isearch-update))
(define-key isearch-mode-map [compend] 'w32-isearch-update)
(define-key isearch-mode-map [kanji] 'isearch-toggle-input-method)

(add-hook 'isearch-mode-hook
          (lambda () (setq w32-ime-composition-window (minibuffer-window))))
(add-hook 'isearch-mode-end-hook
          (lambda () (setq w32-ime-composition-window nil)))
```

ですが、
モジュール環境では `[compend]` も `[kanji]` も送られてきませんし、
`w32-ime-composition-window` も使いませんので、
あまり意味がありません。
ほとんど害も無いので IME パッチ環境と共用の設定ファイルならば、
書いておいても特に問題ないと思います。

### フォント設定（advanced のみ）

IME の未確定文字列のフォント設定は、
IME パッチと同様にフレームパラメータ `ime-font` を設定しておけば、
フォーカス切り替え時（デフォルト設定の場合）に反映されます
（IME パッチの場合はフレームパラメータに設定すると、
即座に反映されます）。
generic ファミリ（`serif`, `sans-serif`, `monospace` など）
は指定できません。

IME パッチ向け設定例でよくある `default-frame-alist`
へ設定しても構いませんが。
これだとこれから新しく開くフレームにしか効果がありません
（IME パッチでも同じです）。
既存の全フレームに同じ設定をして、
さらに新しいフレームでも同じ設定にするには、
以下のようにするとよいでしょう（IME パッチでも効きます）。

```el
(modify-all-frames-parameters '((ime-font . "MS Gothic-12")))
```

なお、フレームパラメータの `font` （フレームのフォント設定）から
`ime-font` へ設定をコピーするのは、あまりお勧めしません。
MinGW の場合は大丈夫かもしれませんが、
Cygwin の場合はフレームパラメータの `font` が文字化けしていることがあり、
文字化けした内容をそのまま `ime-font` にコピーしても設定できません。
双方に同じ設定をしたければ、
以下のように同じ文字列をそれぞれ設定することをお勧めします。

```el
(set-frame-font "MS Gothic-12" nil t)
(modify-all-frames-parameters '((ime-font . "MS Gothic-12")))
```

蛇足ですが、Cygwin でフレームパラメータの `font` が文字化けを起こすのは、
変数 `local-coding-system` が `cp932` になっていないからのようです。
MinGW ではデフォルトで `cp932` なので化けないようです。
Cygwin でもこれを `cp932` に設定してからフォント設定すれば
MinGW と同様に化けなくなるようですが、そうすると Cygwin 由来の文字列は UTF-8
のハズなので、こんどはそっちが化けてしまうのではないかと思っています。

### 全バッファ IME 状態同期

デフォルトでは、バッファ毎に IME 状態の on/off を保持していて、
バッファを切り替えると、それに応じて IME 状態も切り替わります。

これを、全バッファで一つの IME 状態としたい場合には、
以下のようにすればできます。

```el
(setq w32-ime-buffer-switch-p nil)
```

ただし、本設定はあまりメンテされてこなかったようなので、
おかしなことになるかもしれません。

### 環境判定

モジュール環境か IME パッチ環境かで設定を分けたいなら、
`(tr-ime-standard-install)` または `(tr-ime-advanced-install)`
の後に以下のようにしてください。

```el
(cond ((and (boundp 'tr-ime-enabled-features)
        (eq tr-ime-enabled-features 'standard))
       ;; standard 環境用
       (message "tr-ime standard"))
      ((and (boundp 'tr-ime-enabled-features)
        (eq tr-ime-enabled-features 'advanced))
       ;; advanced 環境用
       (message "tr-ime advanced"))
      ((subrp (symbol-function 'ime-get-mode))
       ;; IME パッチ環境用
       (message "IME patched"))
      (t
       ;; いずれでもない環境用
       (message "others")))
```

モジュール環境と IME パッチ環境のみで w32-ime.el の設定をしたいなら、
`(tr-ime-standard-install)` または `(tr-ime-advanced-install)`
の後に以下のようにしてください。

```el
(when (featurep 'w32-ime)
  ;; w32-ime.el の設定
  (message "w32-ime settings"))
```

`(tr-ime-standard-install)` や`(tr-ime-advanced-install)`
自身はウィンドウシステムが w32 であることや、
IME パッチが存在しないことを確認してから動作しますので、
他の環境と共通の設定に書いていただいても大丈夫です。

## autorebase 設定（Cygwin のみ）

tr-ime はモジュール DLL を使うため、
Cygwin の場合は rebase が必要となります。
MELPA でインストールしても、バイナリをインストールしても、
自動ダウンロードでも、自分でビルドしても、
rebase が必要です。
手動で rebase した場合は autorebase の対象にならないため、
Cygwin インストーラの autorebase が走ると衝突してしまう可能性があります。
そこで、手動で rebase するのではなく autorebase の設定をします。
（Cygwin パッケージとしてインストールした場合は、
いちいち設定しなくても autorebase の対象になりますが、
残念ながら Cygwin パッケージにはなっていません。
Cygwin 公式パッケージにするには、
[
パッケージメンテナ 5 人の賛成が必要
](https://cygwin.com/packaging-contributors-guide.html)
です。確保できそうならぜひ教えてください。）

### MELPA でインストールして自動ダウンロードした場合

MELPA で tr-ime をインストールして、
モジュール DLL を自動ダウンロードした場合、
モジュール DLL のファイルは `~/.emacs.d/elpa/tr-ime-YYYYMMDD.XXXX`
というような名前のディレクトリに格納されます。
このディレクトリは、tr-ime をアップデートすると変わってしまい、
そのたびに autorebase の設定をやり直す必要がでてきてしまいます。
そこで、モジュール DLL のファイル（拡張子が `.dll` のファイル）を、
固定の名前で load-path にも含まれているディレクトリへ
移動させることをお勧めします。
移動させたら、
その移動先のモジュール DLL ファイルに対して autorebase 設定をしてください。

### モジュール DLL がホームディレクトリ以外にある場合

モジュール DLL を置いてあるディレクトリを
`/usr/share/emacs/site-lisp/tr-ime`
として説明します。適宜お使いの環境に読み替えてください。

`/var/lib/rebase/dynpath.d` に適当な名前のファイルを作って、
モジュール DLL を置いてある **ディレクトリ**
をフルパスで書いた 1 行を追加します。

```
$ cd /var/lib/rebase/dynpath.d
$ touch tr-ime
$ echo '/usr/share/emacs/site-lisp/tr-ime' >> tr-ime
```

autorebase を手動で実行します。

```
$ /etc/postinstall/0p_000_autorebase.dash
```

### モジュール DLL がホームディレクトリの下にある場合

以下、Cygwin 64 bit （環境名 `x86_64-pc-cygwin`）で、
`tr-ime-mod` の ABI バージョンが `1`、
`tr-ime-modadv` の ABI バージョンが `1`、
ユーザ名 `foobar` で、インストール先が `/home/foobar/.emacs.d/site-lisp`
として説明します。適宜お使いの環境に読み替えてください。

`/var/lib/rebase/user.d` にユーザ名のファイルを（なければ）作って、
モジュール DLL の **ファイル名** をフルパスで書いた行を追加します。

```
$ cd /var/lib/rebase/user.d
$ touch foobar
$ echo '/home/foobar/.emacs.d/site-lisp/tr-ime-mod-1-x86_64-pc-cygwin.dll' \
    >> foobar
$ echo '/home/foobar/.emacs.d/site-lisp/tr-ime-modadv-1-x86_64-pc-cygwin.dll' \
    >> foobar
```

autorebase を手動で実行します。

```
$ /etc/postinstall/0p_000_autorebase.dash
```

## モジュール DLL について

### ファイル名

モジュール DLL のファイル名は、環境によって名前が異なり、
standard 用が `tr-ime-mod-ABIバージョン-環境名.dll` で、
advanced 用が `tr-ime-modadv-ABIバージョン-環境名.dll` のようになります。
具体的な環境名は以下の通りです。
これにより、例えば Cygwin 64 bit の場合の
モジュール DLL ファイル名は `tr-ime-mod-1-x86_64-pc-cygwin.dll`
および `tr-ime-modadv-1-x86_64-pc-cygwin.dll`
のようになります。

* Cygwin
    * 64 bit: `x86_64-pc-cygwin`
    * 32 bit: `i686-pc-cygwin`
* MinGW
    * 64 bit: `x86_64-w64-mingw32`
    * 32 bit: `i686-w64-mingw32`

なお、Emacs 28 で standard を使う場合、
standard 用モジュール DLL に実装した機能が既に
Emacs 本体に取り込まれているため、モジュール DLL は不要です。
Emacs 28 でも advanced を使う場合はモジュール DLL が必要となります。

### 自動ダウンロード

MELPA にはモジュール DLL のファイルが格納されていないため、
必要な DLL が load-path に見つからない場合、
DLL を自動でダウンロードする機能を用意しました。
これは `(tr-ime-standard-install)` および `(tr-ime-advanced-install)`
で動作します。

必要な DLL が見つからない場合は、
ダウンロードするか否か尋ねます。
そこで y を入力するとファイルをダウンロードして、
`tr-ime-download.el` が配置されているディレクトリに展開、
ハッシュを確認した上でロードします。
ダウンロードサイトにアクセスできることと、
ディレクトリの書き込み権限があることが必要です。

一度ダウンロードしたら、
そのディレクトリには load-path が通っているはずなので、
2 回目以降は DLL が見つかるようになり、
再度自動ダウンロードすることはないはずです。

ユーザに尋ねず、モジュール DLL が無ければ常に自動ダウンロードしたい場合には、
オプション引数に non-nil を指定してください。
`(tr-ime-standard-install 'no-confirm)` または
`(tr-ime-advanced-install 'no-confirm)` のようにすればよいです。

自動ダウンロードが嫌であれば、あらかじめ load-path 上に
必要なモジュール DLL を置いておくことで、
自動ダウンロードは動作しなくなります。
また、モジュール DLL が無くてもユーザに尋ねずにエラーにしたいのであれば、
`(tr-ime-standard-install)` や `(tr-ime-advanced-install)`
の代わりに
`(tr-ime-standard-initialize)` や `(tr-ime-advanced-initialize)`
を使えばよいです。

## tr-ime 詳細設定

モジュール環境には IME パッチの動作をエミュレーションするものがあり、
その動作設定ができます。
基本的にはデフォルトのままで構わないと思います。

### コア機能設定

モジュールを使用する際のコア機能の設定です。
通常は設定変更しないでください。

#### IME パッチ特有のアブノーマルフックをエミュレーションするか

IME パッチは、ウィンドウやバッファの状態が変更になったら、
特有のアブノーマルフックを呼ぶようになっており、
このフックによって w32-ime.el が IME/IM 状態の同期を行っています。
このフックは C 実装に直接手を入れて実現しているようですが、
モジュールでは実現困難です。

そこで Emacs の標準的なフックである
window-selection-change-functions と window-buffer-change-functions
を用いて、ウィンドウやバッファの変更を検出したらフックを呼ぶ、
というフックのエミュレーション機構を用意しました。
（Ver 0.3.0 までは post-command-hook と after-focus-change-function
でエミュレーションしていましたが変更しました。）

以下の設定で無効にできます。デフォルトは有効です。

```el
(custom-set-variables '(tr-ime-hook-p nil))
```

#### IME 状態変更関数使用後の状態確認回数上限（GNU Emacs 28 以降）(standard)

standard を使う場合に有効な設定です。
GNU Emacs 28 では IME パッチやモジュールが無くても、
本体だけで IME 状態の変更や確認ができる関数が用意されました。
ですが、IME 状態変更した直後に IME 状態確認すると、
状態変更前を示す返り値が得られることがあります。

そこで、状態変更後に何回か状態確認関数を呼んで、
変更の完了を確認するようにしており、その回数の上限を
`tr-ime-openstatus-emacs28-open-check-counter`
に設定できます（デフォルト `10`）。

この設定の上限回数が足りない場合は、
C-\\ で IME on/off トグルしたときに、
モードラインの表示が切り替わらない現象が起きたり起きなくなったりします。
そういうときは、もっと大きい数を指定するとよいでしょう。

advanced では IME 状態の変更を監視する必要があって、
自前の状態変更関数を使用するため、本設定は使われません。

#### メッセージフックしてフレームをサブクラス化するか否か (advanced)

advanced のほとんどの機能は、メッセージフックとサブクラス化により
Emacs のメッセージ処理を奪い取ることによって実現しています。
これらが有効でなければ機能しないだけではなく、
設定変更すらできないものが存在します。
特別な目的が無い限りはデフォルトの有効のままにしておいてください。

特別な目的があって無効にしたい場合は以下でできます。

```el
(custom-set-variables '(tr-ime-subclassify-p nil))
```

#### スレッドメッセージをディスパッチするか否か (advanced)

GNU Emacs 27 や 28 の UI スレッドでは、
スレッドメッセージがディスパッチされません。
これによって IME の動作に不具合が発生します
（Windows 10 1909 の MS-IME で
IME on/off してもタスクバーの IME 状態表示アイコンが変わらない等）。
そこで、モジュールのメッセージフックで Emacs の代わりに
スレッドメッセージをディスパッチするようにしています。

ただし、将来の Emacs でスレッドメッセージをディスパッチするようになったら、
本設定でモジュールのディスパッチを停止してください。
そうしないと、一つのスレッドメッセージを二重にディスパッチすることになり、
Emacs の動作がおかしくなると思います。

停止したい場合は以下でできます。

```el
(custom-set-variables '(tr-ime-thread-message-dispatch-p nil))
```

#### UI スレッドからの通知を Lisp で受け取る (advanced)

UI スレッドにきた通知を Lisp 側で受け取る機構です。

本機能の動作の仕組みとしては、まず
UI スレッドに Lisp 側へ通知すべきメッセージがきたら、
UI スレッドから Lisp スレッドへの本モジュール内部専用のキューに、
その旨のメッセージを格納してから WM_INPUTLANGCHANGE を post/send します。
これにより Lisp 側で language-change イベントが発生します。
このイベントを受けて advanced の C++ 実装のモジュールにある
`tr-ime-modadv--language-change-handler` 関数を呼びます。
この関数は内部専用キューからメッセージを取り出し、
その種類に応じてノーマルフックを呼び出すなどの動作を行います。

この中で、language-change イベントの発生を受けて、
`tr-ime-modadv--language-change-handler` 関数を呼ぶところについて、
以下のような設定を行っています。

```el
(define-key special-event-map [language-change]
  (lambda ()
    (interactive)
    (tr-ime-modadv--language-change-handler)))
```

本モジュールとは別の language-change イベントを使うツール類と
共存させたい場合は、上記設定をうまく調整してください。
本モジュールの `tr-ime-modadv--language-change-handler` 関数は、
内部専用キューが空であれば何もしませんので、
イベントが来たらとにかく呼ばれるようになっていればよいです。
他のツール類が発生させた language-change イベントの際に
一緒に呼んでしまって構いません。

本機能では、上記の通り
UI スレッドにきた通知を Lisp 側へ通知する動作をしていますが、
これがかなり困難でした。
IME パッチは C 実装でメッセージ処理を追加して、
kanji キーのイベントという形で通知しているようです。
当初、これと同じような処理にするため、WM_KEYDOWN, WM_KEYUP で VK_KANJI を
PostMessage する方法を思いついたのですが、修飾キーがあるとおかしくなり、
一筋縄ではいきませんでした。
一方、w32-imeadv は別プロセスを経由して通知するという
かなり大がかりで複雑な機構を採用しています。
結局色々調べて、
[
ダイナミックモジュールの情報が集まったページ
](https://github.com/jkitchin/emacs-modules) からリンクが貼られていた[
Asynchronous Requests from Emacs Dynamic Modules
](https://nullprogram.com/blog/2017/02/14/)を参考に、
上記のような WM_INPUTLANGCHANGE による方法を実装しました。

この動作を無効にするには、以下のようにすればできます
（デフォルトは有効）。

```el
(custom-set-variables '(tr-ime-recv-notify-p nil))
```

### 再変換 (RECONVERSION) (advanced)

確定済文字列にカーソルを置いて変換キーを押すと、
カーソルのあった場所の確定済文字列が未確定文字列になって、
再変換できるようになるという機能です。

本機能は、UI スレッドに WM_IME_REQUEST IMR_RECONVERTSTRING
メッセージが来たら、内部専用キューにその旨を格納して Lisp に通知し、
Lisp 側でカーソル周辺の文字列やカーソル位置を収集して UI スレッドに通知し、
再変換の処理を始める、という通知の往復があります。
さらに、再変換処理中に、再変換に合わせてカーソルを移動させたり、
再変換前の確定済文字列を消したりといった動作が必要で、
これらも UI スレッドから Lisp への通知などをする、
かなり複雑な動作になっています。

#### 再変換 (RECONVERSION) 動作を行うか否か (advanced)

この動作を無効にするには、以下のようにすればできます
（デフォルトは有効）。

```el
(custom-set-variables '(tr-ime-reconversion-p nil))
```

### 前後の確定済文字列を参照した変換 (DOCUMENTFEED) (advanced)

確定済文字列のあるところにカーソルを置いて文字を入力・変換すると、
カーソルのあった場所の確定済文字列によって変換候補が変わる機能です。
たとえば、通常は「いっぱつ」を変換すると「一発」が最初の候補に出ても、
「危機」の直後にカーソルがある状態だと「一髪」が最初の候補になる、
というものです。

本機能は、UI スレッドに WM_IME_REQUEST IMR_DOCUMENTFEED
メッセージが来たら、内部専用キューにその旨を格納して Lisp に通知し、
Lisp 側でカーソル周辺の文字列やカーソル位置を収集して UI スレッドに通知し、
という通知の往復が必要で、かなり複雑な動作になっています。

#### 前後の確定済文字列を参照した変換 (DOCUMENTFEED) 動作を行うか否か (advanced)

この動作を無効にするには、以下のようにすればできます
（デフォルトは有効）。

```el
(custom-set-variables '(tr-ime-documentfeed-p nil))
```

### IME フォント (advanced)

IME パッチではフレームパラーメータの `ime-font` 設定を変更すると、
即座に未確定文字列フォントの設定に反映されます。
しかし、モジュールではフレームパラーメータの `ime-font` 設定と、
モジュール内にある低レベルの未確定文字列フォント設定は独立しており、
フレームパラメータを変更しただけでは設定反映できません。
そこで、フォーカス変更時あるいはコマンド実行後に、
フレームパラメータ設定を読み取ってモジュールの設定に反映させる、
ime-font 設定エミュレーションを用意しています。

モジュール内の低レベル設定は、スレッド毎の設定なのですが、
GNU Emacs 27 や 28 では、全フレームが同じ UI スレッドで動作しているので、
低レベル設定を一度すると事実上全フレームで設定したことと同じになります。
それに対してフレームパラメータはフレーム毎の設定なので、
デフォルトではフォーカス変更時に変更先フレームの
フレームパラメータを元に、低レベル設定へ反映するようにしています。
ですが、全フレームで同じ設定になればよいということであれば、
一度設定したら設定エミュレーションを無効にしてもよいと思います。
逆に、フレーム単位よりももっと細かく設定を変えたい、
例えばウィンドウ単位で別々の設定にしたいとか、
フェイス毎に変えたいとかいうことであれば、post-command-hook
を利用してコマンド実行後に反映させる設定も用意しています。

また、これらとは別のタイミングで反映したい場合には、
下記のように関数を呼ぶことで反映させることができます。
これはフォーカス変更後の反映やコマンド実行後の反映が両方とも無効でも、
この関数呼び出しによって即座に反映できます。

```el
(tr-ime-font-reflect-frame-parameter)
```

#### フォーカス変更時に ime-font 設定エミュレーションを呼ぶか否か (advanced)

フォーカス変更時の反映を無効にしたい場合は、以下でできます
（デフォルトは有効）。

```el
(custom-set-variables '(tr-ime-font-focus-p nil))
```

#### コマンド実行後に ime-font 設定エミュレーションを呼ぶか否か (advanced)

post-command-hook によるコマンド実行後の反映を有効にしたい場合は、
以下でできます（デフォルトは無効）。

```el
(custom-set-variables '(tr-ime-font-post-command-p t))
```
### isearch-mode (advanced)

standard だと C-s など isearch-mode の検索中、
未確定文字列はミニバッファの文字入力位置ではなくて、
検索開始前に元々入力していた位置に表示されてしまいます。
isearch-mode 時に選択されているウィンドウはミニバッファではないので、
ある意味では正しい挙動ではありますが、
確定するとミニバッファに表示されるので、
位置が食い違っていて使いにくいです。

advanced では IME パッチと同じように、
こうした未確定文字列をミニバッファの文字入力位置に表示できます。
ただし、実現方法が異なるので、微妙な動作の違いがあるかもしれません。

#### isearch-mode 中の未確定文字列表示位置を文字入力位置にするか否か (advanced)

isearch-mode 中の位置設定を無効にするには、以下のようにすればできます
（デフォルトは有効）。

```el
(custom-set-variables '(tr-ime-isearch-p nil))
```

#### WM_IME_STARTCOMPOSITION で常に DefSubclassProc を呼ぶか否か (advanced)

WM_IME_STARTCOMPOSITION ハンドラにおいて、
isearch-mode 中（未確定文字列ウィンドウの位置設定中）は
DefSubcalssProc を呼ばず Emacs のメッセージ処理をスキップしています。
これは Emacs が未確定文字列ウィンドウの位置を isearch-mode
に入る前の文字入力位置に設定してしまうためです。
しかし、何らかの理由で元の Emacs の処理に戻さなければならない時は、
本設定を non-nil にすることで isearch-mode 中であっても、
DefSubcalssProc により Emacs のメッセージ処理が必ず呼ばれるようになります。
その場合は Emacs の処理後に再度位置設定を行いますが、
未確定文字列ウィンドウがチラついて見えることがあります。
なお、isearch-mode 以外では本設定に関わらず、
常に DefSubcalssProc を呼んで Emacs のメッセージ処理が行われます。

何らかの理由で常に呼ぶようにするには、以下のようにすればできます
（デフォルトは呼ばない）。

```el
(custom-set-variables '(tr-ime-isearch-defsubclassproc-p t))
```

### プレフィックスキー検出 (advanced)

コマンドのキーシーケンスになる最初のキーである
プレフィックスキー（C-x など）を検出すると、
自動的に IME off にして、コマンド終了後に IME 状態を戻します。
これにより IME on のまま C-x 1 のような操作をしたときに、
1 が IME に吸われて未確定文字扱いされないようにしています。

本来はプレフィックスキーが来たら呼ばれるフックなどがあればよいのですが、
残念ながら Emacs には存在しないようなので、
単純に以下で指定したプレフィックスキー検出リストに登録された
キーが押下されたら（WM_KEYDOWN メッセージが来たら）IME off にして、
pre-command-hook で IME 状態を戻す処理をしています。

standard 向けにタイマを使って同様の機能を実現したワークアラウンドがありますが、
advanced の本機能はタイマを使わないためタイミング的にも負荷的にも有利です。
本機能を有効にすると standard のワークアラウンドによる
プレフィックスキー検出は無効になります。

#### プレフィックスキー検出対象リスト (advanced)

検出リストは以下のように
プレフィックスキーとして検出したいコードのリストとして設定できます
（standard 向けワークアラウンドの設定方法とはコード体系が異なります）。

```el
(custom-set-variables '(tr-ime-prefix-key-list
                        '(#x20058 #x20048 #x20043 #x1b)))
```

コードは、上位 16 bit が修飾キー、
下位 16 bit が修飾されるキーのバーチャルキーコードを指定します。
修飾キーは Shift (#x10000), Ctrl (#x20000), Alt (#x40000) の
ビット論理和で指定します。
バーチャルキーコードは Windows のもの（`VK_*` などの値）を指定します。

例えば C-x は Ctrl の修飾キー #x20000 と、
X キーのバーチャルキーコード #x58 のビット論理和なので #x20058 を指定します。
C-M-x であれば、さらに Alt の修飾キーを含めて #x60058 を指定します。
上位の例では、C-x, C-h, C-c, ESC を指定したものとなっています。

#### プレフィックスキーを検出して自動的に IME off するか否か (advanced)

プレフィックスキー検出を無効にするには、以下のようにすればできます
（デフォルトは有効）。

```el
(custom-set-variables '(tr-ime-prefix-key-p nil))
```

### IME 状態変更通知による IME/IM 状態同期 (advanced)

Emacs 側トリガ（C-\\ やウィンドウ・バッファの切り替えなど）だけでなく、
IME 側トリガ（半角/全角キーやマウスでの切り替えなど）も含め、
IME 状態が変更されたら WM_IME_NOTIFY IMN_SETOPENSTATUS
が送られてきます。この通知を受けて IM 状態を同期させます。

standard 向けにタイマを使って
IME 状態の食い違いを検出して修正するワークアラウンドがありますが、
advanced の本機能はタイマを使わないためタイミング的にも負荷的にも有利です。
本機能を有効にすると standard のワークアラウンドによる
食い違い検出は無効になります。

本機能は、
UI スレッドに WM_IME_NOTIFY IMN_SETOPENSTATUS がきたら、
UI スレッドからの通知を Lisp で受け取る機構を利用して
setopenstatus を内部専用キューに格納して通知し、
`tr-ime-modadv--language-change-handler` 関数が、
内部専用キューから setopenstatus を受け取ると、
ノーマルフック `tr-ime-sync--setopenstatus` を呼び出し、
そこで一連の IME/IM 同期の動作が行われるようになっています。

#### IME 状態変更通知による IM 状態同期をするか否か (advanced)

IME 状態変更通知による IM 状態同期を無効にするには、
以下のようにすればできます（デフォルトは有効）。

```el
(custom-set-variables '(tr-ime-sync-p nil))
```

### ワークアラウンド設定

モジュールの構成上どうにもならない機能を、
Lisp でタイマを使ってなんとかしているものです。
モジュールの高機能化によって不要になるものもあります。

#### isearch-mode 時の Alt + 半角/全角ワークアラウンド (advanced)

advanced で isearch-mode 時に Alt + 半角/全角キー操作をすると、
なぜかエコーエリアが消えてしまいます。
キー操作時に再表示させるようにしても効果が無い
（恐らくキー操作後にくるイベントか何かで消されている）ので、
Emacs がアイドル状態になったら動作するタイマで再表示させる
ワークアラウンドを用意しました。
このタイマは Alt + 半角/全角キー操作 1 回につき 1 回だけ動作し、
繰り返し動作はしません。

standard を使う場合は問題ありません。
advanced でも
C-\\ や半角/全角単独など、他の方法で IME on/off する場合は問題ありません。
Alt + 半角/全角キー操作はしないとか、
エコーエリアが消えても問題ないという場合は
以下の設定で無効にできます。

```el
(custom-set-variables
 '(tr-ime-workaround-isearch-delayed-update-p nil))
```

Alt + 半角/全角キー操作後に、
アイドル状態になってから再表示するまでの待ち時間（秒）が
`tr-ime-workaround-isearch-delayed-update-time`
に設定できます（デフォルト 0.0001）。

#### IME 状態の食い違いを検出して修正するワークアラウンド (standard)

IME 側トリガの状態変更（半角/全角キーやマウスでの切り替え）
を検出して IM 側を同期させるための機構です。

advanced ではもっと筋が良い対応ができるため、そちらを使えば不要ですが、
standard を使いたい場合に
ワークアラウンドとして、
定期的に動くタイマでポーリングし、
IME と IM の状態が食い違ったら IM 状態を反転して一致させる、
という機構を用意しました。

定期的なタイマで動作するため、
負荷が気になるようでしたら以下の設定で無効にできます。
デフォルトは standard の時のみ有効です。

```el
(custom-set-variables '(tr-ime-workaround-inconsistent-p nil))
```

IME 状態食い違い検出修正用ポーリング時間（秒）が
`tr-ime-workaround-inconsistentent-polling-time`
に設定できます（デフォルト `1.0`）。

#### プレフィックスキー（C-x など）を検出して IME off にするワークアラウンド (standard)

コマンドのキーシーケンスになる最初のキーである
プレフィックスキー（C-x など）を検出すると、
自動的に IME off にする機能です。
これにより IME on のまま C-x 1 のような操作をしたときに、
1 が IME に吸われて未確定文字扱いされなくなります。

advanced ではもっと筋が良い対応ができているため不要ですが、
standard を使いたい場合に
ワークアラウンドとして、
Emacs がアイドル状態になったら動くタイマでポーリングし、
プレフィックスキーが押されていたら IME off にし、
pre-command-hook で IME を復帰させる、という機構を残してあります。

以下の設定で無効にできます。デフォルトは standard の時のみ有効です。

```el
(custom-set-variables '(tr-ime-workaround-prefix-key-p nil))
```

プレフィックスキー検出用ポーリング時間（秒）が
`tr-ime-workaround-prefix-key-polling-time`
に設定できます（デフォルト `0.1`）。
プレフィックスキー検出検出対象リストが
`tr-ime-workaround-prefix-key-list`
に設定できます（デフォルト `'(?\C-x ?\C-h ?\C-c ?\e)`、
C-x, C-h, C-c と ESC です）。

### デバッグ出力 (advanced)

standard, advanced ともに Win32 API の OutputDebugString を使って、
デバッグメッセージの出力をしています。
advanced では出力するレベルを変更することができます。

ほぼ何も出力しないようにしたいなら以下を実行してください。

```el
(custom-set-variables '(tr-ime-debug-verbose-level 0))
```

API の失敗などを出力するなら以下を実行してください。

```el
(custom-set-variables '(tr-ime-debug-verbose-level 3))
```

さらに詳細なデバッグ情報も出力するなら以下を実行してください。

```el
(custom-set-variables '(tr-ime-debug-verbose-level 5))
```

以上の設定では編集中や入力中の文字列はデバッグ出力に含まれません。
再変換や確定済文字列を利用した変換などで、
編集中や入力中の文字列も含んだデバッグ情報も出力したいなら、
以下を実行してください。

```el
(custom-set-variables '(tr-ime-debug-verbose-level 6))
```

バイナリリリースのデフォルトは 5 にしてあります。

## MELPA を使わない

MELPA を使わずにバイナリリリースを使う場合は、
[リリース](https://github.com/trueroad/tr-emacs-ime-module/releases)
にあるバイナリ配布ファイル
（ファイル名が `tr-ime-VERSION-binary.zip` になっているファイル）
をダウンロードして、中に入っているモジュール DLL ファイルと、
モジュールのヘルパとなる Lisp 実装である拡張子 `.el`
のファイルを Emacs の load-path が通っているディレクトリに置いてください。
なお、バージョンによってファイル名や設定方法などが異なるので、
バイナリリリースを使う場合には、
zip に同梱されているドキュメントの方をご覧ください。

ご自分でビルドしてみたい場合は、後述の「ビルド」をご覧ください。

また、設定は MELPA を使った場合とほぼ同じですが、
MELPA で自動的に設定される autoload がないので、
`(tr-ime-standard-install)` や `(tr-ime-advanced-install)`
などの前に `(require 'tr-ime)` をしてください。

## ビルド

ビルドするには動作環境に加えて以下が必要になります。
Cygwin の Emacs で使うならば Cygwin 環境で、
GNU 公式バイナリなどの MinGW の Emacs で使うなら MinGW 環境で、
それぞれ揃えてください。

* C++14 対応コンパイラ、C99 対応コンパイラ
    * 最近の GCC など
* emacs-module.h
    * Emacs についているはずです
        * Cygwin は emacs-w32 パッケージをインストールすると一緒に入ります
        * MinGW は GNU 公式バイナリに入ってないようなので、
          Emacs のソースから持ってきてください
    * ビルド時に使った emacs-module.h の Emacs バージョンが新しくて、
      動作環境の Emacs バージョンが古い場合は、動作しません
* Autotools (autoconf, automake, libtool)
    * [本リポジトリ](https://github.com/trueroad/tr-emacs-ime-module)
      のソースを使ってビルドする場合に必要です
    * [リリース](https://github.com/trueroad/tr-emacs-ime-module/releases)
      にあるソース配布ファイル
      （ファイル名が `tr-ime-VERSION.tar.gz` のファイル）
      を使うのであれば環境に Autotools がインストールされていなくても
      Autotools を使ったビルドができます

### ビルド方法

以下のようにすればビルドできます。
インストール先に応じて `--prefix` オプションの値を変えてください。
[リリース](https://github.com/trueroad/tr-emacs-ime-module/releases)
にあるソース配布ファイル
（ファイル名が `tr-ime-VERSION.tar.gz` のファイル）
を使うのであれば `./autogen.sh` の実行は不要です。

```
$ ./autogen.sh
$ mkdir build
$ cd build
$ ../configure --prefix=/usr
$ make
```

Cygwin 環境で MinGW 用バイナリをクロスコンパイルしたいような場合には、

```
$ ./autogen.sh
$ mkdir build
$ cd build
$ cp /usr/include/emacs-module.h .
$ ../configure --host=x86_64-w64-mingw32 --with-emacs-module-hdir=`pwd`
# make
```

のような感じでやればできます。オプションは適宜変更してください。

その後は以下のようにしてインストールできます。

```
$ make install
```

自分でビルドした場合には w32-ime.el がインストールされませんので、
[MELPA の w32-ime パッケージ](https://melpa.org/#/w32-ime)
をインストールするか、w32-ime.el を load-path 上に置いてください。

### バイナリリリースのビルド方法

バイナリリリースは以下の方法でビルドしたものです。

#### Cygwin 64 bit / 32 bit

いずれも、Cygwin 64 bit / 32 bit の環境で、ソース配布ファイル
tr-ime-VERSION.tar.gz
を使って以下のようにしてビルドしました。

```
$ tar xfvz tr-ime-VERSION.tar.gz
$ cd tr-ime-VERSION
$ mkdir build
$ cd build
$ ../configure --prefix=/usr
$ make
$ make install DESTDIR=`pwd`/tmp
```

#### MinGW 64 bit / 32 bit

Cygwin 64 bit 環境でクロスコンパイルしました。
また、libstdc++ などを静的リンクするために小細工をしています。
（本当は GNU 公式バイナリの libstdc++-6.dll
をそのまま使えるようにしたかったのですが、
GCC のバージョンを下げてみるなどしてもうまくいかず…。
Emacs の bin フォルダにある libstdc++-6.dll を最新のものに置き換えれば
小細工しなくても動くのですが、それはそれで面倒だろうなと思いまして。
また、小細工ではなく、正攻法で静的リンクしたかったのですが、
そもそも DLL を作る際に、
使用するライブラリを静的リンクすることは考慮されていないみたいで、
どうにもなりませんでした。）

```
$ tar xfvz tr-ime-VERSION.tar.gz
$ cd tr-ime-VERSION
$ mkdir build
$ cd build
$ cp /usr/include/emacs-module.h .
$ ../configure --host=x86_64-w64-mingw32 \
    --with-emacs-module-hdir=`pwd`
$ sed -i -e '/^archive_cmds=/s/\\$deplibs/-Wl,-Bstatic,-lstdc++,-lgcc,-lgcc_eh,-Bdynamic \\$deplibs/' \
         -e '/^postdeps=/s/-lstdc++ //' \
         -e '/^postdeps=/s/-lgcc //g' \
         -e '/^postdeps=/s/-lgcc_s //g' \
         libtool
$ make
$ make install DESTDIR=`pwd`/tmp
```

なお、libwinpthread-1.dll は必要になっていますが、
GNU 公式バイナリの依存関係の中に入っているものが使えるので、
そのままで大丈夫のハズです。
（いっそ libwinpthread-1.dll も静的リンクしようかと思ったのですが、
残念ながらどうやってもうまくいかず…。
これも「DLL を作る際に、
使用するライブラリを静的リンクすることは考慮されていない」感じですし、
もしかしたらスレッドローカルストレージ (TLS)
関連の処理のために DLL でなければならないとかの理由もあるのかもしれません。）

MinGW 32 bit の場合は `--host=x86_64-w64-mingw32`
のところを `--host=i686-w64-mingw32` に変えただけです。

## 制約

わかっているだけで以下のような制約があります。

* advanced で isearch-mode 時に Alt + 半角/全角で IME on/off すると
  エコーエリアの表示が消えてしまう
    * ワークアラウンドでなんとかしています
* 未確定文字列フォントの設定 (advanced) で generic ファミリは使用不可
    * 無いと困るという方はいらっしゃいますか？どのような使い方でしょうか？
* 単語登録の機能が使えません
    * IME パッチの C 実装関数 `w32-ime-register-word-dialog`
      を実装していないため、w32-ime.el の `w32-ime-toroku-region`
      が使えません
* 変換モード設定関数が使えません
    * IME パッチの C 実装関数 `w32-set-ime-mode` を実装していません

以下は、IME パッチでも発生する事象で、
これまでに気が付いたものです。

* 「IME 入力モード切替の通知（画面中央に大きく「あ」とか「A」とか出るもの）」
  が C-\\ で切り替えると出ない
    * Windows 10 1909 で確認
        * Windows 10 2004 や 20H2 だと、
          そもそもこのような通知が出なくなったので、関係ありません
    * Alt + 半角/全角や半角/全角だと出ます
    * マウス操作などでウィンドウ（Emacs でいうフレーム）を切り替えたとき、
      最後に「IME 入力モード切替の通知」された状態と、
      （切り替え先ウィンドウの） IME 状態が異なったら、その時には出ます
        * 溜まっていたのがウィンドウ切り替えをトリガに出てくるイメージ？
        * 最後に「IME 入力モード切替の通知」された状態と同じだったら出ません
    * ユーザ操作起因の切り替えの場合は出すが、
      アプリ内部で自動的に切り替えた場合には出さない、
      というロジックになっているのかもしれません
        * C-\\ も Emacs のユーザ操作ではありますが、
          Windows 的にはアプリ内部の切り替えと区別が付かないと思います
* バッファごとに IME/IM 状態を切り替える設定（w32-ime-buffer-switch-p が t）
  で使っている時に、IME of のバッファがあるフレームと
  IME off のバッファがあるフレームを交互にクリックすると、
  フレームが切り替わった時に出る「IME 入力モード切替の通知」が
  実際の IME 状態とは逆になる
    * Windows 10 1909 で確認
        * Windows 10 2004 や 20H2 だと、
          そもそもこのような通知が出なくなったので、関係ありません
    * 「IME 入力モード切替の通知」がおかしいだけで、
      モードラインには正しく出ます
    * 恐らく、フレームを切り替えた瞬間、同じプロセスなので切り替え前の
      IME 状態が引き継がれて「IME 入力モード切替の通知」に表示され、
      その直後、バッファに紐づいた IM 状態が IME 状態と異なるため
      IME 状態が変更されるが、これがアプリ内部処理とみなされて
      「IME 入力モード切替の通知」には表示されず溜まってしまい、
      次にフレームを切り替えた瞬間…、
      というのが交互に発生しているものと思います

## News

[NEWS.md](./NEWS.md)

## ライセンス

Copyright (C) 2020 Masamichi Hosoda

Emulator of GNU Emacs IME patch for Windows (tr-ime)
is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Emulator of GNU Emacs IME patch for Windows (tr-ime)
is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with tr-ime.
If not, see <https://www.gnu.org/licenses/>.

[COPYING](./COPYING)
