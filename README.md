# Simple IME module for GNU Emacs (tr-emacs-ime-module)

Windows 用 (MinGW/Cygwin) GNU Emacs でダイナミックモジュールの機構を利用し、
IME パッチ無しの公式バイナリなどでも、
IME による日本語入力を使いやすくする試みです。

## はじめに

GNU Emacs 26.2 から、
IME パッチがなくても MS-IME などの IME (input method editor)
による日本語入力が「とりあえず」できるようになりました。
しかし IME パッチ無しだと、Emacs 自前の
かな漢字変換 (IM: input method) と IME が連動しないため、

* IME を ON/OFF してもモードラインのかな漢字変換状態表示が変わらない
* IME ON で使っているとき、
  状況に応じて自動的に IME OFF してくれる機能が無く、
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
モジュールの実装は必要最小限として複雑な実装をしません。
そのため、IME パッチの全機能を再現することはできません
（それだとやっぱり使いにくいから実装を増やしていく、
なんてことになってきていますが）。
逆に UI や設定など従来の IME パッチと同じようにしたいため、
[w32-ime.el](https://github.com/trueroad/w32-ime.el)
を、ほとんどそのまま使えるようにします。

## 動作環境

* Windows 用 (MinGW/Cygwin) GNU Emacs
    * IME パッチは不要です
    * GNU Emacs 27.1 以降が必要です
    * ダイナミックモジュールが有効になっている必要があります
        * GNU Emacs 27.1 からデフォルトで有効です
    * Cygwin 64 bit
      [emacs-w32](https://cygwin.com/packages/summary/emacs-w32.html)
      27.1-1 で動作確認しています
    * MinGW では
      GNU 公式バイナリの 27.1 (64 bit) や、GNU が配布する pretest の
      28.0.50-snapshot-2020-07-05-x86_64 バイナリで動作しました
        * MinGW は常用していないので
          気が付いていない不具合があるかもしれません

## インストール

モジュール DLL のファイル名は、
環境によって名前が異なって `tr-ime-module-環境名.dll`
および `tr-ime-module2-環境名.dll` のようになります。
具体的な環境名は以下の通りです。
これにより、例えば Cygwin 64 bit の場合の
モジュール DLL ファイル名は `tr-ime-module-x86_64-pc-cygwin.dll`
および `tr-ime-module2-x86_64-pc-cygwin.dll`
となります。

* Cygwin
    * 64 bit: `x86_64-pc-cygwin`
    * 32 bit: `i686-pc-cygwin`
* MinGW
    * 64 bit: `x86_64-w64-mingw32`
    * 32 bit: `i686-w64-mingw32`

バイナリリリースを使う場合は、
[リリース](https://github.com/trueroad/tr-emacs-ime-module/releases)
にあるバイナリ配布ファイル
tr-emacs-ime-module-VERSION-binary.zip
をダウンロードして、中に入っているモジュール DLL ファイルと、
モジュールのヘルパとなる Lisp 実装である拡張子 `.el`
のファイルを Emacs の load-path が通っているディレクトリに置いてください。

ご自分でビルドしてみたい場合は、下の方の「ビルド」をご覧ください。

### autorebase （Cygwin のみ）

Cygwin の場合はバイナリをインストールしても自分でビルドしても
rebase が必要です。
手動で rebase した場合は autorebase の対象にならないため、
Cygwin インストーラの autorebase が走ると衝突してしまう可能性があります。
そこで、手動で rebase するのではなく autorebase の設定をします。
（Cygwin パッケージとしてインストールした場合は、
いちいち設定しなくても autorebase の対象になりますが、
残念ながらパッケージになっていません。
Cygwin 公式パッケージにするには、
[
パッケージメンテナ 5 人の賛成が必要
](https://cygwin.com/packaging-contributors-guide.html)
です。確保できそうならぜひ教えてください。）

#### ホームディレクトリ以外にインストールした場合

インストール先が `/usr/share/emacs/site-lisp/tr-emacs-ime-module`
として説明します。適宜お使いの環境に読み替えてください。

`/var/lib/rebase/dynpath.d` に適当な名前のファイルを作って、
モジュール DLL を置いてある **ディレクトリ**
をフルパスを書いた 1 行を追加します。

```
$ cd /var/lib/rebase/dynpath.d
$ touch tr-emacs-ime-module
$ echo '/usr/share/emacs/site-lisp/tr-emacs-ime-module' >> tr-emacs-ime-module
```

autorebase を手動で実行します。

```
$ /etc/postinstall/0p_000_autorebase.dash
```

#### ホームディレクトリの下にインストールした場合

以下、Cygwin 64 bit （環境名 `x86_64-pc-cygwin`）で、
ユーザ名 `foobar` で、インストール先が `/home/foobar/.emacs.d/site-lisp`
として説明します。適宜お使いの環境に読み替えてください。

`/var/lib/rebase/user.d` にユーザ名のファイルを（なければ）作って、
モジュール DLL の **ファイル名** をフルパスを書いた行を追加します。

```
$ cd /var/lib/rebase/user.d
$ touch foobar
$ echo '/home/foobar/.emacs.d/site-lisp/tr-ime-module-x86_64-pc-cygwin.dll' \
    >> foobar
$ echo '/home/foobar/.emacs.d/site-lisp/tr-ime-module2-x86_64-pc-cygwin.dll' \
    >> foobar
```

autorebase を手動で実行します。

```
$ /etc/postinstall/0p_000_autorebase.dash
```

## 設定

モジュールは、最低限の単純で基本的な機能のみを実装した Module1 と、
メッセージフックやサブクラス化といった多少複雑な機構で、
より高度な機能を実装した実験的な Module2 の 2 つに分かれています。
安定性を取りたい場合は Module1 を、
実験的でもいいから高度な機能が欲しい場合は Module2 を選んでください。

Module2 の機能は以下の通りです。

* すべての IME ON/OFF 方法に対応（IME 状態変更通知による IME/IM 状態同期）
    * Module1 だけでは Alt + 半角/全角キー（もしくは C-\\）による
      IME ON/OFF のみ対応しており、
      半角/全角キー単独やマウスで切り替えた場合には IM
      状態との食い違いが発生しました
    * Module1 でも、食い違いを何とかするワークアラウンドはありますが、
      タイマ動作なのでタイミングや負荷が問題になる可能性があります
* C-s など isearch-mode の検索中に未確定文字列をミニバッファの
  文字入力位置に表示できる
    * Module1 だけでは、
      isearch-mode へ入る前に元々入力していた位置へ表示されます
* 未確定文字列のフォントが設定できる
    * Module1 だけでは設定できないため、
      変換中は☃や🍣のような文字がおかしな表示（いわゆるトーフ）になります
* IME ON/OFF に連動してタスクバーの IME 状態表示アイコンが切り替わる
    * Module1 ではもちろん IME パッチでもアイコン表示が変わりません
* IME ON の状態で C-x, C-c, C-h など、
  コマンドのキーシーケンスになる最初の文字（以下、プレフィックスキー）
  を押すと自動的に IME OFF になる
    * Module1 はタイマ動作によるワークアラウンドで
      なんとか同じような動作を実現していますが、
      タイマ動作なのでタイミングによっては自動 OFF にならない、
      負荷が問題となる、などの可能性があります
* IME ON/OFF 制御や状態取得に Microsoft
  の公式なドキュメントに記載されている方法を使用
    * Module1 は（Emacs 28 以前の場合）
      公式ドキュメントに記載されていない方法を使用しているので、
      最悪の場合は Windows のアップデートなどにより、
      いきなり動かなくなる可能性もあります

なお、Emacs 28 では Module1 に実装した機能だけであれば
既に本体に取り込まれているため、
Lisp だけ入れれば一応動くようにしてあります。
Module2 の機能が必要であれば DLL も入れてください。

### ロード

以下のようにするとロードできます。
~/.emacs.d/init.el などに追加しておくとよいと思います。

#### Module1

```el
;; IME パッチ無しモジュール有りならば Module1 をロードする
(when (and (eq window-system 'w32)
           (not (fboundp 'ime-get-mode))
           (string= module-file-suffix ".dll")
           (locate-library "tr-ime-module-helper"))
  (require 'tr-ime-module-helper)
  (require 'w32-ime "w32-ime-for-tr-ime-module"))
```

Emacs 28 で上記のようにModule1 をロードした場合、
Module1 の DLL に実装した機能は Emacs 本体が持っているため、
DLL はロードされません。

#### Module2 （実験的）

```el
;; IME パッチ無しモジュール有りならば Module2 をロードする
(when (and (eq window-system 'w32)
           (not (fboundp 'ime-get-mode))
           (string= module-file-suffix ".dll")
           (locate-library "tr-ime-module2-helper"))
  (require 'tr-ime-module2-helper)
  (require 'w32-ime "w32-ime-for-tr-ime-module"))
```

上記で Module2 をロードした場合は自動的に Module1 もロードされます。

### IME パッチ設定

#### 基本設定

一部を除き IME パッチと同じような設定ができます。
ただし Module1 のみ使用する場合は
`(global-set-key [M-kanji] 'ignore)` をしないでください
（Module2 なら問題ありません）。
モジュール環境か IME パッチ環境かで設定を分けるならば、
以下のようにしてください。（分ける必要が無ければ不要です。）

```el
(if (featurep 'tr-ime-module-helper)
    (if (featurep 'tr-ime-module-helper2)
        (progn
          ;; Module2 環境用
          (global-set-key [M-kanji] 'ignore))
      ;; Module1 環境用
      (global-set-key [M-kanji] 'toggle-input-method))
  ;; IME パッチ環境用
  (global-set-key [M-kanji] 'ignore))
```

あとはお好みで以下のような設定をします。

```el
;; IM のデフォルトを IME に設定
(setq default-input-method "W32-IME")
;; IME のモードライン表示設定
(setq-default w32-ime-mode-line-state-indicator "[--]")
(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
;; IME 初期化
(w32-ime-initialize)
;; IME 制御（yes/no などの入力の時に IME を OFF にする）
(wrap-function-to-control-ime 'universal-argument t nil)
(wrap-function-to-control-ime 'read-string nil nil)
(wrap-function-to-control-ime 'read-char nil nil)
(wrap-function-to-control-ime 'read-from-minibuffer nil nil)
(wrap-function-to-control-ime 'y-or-n-p nil nil)
(wrap-function-to-control-ime 'yes-or-no-p nil nil)
(wrap-function-to-control-ime 'map-y-or-n-p nil nil)
```

#### isearch-mode 設定

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

なお、Module1 で isearch-mode 中に Alt + 半角/全角で IME ON/OFF するには、

```
(define-key isearch-mode-map [M-kanji] 'isearch-toggle-input-method)
```

でできます。
これはすでにモジュールのヘルパに書いてありますので、
追加する必要はありません。

#### フォント設定（Module2 のみ）

IME の未確定文字列のフォント設定は、
IME パッチと同様にフレームパラメータ `ime-font` を設定しておけば、
フォーカス切り替え時（デフォルト設定の場合）に反映されます
（IME パッチの場合はフレームパラメータに設定すると、
即座に反映されます）。
generic ファミリ（`serif`, `sans-serif`, `monospace` など）は指定できません。

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

## 詳細設定

モジュール環境には IME パッチの動作をエミュレーションするものがあり、
その動作設定ができます。
基本的にはデフォルトのままで構わないと思います。

### コア機能設定

モジュールを使用する際のコア機能の設定です。
通常は設定変更しないでください。

#### IME パッチ特有のアブノーマルフックをエミュレーションするか否か

IME パッチは、ウィンドウやバッファの状態が変更になったら、
特有のアブノーマルフックを呼ぶようになっており、
このフックによって w32-ime.el が IME/IM 状態の同期を行っています。
このフックは C 実装に直接手を入れて実現しているようですが、
モジュールでは実現困難です。

そこで Emacs の標準的なフックの一つ post-command-hook を用いて、
ウィンドウやバッファの変更を検出したらフックを呼ぶ、
というフックのエミュレーション機構を用意しました。

以下の設定で無効にできます。デフォルトは有効です。

```el
(custom-set-variables '(w32-tr-ime-module-hook-emulator-p nil))
```

#### IME 状態変更関数使用後の状態確認回数上限（GNU Emacs 28 以降のみ）

GNU Emacs 28 では IME パッチやモジュールが無くても、
本体だけで IME 状態の変更や確認ができる関数が用意されました。
ですが、IME 状態変更した直後に IME 状態確認すると、
状態変更前を示す返り値が得られることがあります。

そこで、状態変更後に何回か状態確認関数を呼んで、
変更の完了を確認するようにしており、その回数の上限を
`w32-tr-ime-module-set-ime-open-check-counter`
に設定できます（デフォルト `3`）。

環境によるかもしれませんが、
私の環境では 2 回目で変更完了した値が得られるようでしたので、
安全を見て 1 回増やして 3 回をデフォルトににしています。

#### メッセージフックしてフレームをサブクラス化するか否か (Module2)

Module2 のほとんどの機能は、メッセージフックとサブクラス化により
Emacs のメッセージ処理を奪い取ることによって実現しています。
これらが有効でなければ機能しないだけではなく、
設定変更すらできないものが存在します。
特別な目的が無い限りはデフォルトの有効のままにしておいてください。

特別な目的があって無効にしたい場合は以下でできます。

```el
(custom-set-variables '(w32-tr-ime-module-message-hook-and-subclassify-p nil))
```

#### スレッドメッセージをディスパッチするか否か (Module2)

GNU Emacs 27 や 28 の UI スレッドでは、
スレッドメッセージがディスパッチされません。
これによって IME の動作に不具合が発生します
（IME ON/OFF してもタスクバーの IME 状態表示アイコンが変わらない等）。
そこで、モジュールのメッセージフックで Emacs の代わりに
スレッドメッセージをディスパッチするようにしています。

ただし、将来の Emacs でスレッドメッセージをディスパッチするようになったら、
本設定でモジュールのディスパッチを停止してください。
そうしないと、一つのスレッドメッセージを二重にディスパッチすることになり、
Emacs の動作がおかしくなると思います。

停止したい場合は以下でできます。

```el
(custom-set-variables '(w32-tr-ime-module-dispatch-thread-message-p nil))
```

#### UI スレッドからの通知を Lisp で受け取る

UI スレッドにきた通知を Lisp 側で受け取る機構です。

本機能の動作の仕組みとしては、まず
UI スレッドに通知すべきメッセージがきたら、
UI スレッドから Lisp スレッドへの、本モジュール内部専用のキューに、
その旨のメッセージを格納してから WM_INPUTLANGCHANGE を post/send します。
これにより Lisp 側で language-change イベントが発生します。
このイベントを受けて Module2 の C++ 実装にある
`w32-tr-ime-language-change-handler` 関数を呼びます。
この関数は内部専用キューからメッセージを取り出し、
その種類に応じてノーマルフックを呼び出すなどの動作を行います。

この中で、language-change イベントの発生を受けて、
`w32-tr-ime-language-change-handler` 関数を呼ぶところについて、
以下のような設定を行っています。

```el
(define-key special-event-map [language-change]
  (lambda ()
    (interactive)
    (w32-tr-ime-language-change-handler))))
```

本モジュールとは別の language-change イベントを使うツール類と
共存させたい場合は、上記設定をうまく調整してください。
本モジュールの `w32-tr-ime-language-change-handler` 関数は、
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
ダイナミックモジュールの情報が集まったページ
](https://github.com/jkitchin/emacs-modules) からリンクが貼られていた[
Asynchronous Requests from Emacs Dynamic Modules
](https://nullprogram.com/blog/2017/02/14/)を参考に、
上記のような WM_INPUTLANGCHANGE による方法を実装しました。

この動作を無効にするには、以下のようにすればできます
（デフォルトは有効）。

```el
(custom-set-variables '(w32-tr-ime-module-recv-from-ui-thread-p nil))
```

### IME フォント (Module2)

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
(w32-tr-ime-reflect-frame-parameter-ime-font)
```

#### フォーカス変更時に ime-font 設定エミュレーションを呼ぶか否か (Module2)

フォーカス変更時の反映を無効にしたい場合は、以下でできます
（デフォルトは有効）。

```el
(custom-set-variables '(w32-tr-ime-module-ime-font-focus-p nil))
```

#### コマンド実行後に ime-font 設定エミュレーションを呼ぶか否か (Module2)

post-command-hook によるコマンド実行後の反映を有効にしたい場合は、
以下でできます（デフォルトは無効）。

```el
(custom-set-variables '(w32-tr-ime-module-ime-font-post-command-p t))
```
### isearch-mode (Module2)

Module1 だと C-s など isearch-mode の検索中、
未確定文字列はミニバッファの文字入力位置ではなくて、
検索開始前に元々入力していた位置に表示されてしまいます。
isearch-mode 時に選択されているウィンドウはミニバッファではないので、
ある意味では正しい挙動ではありますが、
確定するとミニバッファに表示されるので、
位置が食い違っていて使いにくいです。

Module2 では IME パッチと同じように、
こうした未確定文字列をミニバッファの文字入力位置に表示できます。
ただし、実現方法が異なるので、微妙な動作の違いがあるかもしれません。

#### isearch-mode 中の未確定文字列表示位置を文字入力位置にするか否か (Module2)

isearch-mode 中の位置設定を無効にするには、以下のようにすればできます
（デフォルトは有効）。

```el
(custom-set-variables '(w32-tr-ime-module-isearch-p nil))
```

#### WM_IME_STARTCOMPOSITION で常に DefSubclassProc を呼ぶか否か (Module2)

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
(custom-set-variables '(w32-tr-ime-module-isearch-defsubclassproc-p t)
```

### プレフィックスキー検出 (Module2)

コマンドのキーシーケンスになる最初のキーである
プレフィックスキー（C-x など）を検出すると、
自動的に IME OFF にして、コマンド終了後に IME 状態を戻します。
これにより IME ON のまま C-x 1 のような操作をしたときに、
1 が IME に吸われて未確定文字扱いされないようにしています。

本来はプレフィックスキーが来たら呼ばれるフックなどがあればよいのですが、
残念ながら Emacs には存在しないようなので、
単純に以下で指定したプレフィックスキー検出リストに登録された
キーが押下されたら（WM_KEYDOWN メッセージが来たら）IME OFFにして、
post-command-hook で IME 状態を戻す処理をしています。

Module1 向けにタイマを使って同様の機能を実現したワークアラウンドがありますが、
Module2 の本機能はタイマを使わないためタイミング的にも負荷的にも有利です。
本機能を有効にすると Module1 のワークアラウンドによる
プレフィックスキー検出は無効になります。

#### プレフィックスキー検出対象リスト (Module2)

検出リストは以下のように
プレフィックスキーとして検出したいコードのリストとして設定できます
（Module1 ワークアラウンドの設定方法とはコード体系が異なります）。

```el
(custom-set-variables '(w32-tr-ime-module-prefix-key-list
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

#### プレフィックスキーを検出して自動的に IME OFF するか否か (Module2)

プレフィックスキー検出を無効にするには、以下のようにすればできます
（デフォルトは有効）。

```el
(custom-set-variables '(w32-tr-ime-module-prefix-key-p nil))
```

### IME 状態変更通知による IME/IM 状態同期 (Module2)

IME 状態が変更されたら WM_IME_NOTIFY IMN_SETOPENSTATUS
が送られてきます。この通知を受けて IM 状態を同期させます。

Module1 向けにタイマを使って
IME 状態の食い違いを検出して修正するワークアラウンドがありますが、
Module2 の本機能はタイマを使わないためタイミング的にも負荷的にも有利です。
本機能を有効にすると Module1 のワークアラウンドによる
食い違い検出は無効になります。

本機能は、
UI スレッドに WM_IME_NOTIFY IMN_SETOPENSTATUS がきたら、
UI スレッドからの通知を Lisp で受け取る機構を利用して
setopenstatus を内部専用キューに格納して通知し、
`w32-tr-ime-language-change-handler` 関数が、
内部専用キューから setopenstatus を受け取ると、
ノーマルフック `w32-tr-ime-module-setopenstatus-hook` を呼び出し、
そこで一連の IME/IM 同期の動作が行われるようになっています。

#### IME 状態変更通知時にフックエミュレーション関数を呼ぶか否か

IME 状態変更通知があった時に、
IME/IM 状態同期の前にフックエミュレーション関数を呼ぶことで、
未検出のウィンドウ変更やバッファ変更を検知し、
IME パッチ特有のアブノーマルフックが呼ばれて IME/IM 状態が整えられます。

この動作を無効にするには、以下のようにすればできます
（デフォルトは有効）。

```el
(custom-set-variables
 '(w32-tr-ime-module-setopenstatus-call-hook-emulator-p nil))
```

#### IME 状態変更通知による IM 状態同期をするか否か

IME 状態変更通知による IM 状態同期を無効にするには、
以下のようにすればできます（デフォルトは有効）。

```el
(custom-set-variables '(w32-tr-ime-module-setopenstatus-sync-p nil))
```

### ワークアラウンド設定

モジュールの構成上どうにもならない機能を、
Lisp でタイマを使ってなんとかしているものです。
モジュールの高機能化によって不要になるものもあります。

#### isearch-mode 時の Alt + 半角/全角ワークアラウンド (Module2)

Module2 で isearch-mode 時に Alt + 半角/全角キー操作をすると、
なぜかエコーエリアが消えてしまいます。
キー操作時に再表示させるようにしても効果が無い
（恐らくキー操作後にくるイベントか何かで消されている）ので、
Emacs がアイドル状態になったら動作するタイマで再表示させる
ワークアラウンドを用意しました。

Module1 だけ使う場合は問題ありません。
Module2 でも
C-\\ や半角/全角単独など、他の方法で IME ON/OFF する場合は問題ありません。
Alt + 半角/全角キー操作はしないとか、
エコーエリアが消えても問題ないという場合は
以下の設定で無効にできます。

```el
(custom-set-variables
 '(w32-tr-ime-module-workaround-isearch-mode-delayed-update-p nil))
```

#### IME 状態の食い違いを検出して修正するワークアラウンド

IME 側トリガの状態変更（半角/全角キーやマウスでの切り替え）
を検出して IM 側を同期させるための機構です。

Module2 ではもっと筋が良い対応ができるため、そちらを使えば不要ですが、
Module1 だけ使いたい場合に
ワークアラウンドとして、
定期的に動くタイマでポーリングし、
IME と IM の状態が食い違ったら IM 状態を反転して一致させる、
という機構を用意しました。

定期的なタイマで動作するため負荷が気になるため、
Module2 がなくてもデフォルトは無効にしています。

以下の設定で有効にできます。

```el
(custom-set-variables '(w32-tr-ime-module-workaround-inconsistent-ime-p t))
```

IME 状態食い違い検出修正用ポーリング時間（秒）が
`w32-tr-ime-module-workaround-inconsistent-ime-polling-time`
に設定できます（デフォルト `1.0`）。
IME 状態食い違い検出修正前にフックエミュレーション関数を呼ぶか否かが
`w32-tr-ime-module-workaround-inconsistent-ime-call-hook-emulator-p`
に設定できます（デフォルト `t`、呼ぶ）。

#### プレフィックスキー（C-x など）を検出して IME OFF にするワークアラウンド

コマンドのキーシーケンスになる最初のキーである
プレフィックスキー（C-x など）を検出すると、
自動的に IME OFF にする機能です。
これにより IME ON のまま C-x 1 のような操作をしたときに、
1 が IME に吸われて未確定文字扱いされなくなります。

Module2 ではもっと筋が良い対応ができているため不要ですが、
Module1 だけ使いたい場合に
ワークアラウンドとして、
Emacs がアイドル状態になったら動くタイマでポーリングし、
プレフィックスキーが押されていたら IME OFF にし、
コマンドが終了したら IME を復帰させる、という機構を残してあります。

以下の設定で無効にできます。デフォルトは Module2 が無い場合のみ有効です。

```el
(custom-set-variables '(w32-tr-ime-module-workaround-prefix-key-p nil))
```

プレフィックスキー検出用ポーリング時間（秒）が
`w32-tr-ime-module-workaround-prefix-key-polling-time`
に設定できます（デフォルト `0.1`）。
プレフィックスキー検出検出対象リストが
`w32-tr-ime-module-workaround-prefix-key-list`
に設定できます（デフォルト `'(?\C-x ?\C-h ?\C-c ?\e)`、
C-x, C-h, C-c と ESC です）。

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
      tr-emacs-ime-module-VERSION.tar.gz
      を使うのであれば環境に Autotools がインストールされていなくても
      Autotools を使ったビルドができます

### ビルド方法

以下のようにすればビルドできます。
インストール先に応じて `--prefix` オプションの値を変えてください。
Module2 もビルドしたい場合は `--enable-module2`
オプションを追加してください。
[リリース](https://github.com/trueroad/tr-emacs-ime-module/releases)
にあるソース配布ファイル
tr-emacs-ime-module-VERSION.tar.gz
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

### バイナリリリースのビルド方法

バイナリリリースは以下の方法でビルドしたものです。

#### Cygwin 64 bit / 32 bit

いずれも、Cygwin 64 bit / 32 bit の環境で、ソース配布ファイル
tr-emacs-ime-module-VERSION.tar.gz
を使って以下のようにしてビルドしました。

```
$ tar xfvz tr-emacs-ime-module-VERSION.tar.gz
$ cd tr-emacs-ime-module-VERSION
$ mkdir build
$ cd build
$ ../configure --enable-module2 --prefix=/usr
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
$ tar xfvz tr-emacs-ime-module-VERSION.tar.gz
$ cd tr-emacs-ime-module-VERSION
$ mkdir build
$ cd build
$ cp /usr/include/emacs-module.h .
$ ../configure --enable-module2 --host=x86_64-w64-mingw32 \
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

* 再変換 (RECONVERSION) や前後の確定済文字列を参照した変換 (DOCUMENTFEED)
  には対応できない
    * 対応するには
      UI スレッドのメッセージループから Lisp へ通知を行った上に、
      Lisp から情報を UI スレッドへ戻して動作を継続する必要がある
      （一方通行ではなくて往復が必要になる）、
      というかなり困難な処理が必要です
* Module2 で isearch-mode 時に Alt + 半角/全角で IME ON/OFF すると
  エコーエリアの表示が消えてしまう
    * ワークアラウンドでなんとかしています
* 未確定文字列フォントの設定 (Module2) で generic ファミリは使用不可
    * 無いと困るという方はいらっしゃいますか？どのような使い方でしょうか？

以下は、IME パッチでも発生する事象で、
これまでに気が付いたものです。

* 「IME 入力モード切替の通知（画面中央に大きく「あ」とか「A」とか出るもの）」
  が C-\\ で切り替えると出ない
    * Windows 10 1909 で確認
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
  で使っている時に、IME ON のバッファがあるフレームと
  IME OFF のバッファがあるフレームを交互にクリックすると、
  フレームが切り替わった時に出る「IME 入力モード切替の通知」が
  実際の IME 状態とは逆になる
    * Windows 10 1909 で確認
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

Simple IME module for GNU Emacs (tr-emacs-ime-module)
is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Simple IME module for GNU Emacs (tr-emacs-ime-module)
is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with tr-emacs-ime-module.
If not, see <https://www.gnu.org/licenses/>.

[COPYING](./COPYING)
