# w32-ime.el

[Simple IME module for GNU Emacs
(tr-emacs-ime-module)](https://github.com/trueroad/tr-emacs-ime-module)
で使う
[w32-ime.el](https://github.com/trueroad/w32-ime.el)
を格納しています。
IME パッチが当たっている Emacs と当たっていない Emacs で
設定を分けることができるように、ファイル名を
[w32-ime-for-tr-ime-module.el](./w32-ime-for-tr-ime-module.el)
に変えていますが、中身は w32-ime.el そのものです。

ここに置いている w32-ime.el は、
[TANE 氏](http://tanehp.ec-net.jp/heppoko-lab/prog/zakki/emacs/emacs.html)
の配布する IME パッチに含まれていた
[w32-ime.el](https://github.com/trueroad/w32-ime.el/tree/20200815_TANE)
をもとに、

* IME 全バッファ同期（w32-ime-buffer-switch-p が nil）設定時に、
  IM 状態も正しく同期できるように
  [修正](https://github.com/trueroad/w32-ime.el/tree/20200824_Hosoda)
* IME 全バッファ同期（w32-ime-buffer-switch-p が nil）設定時に、
  他のフレームのモードラインも同期できるように
  [修正](https://github.com/trueroad/w32-ime.el/tree/20200826_Hosoda)
* IME 全バッファ同期（w32-ime-buffer-switch-p が nil）設定時に、
  IME ON のまま C-s (isearch-forward) すると、
  未変換文字の確定時に IME OFF になってしまう事象を
  [修正](https://github.com/trueroad/w32-ime.el/tree/20200829_Hosoda)

したものです。
