# "A = B: A One-Instruction Esolang" 

「A = B」言語で書いた書き換え規則集を入力文字列に適用する単純な実行器を提供します。
インタラクティブに遊ぶ機能はありません。

ビルド
```
$ stack build
```
起動
```
$ stack exec -- a_b prog/ex11 arama
Input:  arama
arama
    a=b
brama
    a=b
brbma
    a=b
brbmb
Output: brbm
```
