ChessPuzzle
===========

This is a haskell program that solves "8 Queens" type puzzles.

It is my first serious effort written in Haskell, so please excuse me for anything that's horrible!

Any comments or suggestions on improving it are very welcome.

To compile:
```
  ghc -o chess ChessPuzzle.hs Main.hs
```
The output program runs with no arguments:
```
C:\...\ChessPuzzle>chess
Enter rows
4
Enter columns
4
Enter pieces. Any of ['PRNBQK']
KKBBBB
+----+
|BB.B|
|...B|
|K...|
|..K.|
+----+

+----+
|BB..|
|...K|
|B...|
|B.K.|
+----+

+----+
|B.BB|
|B...|
|...K|
|.K..|
+----+

+----+
|B.K.|
|B...|
|...K|
|BB..|
+----+

+----+
|.K.B|
|...B|
|K...|
|..BB|
+----+

+----+
|.K..|
|...K|
|B...|
|B.BB|
+----+

+----+
|..BB|
|K...|
|...B|
|.K.B|
+----+

+----+
|..K.|
|K...|
|...B|
|BB.B|
+----+

Total solutions: 8
```
If there are too many solutions to print, it instead works like this:

```
Enter rows
6
Enter columns
9
Enter pieces. Any of ['PRNBQK']
QRNBKK
Over 100 solutions...
Total solutions: 20136752
```
