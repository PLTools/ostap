  $ ./test001.exe
  Parsed: hasToBeParsed
  Failed.
  $ ./test001.exe
  Parsed: hasToBeParsed
  Failed.
  $ ./test002.exe
  Parsed: left.right
  Parsed: left.right
  Parsed: left.right
  Failed.
  $ ./test003.exe
  Parsed: left
  Parsed: 123
  $ ./test004.exe
  Parsed: left
  Parsed: (abc)
  $ ./test005.exe
  Parsed: rtfgui
  Parsed: abc
  $ ./test006.exe
  Parsed: rtfguiug
  Parsed: abcdef
  $ ./test007.exe
  Parsed: rtfgui
  Failed.
  $ ./test008.exe
  Parsed: rtfgui
  Failed.
  $ ./test009.exe
  Parsed: rtfgui
  Failed.
  $ ./test010.exe
  Parsed: rtfgui
  Failed.
  $ ./test011.exe
  Parsed: rtfgui
  Failed.
  $ ./test012.exe
  Not parsed:
   Error at (1:1): 
      "-" expected at (1:1) 
      "(" expected at (1:1) 
      "constant" expected at (1:1) 
   Error at (1:2): 
      <EOF> expected at (1:2) 
      "-" expected at (1:2) 
      "/" expected at (1:2) 
      "*" expected at (1:2) 
   Error at (1:3): operand expected
      "-" expected at (1:3) 
      "(" expected at (1:3) 
      "constant" expected at (1:3) 
      <EOF> expected at (1:4) 
      "+" expected at (1:4) 
      "/" expected at (1:4) 
      "*" expected at (1:4) operand expected
      "-" expected at (1:5) 
      "(" expected at (1:5) 
      "constant" expected at (1:5) 
      "identifier" expected at (1:5) 
  
  $ ./test013.exe
  Parsed: E[E[E[n]+n]+n]
  $ ./test014.exe
  Parsed: E[E[E[T[n]]-T[(E[E[T[n]]+T[n]])]]+T[n]]
  $ ./test015.exe
  Parsed: E[M[n]+E[M[n]+E[M[n]]]]
  Parsed: E[M[M[M[n]-n]-n]]
$ ./test016.exe
$ ./test017.exe
$ ./test020.exe
