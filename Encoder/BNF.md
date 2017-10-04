DuckyScript v1.0.0 BNF
===

Backus-Naur Form grammar for DuckyScript version 1.0.0.

This is not an LL(1) grammar since the `StringLit`, `IntLit` and `KeyName`
classes overlap.

- The `STRING` command is followed by a single literal space character, and any
  following non-newline characters are part of the `StringLit`.
- `DefaultDelay`, `Statement`s and `LineComment`s are separated by at least one
  newline followed by any amount of whitespace.
- In all other cases, tokens are separated by any amount of spaces and tab
  characters.
- They terminals `REM`, `STRING`, `DELAY`, `REPEAT`, `CONTROL`, `CTRL`, `ALT`,
  `SHIFT, CTRL-ALT`, `CTRL-SHIFT`, `COMMAND-OPTION`, `ALT-SHIFT`, `ALT-TAB`,
  `WINDOWS`, `GUI`, `COMMAND` have priority over the `KeyName` nonterminal.

      Script       ::= LineComment* DefaultDelay? LineComment* (Statement LineComment*)*
      DefaultDelay ::= (DEFAULT_DELAY | DEFAULTDELAY) IntLit
      LineComment  ::= REM.*
      Statement    ::= STRING StringLit
                     | DELAY IntLit
                     | REPEAT IntLit
                     | (CONTROL | CTRL) KeyName?
                     | ALT KeyName?
                     | SHIFT KeyName?
                     | CTRL-ALT KeyName?
                     | CTRL-SHIFT KeyName?
                     | COMMAND-OPTION KeyName?
                     | ALT-SHIFT KeyName?
                     | ALT-TAB KeyName?
                     | (WINDOWS | GUI) KeyName?
                     | COMMAND KeyName?
                     | KeyName
      StringLit    ::= .*
      IntLit       ::= [1-9][0-9]*
      KeyName      ::= .+
