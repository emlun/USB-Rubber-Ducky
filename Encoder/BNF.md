DuckyScript BNF
===

Backus-Naur Form grammar for DuckyScript.

This is not an LL(1) grammar since the `StringLit`, `IntLit` and `KeyName`
classes overlap.

- The `STRING` command is followed by a single literal space character, and any
  following non-newline characters are part of the `StringLit`.
- DefaultDelay and Statements are separated by at least one newline followed by
  any amount of whitespace.
- In all other cases, tokens are separated by any amount of spaces and tab
  characters.

      Script       ::= DefaultDelay? Statement*
      DefaultDelay ::= (DEFAULT_DELAY | DEFAULTDELAY) IntLit
      Statement    ::= Command | LineComment
      LineComment  ::= REM.*
      Command      ::= STRING StringLit
                     | DELAY IntLit
                     | REPEAT IntLit
                     | (CONTROL | CTRL) KeyName?
                     | ALT KeyName?
                     | SHIFT KeyName?
                     | CTRL-ALT KeyName?
                     | CTRL-SHIFT KeyName?
                     | COMMAND-OPTION KeyName?
                     | ALT-SHIFT KeyName?
                     | ALT-TAB
                     | (WINDOWS | GUI) KeyName?
                     | COMMAND KeyName?
                     | KeyName
      StringLit    ::= .*
      IntLit       ::= [1-9][0-9]*
      KeyName      ::= ENTER         | ESC             | BACKSPACE    | TAB
                     | SPACE         | MINUS           | EQUAL        | LEFT_BRACE
                     | RIGHT_BRACE   | BACKSLASH       | NON_US_NUM   | SEMICOLON
                     | QUOTE         | TILDE           | COMMA        | PERIOD
                     | SLASH         | CAPS_LOCK       | F1           | F2
                     | F3            | F4              | F5           | F6
                     | F7            | F8              | F9           | F10
                     | F11           | F12             | PRINTSCREEN  | SCROLL_LOCK
                     | PAUSE         | INSERT          | HOME         | PAGEUP
                     | DELETE        | END             | PAGEDOWN     | RIGHT
                     | LEFT          | DOWN            | UP           | NUM_LOCK
                     | KEYPAD_SLASH  | KEYPAD_ASTERISK | KEYPAD_MINUS | KEYPAD_PLUS
                     | KEYPAD_ENTER  | KEYPAD_EQUALS   | KEYPAD_1     | KEYPAD_2
                     | KEYPAD_3      | KEYPAD_4        | KEYPAD_5     | KEYPAD_6
                     | KEYPAD_7      | KEYPAD_8        | KEYPAD_9     | KEYPAD_0
                     | KEYPAD_PERIOD | KEYPAD_PIPE
