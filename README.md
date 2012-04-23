RPN.hs
======
Unix utility for conversion of infix-form expressions into reverse polish notation (postfix) format

Usage
=====
```
Usage: rpn [-p precision] [-ehsV] [--version] [--help] ["<expression>"].
Don't forget to use quote marks and parentheses.
  -p precision             Number of digits shown after the decimal point.
  -s                       Output in scientific format (e.g. 2.45e2, 1.5e-3).
  -e                       Evaluate expression (instead of conversion).
  -V            --version  Print version and exit...
  -h, -?        --help     Prints this help message.
```