from lark import Lark

logic = Lark(r"""
wff: ATOMIC | and | or | implies | equiv | not

and: "(" wff ")" "∧" "(" wff ")"
or: "(" wff ")" "∨" "(" wff ")"
implies: "(" wff ")" "→" "(" wff ")"
equiv: "(" wff ")" "↔" "(" wff ")"
not: "¬" "(" wff ")"

ATOMIC: /A[0-9]+/

%import common.WS
%ignore WS""", start="wff")


logic.parse("(A1)∨(A2)")
