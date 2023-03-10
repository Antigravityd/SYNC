from lark import Lark, Transformer
from functools import partial

logic = Lark(r"""
wff: ATOMIC | and_ | or_ | implies | equiv | not_

and_: "(" wff ")" "∧" "(" wff ")"
or_: "(" wff ")" "∨" "(" wff ")"
implies: "(" wff ")" "→" "(" wff ")"
equiv: "(" wff ")" "↔" "(" wff ")"
not_: "¬" "(" wff ")"

ATOMIC: /A(₀|[₁-₉][₀-₉]*)/

%import common.WS
%ignore WS""", start="wff")

class Tf(Transformer):
    def ATOMIC(s, lf):
        unsub = {"₀": "0", "₁": "1", "₂": "2", "₃": "3", "₄": "4", "₅": "5", "₆": "6", "₇": "7", "₈": "8", "₉": "9"}
        start = str(lf)
        for ch in unsub:
            start = start.replace(ch, unsub[ch])
        # map/lambda isn't letting me use globals, so there
        return ([start], start)
    def not_(s, wff):
        return (wff[0], f"not ({wff})")
    def equiv(s, wff):
        return (wff[0][0] + wff[1][0], f"({wff[0][1]}) == ({wff[1][1]})")
    def implies(s, wff):
        return (wff[0][0] + wff[1][0], f"({wff[0][1]}) <= ({wff[1][1]})")
    def or_(s, wff):
        return (wff[0][0] + wff[1][0], f"({wff[0][1]}) or ({wff[1][1]})")
    def and_(s, wff):
        return (wff[0][0] + wff[1][0], f"({wff[0][1]}) and ({wff[1][1]})")
    def wff(s, wff):
        return wff[0]


def booleanfunc(st, arity):
    dummy = arity - len(schema[0])
    assert dummy >= 0, f"The wff {st} is not representable as a function of arity {arity}!"
    schema = Tf().transform(logic.parse(st))
    scope = locals()
    exec("def r(" + ''.join([f"{i}," for i in schema[0][:-1]] + [f"B{i}" for i in dummy]) + f"{schema[0][-1]}):\n\treturn {schema[1]}", scope)

    return scope["r"]


#print(logic.parse("(A₀)∨(A₂)").pretty())
