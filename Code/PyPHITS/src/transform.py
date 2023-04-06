from base import *
import sys

vector = Tuple(Real(), Real(), Real())
class Transform(PhitsObject): #
    """An \\(\\mathbb{R}^3\\) isometry represented by a translation vector and a rotation matrix."""
    syntax = {"translation": ("trans", vector, 0),
              "rotation": ("rot", Tuple(vector, vector, vector), 1),
              "rotate_first": (None, FinBij({True: 1, False: -1}), None),
              "units": (None, FinBij({"degrees": "degrees", "radians": "radians"}), None)}
    shape=((lambda: f"*TR{self.index}" if self.units == "degrees" else f"TR{self.index}",
            "trans", "rot" "rotate_first"))

#idTransform = Transform([0.0, 0.0, 0.0], [0.0 for i in range(9)])

__pdoc__ = dict()
__pdoc__["builds"] = False
__pdoc__["slices"] = False
for name, cl in list(sys.modules[__name__].__dict__.items()):
    if type(cl) == type and issubclass(cl, PhitsObject) and cl != PhitsObject: # and cl.__module__ == __name__
        for k, v in cl.syntax.items():
            __pdoc__[cl.__name__] = cl.__doc__ + cl.syntax_desc() if cl.__doc__ else cl.syntax_desc()
