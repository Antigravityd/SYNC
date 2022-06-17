from base import *
class Transform(PhitsObject): # a translation vector and a (flattened) rotation matrix representing an R^3 isometry
    def __init__(self, trans, rot, rotateFirst=False, units="radians", **kwargs):
        super().__init__("transform", **kwargs)
        assert len(trans) == 3, f"First argument to Transform(trans, rot,...) must be of length 3; recieved {trans}"
        if len(rot) == 9:
            self.M = 1 if rotateFirst else -1
        elif len(rot) == 3:
            self.M = 2 if rotateFirst else -2

        self.trans = tuple(trans)
        self.rot = tuple(rot)
        self.units = units

    def definition(self):
        inp = ""
        if self.units == "radians":
            inp += f"TR{self.index} "
        elif tr.units == "degrees":
            inp += f"*TR{self.index} "
        else:
            raise ValueError(f"Encountered invalid angular unit {self.units} among transforms.")
        inp += f"{self.trans[0]} {self.trans[1]} {self.trans[2]} {self.rot[0]} {self.rot[1]} {self.rot[2]} {self.rot[3]} {self.rot[4]} {self.rot[5]} {self.rot[6]} {self.rot[7]} {self.rot[8]} {self.M}\n"
        return inp

idTransform = Transform([0.0, 0.0, 0.0], [0.0 for i in range(9)])
