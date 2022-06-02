class Transform: # a translation vector and a (flattened) rotation matrix representing an R^3 isometry
    def __init__(self, trans, rot, rotateFirst=False, units="radians"):
        assert len(trans) == 3, f"First argument to Transform(trans, rot,...) must be of length 3; recieved {trans}"
        if len(rot) == 9:
            self.M = 1 if rotateFirst else -1
        elif len(rot) == 3:
            self.M = 2 if rotateFirst else -2

        self.trans = trans
        self.rot = rot
        self.units = units

    def __eq__(self, other):
        return self.trans == other.trans and self.rot == other.rot \
            and self.rotateFirst == other.rotateFirst
    
idTransform = Transform([0.0, 0.0, 0.0], [0.0 for i in range(9)])
