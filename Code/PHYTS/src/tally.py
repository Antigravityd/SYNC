from base.py import *
from cell.py import *

class Mesh(PhitsBase):
    def __init__(self, axis, points=None, bounds=None, bins=None, width=None, log_scale=False, **kwargs):
        super("mesh", **kwargs)
        assert axis in ["energy", "time", "x", "y", "z", "radius", "angle", "LET"], f"Unrecognized axis {axis} in mesh definition."
        self.axis = axis
        self.points = points
        self.bounds = bounds
        self.bins = bins
        self.log_scale = log_scale
        if axis != angle:
            if points is not None:
                self.type = 1
            elif bins is not None:
                self.type = 3 if log_scale else 2
            elif width is not None:
                self.type = 5 if log_scale else 4
            else:
                raise ValueError("Insufficient information to define mesh.")
        else:
            # TODO: figure out angle mesh

class Tally(PhitsBase):
    def __init__(self, meshes, where=None, particles="all", **kwargs):
        super("tally", **kwargs)
        self.where = where
        self.meshes = meshes

        self.mesh_type = None
        if where is None:
            self.mesh_type = "reg"
        if isinstance(where, list):
            if isinstance(where[0], Cell):
                self.mesh_type = "reg"
            else:
                raise ValueError(f"Invalid tally location {where}.")
        if isinstance(where, String):
            if where.contains("r"):
                self.mesh = "r-z"
            if where.contains("x"):
                self.mesh = "xyz"
