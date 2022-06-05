from base.py import *
from cell.py import *


class Mesh(PhitsBase):
    def __init__(self, axis, bins=None, **kwargs): # All bin generation is easily done in Python via list comprehensions
        super("mesh", **kwargs)
        assert axis in ["energy", "time", "x", "y", "z", "radius", "angle", "LET"], f"Unrecognized axis {axis} in mesh definition."
        self.axis = axis
        self.bins = bins
        if axis != angle:
            self.type = 2
        else:
            # TODO: figure out angle mesh

    def definition(self):
        return

    class Tally(PhitsBase): # out-mesh is the mesh for the output; in-mesh is a list consisting of an optional origin coordinate in the first entry and a list of meshes for
    def __init__(self, out_mesh, unit, in_meshes=None, dependent_var="reg", particles="all", angel="", sangel="", title="", output_type="", 2d_type=5
                 factor=None, detailed_output=False, region_show=0, gshow=0, axis_titles=[], epsout=0, resolution=1, transform=None, dump=None, **kwargs):
        super("tally", **kwargs)
        self.meshes = meshes

        self.mesh_type = None
        if in_meshes is None:
            self.mesh_type = "reg"
            self.axis="reg"
        if isinstance(in_meshes, list):
            if isinstance(where[0], Cell):
                self.mesh_type = "reg"
            else:
                raise ValueError(f"Invalid tally location {where}.")
        elif next(filter(lambda mesh: mesh.axis in {"x", "y", "z"}, in_meshes), False):
            self.mesh_type = "xyz"
        elif next(filter(lambda mesh: mesh.axis in {"r", "z"}, in_meshes), False):
            self.mesh_type = "r-z"



class TrackLengthFluence(Tally):
    def __init__(self, out_mesh, unit, in_meshes=None, dependent_var="reg", where=None, particles="all", angel="", sangel="", title="", output_type="", 2d_type=5
                 factor=None, detailed_output=False, region_show=0, gshow=0, axis_titles=[], epsout=0, resolution=1, transform=None, dump=None, **kwargs)
