from base import *
from cell import *
import collections as col


class Mesh():
    def __init__(self, axis, bins=None): # All bin generation is easily done in Python via list comprehensions
        assert axis in ["energy", "time", "x", "y", "z", "radius", "angle", "let"], f"Unrecognized axis {axis} in mesh definition."
        self.axis = axis
        self.bins = tuple(bins)
        print(self.bins)
        if axis != "angle":
            self.type = 2
        else:
            pass  # TODO: figure out angle mesh

    def definition(self):
        inp = f"{self.axis[0]}-type = 1\n"
        inp += f"n{self.axis[0]} = {len(self.bins)-1}\n"
        for i in self.bins:
            inp += f"{i} "

        inp += "\n"
        return inp

    def __eq__(self, other):
        if type(self) != type(other):
            return False

        else:
            return {k: v for k, v in self.__dict__.items() if v is not other} \
                == {k: v for k, v in other.__dict__.items() if v is not self}

    def __hash__(self):
        return hash(tuple(v for k, v in sorted(self.__dict__.items()) \
                          if (self not in v.__dict__.values() if hasattr(v, "__dict__") else True)))




class Tally(PhitsObject): # out-mesh is the mesh for the output; in-mesh is a list consisting of an optional origin coordinate in the first entry and a list of meshes for the dependent variables
    def __init__(self, section_title, out_mesh, outfile,  in_meshes=None, dependent_var="reg", particles="all",
                 angel=None, sangel=None,
                 title=None, output_type=None, type_2d=5, factor=None, detailed_output=False, region_show=None,
                 gshow=None, axis_titles=None, epsout=None, resolution=None, transform=None, dump=None, **kwargs): # dump not supported
        super().__init__(section_title, **kwargs)
        self.out_mesh = out_mesh
#        self.unit = unit TODO: set this in children
        self.in_meshes = tuple(in_meshes) if in_meshes is not None else None
        self.dependent_var = dependent_var
        self.particles = particles
        self.angel = angel
        self.sangel = sangel
        self.title = title
        self.output_type = output_type
        self.type_2d = type_2d
        self.factor = factor
        self.detailed_output = detailed_output
        self.region_show = region_show
        self.gshow = gshow
        self.axis_titles = tuple(axis_titles) if axis_titles is not None else None
        self.epsout = epsout
        self.resolution = resolution
        self.transform = transform
        self.dump = dump
        self.outfile = outfile

        self.cell = None


        self.mesh_type = None
        if in_meshes is None:
            self.mesh_type = "reg"
            self.axis="reg"
        if isinstance(in_meshes, col.Iterable):
            if isinstance(in_meshes[0], Cell):
                self.mesh_type = "reg"
            elif next(filter(lambda mesh: mesh.axis in {"x", "y", "z"}, in_meshes), False):
                self.mesh_type = "xyz"
            elif next(filter(lambda mesh: mesh.axis in {"r", "z"}, in_meshes), False):
                self.mesh_type = "r-z"
            else:
                raise ValueError(f"Invalid tally location {in_meshes}.")


    def definition(self): # only for use in defining other definition()s; the child must set units, transform (since id is available only at runtime),
        inp = f"mesh = {self.mesh_type}\n"
        if self.mesh_type == "reg" and self.cell is not None:
            inp += f"reg = {self.cell.index}\n"
        if self.in_meshes is not None:
            for mesh in self.in_meshes:
                inp += mesh.definition() # this one happens to not need data only available in make_input()
        inp += self.out_mesh.definition()

        inp += f"part = {self.particles}\n"
        inp += f"axis = {self.dependent_var}\n"
        if self.outfile is not None:
            inp += f"file = {self.outfile}\n"
        if self.type_2d is not None:
            inp += f"2d-type = {self.type_2d}\n"
        # if self.info:
        #     inp += "info = 1\n"
        if self.region_show is not None:
            inp += f"rshow = {self.region_show}\n"
        if self.axis_titles is not None:
            inp += f"x-txt = {self.axis_titles[0]}\n"
            inp += f"y-txt = {self.axis_titles[1]}\n"
            inp += f"z-txt = {self.axis_titles[2]}\n"
        if self.resolution is not None:
            inp += f"resol = {self.resolution}\n"
        for param, val in {k: v for k, v in self.__dict__.items() if k in {"angel", "sangel", "title", "output_type", "factor", "gshow", "epsout"} and v is not None}.items():
            inp += f"{param} = {val}\n"

        return inp

class TrackLengthFluence(Tally):
    def __init__(self, out_mesh, units, **kwargs):
        super().__init__("t-track", out_mesh,  **kwargs)
        if units == "1/cm^2/source":
            self.units = 1
        elif units == "1/cm^2/MeV/source":
            self.units = 2
        elif units == "1/cm^2/Lethargy/source":
            self.units = 3
        elif units == "cm/source":
            self.units = 4
        elif units == "1/cm^2/nsec/source":
            self.units = 11
        elif units == "1/cm^2/MeV/nsec/source":
            self.units = 12
        elif units == "1/cm^2/Lethargy/nsec/source":
            self.units = 13
        elif units == "cm/nsec/source":
            self.units = 14

    def definition(self):
        inp = super().definition()
        inp += f"unit = {self.units}\n"
        inp += f"trcl = {self.transform.index}\n"

class SurfaceFluence(Tally):
    def __init__(self, out_mesh, units, output, iangform="normal",  **kwargs):
        super().__init__("t-cross", out_mesh,  **kwargs)
        self.output = output
        if units == "1/cm^2/source":
            self.units = 1
        elif units == "1/cm^2/MeV/source":
            self.units = 2
        elif units == "1/cm^2/Lethargy/source":
            self.units = 3
        elif units == "1/cm^2/sr/source":
            self.units = 4
        elif units == "1/cm^2/MeV/sr/source":
            self.units = 5
        elif units == "1/cm^2/Lethargy/sr/source":
            self.units = 6
        elif units == "1/cm^2/nsec/source":
            self.units = 11
        elif units == "1/cm^2/MeV/nsec/source":
            self.units = 12
        elif units == "1/cm^2/Lethargy/nsec/source":
            self.units = 13
        elif units == "1/cm^2/sr/nsec/source":
            self.units = 14
        elif units == "1/cm^2/MeV/sr/nsec/source":
            self.units = 15
        elif units == "1/cm^2/Lethargy/sr/nsec/source":
            self.units = 16

        if iangform == "normal":
            self.iangform = 0
        if iangform == "x":
            self.iangform = 1
        if iangform == "y":
            self.iangform = 2
        if iangfomr == "z":
            self.iangform = 3

    def definition(self):
        inp = super().definition()
        inp += f"units = {self.units}\n"
        inp += f"trcl = {self.transform.index}\n"
        inp += f"iangform = {self.iangform}\n"
        inp += f"output = {self.output}\n"

class PointFluence(Tally):
    def __init__(self, out_mesh, units,  **kwargs):
        super().__init__("t-point", out_mesh, **kwargs)
        if units == "1/cm^2/source":
            self.units = 1
        elif units == "1/cm^2/MeV/source":
            self.units = 2
        elif units == "1/cm^2/Lethargy/source":
            self.units = 3
        elif units == "1/cm^2/nsec/source":
            self.units = 11
        elif units == "1/cm^2/MeV/nsec/source":
            self.units = 12
        elif units == "1/cm^2/Lethargy/nsec/source":
            self.units = 13

    def definition(self):
        inp = super().definition()
        inp += f"units = {self.units}\n"
        inp += f"trcl = {self.transform.index}\n"

class Deposition(Tally): # output parameter is ("dose" | "deposit" | "deposit/particle")
    def __init__(self, out_mesh, output, units, outfile="deposit.out", material=None, let_material=None, **kwargs):
        self.output = "deposit" if "deposit" in output else "dose"
        if "particle" in output:
            self.deposit = 1

        if material is not None:
            self.material = material

        if let_material is not None:
            self.let_material = let_material

        if units == "MeV/cm^3/source":
            self.units = 1
        if units == "MeV/source":
            self.units = 2
        if output == "dose":
            if units == "Gy/source":
                self.units = 0
            if units == "J/m^3/source":
                self.units = 5
        if output == "deposit":
            if units == "1/source":
                self.units = 3
            if units == "1/nsec/source":
                self.units = 4

    def definition(self):
        inp = super().definition()
        print(inp)
        if hasattr(self, "material"):
            inp += "material =\n"
            for mat in self.material:
                inp += f"{mat.index} "
            inp += "\n"
        if hasattr(self, "let_material"):
            inp += f"letmat = {self.let_material.index}\n"
        if hasattr(self, "deposit"):
            inp += "deposit = 1\n"
        inp += f"unit = {self.units}\n"

        return inp
