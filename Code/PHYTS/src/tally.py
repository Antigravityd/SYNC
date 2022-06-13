from base import *
from cell import *



class Mesh():
    def __init__(self, axis, bins=None): # All bin generation is easily done in Python via list comprehensions
        assert axis in ["energy", "time", "x", "y", "z", "radius", "angle", "let"], f"Unrecognized axis {axis} in mesh definition."
        self.axis = axis
        self.bins = bins
        if axis != "angle":
            self.type = 2
        else:
            pass  # TODO: figure out angle mesh

    def definition(self):
        inp = f"{axis[0]}-type = 1\n"
        inp += f"n{axis[0]} = {len(bins)-1}\n"
        for i in bins:
            inp += f"{i} "

        inp += "\n"
        return inp




class Tally(PhitsObject): # out-mesh is the mesh for the output; in-mesh is a list consisting of an optional origin coordinate in the first entry and a list of meshes for the dependent variables
    def __init__(self, section_title, out_mesh, in_meshes=None, dependent_var="reg", particles="all", angel="", sangel="", title="", outfile="", output_type="", type_2d=5,
                 factor=None, detailed_output=False, region_show=0, gshow=0, axis_titles=[], epsout=0, resolution=1, transform=None, dump=None, **kwargs): # dump not supported
        super().__init__(section_title, **kwargs)
        self.out_mesh = out_mesh
#        self.unit = unit TODO: set this in children
        self.in_meshes = in_meshes
        self.dependend_var = dependent_var
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
        self.axis_titles = axis_titles
        self.epsout = epsout
        self.resolution = resolution
        self.transform = transform
        self.dump = dump
        self.outfile = outfile


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

    def definition(self): # only for use in defining other definition()s; the child must set units, transform (since id is available only at runtime),
        inp = f"mesh = {self.mesh_type}\n"
        for mesh in self.in_meshes: # if mesh_type == "reg" the list is none anyway; no need to check
            inp += mesh.definition() # this one happens to not need data only available in make_input()
        inp += out_mesh.definition()

        inp += f"part = {self.particles}\n"
        inp += f"axis = {self.dependent_var}\n"
        inp += f"file = {self.outfile}\n"
        inp += f"2d-type = {self.type_2d}\n"
        inp += f"info = {self.detailed_output}\n"
        inp += f"rshow = {self.region_show}\n"
        inp += f"x-txt = {self.axis_titles[0]}\n"
        inp += f"y-txt = {self.axis_titles[1]}\n"
        inp += f"z-txt = {self.axis_titles[2]}\n"
        inp += f"resol = {self.resolution}\n"
        for param, val in {k: v for k, v in self.__dict__ if k in {"angel", "sangel", "title", "output_type", "factor", "gshow", "epsout"}}:
            inp += f"{param} = {val}\n"

class TrackLengthFluence(Tally):
    def __init__(self, out_mesh, units, in_meshes=None, dependent_var="reg", where=None, particles="all", angel="", sangel="", title="", output_type="", type_2d=3,
                 factor=1.0, detailed_output=False, region_show=0, gshow=0, axis_titles=[], epsout=0, resolution=1, transform=None, dump=None, **kwargs):

        super().__init__("t-track", out_mesh, in_meshes=in_meshes, dependend_var=dependent_var, particles=particles, angel=angel, sangel=sangel, title=title, output_type=output_type,
                         type_2d=type_2d, factor=factor, detailed_output=detailed_output, region_show=region_show, gshow=gshow, axis_titles=axis_titles, epsout=epsout,
                         resolution=resolution, transform=transform, dump=dump, **kwargs)
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
    def __init__(self, out_mesh, units, output, iangform="normal", in_meshes=None, dependent_var="reg", where=None, particles="all", angel="", sangel="", title="", output_type="", type_2d=3,
                 factor=1.0, detailed_output=False, region_show=0, gshow=0, axis_titles=[], epsout=0, resolution=1, transform=None, dump=None, **kwargs):

        super().__init__("t-cross", out_mesh, in_meshes=in_meshes, dependend_var=dependent_var, particles=particles, angel=angel, sangel=sangel, title=title, output_type=output_type,
                         type_2d=type_2d, factor=factor, detailed_output=detailed_output, region_show=region_show, gshow=gshow, axis_titles=axis_titles, epsout=epsout,
                         resolution=resolution, transform=transform, dump=dump, **kwargs)
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
        inp += f"output = {self.output}"

class PointFluence(Tally):
    def __init__(self, out_mesh, units, in_meshes=None, dependent_var="eng", where=None, particles="all", angel="", sangel="", title="", output_type="", type_2d=3,
                 factor=1.0, detailed_output=False, region_show=0, gshow=0, axis_titles=[], epsout=0, resolution=1, transform=None, dump=None, **kwargs):

        super().__init__("t-point", out_mesh, in_meshes=in_meshes, dependend_var=dependent_var, particles=particles, angel=angel, sangel=sangel, title=title, output_type=output_type,
                         type_2d=type_2d, factor=factor, detailed_output=detailed_output, region_show=region_show, gshow=gshow, axis_titles=axis_titles, epsout=epsout,
                         resolution=resolution, transform=transform, dump=dump, **kwargs)
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

class Deposition(Tally):
    def __init__(self, out_mesh, units, in_meshes=None, dependent_var="eng", where=None, particles="all", angel="", sangel="", title="", output_type="", type_2d=3,
                 factor=1.0, detailed_output=False, region_show=0, gshow=0, axis_titles=[], epsout=0, resolution=1, transform=None, dump=None, **kwargs):

        super().__init__("t-deposit", out_mesh, in_meshes=in_meshes, dependend_var=dependent_var, particles=particles, angel=angel, sangel=sangel, title=title, output_type=output_type,
                         type_2d=type_2d, factor=factor, detailed_output=detailed_output, region_show=region_show, gshow=gshow, axis_titles=axis_titles, epsout=epsout,
                         resolution=resolution, transform=transform, dump=dump, **kwargs)
