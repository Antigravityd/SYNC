from base import *
from cell import *
import collections as col






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

class TrackLengthFluence(PhitsObject):
    def __init__(self, *args, **kwargs):
        self.mesh_type = None
        if in_meshes not in kwargs:
            self.mesh_type = "reg"
            self.axis="reg"
        if isinstance(in_meshes, col.Iterable):
            if isinstance(in_meshes[0], Cell):
                self.mesh_type = "reg"
            elif next(filter(lambda mesh: mesh.axis in {"x", "y", "z"}, in_meshes), False):
                self.mesh_type = "xyz"
            elif next(filter(lambda mesh: mesh.axis in {"r", "z"}, in_meshes), False):
                self.mesh_type = "r-z"

        super().__init__("t-track", required=["out_mesh", "units"], positional=["out_mesh", "units"],
                         optional=["mesh_type", "outfile", "in_meshes", "dependent_var", "particles", "angel", "sangel", "title",
                                   "output_type", "type_2d", "factor", "detailed_output", "region_show", "geometry_show",
                                   "axis_titles", "epesout", "resolution", "transform", "dump", "width"],
                         shape=("mesh_type", "out_meshes", "in_meshes", "particles", "outfile", "units", "factor", "title",
                                "angel", "sangel", "type_2d", "gshow", "rshow", "axis_titles",  "resolution", "width",
                                "transform", "epsout"),
                         ident_map={"type_2d": "2d-type", "region_show": "rshow", "geometry_show": "gshow",
                                    "dependent_var": "axis", "outfile": "file", "particles": "part", "units": "unit",
                                    "axis_titles": ("x-txt", "y-txt", "z-txt"), "transform": "trcl", "resolution": "resol",
                                    "mesh_type": "mesh", "output_type": "output"},
                         value_map={"1/cm^2/source": 1, "1/cm^2/MeV/source": 2, "1/cm^2/Lethargy/source": 3,
                                    "cm/source": 4, "1/cm^2/nsec/source": 11, "1/cm^2/MeV/nsec/source": 12,
                                    "1/cm^2/Lethargy/nsec/source": 13, "cm/nsec/source": 14})


class SurfaceFluence(PhitsObject):
    def __init__(self, *args,  **kwargs):
        super().__init__("t-cross", required=["out_mesh", "units"], positional=["out_mesh", "units"],
                         optional=["mesh_type", "outfile", "in_meshes", "dependent_var", "particles", "angel", "sangel", "title",
                                   "output_type", "type_2d", "factor", "detailed_output", "region_show", "geometry_show",
                                   "axis_titles", "epesout", "resolution", "transform", "dump", "width"],
                         shape=("mesh_type", "out_meshes", "in_meshes", "particles", "outfile", "units", "factor", "title",
                                "angel", "sangel", "type_2d", "gshow", "rshow", "axis_titles",  "resolution", "width",
                                "transform", "epsout"),
                         ident_map={"type_2d": "2d-type", "region_show": "rshow", "geometry_show": "gshow",
                                    "dependent_var": "axis", "outfile": "file", "particles": "part", "units": "unit",
                                    "axis_titles": ("x-txt", "y-txt", "z-txt"), "transform": "trcl", "resolution": "resol",
                                    "mesh_type": "mesh", "output_type": "output"},
                         value_map={"1/cm^2/source": 1, "1/cm^2/MeV/source": 2, "1/cm^2/Lethargy/source": 3,
                                    "1/cm^2/sr/source": 4, "1/cm/MeV/sr/source": 5, "1/cm^2/Lethargy/sr/source": 6,
                                    "1/cm^2/nsec/source": 11, "1/cm^2/MeV/nsec/source": 12, "1/cm^2/Lethargy/nsec/source": 13,
                                    "1/cm^2/sr/nsec/source": 14, "1/cm^2/MeV/sr/nsec/source": 15,
                                    "1/cm^2/Lethargy/sr/nsec/source": 16, "normal": 0, "x": 1, "y": 2, "z": 3})

class PointFluence(PhitsObject):
    def __init__(self, out_mesh, units,  **kwargs):
        super().__init__("t-cross", required=["out_mesh", "units"], positional=["out_mesh", "units"],
                         optional=["mesh_type", "outfile", "in_meshes", "dependent_var", "particles", "angel", "sangel", "title",
                                   "output_type", "type_2d", "factor", "detailed_output", "region_show", "geometry_show",
                                   "axis_titles", "epesout", "resolution", "transform", "dump", "width"],
                         shape=("mesh_type", "out_meshes", "in_meshes", "particles", "outfile", "units", "factor", "title",
                                "angel", "sangel", "type_2d", "gshow", "rshow", "axis_titles",  "resolution", "width",
                                "transform", "epsout"),
                         ident_map={"type_2d": "2d-type", "region_show": "rshow", "geometry_show": "gshow",
                                    "dependent_var": "axis", "outfile": "file", "particles": "part", "units": "unit",
                                    "axis_titles": ("x-txt", "y-txt", "z-txt"), "transform": "trcl", "resolution": "resol",
                                    "mesh_type": "mesh", "output_type": "output"},
                         value_map={"1/cm^2/source": 1, "1/cm^2/MeV/source": 2, "1/cm^2/Lethargy/source": 3,
                                    "1/cm^2/sr/source": 4, "1/cm/MeV/sr/source": 5, "1/cm^2/Lethargy/sr/source": 6,
                                    "1/cm^2/nsec/source": 11, "1/cm^2/MeV/nsec/source": 12, "1/cm^2/Lethargy/nsec/source": 13,
                                    "1/cm^2/sr/nsec/source": 14, "1/cm^2/MeV/sr/nsec/source": 15,
                                    "1/cm^2/Lethargy/sr/nsec/source": 16, "normal": 0, "x": 1, "y": 2, "z": 3})
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

class Deposition(PhitsObject):
    def __init__(self, *args, **kwargs):
        super().__init__("t-deposit", required=["out_mesh", "output_type", "units", "dependent_var"],
                         positional=["out_mesh", "output_type", "units", "dependent_var"],
                         optional=["mesh_type", "outfile", "in_meshes", "particles", "material", "let_material",
                                   "angel", "sangel", "title", "deposit", "type_2d",
                                   "factor", "detailed_output", "region_show", "geometry_show","axis_titles", "epsout",
                                   "resolution", "transform", "dump", "width", "gshow", "rshow", "cell"],
                         shape=(lambda: "mesh = reg" if self.mesh_type is None else f"mesh = {self.mesh_type}",
                                lambda: f"reg = {self.cell.index}" if self.mesh_type is None else "",
                                ("out_mesh",), ("in_meshes",), "particles", "output_type", "dependent_var",
                                lambda: f"material = {len(self.material)}" if self.material is not None else "",
                                "material", "let_material", "outfile", "units", "factor", "title",
                                "angel", "sangel", "type_2d", "gshow", "rshow", "axis_titles",  "resolution", "width",
                                "transform", "epsout"),
                         ident_map={"type_2d": "2d-type", "region_show": "rshow", "geometry_show": "gshow",
                                    "dependent_var": "axis", "outfile": "file", "particles": "part", "units": "unit",
                                    "axis_titles": ("x-txt", "y-txt", "z-txt"), "transform": "trcl", "resolution": "resol",
                                    "mesh_type": "mesh", "output_type": "output", "let_material": "letmat"},
                         value_map={"Gy/source": 0, "MeV/cm^3/source": 1, "MeV/source": 2, "1/source": 3,
                                    "1/nsec/source": 4, "J/m^3/source": 5, "total": 0, "per-particle": 1},
                         nones={"outfile": "deposit.out"}, *args, **kwargs)
