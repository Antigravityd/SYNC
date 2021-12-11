import numpy as np
from datetime import datetime
from collections import namedtuple


Region = namedtuple("Region", "surface orientation")
    
class Cell: # dictionary of properties and a set of named tuples with type Region <=> (Surface, "<" | ">")
            # with the second entry being
            # what the equality in the surface function ought to be replaced with to get the orientation.
            # As a heuristic rule, consider r -> ∞ and check the sign of the LHS; choose accordingly.

            # Operations &, |, and ~ are the set operations on the regions.
            # The property dictionary has equivalent natural language and array syntaxes,
            # e.g. {material: "60% water, 40% lead 208"} ↔ {material: [(0.6, "H20"), (0.4, "Pb-206")]}
            # Every property that applies to a cell ought to be supported, like transform, tallies, etc.
    def __init__(self, regions, properties):
        self.regions = set(regions)
        self.properties = properties


        def __or__(a, b, properties="join"): # Union of cells

            return Cell(a.regions | b.regions)

    def __inv__(a): # Set complement of cell
        new = []
        for i in a.regions:
            assert len(i) == 2, f"Encountered incorrectly-sized entry {i} among regions."
            if i.orientation == "<":
                other = ">"
            elif i.orientation == ">":
                other = "<"
            else:
                raise ValueError(f"Encountered incorrect entry {i[1]} among regions.")

            new.append((i.surface, other))
        return Cell(set(new))

    def __and__(a, b): # Intersection of cells
        return Cell(a.regions & b.regions)


def run_phits(sources, cells, title=str(datetime.now()), parameters=dict(),
              mag_field=[], emag_field=[], delta_ray=[], data_max=[],
              frag_data=[], multiplier=[], global_tallies=[]):
    with open(f"{title}.inp", "r+") as inp:
        inp.write("[Title]\n")
        inp.write(title + '\n')

        inp.write("[Parameters]\n")
        for i in parameters:
            inp.write(f"{i} = {parameters[i]}\n")

        inp.write("[Source]\n") # TODO: implement sources

        # construct all cell-wise section bodies in one pass over the list of cells
        optional_sections = {"transform": [],
                    "temperature": [],
                    "mat_time_change": [],
                    "track_structure": [],
                    "super_mirror": [],
                    "elastic_option": [],
                    "importance": [],
                    "weight_window": [],
                    "ww_bias": [],
                    "forced_collisions": [],
                    "repeated_collisions": [],
                    "volume": [],
                    "mat_name_color": [],
                    "reg_name": [],
                    "counter": [],
                    "timer": []
                    }
        for cell in cells:
            for section in optional_sections:
                if section in cell[3]:



def parse_input(file_handle) -> :#< tuple of all arguments of run_phits function>


