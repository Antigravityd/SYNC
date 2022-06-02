from datetime import datetime
import subprocess as sp
import networkx as nx
import itertools as it
import collections as col
from base.py import *
# This is complicated enough to sketch the algorithm before writing code (shocker!)
# Given a list of cells, sources, and measurements (with some wacky options thrown in), we want to generate
# all of the "background" data necessary to build those cells automatically, e.g. generate the surfaces necessary to build the cells.
# To that end, we divide the options into "cell-like," "source-like," and "tally-like," and require their specification in the initialization of the
# object of the corresponding type. This is natural: why should the surfaces I only think about in the context of one or two cells be specified separately?
# There do exist options that don't satisfy this natural classification: transforms come to mind, as they may be used in surface, source, or
# "backend" contexts.

# The user specifies a list of sources, a list of cells, and a list of tallies
# we reconstruct the input file by first going through every "PHITSable" property
# and generating a digraph of the distinct values (up to the equivalence relation of each class's __eq__), with adjacency x → y denoting
# "y needs x for its definition." This is necessary because we don't want to duplicate definitions, but we want to retain the connection
# of parent-child; we need to define two parents with __eq__ children in terms of /one/ .inp line representing the child.
# The set of values of each type are enumerated, and the graph updated to reflect each object's enumeration, representing the order in which the objects will appear in the input file.
# We can now use the child's enumeration in the definition of the parent, which is what we're after.

# In effect, we take the graph of Python object relationships, apply an algorithm that collapses __eq__ nodes into each other while retaining adjacency, (this seems like an extant problem, I just don't know its name)
# map nodes → (idx, nodes) where idx is the result of enumerating all nodes that end up in the same PHITS section, and write it all out.

# The first algorithm is, actually, best implemented by creating an explicit graph, as anything I can think of is formally equivalent to that. However, it may be space-inefficient, as the adjacency data is represented twice.
# The naïve algorithm is suboptimal: taking every node, and comparing it to every other node to construct the equivalence class explicitly is O(n^n), and doesn't use symmetry or transitivity to reduce the number of required comparisons.

# Here's a better candidate algorithm: first, initialize an empty set (since we only need lookup and insertion we can get O(1) on average; the hash values are computed via the equivalence relation in question).
# Iterate over the nodes of the graph; if the a node isn't in the set (i.e. is not equivalent to any in the set, since the hash bins are computed via __eq__), add it.
# If a node is equivalent to one in the set, change all edges that end on the node to end instead on the one to which it is equivalent.
# This is O(#nodes + connectivity)


# sources is list of tuples of a Source value and a numeric weight, cells is a list of Cell values,
# and

def make_input(cells, sources, tallies, title=str(datetime.now()), parameters=dict(),
               data_max=[], frag_data=[], multiplier=[], mat_time_change=[]):
    objgraph = nx.DiGraph()

    def add_to_graph(an_obj):  # Recursively add subtypes to graph if they represent an "entry" in one of the sections
        if isinstance(an_obj, col.Iterable):
            map(add_to_graph, an_obj)
        else:
            if isinstance(an_obj, PhitsBase):
                for name, child in an_obj.__dict__.items():
                    if isinstance(child, PhitsBase):
                        objgraph.add_edge(an_obj, child)
                        add_to_graph(child)


    map(add_to_graph, cells)
    map(lambda tup: add_to_graph(tup[1]), sources)
    map(add_to_graph, tallies)

    def adjust_subobjects(an_obj): # Recursively replace redundant subtypes with the representative that's in the graph
        if isinstance(an_obj, col.Iterable):
            map(adjust_subobjects, an_obj)
        else:
            if isinstance(an_obj, PhitsBase):
                for name, child in an_obj.__dict__.items():
                    if isinstance(child, PhitsBase):
                        representative = filter(lambda node: node == child, objgraph.successors(an_obj))
                        setattr(an_obj, child, representative)
                        adjust_subobjects(child)

    map(adjust_subobjects, cells)
    map(lambda tup: adjust_subobjects(tup[1]), sources)
    map(adjust_subobjets, tallies)

    # We now have that if any two PHITS objects A and B had a property C and D such that C == D, C /is/ D


    # Now we break up the nodes by PHITS type
    type_divided = {"parameters": {},
                    "source": {},
                    "material": {},
                    "surface": {},
                    "cell": {},
                    "transform": {},
                    "temperature": {},
                    "mat_time_change": {},
                    "magnetic_field": {},
                    "electro_magnetic_field": {},
                    "delta_ray": {},
                    "track_structure": {},
                    "super_mirror": {},
                    "elastic_option": {},
                    "importance": {},
                    "weight_window": {},
                    "ww_bias": {},
                    "forced_collisions": {},
                    "repeated_collisions": {},
                    "volume": {},
                    "multiplier": {},
                    "mat_name_color": {},
                    "reg_name": {},
                    "counter": {},
                    "timer": {}}

    for node in objgraph:
        type_divided[node.name].add(node)

    for section, entries in type_divided.items():
        for idx, value in enumerate(entries):
            value.index = idx # this allows us to access the position in which the value will appear if given value alone.

    inp = "[Title]\n"
    inp += title + '\n'


    inp += "[Parameters]\n"
    for var, val in parameters.items: # directly passed global parameters
        inp += f"{var} = {val}\n"

    for dic in type_divided["parameters"]: # parameters from object declarations
        for var, val in dic.dic.items():
            inp += f"{var} = {val}\n"


    inp += "[Source]\n" ## TODO: implement sources
    for weight, source in sources:
        inp += f"<source> = {weight}\n"
        for var, val in source.__dict__.items():
            if val is not None:
                if isinstance(val, PhitsBase):
                    inp += f"{var} = {val.index}\n"
                else:
                    inp += f"{var} = {val}\n"

    inp += "[Material]\n"
    for mat in type_divided["material"]:
        inp += f"MAT[{mat.index}]"
        for # TODO: finish after implementing Material class

    inp += "[Surface]\n"
    for sur in type_divided["surface"]:
        boundary = "*" if sur.reflective else ("+" if sur.white else "")
        inp += f"{boundary}{sur.index} {sur.transform.index} {sur.symbol} "
        for attr, val in sur.__dict__.items():
            if attr not in ["index", "name", "symb", "reflective", "white", "transform"]: # exclude "super" properties and those already handled
                inp += f"{attr} "
        inp += "\n"

    inp += "[Cell]\n"
    for cell in type_divided["cell"]:
        inp += f"{cell.index} {cell.material.index} {cell.density} "
        for sur, orient in cell.regions:
            if orient == "<": # This may not be correct; the "sense" of surfaces is poorly documented.
                inp += f"{sur.index} "
            elif orient == ">":
                inp += f"-{sur.index} "
            else:
                raise ValueError(f"Encountered incorrect orientation {i[1]} among regions.")
        if cell.volume is not None:
            inp += f"VOL={cell.volume} "
        if cell.temperature is not None:
            inp += f"TMP={cell.temperature} "
        inp += "\n"

    inp += "[Transform]\n"
    for tr in type_divided["transform"]:
        if tr.units == "radians":
            inp += f"TR{tr.index} "
        else if tr.units ==

            
        
    # TODO: "sanitize" all input so that hashing works correctly
    # for values that ought to parse equal, i.e. (1, "Pb") and "100% lead"
    # should be put into one form or the other (prefer former form in this case).
    # Also, need to verify that order of material components doesn't affect the hash,
    # by making sure lists are sets by this point. 
    
    # record all distinct values of every  parameter
    distinct = {"material": {},
                "surface": {},
                "transform": {}
                "temperature": {},
                "mag_field": {},
                "em_field": {},
                "delta_ray": {},
                "track_structure": {},
                "super_mirror": {},
                "elastic_option": {},
                "importance": {},
                "weight_window": {},
                "ww_bias": {},
                "forced_collisions": {},
                "repeated_collisions": {},
                "volume": {},
                "mat_name_color": {},
                "reg_name": {},
                "counter": {},
                "timer": {}}
    for cell in cells:
        for k, v in cell.__dict__.items():
            if k == "regions":
                for surface, orient in v:
                    distinct["surface"].add(surface)
            else:
                distinct[k].add(v)
                # TODO: if custom surface is passed, add any transforms to transform list
                # as well as transforms from tallies & source

    # give the distinct values a fixed order
    properties = {k: enumerate(v) for k, v in distinct}

    # associate with each distinct value the index in cells of all cells it corresponds to
    # or, given a property class and a value of that class, output the cell numbers
    cell_lookup = dict()
    for k, v in properties.items():
        cell_lookup[k] = {i: [j for j, e in enumerate(cells) if e.__dict__[k] == i] for i in v}

    # given a property class and a cell number, output the corresponding property number(s).
    # this should only be nonsingleton for surface.
    proplookup = dict()
    for k, v in properties.items():
        flat = [i[1] for i in properties]
        proplookup[k] = dict()
        for cellnum, cell in enumerate(cells):
            if k == "surface":
                surfaces = frozenset({i[0] for i in cell.__dict__["region"]})
                proplookup[k][cellnum] = {i[0] if i[1] == surfaces for i in properties}
            else:
                proplookup[k][cellnum] = {i[0] if i[1] = cell.__dict__[k] for i in properties}

    def tablify(propname):
        res = ""
        for i, prop in properties[propname]:
            val = cell_lookup[propname][prop]
            if len(val) > 1:
                res += f"{condense_set(val)} "
            elif len(val) == 1:
                res += f"{val[0]} "
            res += f"{prop}\n"
        return res


    inp += "[Material]\n"
    for i, mat in properties["material"]:
        inp += f"MAT[{i}] "
        for component in mat:
            inp += f"{component[0]} {component[1]} "
            inp += '\n'

    inp += "[Surface]\n"
    for i, sur in properties["surface"]:
        flat = [i[1] for i in properties["transform"]]
        trid = flat.index(sur.transform) if sur.transform != idTrasform else 0
        
        inp += f"{i} {trid} {sur.symb} {sur.def}\n"
        # TODO: implement the .def property in each surface class
        # Also, add (magic?) methods for transforming surfaces.

    inp += "[Cell]\n"
    for i, cell in enumerate(cells):
        matid = proplookup["material"][i]
        inp += f"{i} {matid} {0} " # TODO: add density cell parameter

        for region in cell.regions:
            regID = properties["surface"].index(region[0])
            regID *= 1 if region[1] == ">" else -1
            inp += f"{regID} "
            trID = proplookup["transform"][i]
            inp += f"TRCL={trID}\n"
            # TODO: Lattices & Universes... I'm not looking forward to it          
    

    inp += "[Transform]\n"
    for i, transform in properties["transform"]: # should we have degree support?
        trans = transform.trans
        rot = transform.rot
        inp += f"TR{i} {trans[0]} {trans[1]} {trans[2]} {rot[0]} {rot[1]} {rot[2]}"
        if transform.M == 2 or transform.M == -2:
            for i in range(6):
                inp += "0 "
        else:
           for i in range(6):
               inp += f"{rot[i+3]} "
               inp += f"{transform.M}\n"

    inp += "[Temperature]\n"
    inp += "reg tmp\n"
    inp += tablify("temperature")


    inp += "[Magnetic Field]\n"
    # TODO: the documentation is horrible

    inp += "[Electro Magnetic Field]\n"
    # TODO: same as above

    inp += "[Delta Ray]\n"
    inp += tablify("delta_ray")



    inp += "[Track Structure]"
    # Track structure seems poorly supported in PHITS at the moment, so I skip.

    inp += "[Super Mirror]\n"
    # TODO: Documentation is awful again

    inp += "[Elastic Option]\n"
    # More bad documentation...

    inp += "[Data Max]\n"
    # I don't understand what this is used for

    inp += "[Frag Data]\n"
    inp += "opt proj targ file\n"
    for i in frag_data:
        for j in i:
            inp += "{j} "
            inp += '\n'

    inp += "[Importance]\n" # TODO: figure out how to config part= and lattice/universe
    inp += "reg imp\n"
    inp += tablify("importance")


    # TODO: forestalling weight-window sections untill I do tallies

    inp += "[Forced Collisions]\n" # TODO: same as Importance
    inp += "reg fcl\n"
    inp += tablify("forced_collision")


    inp += "[Repeated Collisions]\n" # TODO: finish

    inp += "[Volume]\n"
    inp += "reg vol"
    inp += tablify("volume")


    return inp

    
def run_phits(sources, cells, tallies, command="phits", throws=False, title=str(datetime.now()), **kwargs):
    # TODO: consider how to read stdout/output files into returnable formats
    # WARNING: setting the command variable opens up shell injection attacks, as sp.run() with
    # shell=True is done unfiltered. Should see about using shlex.quote() to sanitize. 
    inp = make_input(sources, cells, tallies, title=title, **kwargs)
    with open(f"{title}.inp", "r+") as inp_file:
        inp_file.write(inp)

    result = sp.run(f"phits {title}.inp", shell=True, capture_output=True, text=True, check=throws)

    # TODO: parse STDOUT and STDERR of the CompletedProcess object above for the results of the tallies,
    # and return them, alongside any other useful information.

    
        
            
        
        
        
            

        

def parse_input(file_handle) -> :#< tuple of all arguments of run_phits function>


