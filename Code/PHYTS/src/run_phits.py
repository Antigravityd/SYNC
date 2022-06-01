from datetime import datetime
import subprocess as sp
import networkx as nx
import itertools
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

# The first algorithm is, actually, best implemented by creating an explicit graph, as anything I can think of is formally equivalent to that. However, it may be space-inefficient, as the data is represented twice.
# sources is list of tuples of a Source value and a numeric weight, cells is a list of Cell values,
# and

def make_input(cells, sources, tallies, title=str(datetime.now()), parameters=dict(),
               data_max=[], frag_data=[], multiplier=[], mat_time_change=[]):
    objgraph = nx.DiGraph()

    def add_to_graph(an_obj):  # Recursively add subtypes to graph if they're in the list of acceptable types; ignored values are retained because the nodes /are/ the objects
        for child in map(lambda tup: tup[1] if type(tup[1]).__name__ in phits_types, an_obj.__dict__.items()): # TODO: write down phits_types
            objgraph.add_edge(an_obj, child)
            add_to_graph(child)

    map(add_to_graph, cells)
    map(add_to_graph, sources)
    map(add_to_graph, tallies)

    for
    inp = "[Title]\n"
    inp += title + '\n'

    inp += "[Parameters]\n"
    for var, val in parameters.items:
        inp += f"{var} = {val}\n"

    for var, val in map(lambda tup: tup[0].implied_params if tup[0].implied_params is not None, cells): # implied_params must be a dict of variable: value in all three cases here
        inp += f"{var} = {val}\n"
    for var, val in map(lambda tup: tup[0].implied_params if tup[0].implied_params is not None, sources):
        inp +=f"{var} = {val}\n"
    for var, val in map(lambda tup: tup[0].implied_params if tup[0].implied_params is not None, tallies):
        inp +=f"{var} = {val}\n"



    inp += "[Source]\n" ## TODO: implement sources
    if isinstance(sources, list):
        for weight, source in sources:
            inp += f"<source> = {weight}\n"
            for k, v in source.__dict__.items():
                if v is not None:
                    inp += f"{k} = {v}\n"
    else:
        for k, v in sources.__dict__.items():
            if v is not None:
                inp += f"{k} = {v}\n"
            
        
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


