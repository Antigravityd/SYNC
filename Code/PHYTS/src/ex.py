from math import *
import numpy as np
from run_phits import *
from material import *
from cell import *
from surface import *
from tally import *
from source import *


# A dead whale, modelled by concentric spherical annuli of an increasingly less-concentrated carbon-water mixture,
# has swallowed an orphaned 241Am RTG source and is sinking into the ocean.
# Find how much the water will heat, to guide recovery efforts.

# Construct objects via list comprehensions
mats = [Material([("C", float(i)/100.), ("H20", float(100-i)/100.)]) for i in range(55,100)]

# Or loops
cells = [Cell([(Sphere(1), "<")], mats[0], -1)]
for i in range(1,11):
    new = Cell([(Sphere(i+1), "<"), (Sphere(i), ">")], mats[i], -1)
    cells.append(new)

ocean = Cell([(Sphere(11), ">")],
             Material(["H2O", 1.0]),
             -1,                             # Assign objects associated with a cell /to/ the cell
             tally=Deposition(Mesh("energy", [float(i) in np.arange(0, 10, 0.25)])))

cells.append(ocean)

source = Cylindrical("241Am", fission="neutrons", bounds=(-0.25,0.25), r_out=0.3)

# Capture input for further analysis in Python as your choice of many common data formats,
# or render and return the .eps
# Pass raw input data in case of an error or unimplemented feature
table = run_phits(cells, [source], [], return_a="dataframe", raw="# This could be e.g. an [Elastic Option] section.\n")
