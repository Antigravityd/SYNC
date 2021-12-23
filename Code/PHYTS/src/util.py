import parsec as pc

mat_identifiers =  {"ac", "actinium", "ag", "silver", "al", "aluminum", "am", "americium", "ar", "argon", "as", "arsenic", "at", "astatine", "au", "gold", "b", "boron", "ba", "barium", "be", "beryllium", "bh", "bohrium", "bi", "bismuth", "bk", "berkelium", "br", "bromine", "c", "carbon", "ca", "calcium", "cd", "cadmium", "ce", "cerium", "cf", "californium", "cl", "chlorine", "cm", "curium", "cn", "copernicium", "co", "cobalt", "cr", "chromium", "cs", "cesium", "cu", "copper", "db", "dubnium", "ds", "darmstadtium", "dy", "dysprosium", "er", "erbium", "es", "einsteinium", "eu", "europium", "f", "fluorine", "fe", "iron", "fl", "flerovium", "fm", "fermium", "fr", "francium", "ga", "gallium", "gd", "gadolinium", "ge", "germanium", "h", "hydrogen", "he", "helium", "hf", "hafnium", "hg", "mercury", "ho", "holmium", "hs", "hassium", "i", "iodine", "in", "indium", "ir", "iridium", "k", "potassium", "kr", "krypton", "la", "lanthanum", "li", "lithium", "lr", "lawrencium", "lu", "lutetium", "lv", "livermorium", "mc", "moscovium", "md", "mendelevium", "mg", "magnesium", "mn", "manganese", "mo", "molybdenum", "mt", "meitnerium", "n", "nitrogen", "na", "sodium", "nb", "niobium", "nd", "neodymium", "ne", "neon", "nh", "nihonium", "ni", "nickel", "no", "nobelium", "np", "neptunium", "o", "oxygen", "og", "oganesson", "os", "osmium", "p", "phosphorus", "pa", "protactinium", "pb", "lead", "pd", "palladium", "pm", "promethium", "po", "polonium", "pr", "praseodymium", "pt", "platinum", "pu", "plutonium", "ra", "radium", "rb", "rubidium", "re", "rhenium", "rf", "rutherfordium", "rg", "roentgenium", "rh", "rhodium", "rn", "radon", "ru", "ruthenium", "s", "sulfur", "sb", "antimony", "sc", "scandium", "se", "selenium", "sg", "seaborgium", "si", "silicon", "sm", "samarium", "sn", "tin", "sr", "strontium", "ta", "tantalum", "tb", "terbium", "tc", "technetium", "te", "tellurium", "th", "thorium", "ti", "titanium", "tl", "thallium", "tm", "thulium", "ts", "tennnessine", "u", "uranium", "v", "vanadium", "w", "tungsten", "xe", "xenon", "y", "yttrium", "yb", "ytterbium", "zn", "zinc", "zr", "zirconium"}
 
def parse_mat_long(s, index):
    
def condense_set(ints):
    ints = list(set(ints)).sort()

    result = "( "
    globbing = 0
    for i, e in enumerate(ints):
        if i == 0:
            break
        if e - ints[i-1] > 1:
            if globbing:
                result += f"\{ {globbing} - {ints[i-1]} \} {e} "
            else:
                result += e + ' '
        else:
            globbing = e

    result += ')'

    return result
    
    
    
    
