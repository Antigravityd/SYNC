import enum
from parsec import *

class Color(Enum):
    red
@dataclass
class Comment():
    comment: str
    x: float
    y: float
    x_justify: str
    y_justify: str
    scale: float
    color: str
    rotate: float
    font: str

@dataclass
class MultilineComment():
    comment: str
    x: float
    y: float
    x_justify: str
    y_justify: str
    scale: float
    color: str
    rotate: float
    font: str
    line_spacing: float
    box: Box
    box_bgd_color: str
    box_frame_color: str
    box_shadow_color: str

@dataclass
class TabularComment():
    comment: str
    column_count: int
    x: float
    y: float
    x_justify: str
    y_justify: str
    scale: float
    color: str
    rotate: float
    font: str
    bgd_color: str
    frame_color: str


@dataclass
class Arrow():
    start_x: float
    start_y: float
    end_x: float
    end_y: float
    color: str
    open_angle: float
    thickness: int
    line: bool

class HollowArrow():
    start_x: float
    start_y: float
    end_x: float
    end_y: float
    line_color: str
    inside_color: str
    open_angle: float
    thickness: int
    line: bool



@dataclass
class CommentArrow():
    comment: str
    start_x: float
    start_y: float
    end_x: float
    end_y: float
    justify: str
    scale: float
    color: str
    open_angle: float
    thickness: int
    line: bool


@dataclass
class Message():
    message: str
    position: str

@dataclass
class Polygon():
    center_x: float
    center_y: float
    x_scale: float
    y_scale: float
    line_color: str
    inside_color: str
    rotate: float
    side_count: int

@dataclass
class Box():
    center_x: float
    center_y: float
    x_scale: float
    y_scale: float
    line_color: str
    inside_color: str
    shadow_color: str
    rotate: float
    box_type: str

@dataclass
class Ribbon():
    center_x: float
    center_y: float
    x_scale: float
    y_scale: float
    line_color: str
    inside_color: str
    shadow_color: str
    thickness: int

@dataclass
class Star():
    center_x: float
    center_y: float
    x_scale: float
    y_scale: float
    line_color: str
    inside_color: str
    rotate: float
    thickness: int
    point_count: int
    unevenness: float


@dataclass
class Subfigure():
    filename: str
    x: float = None
    y: float = None
    origin_x: float = None
    origin_y: float = None
    x_scale: float = None
    y_scale: float = None
    rotation: float = None


@dataclass
class Function():
    point_min: float
    point_max: float
    point_count: int
    x_formula: str
    y_formula: str


@dataclass
class Series():
    skipped: bool
    legend_entry: str
    line_params: dict
    data: list[float]
    function: str
    transform: str
    fit: tuple[str]


@dataclass
class Graph():
    title: str
    xtitle: str
    ytitle: str
    graph_type: str
    schema: list[str]
    contents: list[Series]




@dataclass
class ContourPlot():
    xmin: float
    xmax: float
    xwidth: float
    ymin: float
    ymax: float
    ywidth: float
    data: dict # maps (x, y) → height

@dataclass
class ClusterPlot():
    xmin: float
    xmax: float
    xwidth: float
    ymin: float
    ymax: float
    ywidth: float
    data: dict # maps (x, y) → height

@dataclass
class ColorClusterPlot():
    xmin: float
    xmax: float
    xwidth: float
    ymin: float
    ymax: float
    ywidth: float
    data: dict # maps (x, y) → height

@dataclass
class Bitmap():
    clip: bool
    line: bool
    clip_color: str
    clip_area_color: str
    thickness: int
    bitmap_color: str
    path_color: str
    width: float
    height:float
    frame:
    clip: list[tuple[float, float]]
    path: list[tuple[float, float]]
    bitmap: list[tuple[float, float, int, int, int]] # x, y, color_component1, color_component2, color_component3

@dataclass
class Page():
    parameters: dict
    constants: dict
    graphs: list[Graph]
    contours: list[ContourPlot]
    clusters: list[ClusterPlot]
    color_clusters: list[ColorClusterPlot]
    bitmaps: list[Bitmap]
    subfigures: list[Subfigure]
    comments: list[Comment]
    multiline_comments: list[MultilineComment]
    tabular_comments: list[TabularComment]
    comment_arrows: list[CommentArrow]
    messages: list[Message]
    arrows: list[Arrow]
    bold_arrows: list[BoldArrow]
    polygons: list[Polygon]
    boxes: list[Boxes]
    ribbons: list[Ribbon]
    stars: list[Stars]
    positions: list[tuple[float, float]]

# The input definitely needs line-continuations reversed and INFL: declarations expanded, and possibly comments and blank
# lines stripped, and input after QP: trimmed.
def parse_angel_input(fd): # I very much wish this was Haskell, as making parsers for the above ADTs would be trivial...
    @generate
    def inps():
        yield EOL >> blanks >> regex("INPS:", re.CASEINSENSITIVE)
        filename = yield blanks + string("{") + blanks >> many1(letter() | digit() | oneof("-_.")) << blanks + string("}")
        x = yield optional(blanks + regex("X(", re.CASEINSENSITIVE) + blanks >> number << blanks + string(")"))
        y = yield optional(blanks + regex("Y(", re.CASEINSENSITIVE) + blanks >> number << blanks + string(")"))
        ix = yield optional(blanks + regex("IX(", re.CASEINSENSITIVE) + blanks >> number << blanks + string(")"))
        iy = yield optional(blanks + regex("IY(", re.CASEINSENSITIVE) + blanks >> number << blanks + string(")"))
        s = yield optional(blanks + regex("S(", re.CASEINSENSITIVE) + blanks >> number << blanks + string(")"))
        sx = yield optional(blanks + regex("SX(", re.CASEINSENSITIVE) + blanks >> number << blanks + string(")"))
        sy = yield optional(blanks + regex("SY(", re.CASEINSENSITIVE) + blanks >> number << blanks + string(")"))
        a = yield optional(blanks + regex("A(", re.CASEINSENSITIVE) + blanks >> number << blanks + string(")"))

        return Subfigure(filename=filename, x=x, y=y, origin_x=ix, origin_y=iy, x_scale=(sx if sx else s),
                         y_scale=(sy if sy else s), rotation=a)

    @generate
    def constant():
        name = yield many1(letter() | digit()) << string("[") + blanks
        val = yield number << blanks + string("]")
        return (name, val)

    @generate
    def set():
        yield EOL + blanks >> regex("SET:", re.CASEINSENSITIVE) << blanks
        consts = yield sepBy(constant, blanks)
        yield blanks
        return {k: v for k, v in consts}

    @generate
    def x():
        yield EOL + blanks >> regex("X:", re.CASEINSENSITIVE) << blanks
        title = yield many1(none_of("\n"))
        return title.rstrip()

    @generate
    def y():
        yield EOL + blanks >> regex("Y:", re.CASEINSENSITIVE) << blanks
        title = yield many1(none_of("\n"))
        return title.rstrip()

    @generate
    def col_function():
        formula = yield string("=[") + any1(none_of("]")) + string("]")
        # TODO: processing to turn formula into a function
        return formula

    @generate
    def transform():
        spec_num = optional(string("(")) >> number << optional(string(")"))
        power = yield optional(string("**") >> spec_num)
        mult = yield optional(one_of("*/") + spec_num)
        add = yield optional(one_of("+-") + spec_num)

        if mult != "" and mult[0] == "/":
            mult = str(1 / float(mult[1:]))
        elif mult != "" and mult[0] == "*":
            mult = mult[1:]

        # TODO: processing to turn formula into a function
        return (power, mult, add)


    @generate
    def line_params():
        yield string(",")
        # These may be freely orderable, which means parsing doesn't work. You'd just check if a substring is in line_params
        line_type = yield optional(one_of("NLMDUPQVInlmdupqvi") | string("II") | string("ii"))
        line_thickness = yield regex("Z{,3}", re.CASEINSENSITIVE) | regex("T{,3}", re.CASEINSENSITIVE)
        symbol_size = yield regex("X{,3}", re.CASEINSENSITIVE) | regex("A{,3}", re.CASEINSENSITIVE)
        symbol = yield optional(many(digit()))
        spline = yield optional(one_of("Ss") + optional(string("[") >> many(digit()) << string("]")))
        symbol_inside_color = yield optional(string("(") >> many(letter()) << string(")"))
        histogram = yield regex("H{,3}", re.CASEINSENSITIVE)
        color = yield regex("[WOKJFE]", re.CASEINSENSITIVE) | regex("[RGB]{,3}", re.CASEINSENSITIVE) \
            | one_of("Cc") + string("[") + one_of("Hh")

    @generate
    def legend_title():
        legend_title = yield string("(") >> many1(none_of("()")) << string(")")
        return legend_title


    @generate
    def fit():
        col_param = yield string("=F{") >> many(none_of("}")) << string("}")
        fn = yield string("[") >> many(none_of("]")) << string("]")
        return (col_param, fn)



    @generate
    def normal_h():
        graph_title = yield optional(title)
        xtitle = yield optional(x())
        ytitle = yield optional(y())

        yield EOL + blanks >> regex("H:", re.CASEINSENSITIVE) << blanks
        indvar = regex("X", re.CASEINSENSITIVE) + optional(col_function)
        depvar = regex("Y", re.CASEINSENSITIVE) + optional(digit()) + optional(col_function | fit \
                                                                       | transform + optional(legend_title) + line_params)
        inderr = regex("DX") + optional(col_function)
        deperr = regex("D", re.CASEINSENSITIVE) + optional(digit()) + optional(one_of("+-")) + optional(col_function | transform)
        skip = regex("N", re.CASEINSENSITIVE) + optional(indvar | depvar | inderr | deperr)
        column = indvar | depvar | inderr | deperr | skip
        schema = yield sepBy(column, blanks)
        values = list(zip(*(yield many1(sepBy(number, string("\n")))))) # list of column lists

        contents = []
        idx = 0
        for col in schema:
            if not parse(skip, col):
                idx += 1
                contents.append(Series(internal_name=col,
                                       skipped=(True if parse(skip, col)),
                                       legend_entry=parse(legend_entry, col),
                                       line_params=parse(line_params, col),
                                       function=parse(col_function, col),
                                       transform=parse(transform, col),
                                       fit=parse(fit, col),
                                       data=values[i]))
        return contents

    @generate
    def self_running_h():
        yield EOL + blanks >> regex("H:", re.CASEINSENSITIVE) << blanks
        value = letter() | digits() | one_of("*/+-() \t")
        start = yield string("V+[") >> many1(value) << string(",")
        end = yield value << string(",")
        count = yield blanks >> many1(digit()) << blanks + string("]")
        x = yield blanks + string("X=[") >> many1(value) << string("]")
        y = yield blanks + string("Y=[") >> many1(value) << string("]")
        params = yield line_params








    @generate
    def multigraph():
        pass

    blanks = many(one_of(" \t"))
    EOL = regex("\n")

    def control(name):
        return regex(f"^\\s*{name}:.*$", flags=re.CASEINSENSITIVE)

    comment = EOL >> string("#") >> many(any()) < EOL
    title = EOL >> many(blanks) >> string("'") + many(none_of("'")) + string("'") < EOL


    sign = oneof("+-")
    decimal = string(".") + many(digit())
    exponent = oneof("Ee") + optional(sign) + many1(digit())
    number = optional(sign) + many1(digit()) + optional(decimal) + optional(exponent)

    data = EOL >> many1(blanks >> number << blanks) < EOL

    skipped = control("C") | control("N") | comment
    newpage_control = control("NEWPAGE") | control("QP") | control("SKIPPAGE")
    end_control = control("Q") | eof()

    INPS =

    other_control = control("INPS") | control("SET") | control("X") | control("Y") | control("Z") | control("H") \
        | control("H2") | control("HD") | control("HC") | control("HB") | control("W") | control("AW") | control("MSUL") \
        | control("MSUC") | control("MSUR") | control("MSDL") | control("MSDC") | control("MSDR") | control("A") \
        | control("AB") | control("POLG") | control("BOX") | control("RIBN") | control("STAR") | control("P") | title


    non_ended = other_control + many(data | not_control)
    ended = control("WT") | control("WTAB") + many(not_control) + control("E")


    page = optional(newpage_control) >> many1(ended | not_ended) < newpage_control | end_control

    infile = many(page)



    pages = sepBy1()
    pages = []
    title = ""
    parameters = dict()
    constants = dict()
    graphs = []
    contours = []
    clusters = []
    color_clusters = []
    bitmaps = []
    subfigures = []
    comments = []
    multiline_comments = []
    tabular_comments = []
    comment_arrows = []
    messages = []
    arrows = []
    bold_arrows = []
    polygons = []
    boxes = []
    ribbons = []
    stars = []
    positions = []

    thisobj = dict()

    sections=["^'.", "^INPS:", "^SET:", "^X:", "^Y:", "^NEWPAGE:", "^C:", "^N:", "^Q:", "^QP:", "^SKIPPAGE:", "^Z:", "^H:",
              "^H2:", "^HD:", "^HC:", "^HB:", "^W:", "^WT:", "^WTAB:", "^E:", "^AW:", "^MSUL:", "^MSUC:", "^MSURL", "^MSUR:",
              "^MSDL:", "^MSDC:", "^MSDR:", "^A:", "^AB:", "^POLG:", "^BOX:", "^RIBN:", "^STAR:", "^P:"]
    current_section = ""
    for line in fd:
        for re in sections:
            if re.match(re, line, flags=re.CASEINSENSITIVE):
                current_section = re[1:-1]
        if current_section == "'":
            title = str(re.search())
        elif current_section == "INPS":
