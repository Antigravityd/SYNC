from parsec import *
from dataclasses import dataclass
import re

def collapse(iterable, base_type=None, levels=None):
    # from more_itertools
    def walk(node, level):
        if (
            ((levels is not None) and (level > levels))
            or isinstance(node, (str, bytes))
            or ((base_type is not None) and isinstance(node, base_type))
        ):
            yield node
            return

        try:
            tree = iter(node)
        except TypeError:
            yield node
            return
        else:
            for child in tree:
                yield from walk(child, level + 1)

    yield from walk(iterable, 0)

def match_index(it, f, default=-1):
    return next((i for i, e in enumerate(it) if f(e)), default)

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
    boxname: str
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
    name: str
    skipped: bool
    legend_entry: str
    line_params: dict
    data: list[tuple[float]]
    transform: str
    fit: tuple[str]


@dataclass
class Histogram():
    title: str
    xtitle: str
    ytitle: str
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
    clip: list[tuple[float, float]]
    path: list[tuple[float, float]]
    bitmap: list[tuple[float, float, int, int, int]] # x, y, color_component1, color_component2, color_component3


blanks = many(one_of(" \t"))
EOL = optional(regex("\n"))

def control(name):
    return regex(f"^\\s*{name}:.*$", flags=re.IGNORECASE)

comment = EOL >> string("#") >> many(none_of("\n")) < EOL

@generate
def number():
    sign = one_of("+-")
    decimal = string(".") + many(digit())
    exponent = one_of("Ee") + optional(sign) + many1(digit())
    number = yield optional(sign) + many1(digit()) + optional(decimal) + optional(exponent)

    chars = collapse(number)
    return float("".join([i for i in chars if i is not None]))

data = many1(EOL >> many1(blanks >> number << blanks) < EOL)

skipped = control("C") | control("N") | comment
newpage_control = control("NEWPAGE") | control("QP") | control("SKIPPAGE")
end_control = control("Q") | eof()

title = EOL + blanks + string("'") >> many(none_of("'")) << string("'")

@generate
def inps():
    yield EOL + blanks + regex("INPS:", re.IGNORECASE)
    filename = yield blanks + string("{") + blanks >> many1(letter() | digit() | one_of("-_.")) << blanks + string("}")
    x = yield optional(blanks + regex("X\(", re.IGNORECASE) + blanks >> number << blanks + string(")"))
    y = yield optional(blanks + regex("Y\(", re.IGNORECASE) + blanks >> number << blanks + string(")"))
    ix = yield optional(blanks + regex("IX\(", re.IGNORECASE) + blanks >> number << blanks + string(")"))
    iy = yield optional(blanks + regex("IY\(", re.IGNORECASE) + blanks >> number << blanks + string(")"))
    s = yield optional(blanks + regex("S\(", re.IGNORECASE) + blanks >> number << blanks + string(")"))
    sx = yield optional(blanks + regex("SX\(", re.IGNORECASE) + blanks >> number << blanks + string(")"))
    sy = yield optional(blanks + regex("SY\(", re.IGNORECASE) + blanks >> number << blanks + string(")"))
    a = yield optional(blanks + regex("A\(", re.IGNORECASE) + blanks >> number << blanks + string(")"))

    return Subfigure(filename="".join(filename), x=x, y=y, origin_x=ix, origin_y=iy, x_scale=(sx if sx else s),
                     y_scale=(sy if sy else s), rotation=a)


@generate
def constant():
    name = yield many1(letter() | digit()) << string("[") + blanks
    value = many1(blanks + (number | one_of("+-*/()") + blanks)) # TODO: intrinsics
    val = yield value << blanks + string("]")

    return ("".join(name), list(collapse(val)))

@generate
def set_stmt():
    yield EOL + blanks >> regex("SET:", re.IGNORECASE) << blanks
    consts = yield sepBy(constant, blanks)
    yield blanks
    return {k: v for k, v in consts}

@generate
def x():
    yield EOL + blanks >> regex("X:", re.IGNORECASE) << blanks
    title = yield many1(none_of("\n"))

    return "".join(title).rstrip()

@generate
def y():
    yield EOL + blanks >> regex("Y:", re.IGNORECASE) << blanks
    title = yield many1(none_of("\n"))
    return "".join(title).rstrip()

@generate
def col_function():
    formula = yield string("=[") >> many1(none_of("]")) << string("]")
    # TODO: processing to turn formula into a function
    return formula

@generate
def transform():
    spec_num = optional(string("(")) >> number << optional(string(")"))
    power = yield optional(string("**") >> spec_num)
    mult = yield optional(one_of("*/") + spec_num)
    add = yield optional(one_of("+-") + spec_num)

    if mult and mult[0] == "/":
        mult = 1 / mult[1]
    elif mult and mult[0] == "*":
        mult = mult[1]

    if add and add[0] == "-":
        add = -1 * add[1]
    elif add and add[0] == "+":
        add = add[1]

    if power or mult or add:
        power = lambda num: num ** power if power else num
        mult = lambda num: num * mult if mult else num
        add = lambda num: num + add if add else num
        return lambda num: add(mult(power(num)))
    else:
        return None


@generate
def line_params():
    yield string(",")
    # These may be freely orderable, which means parsing doesn't work. You'd just check if a substring is in line_params
    line_type = yield optional(one_of("NLMDUPQVInlmdupqvi") | string("II") | string("ii"))
    line_thickness = yield regex("Z{,3}", re.IGNORECASE) | regex("T{,3}", re.IGNORECASE)
    symbol = yield optional(many(digit()))
    symbol_size = yield regex("X{,3}", re.IGNORECASE) | regex("A{,3}", re.IGNORECASE)
    spline = yield optional(one_of("Ss") + optional(string("[") >> many(digit()) << string("]")))
    # symbol_inside_color = yield optional(string("(") >> many(letter()) << string(")"))
    histogram = yield regex("H{,3}", re.IGNORECASE)
    color = yield regex("[WOKJFE]", re.IGNORECASE) | regex("[RGB]{,3}", re.IGNORECASE) \
        | one_of("Cc") + string("[") + one_of("Hh")

@generate
def legend_title():
    legend_title = yield string("(") >> many1(none_of("()")) << string(")")
    return "".join(legend_title)


@generate
def fit():
    col_param = yield string("=F{") >> many(none_of("}")) << string("}")
    fn = yield string("[") >> many(none_of("]")) << string("]")
    return (col_param, fn)


@generate
def normal_h():
    graph_title = yield optional(title)
    xtitle = yield optional(x)
    ytitle = yield optional(y)

    yield EOL + blanks >> regex("H:", re.IGNORECASE) << blanks
    indvar = regex("X", re.IGNORECASE) + optional(col_function)

    depvar = regex("Y", re.IGNORECASE) + optional(digit()) + optional(digit()) + optional(col_function | transform) + optional(fit) \
        + optional(legend_title) + optional(line_params)
    inderr = regex("DX") + optional(col_function)
    deperr = regex("D", re.IGNORECASE) + optional(digit()) + optional(one_of("+-")) + optional(col_function | transform)
    skip = regex("N", re.IGNORECASE) + optional(indvar | depvar | inderr | deperr)
    column = indvar | depvar | inderr | deperr | skip
    schema = yield sepBy(column, blanks)
    schema = list(map(lambda tup: list(collapse(tup)), schema))
    dat = yield data
    values = list(zip(*(dat))) # list of column lists


    contents = []
    idx = 0
    xidx = match_index(schema, lambda it: re.match(it[0], "X", re.IGNORECASE))

    for col in schema:
        if col[0] == "Y":
            contents.append(Series(name="".join([i for i in col[0:3] if i]),
                                   skipped=False,
                                   transform=col[3],
                                   fit=col[4],
                                   line_params=col[5],
                                   legend_entry=col[6],
                                   data=zip(values[xidx], values[idx])))
        elif col[0] == "D":
            contents.append(Series(name="".join([i for i in col[0:3] if i]),
                                   skipped=False,
                                   transform=col[3],
                                   fit=None,
                                   line_params=None,
                                   legend_entry=None,
                                   data=zip(values[xidx], values[idx])))

        idx += 1


    return Histogram(title="".join(graph_title), xtitle=xtitle, ytitle=ytitle, contents=contents)

@generate
def self_running_h():
    yield EOL + blanks >> regex("H:", re.IGNORECASE) << blanks
    value = letter() | digits() | one_of("*/+-() \t")
    start = yield string("V+[") >> many1(value) << string(",")
    end = yield value << string(",")
    count = yield blanks >> many1(digit()) << blanks + string("]")
    x = yield blanks + string("X=[") >> many1(value) << string("]")
    y = yield blanks + string("Y=[") >> many1(value) << string("]")
    params = yield line_params

@generate
def header():
    which = yield regex("Y", re.IGNORECASE) | regex("X", re.IGNORECASE)
    start = yield blanks + string("=") + blanks >> number
    end = yield blanks + regex("TO", re.IGNORECASE) + blanks >> number
    sep = yield blanks + regex("BY", re.IGNORECASE) + blanks >> number << blanks + string(";") + many(EOL)
    return (which, start, end, sep)

@generate
def contour():
    yield EOL + blanks + regex("H2:", re.IGNORECASE) + blanks


    first = yield header
    second = yield header
    the_data = yield data


    if first[0] in "Xx":
        return ContourPlot(xmin=first[1], xmax=first[2], xwidth=first[3], ymin=second[1], ymax=second[2], ywidth=second[3],
                           data=the_data)
    elif first[0] in "Yy":
        return ContourPlot(xmin=second[1], xmax=second[2], xwidth=second[3], ymin=first[1], ymax=first[2], ywidth=first[3],
                           data=data)

@generate
def cluster():
    yield EOL + blanks + regex("HD:", re.IGNORECASE) + blanks

    first = yield header
    second = yield header
    the_data = yield data


    if first[0] in "Xx":
        return ClusterPlot(xmin=first[1], xmax=first[2], xwidth=first[3], ymin=second[1], ymax=second[2], ywidth=second[3],
                               data=the_data)
    elif first[0] in "Yy":
        return ClusterPlot(xmin=second[1], xmax=second[2], xwidth=second[3], ymin=first[1], ymax=first[2], ywidth=first[3],
                           data=the_data)

@generate
def color_cluster():
    yield EOL + blanks + regex("HC:", re.IGNORECASE) + blanks

    first = yield header
    second = yield header
    the_data = yield data

    if first[0] in "Xx":
        return ColorClusterPlot(xmin=first[1], xmax=first[2], xwidth=first[3], ymin=second[1], ymax=second[2], ywidth=second[3],
                                data=the_data)
    elif first[0] in "Yy":
        return ColorClusterPlot(xmin=second[1], xmax=second[2], xwidth=second[3], ymin=first[1], ymax=first[2], ywidth=first[3],
                                data=the_data)

@generate
def bitmap():
    pass

@generate
def comment():
    text = yield EOL + blanks + regex("W:", re.IGNORECASE) >> many(none_of("/")) << string("/")
    x = yield blanks + regex("X(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y = yield blanks + regex("Y(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    x_justify = yield blanks + regex("IX(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y_justify = yield blanks + regex("IY(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    factor = yield blanks + regex("S(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    color = yield blanks + regex("C(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    rotate = yield blanks + regex("A(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    font = yield blanks + regex("F(", re.IGNORECASE) + blanks >> number << blanks + string(")")

    return Comment(comment=text, x=x, y=y, x_justify=x_justify, y_justify=y_justify, scale=factor, color=color, rotate=rotate, font=font)

@generate
def multiline_comment():
    yield EOL + blanks + regex("WT:", re.IGNORECASE)
    x = yield blanks + regex("X(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y = yield blanks + regex("Y(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    x_justify = yield blanks + regex("IX(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y_justify = yield blanks + regex("IY(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    factor = yield blanks + regex("S(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    color = yield blanks + regex("C(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    rotate = yield blanks + regex("A(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    font = yield blanks + regex("F(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    line_spacing = yield blanks + regex("B(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    boxname = yield blanks + regex("BOX(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    box_background = yield blanks + regex("CB(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    box_frame = yield blanks + regex("CL(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    box_shadow = yield blanks + regex("CS(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")

    not_end = EOL + regex("(?!E:\n)", re.IGNORECASE)
    text = yield many(not_end) << regex("E:", re.IGNORECASE)

    return MultilineComment(comment=text, x=x, y=y, x_justify=x_justify, y_justify=y_justify, scale=factor, color=color,
                            rotate=rotate, font=font, line_spacing=line_spacing, boxname=boxname,
                            box_bgd_color=box_background, box_frame_color=box_frame, box_shadow_color=box_shadow)

@generate
def tabular_comment():
    yield EOL + blanks + regex("WTAB:", re.IGNORECASE)
    tab = yield blanks + regex("TAB{", re.IGNORECASE) + blanks >> number << blanks + string("}")
    x = yield blanks + regex("X(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y = yield blanks + regex("Y(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    x_justify = yield blanks + regex("IX(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y_justify = yield blanks + regex("IY(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    factor = yield blanks + regex("S(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    color = yield blanks + regex("C(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    rotate = yield blanks + regex("A(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    font = yield blanks + regex("F(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    box_background = yield blanks + regex("CB(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    box_frame = yield blanks + regex("CL(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")

    not_end = EOL + regex("(?!E:\n)", re.IGNORECASE)
    text = yield many(not_end) << regex("E:", re.IGNORECASE)

    return TabularComment(comment=text, column_count=tab, x=x, y=y, x_justify=x_justify, y_justify=y_justify,
                          scale=factor, color=color, rotate=rotate, font=font, bgd_color=box_background,
                          frame_color=box_frame)

@generate
def comment_arrow():
    text = yield EOL + blanks + regex("AW:", re.IGNORECASE) >> many(none_of("/")) << string("/")
    x = yield blanks + regex("X(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y = yield blanks + regex("Y(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    end_x = yield blanks + regex("AX(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    end_y = yield blanks + regex("AY(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    position = yield blanks >> regex("IR", re.IGNORECASE) | regex("IL", re.IGNORECASE)
    factor = yield blanks + regex("S(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    color = yield blanks + regex("C(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    rotate = yield blanks + regex("A(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    thickness = yield blanks >> many1(regex("T", re.IGNORECASE) | regex("Z", re.IGNORECASE)) << blanks

    return CommentArrow(comment=text, start_x=x, start_y=y, end_x=end_x, end_y=end_y, justify=position,
                        scale=float(factor), color=color, open_angle=rotate,
                        thickness=len(thickness)*(-1 if thickness[0] in "Zz" else 1))

@generate
def message():
    pos = yield EOL + blanks >> regex("MS[UD][RCL]:", re.IGNORECASE)
    text = yield many(none_of("\n")) < EOL

    return Message(message=text, position=pos)

@generate
def arrow():
    yield EOL + blanks + regex("A:", re.IGNORECASE)
    x = yield blanks + regex("X(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y = yield blanks + regex("Y(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    end_x = yield blanks + regex("AX(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    end_y = yield blanks + regex("AY(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    color = yield blanks + regex("C(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    rotate = yield blanks + regex("A(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    draw = blanks >> optional(one_of("Nn"))
    thickness = yield blanks >> many(regex("T", re.IGNORECASE) | regex("Z", re.IGNORECASE)) << blanks

    return Arrow(start_x=x, start_y=y, end_x=end_x, end_y=end_y, color=color, open_angle=rotate,
                 thickness=len(thickness)*(-1 if thickness[0] in "Zz" else 1), line=bool(draw))

@generate
def hollow_arrow():
    yield EOL + blanks + regex("AB:", re.IGNORECASE)
    x = yield blanks + regex("X(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y = yield blanks + regex("Y(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    end_x = yield blanks + regex("AX(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    end_y = yield blanks + regex("AY(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    color = yield blanks + regex("C(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    inside_color = yield blanks + regex("CB(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    rotate = yield blanks + regex("A(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    draw = blanks >> optional(one_of("Nn"))
    thickness = yield blanks >> many(regex("T", re.IGNORECASE) | regex("Z", re.IGNORECASE)) << blanks

    return HollowArrow(start_x=x, start_y=y, end_x=end_x, end_y=end_y, line_color=color, inside_color=inside_color,
                       open_angle=rotate, thickness=len(thickness)*(-1 if thickness[0] in "Zz" else 1), line=bool(draw))

@generate
def polygon():
    yield EOL + blanks + regex("POLG:", re.IGNORECASE)
    x = yield blanks + regex("X(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y = yield blanks + regex("Y(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    factor = yield blanks + regex("S(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    x_factor = yield blanks + regex("SX(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y_factor = yield blanks + regex("SY(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    line_color = yield blanks + regex("CL(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    inside_color = yield blanks + regex("CB(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    rotate = yield blanks + regex("A(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    sides = yield blanks + regex("PL(", re.IGNORECASE) + blanks >> number << blanks + string(")")

    if factor:
        x_factor = y_factor = factor
    return Polygon(center_x=float(x), center_y=float(y), x_scale=float(x_factor), y_scale=float(y_factor),
                   line_color=line_color, inside_color=inside_color, rotate=float(rotate), side_count=int(sides))

@generate
def box():
    yield EOL + blanks + regex("BOX:", re.IGNORECASE)
    x = yield blanks + regex("X(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y = yield blanks + regex("Y(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    factor = yield blanks + regex("S(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    x_factor = yield blanks + regex("SX(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y_factor = yield blanks + regex("SY(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    line_color = yield blanks + regex("CL(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    inside_color = yield blanks + regex("CB(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    shadow_color = yield blanks + regex("CS(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    rotate = yield blanks + regex("A(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    typ = yield blanks + regex("BOX(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")

    if factor:
        x_factor = y_factor = factor
    return Box(center_x=float(x), center_y=float(y), x_scale=float(x_factor), y_scale=float(y_factor),line_color=line_color,
               inside_color=inside_color, shadow_color=shadow_color, rotate=float(rotate), box_type=typ)


@generate
def ribbon():
    yield EOL + blanks + regex("RIBN:", re.IGNORECASE)
    x = yield blanks + regex("X(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y = yield blanks + regex("Y(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    factor = yield blanks + regex("S(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    x_factor = yield blanks + regex("SX(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y_factor = yield blanks + regex("SY(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    line_color = yield blanks + regex("CL(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    inside_color = yield blanks + regex("CB(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    shadow_color = yield blanks + regex("CS(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    rotate = yield blanks + regex("A(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    thickness = yield blanks >> many(regex("T", re.IGNORECASE) | regex("Z", re.IGNORECASE)) << blanks

    if factor:
        x_factor = y_factor = factor
    return Ribbon(center_x=float(x), center_y=float(y), x_scale=float(x_factor), y_scale=float(y_factor),
                  line_color=line_color, inside_color=inside_color, shadow_color=shadow_color, rotate=float(rotate),
                  thickness=len(thickness)*(-1 if thickness[0] in "Zz" else 1))


@generate
def star():
    yield EOL + blanks + regex("STAR:", re.IGNORECASE)
    x = yield blanks + regex("X(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y = yield blanks + regex("Y(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    factor = yield blanks + regex("S(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    x_factor = yield blanks + regex("SX(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    y_factor = yield blanks + regex("SY(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    line_color = yield blanks + regex("CL(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    inside_color = yield blanks + regex("CB(", re.IGNORECASE) + blanks >> many(none_of(")")) << blanks + string(")")
    rotate = yield blanks + regex("A(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    thickness = yield blanks >> many(regex("T", re.IGNORECASE) | regex("Z", re.IGNORECASE)) << blanks
    sides = yield blanks + regex("PL(", re.IGNORECASE) + blanks >> number << blanks + string(")")
    unevenness = yield blanks + regex("V(", re.IGNORECASE) + blanks >> number << blanks + string(")")


    if factor:
        x_factor = y_factor = factor
    return Star(center_x=float(x), center_y=float(y), x_scale=float(x_factor), y_scale=float(y_factor),
                line_color=line_color, inside_color=inside_color, rotate=float(rotate),
                thickness=len(thickness)*(-1 if thickness[0] in "Zz" else 1), point_count=int(sides),
                unevenness=int(unevenness))


@generate
def variable():
    var = yield times(letter(), 4, 4)
    val = yield string("(") + blanks >> many(none_of(")")) << string(")")
    return (var, val)

parameters = EOL + blanks + regex("P:", re.IGNORECASE) >> many(blanks >> variable)








@generate
def page():
    pass



@generate
def multigraph():
    pass


# The input definitely needs line-continuations reversed and INFL: declarations expanded, and possibly comments and blank
# lines stripped, and input after QP: trimmed.
def parse_angel_input(fd): # I very much wish this was Haskell, as making parsers for the above ADTs would be trivial...


    blanks = many(one_of(" \t"))
    EOL = regex("\n")

    def control(name):
        return regex(f"^\\s*{name}:.*$", flags=re.IGNORECASE)

    comment = EOL >> string("#") >> many(none_of("\n")) < EOL


    sign = one_of("+-")
    decimal = string(".") + many(digit())
    exponent = one_of("Ee") + optional(sign) + many1(digit())
    number = optional(sign) + many1(digit()) + optional(decimal) + optional(exponent)

    data = EOL >> many1(blanks >> number << blanks) < EOL

    skipped = control("C") | control("N") | comment
    newpage_control = control("NEWPAGE") | control("QP") | control("SKIPPAGE")
    end_control = control("Q") | eof()


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
            if re.match(re, line, flags=re.IGNORECASE):
                current_section = re[1:-1]
        if current_section == "'":
            title = str(re.search())
        elif current_section == "INPS":
            pass
