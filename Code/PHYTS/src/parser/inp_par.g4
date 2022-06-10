parser grammar inp_par;

options { tokenVocab = PHITSL; }

// We have to worry about: #, (), <, >, and : operators in the [Cell] section,
// No support for lattice-universe in Python, so no support for it here. Make the lexer rule an option for entry: for that to somewhat work.

input_file: section+ TERMINATE ;
section: section_heading statement+ ;
section_heading: (SECTION | SURF | CELL) ;

statement: (assignment | array | label) EOL ;
assignment: IDENT_LOOSE '=' (value | FILENAME)+ ;
entry: value | RANGE | RANGE_PLUS | ALT_TRANSFORM_NUM ;
array: entry (entry | assignment)* ;
label: IDENT_LOOSE ':' (value | IDENT '[' value ']' | FILENAME)+ ;

value: IDENT_STRICT | NUMBER | INTRINSIC '(' value ')' | value ARITHMETIC value | '(' value ')' | '-' value ;
