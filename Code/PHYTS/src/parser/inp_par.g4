parser grammar inp_par;

options { tokenVocab = inp_lex; }

// We have to worry about: #, (), <, >, and : operators in the [Cell] section,
// No support for lattice-universe in Python, so no support for it here. Make the lexer rule an option for entry: for that to somewhat work.

input_file: section+? (TERMINATE | ALT_TERMINATE) ;
section: section_heading statement+ ;
section_heading: (SECTION | SURF | CELL | ALT_SECTION | ALT_SURF | ALT_CELL | RESUME_SECTION) ;

statement: (assignment | array | label) (EOL | ALT_EOL) ;
assignment: (LOOSE_IDENT | ALT_LOOSE_IDENT) EQ (value | (FILENAME | ALT_FILENAME))+ ;
entry: value | RANGE | RANGE_PLUS | ALT_TRANSFORM_NUM ;
array: entry (entry | assignment)* ;
label: (LOOSE_IDENT | ALT_LOOSE_IDENT) COLON (value | (STRICT_IDENT | ALT_STRICT_IDENT) OBRACK value CBRACK | FILENAME)+ ;

value: (STRICT_IDENT | ALT_STRICT_IDENT) | (NUMBER | ALT_NUMBER) | (INTRINSIC | ALT_INTRINSIC) OPAREN value CPAREN
  | value (ARITHMETIC | ALT_ARITHMETIC) value | OPAREN value CPAREN | DASH value ;
