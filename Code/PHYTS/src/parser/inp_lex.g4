lexer grammar PHITSL;

options = { caseInsensitive = true; }


COMMENT: ('#' | '%' | '!' | '$') (~ [\r\n\f])* EOL -> skip ;
OLDCOMMENT: {getCharPositionInLine() < 6}? 'c ' (~ [\r\n\f])* EOL -> skip ;
TERM_SECTION: {getCharPositionInLine() == 0}? 'qp:' -> skip, pushMode(DISABLED) ;
TERMINATE: ({getCharPositionInLine() < 5}? '[' E N D ']' [ \t]* EOL) | 'q:';

/** Using modes to turn off comment characters in the sections wherein they aren't accepted */
fragment S: 's' ;
fragment U: 'u' ;
fragment R: 'r' ;
fragment F: 'f' ;
fragment A: 'a' ;
fragment C: 'c' ;
fragment E: 'e' ;
fragment L: 'l' ;
fragment N: 'n' ;
fragment D: 'd' ;

SURF: {getCharPositionInLine() < 5}? '[' S U R F A C E ']' EOL -> mode(SURFORCELL) ;
CELL: {getCharPositionInLine() < 5}? '[' C E L L ']' EOL -> mode(SURFORCELL) ;

DISABLEDSECTION: | {getCharPositionInLine() < 5}? '[' (~ [\r\n\f]])* ']' [ \t]* 'off' [ \t]* EOL -> skip, mode(DISABLED) ;
SECTION: {getCharPositionInLine() < 5}? '[' (~ [\r\n\f])* ']' [ \t]* EOL ;


fragment DIGIT: ('0' .. '9') ;
fragment SIGN: ('+' | '-') ;
fragment EXPONENT: ('e' | 'E') SIGN? NUM+ ;
NUMBER: DIGIT+ '.' DIGIT* EXPONENT? ;



fragment ALPHA: ('a' .. 'z') | ('A' .. 'Z') ;
fragment ALNUM: (ALPHA | DIGIT) ;
LOOSE_IDENT: (ALNUM | '<' | '>' | '-' | '(' | ')' | '[' | ']')+ ;
STRICT_IDENT: ALNUM+  ;

ALT_TRANSFORM_NUM: '*TR' DIGIT+ ;
RANGE: '{' WS* DIGIT+ WS* '-' WS* DIGIT+ WS* '}' ;
RANGE_PLUS: '(' WS* ((RANGE | DIGIT+) WS*)+')' ;

LAT_UNIV: '(' WS* ((RANGE_PLUS | DIGIT+) WS*)+ ('<' WS* (RANGE_PLUS | DIGIT+ WS* ('[' WS* (DIGIT+ WS* ':' WS* DIGIT+ WS*)+ ']')? | 'u' WS* '=' WS* DIGIT+)+ WS* ')' ; // this sus

FILENAME: ('/'? (ALNUM | '.' | '-' | '_') '/'?)+ ;



WS: [ \t] -> skip ;
CONTINUATION: '\\' WS* [\r\n\f]+ -> skip ;

EOL: [\r\n\f;]+ ;



INTRINSIC: FLOAT | INT | ABS | EXP | LOG | LOG10 | MAX | MIN | MOD | NINT | SIGN | SQRT | ACOS | ASIN | ATAN | ATAN2 | COS | COSH | SIN | SINH | TAN | TANH;

FLOAT: 'float' ;
INT: 'int' ;
ABS: 'abs' ;
EXP: 'exp' ;
LOG: 'log' ;
LOG10: 'log10' ;
MAX: 'max' ;
MIN: 'min' ;
MOD: 'mod' ;
NINT: 'nint' ;
SIGN: 'sign' ;
SQRT: 'sqrt' ;
ACOS: 'acos' ;
ASIN: 'asin' ;
ATAN: 'atan' ;
ATAN2: 'atan2' ;
COS: 'cos' ;
COSH: 'cosh' ;
SIN: 'sin' ;
SINH: 'sinh' ;
TAN: 'tan' ;
TANH: 'tanh' ;


ARITHMETIC: '+' | '-' | '/' | '**' ;


mode SURFORCELL;
COMMENT: ('#' | '%' | '!' | '$') (~ [\r\n\f])* EOL -> skip ;
OLDCOMMENT: {getCharPositionInLine() < 6}? 'c ' (~ [\r\n\f])* EOL -> skip ;

/** Using modes to turn off comment characters in the sections wherein they aren't accepted */
fragment S: 's' ;
fragment U: 'u' ;
fragment R: 'r' ;
fragment F: 'f' ;
fragment A: 'a' ;
fragment C: 'c' ;
fragment E: 'e' ;
fragment L: 'l' ;
fragment N: 'n' ;
fragment D: 'd' ;

SURF: {getCharPositionInLine() < 5}? '[' S U R F A C E ']' EOL -> mode(SURFORCELL) ;
CELL: {getCharPositionInLine() < 5}? '[' C E L L ']' EOL -> mode(SURFORCELL) ;
TERMINATE: ({getCharPositionInLine() < 5}? '[' E N D ']' [ \t]* EOL) | 'q:';
DISABLEDSECTION: {getCharPositionInLine() < 5}? '[' (~ [\r\n\f]])* ']' [ \t]* 'off' [ \t]* EOL -> skip, mode(DISABLED) ;
SECTION: {getCharPositionInLine() < 5}? '[' (~ [\r\n\f])* ']' [ \t]* EOL -> mode(DEFAULT);


fragment DIGIT: ('0' .. '9') ;
fragment SIGN: ('+' | '-') ;
fragment EXPONENT: ('e' | 'E') SIGN? NUM+ ;
NUMBER: DIGIT+ '.' DIGIT* EXPONENT? ;



fragment ALPHA: ('a' .. 'z') | ('A' .. 'Z') ;
fragment ALNUM: (ALPHA | DIGIT) ;
LOOSE_IDENT: (ALNUM | '<' | '>' | '-' | '(' | ')' | '[' | ']')+ ;
STRICT_IDENT: ALNUM+  ;

ALT_TRANSFORM_NUM: '*TR' DIGIT+ ;
RANGE: '{' WS* DIGIT+ WS* '-' WS* DIGIT+ WS* '}' ;
RANGE_PLUS: '(' WS* ((RANGE | DIGIT+) WS*)+')' ;

LAT_UNIV: '(' WS* ((RANGE_PLUS | DIGIT+) WS*)+ ('<' WS* (RANGE_PLUS | DIGIT+ WS* ('[' WS* (DIGIT+ WS* ':' WS* DIGIT+ WS*)+ ']')? | 'u' WS* '=' WS* DIGIT+)+ WS* ')' ; // this sus

FILENAME: ('/'? (ALNUM | '.' | '-' | '_') '/'?)+ ;



WS: [ \t] -> skip ;
CONTINUATION: '\\' WS* [\r\n\f]+ -> skip ;

EOL: [\r\n\f;]+ ;

TERM_SECTION: {getCharPositionInLine() == 0}? 'qp:' -> skip, pushMode(DISABLED) ;

INTRINSIC: FLOAT | INT | ABS | EXP | LOG | LOG10 | MAX | MIN | MOD | NINT | SIGN | SQRT | ACOS | ASIN | ATAN | ATAN2 | COS | COSH | SIN | SINH | TAN | TANH;

FLOAT: 'float' ;
INT: 'int' ;
ABS: 'abs' ;
EXP: 'exp' ;
LOG: 'log' ;
LOG10: 'log10' ;
MAX: 'max' ;
MIN: 'min' ;
MOD: 'mod' ;
NINT: 'nint' ;
SIGN: 'sign' ;
SQRT: 'sqrt' ;
ACOS: 'acos' ;
ASIN: 'asin' ;
ATAN: 'atan' ;
ATAN2: 'atan2' ;
COS: 'cos' ;
COSH: 'cosh' ;
SIN: 'sin' ;
SINH: 'sinh' ;
TAN: 'tan' ;
TANH: 'tanh' ;


ARITHMETIC: '+' | '-' | '/' | '**' ;


mode DISABLED;
EOL: [\r\n\f;]+ ;
SECTION: {getCharPositionInLine() < 5}? '[' (~ [\r\n\f]])* ']' [ \t]* EOL -> mode(DEFAULT);
OTHER: .* -> skip ;
