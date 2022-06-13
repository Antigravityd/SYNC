lexer grammar inp_lex;

options { caseInsensitive = true; }

EQ: '=' ;
COLON: ':' ;
OBRACK: '[' ;
CBRACK: ']' ;
OPAREN: '(' ;
CPAREN: ')' ;
DASH: '-' ;


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

DISABLEDSECTION:  {getCharPositionInLine() < 5}? '[' (~ [\r\n\f\]])* ']' [ \t]* 'off' [ \t]* EOL -> skip, mode(DISABLED) ;
SECTION: {getCharPositionInLine() < 5}? '[' (~ [\r\n\f])* ']' [ \t]* EOL ;


fragment DIGIT: ('0' .. '9') ;
fragment SGN: ('+' | '-') ;
fragment EXPONENT: ('e' | 'E') SGN? DIGIT+ ;
NUMBER: DIGIT+ '.' DIGIT* EXPONENT? ;



fragment ALPHA: ('a' .. 'z') | ('A' .. 'Z') ;
fragment ALNUM: (ALPHA | DIGIT) ;
LOOSE_IDENT: (ALNUM | '<' | '>' | '-' | '(' | ')' | '[' | ']')+ ;
STRICT_IDENT: ALNUM+  ;

ALT_TRANSFORM_NUM: '*TR' DIGIT+ ;
RANGE: '{' WS* DIGIT+ WS* '-' WS* DIGIT+ WS* '}' ;
RANGE_PLUS: '(' WS* ((RANGE | DIGIT+) WS*)+')' ;

// LAT_UNIV: '(' WS* ((RANGE_PLUS | DIGIT+) WS*)+ ('<' WS* (RANGE_PLUS | DIGIT+ WS* ('[' WS* (DIGIT+ WS* ':' WS* DIGIT+ WS*)+ ']')? | 'u' WS* '=' WS* DIGIT+)+ WS* ')' ; // this sus

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


ARITHMETIC: '+' | DASH | '/' | '**' ;


mode SURFORCELL;
ALT_COMMENT: ('#' | '%' | '!' | '$') (~ [\r\n\f])* EOL -> skip ;
ALT_OLDCOMMENT: {getCharPositionInLine() < 6}? 'c ' (~ [\r\n\f])* EOL -> skip ;

ALT_SURF: {getCharPositionInLine() < 5}? '[' S U R F A C E ']' EOL -> mode(SURFORCELL) ;
ALT_CELL: {getCharPositionInLine() < 5}? '[' C E L L ']' EOL -> mode(SURFORCELL) ;
ALT_TERMINATE: ({getCharPositionInLine() < 5}? '[' E N D ']' [ \t]* EOL) | 'q:';
ALT_DISABLEDSECTION: {getCharPositionInLine() < 5}? '[' (~ [\r\n\f\]])* ']' [ \t]* 'off' [ \t]* EOL -> skip, mode(DISABLED) ;
ALT_SECTION: {getCharPositionInLine() < 5}? '[' (~ [\r\n\f])* ']' [ \t]* EOL -> mode(DEFAULT_MODE);

ALT_NUMBER: DIGIT+ '.' DIGIT* EXPONENT? ;

ALT_LOOSE_IDENT: (ALNUM | '<' | '>' | '-' | '(' | ')' | '[' | ']')+ ;
ALT_STRICT_IDENT: ALNUM+  ;

ALT_FILENAME: ('/'? (ALNUM | '.' | '-' | '_') '/'?)+ ;

ALT_WS: [ \t] -> skip ;
ALT_CONTINUATION: '\\' WS* [\r\n\f]+ -> skip ;

ALT_EOL: [\r\n\f;]+ ;

ALT_TERM_SECTION: {getCharPositionInLine() == 0}? 'qp:' -> skip, pushMode(DISABLED) ;

ALT_INTRINSIC: FLOAT | INT | ABS | EXP | LOG | LOG10 | MAX | MIN | MOD | NINT | SIGN | SQRT | ACOS | ASIN | ATAN | ATAN2 | COS | COSH | SIN | SINH | TAN | TANH;


ALT_ARITHMETIC: '+' | DASH | '/' | '**' ;


mode DISABLED;
RESUME_SECTION: {getCharPositionInLine() < 5}? '[' (~ [\r\n\f\]])* ']' [ \t]* EOL -> mode(DEFAULT_MODE);
OTHER: . -> skip ;
