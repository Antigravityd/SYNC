/** Nearly verbatim from https://mlochbaum.github.io/BQN/spec/grammar.html */
grammar BQNHDL;

/** lexer rules */
NUMBER : COMPONENT ( ( 'i' | 'I' ) COMPONENT )? ;
fragment COMPONENT : '¯'? ( '∞' | MANTISSA ( ( 'e' | 'E' ) EXPONENT )? ) ;
fragment EXPONENT : '¯'? DIGIT+ ;
fragment MANTISSA : 'π' | DIGIT+ ( '.' DIGIT+ )? ;
fragment DIGIT : [0-9] ;

CHARACTER : '\'' . '\'' | '•' CHARACTER ; /** Need to see if '#' char and "asdf # asdf" string are parsed correctly. Also see if this string parse works to escape '"'. */
STRING : '\"' ( '\"\"' | . )*? '\"' | '•' STRING ;

Sl : NUMBER | CHARACTER | STRING | '•' Sl;
Fl : '+' | '-' | '×' | '÷' | '⋆' | '√' | '⌊' | '⌈' | '|' | '¬' | '∧' | '∨'
   | '<' | '>' | '≠' | '=' | '≤' | '≥' | '≡' | '≢' | '⊣' | '⊢' | '⥊' | '∾'
   | '≍' | '↑' | '↓' | '↕' | '«' | '»' | '⌽' | '⍉' | '/' | '⍋' | '⍒' | '⊏'
   | '⊑' | '⊐' | '⊒' | '∊' | '⍷' | '⊔' | '!' | '•' Fl ;
Ml : '˙' | '˜' | '˘' | '¨' | '⌜' | '⁼' | '´' | '˝' | '`' | '•' Ml ;
Cl : '∘' | '○' | '⊸' | '⟜' | '⌾' | '⊘' | '◶' | '⎉' | '⚇' | '⍟' | '⎊' | '•' Cl;

S : [a-z] ALPHANUMERIC* | '•' S ;
F : [A-Z] ALPHANUMERIC* | '•' F ;
M : '_' ALPHABETIC ALPHANUMERIC* | '•' M ; /** Don't know how this and subsequent line interact... how is "_2_modifier_with_snake_case_" lexed?  "_1_modifier_with_snake_case"? */
C : '_' ALPHABETIC ALPHANUMERIC* '_' | '•' C ;

fragment ALPHANUMERIC : ALPHABETIC | NUMERIC ;
fragment ALPHABETIC : '_' | [a-zA-Z] ;
fragment NUMERIC : '¯' | '∞' | 'π' | [0-9] | '.' [0-9] ;

WS : [ \t] -> skip ;
COMMENT :  '#' .*? '\n' -> skip ;



/** parser rules */
program : sep?  ( stmt sep )* stmt sep? ;
stmt : expr | export | nothing ;
sep : ( '⋄' | ',' | '\n' )+ ;
expr : subExpr | funcExpr | m1Expr | m2Expr ;
export : lhs_elt? '⇐' ;

anything : atom | func | mod1 | mod2 ;
mod2 : ( atom '.' )? C | Cl | '(' m1Expr ')'   | brMod2 ;
mod1 : ( atom '.' )? M | Ml | '(' m2Expr ')'   | brMod1 ;
func : ( atom '.' )? F | Fl | '(' funcExpr ')' | brFunc ;
atom : atom '.' S | S | Sl | '(' subExpr ')'  | brSub  | lis ;
lis : '⟨' sep? ( ( expr sep )* expr sep? )? '⟩' ;
subject : atom | anything ( '‿' anything )+ ;

asgn : '←' | '⇐' | '↩' ;
m2Expr : mod2 | C asgn m2Expr ;
m1Expr : mod1 | mod2 ( subject | func ) | operand mod2 | M asgn m1Expr ;

derv : func | subject mod1 | derv mod1 | subject mod2 ( subject | func )
            | derv mod2 ( subject | func ) | ret;
ret : ( name | '𝕊' | '𝕣' ) '→' ;
operand : subject | derv ;
fork : derv | operand derv fork | nothing derv fork ;
train : fork | derv fork ;
funcExpr : train | F asgn funcExpr ;

arg : subExpr | ( subject | nothing )? derv arg ;
nothing : '·' | derv nothing | subject derv nothing | nothing derv nothing;
name : S | F | M | C ;
lhs_any : name | lhsList ;
lhs_atom : lhs_any | '(' lhsStr ')' ;
lhs_elt : lhs_any | lhsStr ;
lhs_entry : lhs_elt | lhs '⇐' name ;
lhsStr : lhs_atom ( '‿' lhs_atom )+ ;
lhsList : '⟨' sep? ( ( lhs_entry sep )* lhs_entry sep? )? '⟩' ;
lhs : S | lhsList | lhsStr ;
subExpr : ( subject | nothing )? derv arg | lhs asgn subExpr | lhs derv '↩' subExpr ;

headW : subject | '𝕨' ;
headX : subject | '𝕩' ;
headF : F | '𝕗' | '𝔽' ;
headG : F | '𝕘' | '𝔾' ;
mod1H1 : headF ( M | '_𝕣' ) ;
mod2H1 : headF ( C | '_𝕣_' ) headG ;
funcHead : headW? ( F | '𝕊' ) headX | Sl | '(' subExpr ')' | brSub | lis | anything ( '‿' anything )+ | undoHead ;
m1Head : headW? mod1H1 headX ;
m2Head :  headW? mod2H1 headX ;

undoHead : headW? ( F | '𝕊' ) '⁼' headX | headW ( F | '𝕊' ) '˜' '⁼' headX | ( F | '𝕊' ) '˜'? '⁼' ;

body : program ;
fCase : sep? funcHead ':' body ;
mCase : sep? m1Head ':' body ;
cCase : sep? m2Head ':' body ;
fMain : ( sep? ( F | '𝕊' ) ':' )? body ;
mMain : ( sep? ( M | '_𝕣'  | mod1H1 ) ':' )? body ;
cMain : ( sep? ( C | '_𝕣_' | mod2H1 ) ':' )? body ;
brSub : '{' ( sep? S ':' )? body '}' ;
brFunc : '{' ( fCase ';' )* ( fCase | fMain ( ';' fMain )? ) '}' ;
brMod1 : '{' ( mCase ';' )* ( mCase | mMain ( ';' mMain )? ) '}' ;
brMod2 : '{' ( cCase ';' )* ( cCase | cMain ( ';' cMain )? ) '}' ;

// need to do special name handling garbage
