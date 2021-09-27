/** Nearly verbatim from https://mlochbaum.github.io/BQN/spec/grammar.html */
grammar BQNHDL;

tokens {s, f, m, c};

program : ⋄?  ( stmt ⋄ )* stmt ⋄? ;
stmt : expr | export | nothing ;
⋄ : ( "⋄" | "," | "\n" )+ ;
expr : subExpr | funcExpr | m1Expr | m2Expr ;
export : lhs_elt? "⇐" ;

any : atom | func | mod1 | mod2 ;
mod2 : ( atom "." )? c | cl | "(" m1Expr ")"   | brMod2 ;
mod1 : ( atom "." )? m | ml | "(" m2Expr ")"   | brMod1 ;
func : ( atom "." )? f | fl | "(" funcExpr ")" | brFunc ;
atom : ( atom "." )? s | sl | "(" subExpr ")"  | brSub  | list ;
list : "⟨" ⋄? ( ( expr ⋄ )* expr ⋄? )? "⟩" ;
subject : atom | any ( "‿" any )+ ;

asgn : "←" | "⇐" | "↩" ;
m2Expr : mod2 | c asgn m2Expr ;
m1Expr : mod1 | mod2 ( subject | func ) | operand mod2 | m asgn m1Expr ;

derv : func | operand mod1 | operand mod2 ( subject | func ) | return ;
return : ( name | "𝕊" | "𝕣" ) "→" ;
operand : subject | derv ;
fork : derv | operand derv fork | nothing derv fork ;
train : fork | derv fork ;
funcExpr : train | f asgn funcExpr ;

arg : subExpr | ( subject | nothing )? derv arg ;
nothing : "·" | ( subject | nothing )? derv nothing ;
name : s | f | m | c ;
lhs_any : name | lhsList ;
lhs_atom : lhs_any | "(" lhsStr ")" ;
lhs_elt : lhs_any | lhsStr ;
lhs_entry : lhs_elt | lhs "⇐" name ;
lhsStr : lhs_atom ( "‿" lhs_atom )+ ;
lhsList : "⟨" ⋄? ( ( lhs_entry ⋄ )* lhs_entry ⋄? )? "⟩" ;
lhs : s | lhsList | lhsStr ;
subExpr : arg | lhs asgn subExpr | lhs derv "↩" subExpr ;

headW : subject | "𝕨" ;
headX : subject | "𝕩" ;
headF : f | "𝕗" | "𝔽" ;
headG : f | "𝕘" | "𝔾" ;
mod1H1 : headF ( m | "_𝕣" ) ;
mod2H1 : headF ( c | "_𝕣_" ) headG ;
funcHead : headW? ( F | "𝕊" ) headX | sl | "(" subExpr ")" | brSub | list | any ( "‿" any )+ | undoHead ;
m1Head : headW? mod1H1 headX ;
m2head :  headW? mod2H1 headX ;

undoHead : headW? ( f | "𝕊" ) "⁼" headX | headW ( f | "𝕊" ) "˜" "⁼" headX | ( f | "𝕊" ) "˜"? "⁼" ;

body : program ;
fCase : ⋄? funcHead ":" body ;
mCase : ⋄? m1Head ":" body ;
cCase : ⋄? m2Head ":" body ;
fMain : ( ⋄? ( f | "𝕊" ) ":" )? body ;
mMain : ( ⋄? ( m | "_𝕣"  | mod1H1 ) ":" )? body ;
cMain : ( ⋄? ( c | "_𝕣_" | mod2H1 ) ":" )? body ;
brSub : "{" ( ⋄? s ":" )? body "}" ;
brFunc : "{" ( fCase ";" )* ( fCase | fMain ( ";" fMain )? ) "}" ;
brMod1 : "{" ( mCase ";" )* ( mCase | mMain ( ";" mMain )? ) "}" ;
brMod2 : "{" ( cCase ";" )* ( cCase | cMain ( ";" cMain )? ) "}" ;
