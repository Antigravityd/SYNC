/** Nearly verbatim from https://mlochbaum.github.io/BQN/spec/grammar.html */
grammar BQNHDL;

tokens {C, M, F, S, Cl, Ml, Fl, Sl};

program : ⋄?  ( stmt ⋄ )* stmt ⋄? ;
stmt : expr | export | nothing ;
⋄ : ( "⋄" | "," | "\n" )+ ;
expr : subExpr | funcExpr | m1Expr | m2Expr ;
export : lhs_elt? "⇐" ;

any : atom | func | mod1 | mod2 ;
mod2 : ( atom "." )? C | Cl | "(" m1Expr ")"   | brMod2 ;
mod1 : ( atom "." )? M | Ml | "(" m2Expr ")"   | brMod1 ;
func : ( atom "." )? F | Fl | "(" funcExpr ")" | brFunc ;
atom : ( atom "." )? S | Sl | "(" subExpr ")"  | brSub  | list ;
list : "⟨" ⋄? ( ( expr ⋄ )* expr ⋄? )? "⟩" ;
subject : atom | any ( "‿" any )+ ;

asgn : "←" | "⇐" | "↩" ;
m2Expr : mod2 | C asgn m2Expr ;
m1Expr : mod1 | mod2 ( subject | func ) | operand mod2 | M asgn m1Expr ;

derv : func | operand mod1 | operand mod2 ( subject | func ) | return ;
return : ( name | "𝕊" | "𝕣" ) "→" ;
operand : subject | derv ;
fork : derv | operand derv fork | nothing derv fork ;
train : fork | derv fork ;
funcExpr : train | F asgn funcExpr ;

arg : subExpr | ( subject | nothing )? derv arg ;
nothing : "·" | ( subject | nothing )? derv nothing ;
name : S | F | M | C ;
lhs_any : name | lhsList ;
lhs_atom : lhs_any | "(" lhsStr ")" ;
lhs_elt : lhs_any | lhsStr ;
lhs_entry : lhs_elt | lhs "⇐" name ;
lhsStr : lhs_atom ( "‿" lhs_atom )+ ;
lhsList : "⟨" ⋄? ( ( lhs_entry ⋄ )* lhs_entry ⋄? )? "⟩" ;
lhs : S | lhsList | lhsStr ;
subExpr : arg | lhs asgn subExpr | lhs derv "↩" subExpr ;

headW : subject | "𝕨" ;
headX : subject | "𝕩" ;
headF : F | "𝕗" | "𝔽" ;
headG : F | "𝕘" | "𝔾" ;
mod1H1 : headF ( M | "_𝕣" ) ;
mod2H1 : headF ( C | "_𝕣_" ) headG ;
funcHead : headW? ( F | "𝕊" ) headX | Sl | "(" subExpr ")" | brSub | list | any ( "‿" any )+ | undoHead ;
m1Head : headW? mod1H1 headX ;
m2head :  headW? mod2H1 headX ;

undoHead : headW? ( f | "𝕊" ) "⁼" headX | headW ( f | "𝕊" ) "˜" "⁼" headX | ( f | "𝕊" ) "˜"? "⁼" ;

body : program ;
fCase : ⋄? funcHead ":" body ;
mCase : ⋄? m1Head ":" body ;
cCase : ⋄? m2Head ":" body ;
fMain : ( ⋄? ( F | "𝕊" ) ":" )? body ;
mMain : ( ⋄? ( M | "_𝕣"  | mod1H1 ) ":" )? body ;
cMain : ( ⋄? ( C | "_𝕣_" | mod2H1 ) ":" )? body ;
brSub : "{" ( ⋄? S ":" )? body "}" ;
brFunc : "{" ( fCase ";" )* ( fCase | fMain ( ";" fMain )? ) "}" ;
brMod1 : "{" ( mCase ";" )* ( mCase | mMain ( ";" mMain )? ) "}" ;
brMod2 : "{" ( cCase ";" )* ( cCase | cMain ( ";" cMain )? ) "}" ;
