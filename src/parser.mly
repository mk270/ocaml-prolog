%{

(* --- Header --- *)

        open Types;;
   
%}

/* ocamlyacc declarations */

%token <string> STRING  
%token <string> VARIABLE
%token <string> NAME
%token <float> UNSIGNEDFLOAT
%token <int> UNSIGNEDINTEGER
%token <float> SIGNEDFLOAT
%token <int> SIGNEDINTEGER
%token DOT DOUBLEDOT
%token COLONHYPHEN
%token ARROW 
%token NOT
%token TERM_EQ TERM_INEQ IS TERM_DECOMP TERM_UNIFY TERM_NOTUNIFY
       ARITH_EQ ARITH_INEQ ARITH_LESS ARITH_GREATER ARITH_GEQ
       ARITH_LEQ TERM_ORDER_EQ TERM_ORDER_INEQ TERM_ORDER_GREATER
       TERM_ORDER_LESS TERM_ORDER_GEQ TERM_ORDER_LEQ
%token DOUBLECOLON
%token PLUS MINUS
%token MULT DIV
       BITWISE_AND
       BITWISE_OR BITWISE_NOT VAR_INSTANTIATED
%token SEMICOLON COMMA COLON
%token UMINUS UPLUS
%token CUT
%token LPAREN RPAREN LBRACKET RBRACKET PIPE
%token EOF

%right SEMICOLON COMMA
%left PLUS MINUS
%left MULT DIV
%left BITWISE_AND BITWISE_OR BITWISE_NOT POWER
%left TERM_EQ 
%left TERM_UNIFY
%left TERM_INEQ 
%left TERM_NOTUNIFY
%left ARITH_EQ ARITH_INEQ ARITH_LESS ARITH_GREATER
      ARITH_LEQ ARITH_GEQ
%left TERM_ORDER_EQ TERM_ORDER_INEQ TERM_ORDER_LESS TERM_ORDER_GREATER
      TERM_ORDER_GEQ TERM_ORDER_LEQ
%left IS 
%left TERM_DECOMP
%left DOUBLECOLON
%right ARROW
%nonassoc COLONHYPHEN VAR_INSTANTIATED PIPE
%nonassoc DOT DOUBLEDOT COLON CUT

%type <Types.clause list> clause_list
%start clause_list

%type <Types.term> query
%start query

/* grammar rules */

%%

clause_list: 
    | clause DOT clause_list 
    {
        $1 :: $3 
    }
    | clause DOT EOF 
    { 
        [$1] 
    } 
;

query:
    | goal DOT 
    { 
        $1 
    }
;

clause:
    | head COLONHYPHEN body 
    { 
        Types.ClauseImplication ($1, $3)
    }
    | head
    { 
        Types.SingleClause $1 
    }
;

head:
    | goal
    {
        $1 
    }
;

/* we let only boolean operators appear in body part */

body:
    | goal SEMICOLON body 
    { 
        Types.TermBinOp (Types.TermOr, $1, $3)
    }
    | goal COMMA body 
    { 
        Types.TermBinOp (Types.TermAnd, $1, $3) 
    }
    | goal 
    { 
        $1 
    }
;

goal:
    | term
    { 
        $1 
    }
;

term:
    | term0 { $1 }
;

term0:
    | term1 { $1 }
;

term1:
    | term2 { $1 }
;

term2:
    | term2 ARROW term3 
    { 
        Types.TermIfThen ($1, $3)  
    } 
    | term2 ARROW term3 COLON term3 
    {
        Types.TermIfThenElse ($1, $3, $5)
    }
    | term3 
    { 
        $1 
    }
;

term3: 
    | term4 
    { 
        $1 
    }
;

term4:
    | NOT term5 
    { 
        Types.TermNegation $2
    }
    | term5 
    { 
        $1 
    }
;

term5:
    | term5 ARITH_EQ term5
    {
        Types.TermBinOp (Types.TermArithmeticEquality, $1, $3)
    }
    | term5 ARITH_INEQ term5
    {
        Types.TermBinOp (Types.TermArithmeticInequality, $1, $3)
    }
    | term5 TERM_UNIFY term5
    {
        Types.TermBinOp (Types.TermTermUnify, $1, $3)
    }
    | term5 TERM_NOTUNIFY term5
    {
        Types.TermBinOp (Types.TermTermNotUnify, $1, $3)
    }
    | term5 TERM_EQ term5 
    {
        Types.TermBinOp (Types.TermTermEquality, $1, $3)
    }
    | term5 TERM_INEQ term5
    {
        Types.TermTermInequality ($1, $3)
    }
    | term5 IS term5 
    {
        Types.TermBinOp (Types.TermIs, $1, $3) 
    }
    | term5 TERM_DECOMP term5
    {
        Types.TermDecomposition ($1, $3)
    } 
    | term5 ARITH_GEQ term5 
    {
        Types.TermBinOp (Types.TermArithmeticGeq, $1, $3)
    }
    | term5 ARITH_LEQ term5 
    {
        Types.TermBinOp (Types.TermArithmeticLeq, $1, $3)
    }
    | term5 ARITH_LESS term5
    {
        Types.TermBinOp (Types.TermArithmeticLess, $1, $3)
    }
    | term5 ARITH_GREATER term5
    {
        Types.TermBinOp (Types.TermArithmeticGreater, $1, $3)
    }
    | term5 TERM_ORDER_EQ term5
    {
        Types.TermTermOrderEquality ($1, $3)
    }
    | term5 TERM_ORDER_INEQ term5
    {
        Types.TermTermOrderInequality ($1, $3)
    }
    | term5 TERM_ORDER_LESS term5
    {
        Types.TermTermOrderLess ($1, $3)
    }
    | term5 TERM_ORDER_GREATER term5
    {
        Types.TermTermOrderGreater ($1, $3)
    }
    | term5 TERM_ORDER_GEQ term5
    { 
        Types.TermTermOrderGeq ($1, $3)
    }
    | term5 TERM_ORDER_LEQ term5
    {
        Types.TermTermOrderLeq ($1, $3)
    }
    | term6 
    { 
        $1 
    }
;

term6:
    | term6 DOUBLECOLON term6 
    {
        Types.TermModule ($1, $3)
    }
    | term7 { $1 }
;

term7:
    | term7 PLUS term7
    {
        Types.TermBinOp (Types.TermArithmeticPlus, $1, $3)
    }
    | term7 MINUS term7
    {
        Types.TermBinOp (Types.TermArithmeticMinus, $1, $3)
    } 
    | term8 { $1 }
;

term8:
    | term8 DIV term8
    {
        Types.TermBinOp (Types.TermArithmeticDiv, $1, $3)
    }
    | term8 MULT term8
    {
        Types.TermBinOp (Types.TermArithmeticMult, $1, $3)
    }
    | VAR_INSTANTIATED term8
    {
        Types.TermVariableInstantiated $2
    }
    | term8 BITWISE_AND term8
    {
        Types.TermBitwiseAnd ($1, $3)
    }
    | term8 BITWISE_OR term8
    {
        Types.TermBitwiseOr ($1, $3)
    }
    | term8 BITWISE_NOT term8
    {
        Types.TermBitwiseNot ($1, $3)
    }
    | term9 { $1 }
;

term9:
    | CUT 
    { 
            Types.TermCut 
    }
    | term10 
    {
        $1 
    }
;

term10:
    | list_term
    {
        $1
    }
    | LPAREN term0 RPAREN 
    { 
        $2 
    }
    | STRING 
    { 
        Types.TermString $1 
    }
    | constant 
    { 
        Types.TermConstant $1 
    }
    | VARIABLE 
    { 
        Types.TermVariable $1 
    }
    | functor_name LPAREN arguments RPAREN 
    { 
        Types.TermFunctor ($1, $3) 
    }
;

list_term:
    | LBRACKET RBRACKET 
    {
        Types.TermList (Types.EmptyList)
    }
    | LBRACKET list_body RBRACKET
    {
        Types.TermList $2
    }
;

list_body:
    | arguments
    {
        Types.NormalList $1
    }
    | arguments PIPE term
    {
        Types.DividedList ($1, $3)
    }

functor_name:
    | name 
    { 
        $1
    } 
;

arguments:
    | term0 COMMA arguments 
    { 
        ($1) :: ($3) 
    }
    | term0 
    { 
        [$1] 
    }
;

constant:
    | name 
    { 
        ConstantAtom $1 
    }
    | number 
    {
        ConstantNumber $1 
    }
;

name:
    | NAME 
    { 
        $1 
    }
;

number:
    | UNSIGNEDINTEGER
    {
        Types.Integer ($1)
    }
    | MINUS UNSIGNEDINTEGER %prec UMINUS
    { 
        Types.Integer (-$2) 
    }
    | PLUS UNSIGNEDINTEGER %prec UPLUS
    {
        Types.Integer ($2)
    }
%%  


(* --- Trailer --- *)

 

