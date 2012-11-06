(* header section *)
{
        open Parser;;

        (* predefined operators list.  
         * semantics at: 
         * http://www.trinc-prolog.com/doc/pl_pred.htm
         * http://www.amzi.com/manuals/amzi7/pro/ref_math.htm
         *)   

        let keywords = Hashtbl.create 32;;
   
        let _ = List.iter (fun (kwd, tok) -> Hashtbl.add keywords kwd tok)
            [("not",    NOT);                   (* boolean negation *)
             ("=:=",    ARITH_EQ);              (* arithmetical equality *)
             ("=\\=",   ARITH_INEQ);            (* arithmetical inequality *)
             ("->",     ARROW);                 (* if then [else] *)
             ("\\=",    TERM_NOTUNIFY);         (* terms do not unify *)
             ("=..",    TERM_DECOMP);           (* term composition/decomposition *)
             ("==",     TERM_EQ);               (* term equality *)
             ("@=<",    TERM_ORDER_LEQ);        (* term less or equal to (order of terms) *)
             ("@>=",    TERM_ORDER_GEQ);        (* term greater or equal to (order of terms) *)
             ("@=",     TERM_ORDER_EQ);         (* term equality (order of terms) *)
             ("@\\=",   TERM_ORDER_INEQ);       (* term inequality (order of terms) *)
             ("@<",     TERM_ORDER_LESS);       (* term less than (order of terms) *)
             ("@>",     TERM_ORDER_GREATER);    (* term greater than (order of terms) *)
             (">=",     ARITH_GEQ);             (* arithmetical greater or equal to *)
             ("<=",     ARITH_LEQ);             (* arithmetical less or equal to *)
             ("is",     IS);                    (* variable instantiation *)
             ("::",     DOUBLECOLON);           (* module(database) specifier *)
             ("\\/",    BITWISE_AND);           (* bitwise and *)
             ("/\\",    BITWISE_OR);            (* bitwise or *)
             ("\\",     BITWISE_NOT);           (* bitwise not *)
             ("^",      VAR_INSTANTIATED);      (* is variable instantiated? *)
             ("+",      PLUS);                  (* arithmetical plus *)
             ("-",      MINUS);                 (* arithmetical minus *)
             ("*",      MULT);                  (* arithmetical multiplication *)
             ("/",      DIV);                   (* arithmetical division *)
             ("(",      LPAREN);                (* left parenthesis *)
             (")",      RPAREN);                (* right parenthesis *)
             (":",      COLON);                 (* else *)
             (",",      COMMA);                 (* logical and *)        
             (";",      SEMICOLON);             (* logical or *)
             ("=",      TERM_UNIFY);            (* unify terms *)
             ("<",      ARITH_LESS);            (* arithmetical less than *)
             (">",      ARITH_GREATER);         (* arithmetical greater than *)
             ("!",      CUT);                   (* cut operator *)
             (":-",     COLONHYPHEN);           (* logical implication *)
             ("[",      LBRACKET);              (* left bracket for lists *)
             ("]",      RBRACKET);              (* right bracket for lists *)
             ("|",      PIPE)]                  (* head-tail delimiter for lists *)
      ;;
        
}

(* definitions section *)

let capital = ['A'-'Z']         (* capital letters *)
let small = ['a'-'z']           (* small letters *)
let digit = ['0'-'9']           (* digits *)
let underline = ['_']           (* underline character *)

let alpha = capital | small | digit | underline          (* any alphanumeric character*)

let word = small alpha*                                  (* prolog words *)
let quoted_name = '\'' [^ '\''] '\''                     (* quoted names *)
let symbol = ['+' '-' '*' '/' '\\' '^' '<' '>' '=' '~' ':' '?' '@' '#' '$' '&'] 
let solo_char = ['!' ';' '.' '[' ']' '(' ')' ',' '|']               

let name = quoted_name | word | symbol+ | solo_char      (* valid prolog names *)

let variable = (capital | underline) alpha*              (* prolog variables *)

let nstring = '"' [^ '"']* '"'                           (* prolog strings *)

let sign = '+' | '-'                                     (* signs *)
let exp = ('e' | 'E') sign? digit+                       (* optional exponent *)
let simple_integer = digit+                              (* simplest integer *)
let unsigned_integer = simple_integer exp?               (* integers with no sign *)

let whitespace = [' ' '\t' '\n']

rule token = parse
        | eof
        {       EOF       }

        | whitespace 
        {       token lexbuf    }

        | ".."
        {       
                DOUBLEDOT       
        }

        | "."   
        {      
                DOT     }

        | '%' 
        {       single_line_comment lexbuf    }

        | "/*"
        {       multiline_comment 0 lexbuf    }

        | name as id          
        {       
                try
                    Hashtbl.find keywords id
                with
                    | Not_found -> NAME (id) 
        }

        | unsigned_integer 
        {       
                UNSIGNEDINTEGER (int_of_string (Lexing.lexeme lexbuf))    
        }
        | nstring              
        {
                STRING (Lexing.lexeme lexbuf)        
        }
        
        | variable               
        {
                VARIABLE (Lexing.lexeme lexbuf)
        }

and single_line_comment = parse 
        | "\n" 
        {       token lexbuf    }
       
        | eof
        {       EOF       }

        |   _ 
        {       single_line_comment lexbuf       }

and multiline_comment level = parse
        | "*/"
        {       if level = 0 
                    then token lexbuf
                    else multiline_comment (level - 1) lexbuf    
        }
       
        | "/*"
        {       multiline_comment (level + 1) lexbuf    }

        | eof
        {       failwith "Unclosed comment!";           }

        |  _    
        {       multiline_comment level lexbuf          }

(* trailer section *)
{
}

