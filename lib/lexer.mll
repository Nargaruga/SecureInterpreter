{
    open Parser

    exception SyntaxError of string


    (** Creates a hashtable of size [k] and fills it with data from [init]. *)
    let create_hashtable k init =
        let tbl = Hashtbl.create k
        in List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
        tbl

    (** The language's reserved keywords. *)
    let keywords =
        create_hashtable 12 [
            ("if", IF);
            ("then", THEN);
            ("else", ELSE);
            ("let", LET);
            ("in", IN);
            ("enclave", ENCLAVE);
            ("end", END);
            ("secret", SECRET);
            ("gateway", GATEWAY);
            ("include", INCLUDE);
            ("execute", EXECUTE);
            ("print", PRINT);
        ]
}


(* Regular expressions *)
let whitespace = [' ' '\t' '\n']+

let digit = ['0'-'9']
let int = digit+

let string = [^'\"']*
let identifier = ['_' 'a'-'z' 'A'-'Z']+

(* Lexing rules *)
rule read = 
    parse
    | whitespace { read lexbuf }

    | int as i { INT (int_of_string i) }
    | "true" { BOOL true }
    | "false" { BOOL false }
    | '"' (string as s) '"' { STR s }

    | "(" { LPAREN }
    | ")" { RPAREN }

    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { TIMES }
    | "/" { DIV }

    | "and" { AND }
    | "or" { OR }
    | "not" { NOT }

    | "==" { EQ }
    | ">" { GT }
    | ">=" { GTE }
    | "<" { LT }
    | "<=" { LTE }

    | "=" { ASSIGN }

    | identifier as word {
        try 
            Hashtbl.find keywords word 
        with Not_found ->
            ID word
     }


    | eof { EOF }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }