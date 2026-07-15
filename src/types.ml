open Printf

module ANF = struct
  type iexpr = INum of int | IId of string

  type cexpr =
    | CAdd of iexpr * iexpr
    | CSub of iexpr * iexpr
    | CMul of iexpr * iexpr
    | CLesseq of iexpr * iexpr
    | CCall of string * iexpr list
    | CIExpr of iexpr

  type aexpr =
    | ALet of string * cexpr * aexpr (* let <id> = <body> in <where> *)
    | AIte of cexpr * aexpr * aexpr (* if <cond> then <then> else <else> *)
    | AFun of string * string list * aexpr (* fun <id> <args> = <body> *)
    | ACExpr of cexpr

  let str_iexpr expr =
    match expr with INum n -> sprintf "%d" n | IId id -> sprintf "%s" id

  let str_cexpr expr =
    match expr with
    | CAdd (a, b) -> sprintf "Add (%s, %s)" (str_iexpr a) (str_iexpr b)
    | CSub (a, b) -> sprintf "Sub (%s, %s)" (str_iexpr a) (str_iexpr b)
    | CMul (a, b) -> sprintf "Mul (%s, %s)" (str_iexpr a) (str_iexpr b)
    | CLesseq (a, b) -> sprintf "Lesseq (%s, %s)" (str_iexpr a) (str_iexpr b)
    | CCall (name, args) ->
        sprintf "Call (%s, %s)" name
          (String.concat " " (List.map str_iexpr args))
    | CIExpr e -> str_iexpr e

  let rec str_aexpr expr =
    match expr with
    | ALet (id, body, where) ->
        sprintf "Let (%s, %s, %s)" id (str_cexpr body) (str_aexpr where)
    | AIte (cond, th, el) ->
        sprintf "Ite (%s, %s, %s)" (str_cexpr cond) (str_aexpr th)
          (str_aexpr el)
    | AFun (name, args, body) ->
        sprintf "Fun (%s, %s, %s)" name (String.concat " " args)
          (str_aexpr body)
    | ACExpr e -> str_cexpr e

  let print expr_list =
    List.iter (fun x -> printf "%s\n" (str_aexpr x)) expr_list
end

module Assembly = struct
  type t =
    | Section of string
    | Global of string
    | Num of int
    | Id of string
    | Branch of string
    | Addi of string * string * int
    | Add of string * string * string
    | Sub of string * string * string
    | Mul of string * string * string
    | Li of string * int
    | Sd of string * int * string
    | Ld of string * int * string
    | Mv of string * string
    | J of string
    | Call of string
    | Ble of string * string * string
    | Ret
    | Ecall
    | WIP of ANF.aexpr

  let print_expr expr =
    match expr with
    | Section s -> printf ".section %s\n" s
    | Global s -> printf ".global %s\n" s
    | Branch br -> printf "%s:\n" br
    | Li (id, n) -> printf "    li %s, %d\n" id n
    | Sd (id, off, s) -> printf "    sd %s, %d(%s)\n" id off s
    | Ld (id, off, s) -> printf "    ld %s, %d(%s)\n" id off s
    | Mv (id, s) -> printf "    mv %s, %s\n" id s
    | Addi (res, a, b) -> printf "    addi %s, %s, %d\n" res a b
    | Add (res, a, b) -> printf "    add %s, %s, %s\n" res a b
    | Sub (res, a, b) -> printf "    sub %s, %s, %s\n" res a b
    | Mul (res, a, b) -> printf "    mul %s, %s, %s\n" res a b
    | J br -> printf "    j %s\n" br
    | Ble (a, b, br) -> printf "    ble %s, %s, %s\n" a b br
    | Call br -> printf "    call %s\n" br
    | Ret -> printf "    ret\n"
    | Ecall -> printf "    ecall\n"
    | WIP e ->
        printf "WIP: ";
        ANF.print [ e ]
    | _ -> printf "WIP\n"

  let print expr_list = List.iter print_expr expr_list
end
