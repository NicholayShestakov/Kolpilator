open Printf

module Tokens = struct
  type token =
    | Num of int
    | Id of string
    | Add
    | Sub
    | Mul
    | Less
    | Fun
    | Let
    | Assign
    | In
    | If
    | Then
    | Else
    | LPar
    | RPar

  let print_token token =
    match token with
    | Num n -> printf "%d" n
    | Id i -> printf "\"%s\"" i
    | Add -> printf " add "
    | Sub -> printf " sub "
    | Mul -> printf " mul "
    | Less -> printf " less "
    | Fun -> printf "fun "
    | Let -> printf "let "
    | Assign -> printf " = "
    | In -> printf " in "
    | If -> printf "if "
    | Then -> printf " then "
    | Else -> printf " else "
    | LPar -> printf "("
    | RPar -> printf ")"

  let print token_list = List.iter print_token token_list
end

module BeginForm = struct
  type expr =
    | Num of int
    | Id of string
    | Add of expr * expr
    | Sub of expr * expr
    | Mul of expr * expr
    | Less of expr * expr
    | Fun of string * expr
    | Let of string * expr * expr
    | Ite of expr * expr * expr
    | Call of expr * expr
    | DefFun of string * string * expr

  type program = { defs : expr list; main : expr }
end

module StringMap = Map.Make (String)

module Env = struct
  type register = char * int
  type t = register StringMap.t

  let first_unused (env : t) sym =
    let number =
      StringMap.fold
        (fun _ v num -> max num (snd v + 1))
        (StringMap.filter (fun _ v -> fst v = sym) env)
        (if sym = 's' then 2 else 0)
    in
    (sym, number)

  let push (env : t) var (reg : register) =
    StringMap.add var reg (StringMap.filter (fun _ v -> not (v = reg)) env)

  let push_next (env : t) var sym = push env var (first_unused env sym)

  let get (env : t) var =
    if StringMap.find_opt var env = None then ('n', -1)
    else StringMap.find var env

  let empty = StringMap.empty
  let to_str (reg : register) = Printf.sprintf "%c%d" (fst reg) (snd reg)
end

module ANF = struct
  type iexpr = INum of int | IId of string

  and cexpr =
    | CAdd of iexpr * iexpr
    | CSub of iexpr * iexpr
    | CMul of iexpr * iexpr
    | CLess of iexpr * iexpr
    | CFun of string * aexpr
    | CCall of iexpr * iexpr
    | CIte of iexpr * aexpr * aexpr
      (* if <cond> then <then> else <else>, где cond = 0 - ложь, cond != 0 - истина *)
    | CIExpr of iexpr

  and aexpr =
    | ALet of string * cexpr * aexpr (* let <id> = <body> in <where> *)
    | ACExpr of cexpr

  and def = DFun of string * string * aexpr

  type program = { defs : def list; main : aexpr }

  let rec str_iexpr expr =
    match expr with INum n -> sprintf "%d" n | IId id -> sprintf "\"%s\"" id

  and str_cexpr expr =
    match expr with
    | CAdd (a, b) -> sprintf "Add (%s, %s)" (str_iexpr a) (str_iexpr b)
    | CSub (a, b) -> sprintf "Sub (%s, %s)" (str_iexpr a) (str_iexpr b)
    | CMul (a, b) -> sprintf "Mul (%s, %s)" (str_iexpr a) (str_iexpr b)
    | CLess (a, b) -> sprintf "Less (%s, %s)" (str_iexpr a) (str_iexpr b)
    | CCall (name, arg) ->
        sprintf "Call (%s, %s)" (str_iexpr name) (str_iexpr arg)
    | CFun (arg, body) -> sprintf "Fun (\"%s\", %s)" arg (str_aexpr body)
    | CIte (cond, th, el) ->
        sprintf "Ite (%s, %s, %s)\n" (str_iexpr cond) (str_aexpr th)
          (str_aexpr el)
    | CIExpr e -> str_iexpr e

  and str_aexpr expr =
    match expr with
    | ALet (id, body, where) ->
        sprintf "Let (\"%s\",\n%s,\n%s)\n" id (str_cexpr body) (str_aexpr where)
    | ACExpr e -> str_cexpr e

  and str_dexpr expr =
    match expr with
    | DFun (name, arg, body) ->
        sprintf "DFun (\"%s\", \"%s\",\n%s)\n" name arg (str_aexpr body)

  let print program =
    List.iter (fun x -> printf "%s\n" (str_dexpr x)) program.defs;
    printf "%s" (str_aexpr program.main)
end

module Assembly = struct
  open Env

  type t =
    | Section of string
    | Global of string
    | Branch of string
    | Addi of register * register * int
    | Add of register * register * register
    | Sub of register * register * register
    | Mul of register * register * register
    | Li of register * int
    | Sd of register * int * register
    | Ld of register * int * register
    | Mv of register * register
    | J of string
    | Call of string
    | Slti of register * register * int
    | Slt of register * register * register
    | Ble of register * register * string
    | Beq of register * register * string
    | Beqz of register * string
    | Ret
    | Ecall
    | WIP of ANF.aexpr

  let print_expr expr =
    match expr with
    | Section s -> printf ".section %s\n" s
    | Global s -> printf ".global %s\n" s
    | Branch br -> printf "%s:\n" br
    | Li (id, n) -> printf "    li %s, %d\n" (to_str id) n
    | Sd (id, off, s) -> printf "    sd %s, %d(%s)\n" (to_str id) off (to_str s)
    | Ld (id, off, s) -> printf "    ld %s, %d(%s)\n" (to_str id) off (to_str s)
    | Mv (id, s) -> printf "    mv %s, %s\n" (to_str id) (to_str s)
    | Addi (res, a, b) ->
        printf "    addi %s, %s, %d\n" (to_str res) (to_str a) b
    | Add (res, a, b) ->
        printf "    add %s, %s, %s\n" (to_str res) (to_str a) (to_str b)
    | Sub (res, a, b) ->
        printf "    sub %s, %s, %s\n" (to_str res) (to_str a) (to_str b)
    | Mul (res, a, b) ->
        printf "    mul %s, %s, %s\n" (to_str res) (to_str a) (to_str b)
    | J br -> printf "    j %s\n" br
    | Slti (res, a, b) ->
        printf "    addi %s, %s, %d\n" (to_str res) (to_str a) b
    | Slt (res, a, b) ->
        printf "    slt %s, %s, %s\n" (to_str res) (to_str a) (to_str b)
    | Ble (a, b, br) -> printf "    ble %s, %s, %s\n" (to_str a) (to_str b) br
    | Beq (a, b, br) -> printf "    beq %s, %s, %s\n" (to_str a) (to_str b) br
    | Beqz (a, br) -> printf "    beqz %s, %s\n" (to_str a) br
    | Call br -> printf "    call %s\n" br
    | Ret -> printf "    ret\n"
    | Ecall -> printf "    ecall\n"
    | WIP e -> printf "WIP: %s" (ANF.str_aexpr e)

  let print expr_list = List.iter print_expr expr_list
end
