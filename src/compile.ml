open Types

module Env = struct
  let count_vars_with_sym env sym =
    List.length (List.filter (fun x -> String.contains (snd x) sym) env)

  let next_sym_var env sym =
    Printf.sprintf "%c%d" sym (count_vars_with_sym env sym + 1)
  (* делаем + 1, чтобы в случае s не занимать s0 *)

  let sym_push env var sym =
    [ (var, Printf.sprintf "%c%d" sym (count_vars_with_sym env sym + 1)) ] @ env

  let get env var = snd (List.find (fun x -> fst x = var) env)

  let push_args env vars =
    List.mapi (fun i x -> (x, Printf.sprintf "a%d" i)) vars @ env

  let get_args env args =
    List.mapi
      (fun i x ->
        match x with
        | ANF.INum x -> Assembly.Li (Printf.sprintf "a%d" i, x)
        | IId x -> Assembly.Mv (Printf.sprintf "a%d" i, get env x))
      args

  let save env =
    List.mapi (fun i x -> Assembly.Sd (snd x, (i + 1) * 8, "sp")) env

  let load env =
    List.mapi (fun i x -> Assembly.Ld (snd x, (i + 1) * 8, "sp")) env
end

module ToAssembly = struct
  open Env

  let rec list_delete_end lst =
    match lst with
    | [] -> []
    | [ head; second ] -> [ head ]
    | head :: tail -> [ head ] @ list_delete_end tail

  let to_assembly_complex expr env =
    match expr with
    | ANF.CIExpr (INum n) -> [ Assembly.Li (next_sym_var env 's', n) ]
    | CIExpr (IId a) -> [ Mv (next_sym_var env 's', get env a) ]
    | CAdd (IId a, IId b) ->
        [ Add (next_sym_var env 's', get env a, get env b) ]
    | CAdd (IId a, INum b) -> [ Addi (next_sym_var env 's', get env a, b) ]
    | CAdd (INum a, IId b) -> [ Addi (next_sym_var env 's', get env b, a) ]
    | CAdd (INum a, INum b) -> [ Li (next_sym_var env 's', a + b) ]
    | CSub (IId a, IId b) ->
        [ Add (next_sym_var env 's', get env a, get env b) ]
    | CSub (IId a, INum b) -> [ Addi (next_sym_var env 's', get env a, -1 * b) ]
    | CSub (INum a, IId b) -> [ WIP (ACExpr expr) ]
    | CSub (INum a, INum b) -> [ Li (next_sym_var env 's', a - b) ]
    | CMul (IId a, IId b) ->
        [ Mul (next_sym_var env 's', get env a, get env b) ]
    | CCall (name, args) ->
        save env @ get_args env args
        @ [ Assembly.Call name; Mv (next_sym_var env 's', "a0") ]
        @ load env
    | _ -> [ WIP (ACExpr expr) ]

  let rec to_assembly_arbitrary expr env =
    match expr with
    | ANF.ACExpr e -> to_assembly_complex e env
    | ALet (id, body, where) -> (
        to_assembly_complex body env
        @
        let env = sym_push env id 's' in
        match where with
        | ACExpr e ->
            to_assembly_complex e env
            @ [
                Mv ("a0", next_sym_var env 's');
                Ld ("ra", 0, "sp");
                Addi ("sp", "sp", 256);
                Ret;
              ]
        | _ -> to_assembly_arbitrary where env)
    | AIte (cond, th, el) ->
        (match cond with
          | CLesseq (INum a, INum b) ->
              [ Assembly.Li ("t0", a); Li ("t1", b); Ble ("t0", "t1", ".then") ]
          | CLesseq (IId a, INum b) ->
              [ Li ("t0", b); Ble (get env a, "t0", ".then") ]
          | CLesseq (IId a, IId b) -> [ Ble (get env a, get env b, ".then") ]
          | _ -> [ WIP expr ])
        @ (match el with
          | ACExpr e ->
              to_assembly_complex e env
              @ [
                  Mv ("a0", next_sym_var env 's');
                  Ld ("ra", 0, "sp");
                  Addi ("sp", "sp", 256);
                  Ret;
                ]
          | _ -> to_assembly_arbitrary el env)
        @ [ Assembly.J ".if_end"; Branch ".then" ]
        @ (match th with
          | ACExpr e ->
              to_assembly_complex e env
              @ [
                  Mv ("a0", next_sym_var env 's');
                  Ld ("ra", 0, "sp");
                  Addi ("sp", "sp", 256);
                  Ret;
                ]
          | _ -> to_assembly_arbitrary th env)
        @ [ Branch ".if_end" ]
    | AFun ("main", args, body) ->
        [
          Assembly.Branch "_start"; Addi ("sp", "sp", -256); Sd ("ra", 0, "sp");
        ]
        @ list_delete_end (to_assembly_arbitrary body (push_args env args))
        @ [ Assembly.Li ("a7", 93); Ecall ]
    | AFun (id, args, body) ->
        [ Assembly.Branch id; Addi ("sp", "sp", -256); Sd ("ra", 0, "sp") ]
        @ to_assembly_arbitrary body (push_args env args)

  let to_assembly_program expr_list =
    [ Assembly.Section ".text"; Global "_start" ]
    @ List.concat (List.map (fun x -> to_assembly_arbitrary x []) expr_list)
end
