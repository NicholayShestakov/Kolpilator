open Types

module Parser = struct
  open Tokens

  let is_whitespace c = c = ' ' || c = '\n' || c = '\t' || c = '\r'
  let is_digit c = match c with '0' .. '9' -> true | _ -> false
  let is_alpha c = match c with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

  let char_list_to_str char_list =
    List.fold_left (fun s c -> s ^ String.make 1 c) "" char_list

  let rec to_tokens_while cond char_list =
    match char_list with
    | c :: tail when cond c ->
        let taken, tail1 = to_tokens_while cond tail in
        (c :: taken, tail1)
    | _ -> ([], char_list)

  let rec to_tokens char_list token_list : token list =
    match char_list with
    | [] -> token_list
    | c :: tail when is_whitespace c -> to_tokens tail token_list
    | c :: tail when is_digit c ->
        let taken, tail1 = to_tokens_while is_digit char_list in
        to_tokens tail1
          (token_list @ [ Num (int_of_string (char_list_to_str taken)) ])
    | c :: tail when is_alpha c ->
        let taken, tail1 =
          to_tokens_while (fun x -> is_digit x || is_alpha x) char_list
        in
        to_tokens tail1
          (token_list
          @ [
              (match char_list_to_str taken with
              | "fun" -> Fun
              | "let" -> Let
              | "in" -> In
              | "if" -> If
              | "then" -> Then
              | "else" -> Else
              | id -> Id id);
            ])
    | '+' :: tail -> to_tokens tail (token_list @ [ Add ])
    | '-' :: tail -> to_tokens tail (token_list @ [ Sub ])
    | '*' :: tail -> to_tokens tail (token_list @ [ Mul ])
    | '<' :: tail -> to_tokens tail (token_list @ [ Less ])
    | '=' :: tail -> to_tokens tail (token_list @ [ Assign ])
    | '(' :: tail -> to_tokens tail (token_list @ [ LPar ])
    | ')' :: tail -> to_tokens tail (token_list @ [ RPar ])
    | c :: _ -> failwith (Printf.sprintf "unexspected symbol: %c" c)

  let to_tokens_program char_list = to_tokens char_list []

  let to_char_list filename =
    List.of_seq
      (String.to_seq (In_channel.with_open_text filename In_channel.input_all))
end

module ToBeginForm = struct
  let rec to_begin_form_imm token_list =
    match token_list with
    | Tokens.Num n :: tail -> (BeginForm.Num n, tail)
    | Id i :: tail -> (
        match tail with
        | Id arg :: tail1 -> (BeginForm.Call (Id i, Id arg), tail1)
        | Num arg :: tail1 -> (BeginForm.Call (Id i, Num arg), tail1)
        | LPar :: tail1 -> (
            let in_par, in_par_tail = to_begin_form_expr tail1 in
            match in_par_tail with
            | Tokens.RPar :: tail2 -> (BeginForm.Call (Id i, in_par), tail2)
            | _ -> failwith "missing closing parenthesis")
        | _ -> (BeginForm.Id i, tail))
    | LPar :: tail -> (
        let in_par, in_par_tail = to_begin_form_expr tail in
        match in_par_tail with
        | Tokens.RPar :: tail1 -> (in_par, tail1)
        | _ -> failwith "missing closing parenthesis")
    | _ -> failwith "incorrect immediate"

  and to_begin_form_high_expr token_list =
    let left, left_tail = to_begin_form_imm token_list in
    let rec loop acc current_tail =
      match current_tail with
      | Tokens.Mul :: tail ->
          let right, right_tail = to_begin_form_imm tail in
          loop (BeginForm.Mul (acc, right)) right_tail
      | _ -> (acc, current_tail)
    in
    loop left left_tail

  and to_begin_form_expr token_list =
    let left, left_tail = to_begin_form_high_expr token_list in
    let rec loop acc current_tail =
      match current_tail with
      | Tokens.Add :: tail ->
          let right, right_tail = to_begin_form_high_expr tail in
          loop (BeginForm.Add (acc, right)) right_tail
      | Sub :: tail ->
          let right, right_tail = to_begin_form_high_expr tail in
          loop (Sub (acc, right)) right_tail
      | Less :: tail ->
          let right, right_tail = to_begin_form_high_expr tail in
          loop (Less (acc, right)) right_tail
      | _ -> (acc, current_tail)
    in
    loop left left_tail

  let rec to_begin_form_block token_list =
    match token_list with
    | Tokens.Let :: Id id :: Assign :: tail -> (
        let body, body_tail = to_begin_form_block tail in
        match body_tail with
        | Tokens.In :: tail1 ->
            let where, next = to_begin_form_block tail1 in
            (BeginForm.Let (id, body, where), next)
        | _ -> failwith "missing 'in' after 'let' body")
    | If :: tail -> (
        let cond, cond_tail = to_begin_form_expr tail in
        match cond_tail with
        | Then :: tail1 -> (
            let th, th_tail = to_begin_form_block tail1 in
            match th_tail with
            | Else :: tail2 ->
                let el, next = to_begin_form_block tail2 in
                (Ite (cond, th, el), next)
            | _ -> failwith "missing 'else' after 'then' body")
        | _ -> failwith "missing 'then' after 'if' cond")
    | _ -> to_begin_form_expr token_list

  let rec to_begin_form_defs token_list (program : BeginForm.program) =
    match token_list with
    | [] -> program
    | Tokens.Let :: Id "main" :: Assign :: tail ->
        let body, next = to_begin_form_block tail in
        to_begin_form_defs next { defs = program.defs; main = body }
    | Let :: Id name :: Id arg :: Assign :: tail ->
        let body, next = to_begin_form_block tail in
        to_begin_form_defs next
          {
            defs = program.defs @ [ BeginForm.DefFun (name, arg, body) ];
            main = program.main;
          }
    | _ -> failwith "expression not in function"

  let to_begin_form_program token_list =
    to_begin_form_defs token_list { defs = []; main = Num 0 }
end

module ToANF = struct
  let gen_var =
    let count = ref 0 in
    fun base ->
      count := !count + 1;
      Printf.sprintf "%s_%d" base !count

  let rec to_anf_expr (expr : BeginForm.expr)
      (hole_expr : ANF.iexpr -> ANF.aexpr) : ANF.aexpr =
    match expr with
    | Num n -> hole_expr (INum n)
    | Id s -> hole_expr (IId s)
    | Add (a, b) ->
        let var_name = gen_var "add_res" in
        to_anf_expr a (fun aprim ->
            to_anf_expr b (fun bprim ->
                ALet (var_name, CAdd (aprim, bprim), hole_expr (IId var_name))))
    | Sub (a, b) ->
        let var_name = gen_var "sub_res" in
        to_anf_expr a (fun aprim ->
            to_anf_expr b (fun bprim ->
                ALet (var_name, CSub (aprim, bprim), hole_expr (IId var_name))))
    | Mul (a, b) ->
        let var_name = gen_var "mul_res" in
        to_anf_expr a (fun aprim ->
            to_anf_expr b (fun bprim ->
                ALet (var_name, CMul (aprim, bprim), hole_expr (IId var_name))))
    | Less (a, b) ->
        let var_name = gen_var "less_res" in
        to_anf_expr a (fun aprim ->
            to_anf_expr b (fun bprim ->
                ALet (var_name, CLess (aprim, bprim), hole_expr (IId var_name))))
    | Ite (cond, th, el) ->
        let var_name = gen_var "ite_res" in
        to_anf_expr cond (fun icond ->
            ALet
              ( var_name,
                CIte
                  ( icond,
                    to_anf_expr th (fun ie -> ACExpr (CIExpr ie)),
                    to_anf_expr el (fun ie -> ACExpr (CIExpr ie)) ),
                hole_expr (IId var_name) ))
    | Fun (arg, body) ->
        let body_expr = to_anf_expr body (fun ibody -> ACExpr (CIExpr ibody)) in
        let fun_name = gen_var "fun" in
        ALet (fun_name, CFun (arg, body_expr), hole_expr (IId fun_name))
    | Let (id, body, where) ->
        to_anf_expr body (fun ibody ->
            ALet (id, CIExpr ibody, to_anf_expr where hole_expr))
    | Call (f, arg) ->
        let var_name = gen_var "call_res" in
        to_anf_expr f (fun immf ->
            to_anf_expr arg (fun iarg ->
                ALet (var_name, CCall (immf, iarg), hole_expr (IId var_name))))
    | _ -> failwith "unmatched case"

  let to_anf_program (program : BeginForm.program) : ANF.program =
    {
      defs =
        List.map
          (fun expr ->
            match expr with
            | BeginForm.DefFun (name, arg, body) ->
                ANF.DFun
                  (name, arg, to_anf_expr body (fun ie -> ACExpr (CIExpr ie)))
            | _ -> failwith "incorrect def expr")
          program.defs;
      main = to_anf_expr program.main (fun ie -> ACExpr (CIExpr ie));
    }
end

module ToAssembly = struct
  (* Код; переменная с результатом; финальное окружение *)
  type comp_res = Assembly.t list * Env.register * Env.t

  let id_counter = ref 0

  let gen_id name =
    id_counter := !id_counter + 1;
    Printf.sprintf "%s_%d" name !id_counter

  let save_regs sym from until start_offset =
    List.init
      (until - from + 1)
      (fun i -> Assembly.Sd ((sym, i + from), (i + 1) * start_offset, ('x', 2)))

  let load_regs sym from until start_offset =
    List.init
      (until - from + 1)
      (fun i -> Assembly.Ld ((sym, i + from), (i + 1) * start_offset, ('x', 2)))

  let to_assembly_immediate expr env : comp_res =
    match expr with
    | ANF.INum n ->
        let res_id = Env.first_unused env 's' in
        ( [ Assembly.Li (res_id, n) ],
          res_id,
          Env.push env (gen_id "temp_var") res_id )
    | IId a -> ([], Env.get env a, env)

  let to_a_bin_oper a b env oper : comp_res =
    let res_id = Env.first_unused env 's' in
    let a_code, a_id, a_env = to_assembly_immediate a env in
    let b_code, b_id, _ = to_assembly_immediate b a_env in
    (a_code @ b_code @ [ oper (res_id, a_id, b_id) ], res_id, env)

  let rec to_assembly_complex expr env : comp_res =
    match expr with
    | ANF.CIExpr a -> to_assembly_immediate a env
    | CAdd (a, b) -> to_a_bin_oper a b env (fun (x, y, z) -> Add (x, y, z))
    | CSub (a, b) -> to_a_bin_oper a b env (fun (x, y, z) -> Sub (x, y, z))
    | CMul (a, b) -> to_a_bin_oper a b env (fun (x, y, z) -> Mul (x, y, z))
    | CLess (a, b) -> to_a_bin_oper a b env (fun (x, y, z) -> Slt (x, y, z))
    | CCall (IId name, arg) ->
        let res_id = Env.first_unused env 's' in
        let arg_code, arg_id, _ = to_assembly_immediate arg env in
        ( arg_code
          @ [
              Sd (('a', 0), 88, ('x', 2));
              Mv (('a', 0), arg_id);
              Call name;
              Mv (res_id, ('a', 0));
              Ld (('a', 0), 88, ('x', 2));
            ],
          res_id,
          env )
    | CIte (cond, th, el) ->
        let res_id = Env.first_unused env 's' in
        let cond_code, cond_id, cond_env = to_assembly_immediate cond env in
        let th_code, th_id, th_env = to_assembly_arbitrary th cond_env in
        let el_code, el_id, _ = to_assembly_arbitrary el th_env in
        let el_br = gen_id ".else" in
        let if_end_br = gen_id ".if_end" in
        ( cond_code
          @ [ Assembly.Beqz (cond_id, el_br) ]
          @ th_code
          @ [ Assembly.Mv (res_id, th_id); J if_end_br; Branch el_br ]
          @ el_code
          @ [ Assembly.Mv (res_id, el_id); Branch if_end_br ],
          res_id,
          env )
    | a -> ([ WIP (ACExpr a) ], (' ', -1), Env.empty)

  and to_assembly_arbitrary expr env : comp_res =
    match expr with
    | ANF.ACExpr e -> to_assembly_complex e env
    | ALet (id, body, where) ->
        let body_code, body_id, body_env = to_assembly_complex body env in
        let env = Env.push body_env id body_id in
        let where_code, where_id, where_env = to_assembly_arbitrary where env in
        (body_code @ where_code, where_id, where_env)

  let to_assembly_def expr =
    match expr with
    | ANF.DFun (name, arg, body) ->
        let body_code, body_id, body_env =
          to_assembly_arbitrary body (Env.push_next Env.empty arg 'a')
        in
        [
          Assembly.Branch name;
          Addi (('x', 2), ('x', 2), -256);
          Sd (('x', 1), 0, ('x', 2));
        ]
        @ save_regs 's' 2 11 8 @ body_code
        @ [ Assembly.Mv (('a', 0), body_id) ]
        @ load_regs 's' 2 11 8
        @ [ Ld (('x', 1), 0, ('x', 2)); Addi (('x', 2), ('x', 2), 256); Ret ]

  let to_assembly_program (program : ANF.program) =
    let main_code, main_id, main_env =
      to_assembly_arbitrary program.main Env.empty
    in
    [ Assembly.Section ".text"; Global "_start" ]
    @ [ Assembly.Branch "_start"; Addi (('x', 2), ('x', 2), -256) ]
    @ main_code
    @ [ Assembly.Mv (('a', 0), main_id); Li (('a', 7), 93); Ecall ]
    @ List.concat (List.map to_assembly_def program.defs)
end

module ToFile = struct
  let to_file assembly_list filename =
    let program_string = Assembly.to_str assembly_list in
    let channel = open_out filename in
    output_string channel program_string;
    close_out channel
end
