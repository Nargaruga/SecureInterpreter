open OUnit2
open Secure_interpreter_lib.Interpreter

(** Creates a test of name [name] which checks whether the expression [expr]
    evaluates to a value matching [res]. *)
let test_result name expected expr =
  name >:: fun _ -> assert_equal expected (interpret expr)

(** Creates a test of name [name] which checks whether the expression [expr]
  raises an UnboundException with the message [expected_str] *)
let test_unbound name expected_str expr =
  name >:: fun _ ->
  assert_raises (UnboundException expected_str) (fun _ -> interpret expr)

(** Creates a test of name [name] which checks whether the expression [expr]
  raises a SecurityException with the message [expected_str] *)
let test_security name expected_str expr =
  name >:: fun _ ->
  assert_raises (SecurityException expected_str) (fun _ -> interpret expr)

let suite =
  [
    (* Operations on integers *)
    test_result "int" "22" "22";
    test_result "negative_int" "-22" "-22";
    test_result "sum" "15" "10+5";
    test_result "subtraction" "5" "10-5";
    test_result "multiplication" "50" "10*5";
    test_result "division" "2" "10/5";
    (* Comparison operators *)
    test_result "equals" "true" "10==10";
    test_result "greater than" "true" "5>2";
    test_result "greater than or equal to" "true" "5>=5";
    test_result "less than" "true" "2<5";
    test_result "less than or equal to" "true" "5<=5";
    (* Operations on booleans *)
    test_result "bool" "true" "true";
    test_result "and" "true" "true and true";
    test_result "or" "true" "true or false";
    test_result "not" "true" "not false";
    test_result "complex expression 1" "true" "not (true and false)";
    test_result "complex expression 2" "true" "true and (not false)";
    (* Strings *)
    test_result "string" "hello" "\"hello\"";
    (* Conditional *)
    test_result "if then" "1" "if true then 1 else 0";
    test_result "if else" "0" "if false then 1 else 0";
    test_result "if complicated guard" "1"
      "if (true or (not false)) then 1 else 0";
    (* Variables *)
    test_result "let" "5" "let x = 3 in x + 2";
    test_result "assign one variable to another" "5"
      "let x = 5 in let y = x in y";
    (* Functions *)
    test_result "function" "5" "let f x = x + 1 in f(4)";
    test_result "passing functions as args" "5"
      "
      let foo g = g(0) in
      let bar x = x + 5 in
      foo(bar)
    ";
    (* Enclaves, secrets and gateways *)
    test_result "enclave non-secret let" "5"
      "
        enclave
          let x = 5 
          in 0 
        end
        x
      ";
    test_unbound "enclave secret let" "Unbound symbol: x"
      "
        enclave 
          secret x = 5 in 0
        end
        x
        ";
    test_unbound "call non-gateway function" "Unbound symbol: f"
      "
        enclave 
          let f x = x + 1 in 0
        end
        f(3)
      ";
    test_result "call gateway function" "5"
      "
      enclave 
        secret x = 3 in 
        let f y = x + y in 
        gateway f in 0 
      end 
      f(2)
      ";
    test_result "multiple enclaves" "0"
      "
        enclave 
          0 
        end
        enclave 
          0 
        end
        0
        ";
    (* Untrusted code *)
    test_result "include" "[Untrusted Code]"
      "include untr_code = let x = 1 in x end
      untr_code";
    test_result "execute" "0"
      "include untr_code = let x = 1 in x end
       execute(untr_code)
       0";
    test_security "direct security exception"
      "Untrusted code cannot call gateway functions."
      "
        enclave
          secret x = 3 in
          let f y = x + y in
          gateway f in
          0
        end
        include untr_code = let v = f(3) in v end
        execute(untr_code)
        0
      ";
    test_security "indirect security exception"
      "Untrusted code cannot call gateway functions."
      "
        enclave
          secret x = 3 in
          let f y = x + y in
          gateway f in
          0
        end
        let applies_f x = f(x) in
        include untr_code = let v = applies_f(3) in v end
        execute(untr_code)
        0
     ";
    test_unbound "untrusted code doesn't alter the environment"
      "Unbound symbol: x"
      "
        include untr_code = let x = 5 in 0 end
        execute(untr_code)
        x
     ";
  ]

let _ = run_test_tt_main ("suite" >::: suite)
