open Secure_interpreter_lib.Interpreter;;

print_endline "";
let res = interpret_file "input.txt" in
print_endline res
