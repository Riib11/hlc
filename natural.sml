structure Natural = struct

open Utilities;

exception Malformed_nat of string;
exception Nat_of_negative_int of int;

datatype nat = O | S of nat

fun nat_of_int i =
    if i < 0 then raise (Nat_of_negative_int i) else
    if i = 0 then O
    else S (nat_of_int (i - 1))

fun int_of_nat O = 0
  | int_of_nat (S n) = 1 + int_of_nat n

val is_nat : string -> bool =
    List.all Char.isDigit o String.explode

fun read_nat (s : string) : nat =
    case Int.fromString s of
	SOME i => nat_of_int i
      | NONE => raise (Malformed_nat s)

val toString : nat -> string = Int.toString o int_of_nat
				  
end
