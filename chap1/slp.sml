type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
       | OpExp of exp * binop * exp
       | EseqExp of stm * exp

val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

(* Getting number of args out of lists *)
fun explist ([]) = 0
  | explist (e::t) = Int.max(expargs(e), explist(t))
(* Getting number of args out of expressions *)
and expargs (EseqExp (s, e)) = Int.max(maxargs s, expargs e)
	| expargs (OpExp (e1, _, e2)) = Int.max(expargs e1, expargs e2)
	| expargs (i) = 0
and maxargs (CompoundStm (s, t)) = Int.max(maxargs s, maxargs t)
	| maxargs (AssignStm (_, e)) = expargs e
	| maxargs (PrintStm (l)) = Int.max(length l, explist l)

structure IDTable = 
struct
	datatype table = Table of (id * int) list

	(* Table lookup *)
	fun lookup (Table ([]), _) = NONE
		| lookup (Table (x::xs), k) = 
			if k = (#1 x) then (SOME (#2 x)) else lookup (Table (xs), k)
	(* Table insert*)
	and update (Table ([]), k, v) = Table ([(k, v)]) 
		| update (Table (xs), k, v) = Table ((k, v) :: xs)
end;

fun interp s = let 
  fun interpExp (IdExp (i), t) = 
    (valOf(IDTable.lookup(t, i)), t)
    | interpExp (NumExp (i), IDTable.Table (t)) = (i, IDTable.Table (t))
    | interpExp (OpExp (e1, bop, e2), t) =
        let 
          val (e1', t') = interpExp (e1, t) 
          val (e2', t'') = interpExp (e2, t') in 
          case bop of
            Plus => (e1' + e2', t'')
            | Minus => (e1' - e2', t'')
            | Times => (e1' * e2', t'') 
            | Div => (e1' div e2', t'') 
        end
    | interpExp (EseqExp (s, e), t) = interpExp (e, interpStm (s, t))
  and interpStm (AssignStm (i, e), t) = 
    let val (e', t') = interpExp (e, t) in IDTable.update(t', i, e') end
    | interpStm (PrintStm [], t) = (print ("Nothing to print\n"); t)
    | interpStm (PrintStm (e::xs), t) = let val (e', t') = interpExp (e, t) in 
        (print (Int.toString e' ^ "\n"); interpStm (PrintStm xs, t))
      end
    | interpStm (CompoundStm (s1, s2), t) = interpStm (s2, interpStm (s1, t))
  in interpStm (s, IDTable.Table ([])) end

(* val test = interpExp (IdExp ("x"), IDTable.Table ([("x", 24)]))
val test2 = interpExp (NumExp (5), IDTable.Table ([("x", 24)]))
val testfive = NumExp (5)
val testtable = IDTable.Table ([("x", 24)])
val testOpExp = OpExp (testfive, Plus, testfive)
val test3 = interpExp (testOpExp, testtable)
val test4 = interpStm (AssignStm ("z", NumExp (42)), testtable)
val test5 = interpStm (PrintStm ([]), testtable)
val test6 = interpStm (PrintStm ([NumExp (5), NumExp (6)]), testtable) *)
val test7 = interp prog
