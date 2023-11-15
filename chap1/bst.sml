(* Control.Print.printDepth := 1024;
 *
 * ^ Run this command in sml to stop truncation
 *)

type key = string
type value = int

datatype tree = LEAF | TREE of tree * key * value * tree

val empty = LEAF

(* Extended below
fun insert (key, LEAF) = TREE(LEAF, key, LEAF)
    | insert (key, TREE(l, k, r)) = 
        if key < k
          then TREE(insert(key,l), k, r)
        else if key > k
          then TREE(l, k, insert(key,r))
        else TREE(l, key, r)

fun member (key, LEAF) = false
    | member (key, (TREE(l, k, r))) = 
        if k = key 
          then true
        else if k > key
          then member (key, r)
        else member (key, l) 
*)

fun insert (k: key, v: value, LEAF): tree = TREE(LEAF, k, v, LEAF)
    | insert (k: key, v: value, TREE(l, k', v', r)) = 
        if k < k'
          then TREE(insert(k, v, l), k', v', r)
        else if k > k'
          then TREE(l, k', v', insert(k, v, r))
        else TREE(l, k, v, r)

fun lookup (key, LEAF) = false
    | lookup (key, (TREE(l, k, _, r))) = 
        if k = key 
          then true
        else if k > key
          then lookup (key, r)
        else lookup (key, l)

val mytree = LEAF
val myinsert10 = insert("a", 10, mytree)
val myinsert3 = insert("b", 3, myinsert10)
val myinsert12 = insert("c", 12, myinsert3)
val mylookup20 = lookup("20", myinsert12)
val mylookup12 = lookup("12", myinsert12)
val mylookup10 = lookup("10", myinsert12)
val unballist1 = ["t", "s", "p", "i", "p", "f", "b", "s", "t"]
val unbaltree1 = insert("t", 0, 
  insert("s", 0, 
  insert("b", 0, 
  insert("f", 0, 
  insert("p", 0, 
  insert("i", 0, 
  insert("p", 0, 
  insert("s", 0, 
  insert("t", 0, LEAF)))))))))
val unballist2 = ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
val unbaltree2 = insert("i", 0, 
  insert("h", 0, 
  insert("g", 0, 
  insert("f", 0, 
  insert("e", 0, 
  insert("d", 0, 
  insert("c", 0, 
  insert("b", 0, 
  insert("a", 0, LEAF)))))))))

(* Balanced BST

Don't want to spend much time on this, and might come back to it. 

Idea: Just write a procedure that balances the tree by picking a new root. 

We could do this by finding the midpoint of the set, which suggests that we 
want to maintain a list of sorted elements. 

So we have a sorted list to use as an auxiliary. Inserting an element will mean
inserting into the list, and building the BST from the list. 

 *)
