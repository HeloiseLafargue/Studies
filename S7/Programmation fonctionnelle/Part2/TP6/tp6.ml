type zero = private Dummy1
type _ succ = private Dummy2
type nil = private Dummy3

module Ex1 =
  struct
    type (_,_) nlist = 
      | Nil : ('a,zero) nlist
      | Cons : 'a * ('a,'n) nlist -> ('a,'n succ) nlist

    let rec map : type n.('a->'b)->('a,n) nlist->('b,n) nlist =
      fun f l -> match l with
        | Nil -> Nil
        | Cons(t,q) -> Cons(f t, map f q)
  
    let rec snoc : type n. 'a->('a,n)nlist -> ('a,n succ)nlist =
      fun e l -> match l with
        | Nil -> Cons(e, Nil)
        | Cons(t,q) -> Cons(t, snoc e q)

    let tail : type n. ('a,n succ) nlist -> ('a,n) nlist = 
      fun l -> match l with
        | Cons(_,q) -> q
    
    let rec rev : type n. ('a,n) nlist -> ('a,n) nlist =
      fun l -> match l with
        | Nil -> Nil
        | Cons(t,Nil) -> Cons(t,Nil)
        | Cons(t1, Cons(t2,q)) -> rev( Cons(t1, Cons(t2, q)))
        (*|Cons(x,xs) -> tail*)
        
end

module Ex2 =
struct
  open Ex1
  let rec insert x l =
    match l with
    |[] -> x::l
    |t :: q -> if t < x then t:: insert x q else x :: l ;;
  let rec insertion_sort l =
    match l with
    |[] -> []
    |t :: q -> insert t ( insertion_sort q );;
end

module Ex3 = struct
  type 'p hlist = |Nil : nil hlist | Cons : 
end