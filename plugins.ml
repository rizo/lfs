(* automatically generated from plugins source *)
(* useful only to construct core LFS *)
(* useful only for test/devel purpose *)
open Common
open Lfs
open Parser_combinators.Infix


(* INT logic (syntax = <,>, [x..y],  sugar for <=, >= *)

type interval = Val of int | Sup of int | Inf of int | In of (int * int)

let parse = fun s -> 
  match s with
  | s when s =~ "^[0-9]+$"  -> Val (s_to_i s)
  | s when s =~ "^>\\([0-9]+\\)$" -> Sup (s_to_i (matched1 s))
  | s when s =~ "^<\\([0-9]+\\)$" -> Inf (s_to_i (matched1 s))
  | s when s =~ "^\\[\\([0-9]+\\)\\.\\.\\([0-9]+\\)\\]$" -> 
      let (x1, x2) = matched2 s |> Common2.pair s_to_i in
      let _ = assert(x1 < x2) in
      In (x1, x2)
  (* sugar *)
  | s when s =~ "^>=\\([0-9]+\\)$" -> Sup (s_to_i (matched1 s) - 1)
  | s when s =~ "^<=\\([0-9]+\\)$" -> Inf (s_to_i (matched1 s) + 1)
  | _ -> failwith "parsing error on interval"
(* note pour <= et >= aimerait ptet via |, mais peut pas :( => decaler l'entier :)  
   mais ptet certains sugar  neederait ca => comment faire ? peut faire des trucs via axiomes, mais bon 
*)


let (interval_logic: logic) = fun (Prop s1) (Prop s2) -> 
  let (x1, x2) = (parse s1, parse s2) in
  (match (x1, x2) with
  | (Val x, Val y) -> x = y                           (* 2 |= 2 *)
  | (Val x, Sup y) -> x > y                            (* 2 |= >1 *)
  | (Val x, Inf y) -> x < y                            (* 2 |= <3 *)
  | (Val x, In (y, z)) -> x <= z && x >= y             (* 2 |= [0..3] *)
  | (Sup x, Sup y) -> x >= y                           (* >3 |= >2 *)
  | (Inf x, Inf y) -> x <= y                           (* <2 |= <3 *)
  | (In (x1,y1), In (x2, y2)) -> x1 >= x2 && y1 <= y2  (* [2..3] |= [0..4] *)
  | (In (x,y), Sup z) -> x > z                         (* [1..4] |= >0 *)
  | (In (x,y), Inf z) -> y < z                         (* [1..4] |= <5 *)
  | _ -> false
   )

let is_formula_int (Prop s) = match parse s with (Val _) -> false | _ -> true



(* Logic for string via basic regexp (<,>, <>),
 * very similar to int logic.
 * 
 * Inspired by sebastien ferre code.
 *)

type basic_regexp = 
  | Begin of string 
  | End of string 
  | Contain of string 
  | Str of string
(* old: | Joker     not needed in lfs, author:* is handled via simply author: *)

let parse = fun s -> 
  match s with
  | s when s =~ "^<\\([a-zA-Z0-9_-]+\\)>$"  -> Contain (matched1 s)
  | s when s =~ "^<\\([a-zA-Z0-9_-]+\\)$"   -> Begin   (matched1 s)
  | s when s =~  "^\\([a-zA-Z0-9_-]+\\)>$"  -> End     (matched1 s)
  | s -> Str s

(* 
 * todo? allow full regexp logic ? (cf regexp_logic.pl).
 * But because of builtin &|!, you can do more advanced regexp than 
 * we think even with this simple logic.
 * 
 * TODO allow at least more than [a-zA-Z0-9_-], in that case cant  
 * use anymore regexp, or need escaping
 * => for Begin, need do String.sub s 0 (slength y) = y -> true 
 * TODO fuzzy (a la google and agrep)
 *)

let (string_logic:logic) = fun (Prop s1) (Prop s2) -> 
  let (x1, x2) = (parse s1, parse s2) in
  (match (x1, x2) with
  | (Str x, Str y)  -> x = y
  | (Str x, Begin y) -> x =~ ("^" ^ y)
  | (Str x, End y)   -> x =~ (".*" ^ y ^ "$")
  | (Str x, Contain y) -> x =~ (".*" ^ y ^ ".*")
  | (Begin x, Begin y) -> x =~ ("^" ^ y)             (* <abc |= <ab *)
  | (End x, End y)     -> x =~ (".*" ^ y ^ "$")      (* abc> |= bc> *)
  | (Contain x, Contain y) -> x =~ (".*" ^ y ^ ".*") (* <aaa> |= <a> *)
  | _ -> false
  )

let is_formula (Prop s) = match parse s with Str _ -> false | _ -> true



(* 
 * Logic for date. 
 * 
 * obsolete? use ferre date instead ?
 * 
 * syntax:  *-*-1992:*:*:*
 * sugar:
 * -*-1992
 * day:23
 * month:January|12
 * year:1992
 * hour:1
 * minute:1
 * second:2 :) not very useful (in fact yes if use this solver on duree (ex duree mp3)
 * summer/autumn/...
 * 
 * par rapport a ferre: peut dire juste le mois ou juste le jour
 * 
 * TODO will enable lifestream like ?
 * cd futur ?
 * cd yesterday (=> have sugar a la filter)
 * cd now (today)
 * => pas de rep todo :)
 * 
 * RESEARCH logic temporal can give good idea ?
 * 
 * 
 * TODO intlogic
 * 
 * TODO nameday:Lundi
 * 
 * TODO man find, no need newer,... simpler, no need to use a -or for find, ...
 *)

(* d/m/y:h/m/s *)
type date = ((interv option * interv option  * interv option) * 
	     (interv option * interv option  * interv option))
    and interv = Exact of int (* intlogic *)

let parse_int = fun s -> 
  if s = "*" then None
  else Some (Exact (s_to_i s)) (* intlogic *)

let parse_month = function
  | "*" -> None
  | s -> 
      Some 
        (Exact (
	  match s with
	  | ("January"|"Janvier") -> 1
	  | ("February"|"Fevrier") -> 2
	  | ("March"|"Mars") -> 3
	  | ("April"|"Avril") -> 4
	  | ("May"|"Mai") -> 5
	  | ("June"|"Juin") -> 6
	  | ("July"|"Juillet") -> 7
	  | ("August"|"Aout") -> 8
	  | ("September"|"Septembre") -> 9
	  | ("October"|"Octobre") -> 10
	  | ("November"|"Novembre") -> 11
	  | ("December"|"Decembre") -> 12
	  | s -> s_to_i s
	))

let rec parse = fun s -> 
  match s with
  | s when s =~ "^\\(.+\\)-\\(.+\\)-\\(.+\\):\\(.+\\):\\(.+\\):\\(.+\\)$" -> 
      let (d,mo,y, h,m,s) = matched6 s in
      ((parse_int d, parse_month mo, parse_int y), (* TODO? parse_year, allow 77 *)
       (parse_int h, parse_int m, parse_int s))
  (* sugar *)
  | s when s =~ "^\\(.+\\)-\\(.+\\)-\\(.+\\)*" -> parse (s ^ ":*:*:*")
  | s when s =~ "^\\(.+\\):\\(.+\\):\\(.+\\)*" -> parse ("*-*-*:" ^ s)
  | s when s =~ "day:\\(.+\\)"   -> parse (matched1 s ^ "-*-*")
  | s when s =~ "month:\\(.+\\)" -> parse ("*-" ^ matched1 s ^ "-*")
  | s when s =~ "year:\\(.+\\)"  -> parse ("*-*-" ^ matched1 s)
  | s when s =~ "hour:\\(.+\\)"   -> parse (matched1 s ^ ":*:*")
	(* TODO:  handle usa, pm am sugar *)
  | s when s =~ "minute:\\(.+\\)" -> parse ("*:" ^ matched1 s ^ ":*")
  | s when s =~ "second:\\(.+\\)" -> parse ("*:*:" ^ matched1 s)
  | _ -> failwith "syntax error"

let rec invariant = fun ((d, mo, y), (h, m, s)) -> 
  let wrap f x = 
    match x with 
    | None -> true
    | Some (Exact i) -> f i 
  in
  d  |> wrap (fun i -> i >= 1 && i <= 31) && (* TODO? sometimes can check when given month if valid *)
  mo |> wrap (fun i -> i >= 1 && i <= 12) &&
  h  |> wrap (fun i -> i >= 0 && i <= 24) &&
  m  |> wrap (fun i -> i >= 0 && i <= 59) &&
  s  |> wrap (fun i -> i >= 0 && i <= 59) &&
  true

type datelist = interv option list
let string_of_datelist = fun xs -> 
  Common2.join "-" (xs |> Common2.map (function | None -> "*" | Some (Exact i) -> i_to_s i))

let rec (date_logic:logic) = fun (Prop s1) (Prop s2) -> 
  let (x1, x2) = (parse s1, parse s2) in
  let _ = assert (invariant x1) in
  let _ = assert (invariant x2) in
  (match (x1, x2) with
  | (((d1, mo1, y1), (h1,m1,s1)) , ((d2, mo2, y2), (h2,m2,s2))) -> 
      solver_rec ([d1;mo1;y1;h1;m1;s1], [d2;mo2;y2;h2;m2;s2])
  )
  and solver_rec = function
  | ([],[]) -> true
  | (x::xs, y::ys) -> 
(*debug:      let _ = pr2 (string_of_datelist (x::xs)) in let _ = pr2 (string_of_datelist (y::ys)) in *)
      (match (x,y) with
      |	(None, None) -> solver_rec (xs, ys)
      |	(None, Some _) -> false
      |	(Some _, None) ->  true   && solver_rec (xs, ys)  (* -22- |= * *)
      |	(Some x, Some y) -> x = y && solver_rec (xs, ys) (* intlogic *)
      )
  | _ -> raise Impossible

let is_formula (Prop s) = true (* TODO, but as we allow different form for same leaf, cant optimise :( *)



module PC = Parser_combinators

(*****************************************************************************)
(* PROP logic (syntax = a AND b, OR, NOT, IMP, (, ), TOP, BOTTOM *)

(* TODO 
 * test, how ?? ex pb du b&c&a |= b&c qui buggait 
 *  - use assign bool tech, and compare
 *  - use quickcheck
 *  - faster if assign bool and see 
 *    (if number of obj < 4 => 4^4 combinaison, can be faster ?? 
 *  use priority as in chaza ? 
 *  could embed in atom more complex stuff (interval, regexp) 
 * => the intersection test for atoms will be in fact a call to embed solver
 *)


(*****************************************************************************)

type atom = string
type formula = 
  | Top    (* true *)
  | Bottom (* false  redundant with Not Top *)

  | Or  of formula * formula
  | And of formula * formula
  | Imp of formula * formula
  | Not of formula

  | Atom of atom




(*****************************************************************************)
(* old: trop dur comme ca 
 * let (relation: formule * formule -> relation) = function
 * | (Top, Top) -> Equal
 * | (Top, x)   -> Imply
 * | (x,   Top) -> Implied
 * | (Atom x, Atom y) -> if (x=y) then Equal else Uncomparable
 * | (Or x1 x2, Or x3 x4)   -> 
 *  match(relation (x1,x3) 
 * 
 * easier to use systeme a la sequent, imply, simplify from left and right, 
 * as sequent calculus sequent must be formule list * formule list, 
 * sinon balaise car faut reduire les & en list
 * 
 * type sequent = (formule list) * (formule list)
 * 
 * le pb c'est si on veut appliquer une regle, ca peut etre sur n'importe
 * quel elt de la liste, en prolog ca va on fait un member (A et B) LG et hop
 * la pas pareil, on serait oblige d'avoir un
 * List.exists (fun (And x y) -> true | _ -> false) puis de le deleter
 * de le recuperer, ... better to do as chaza, diviser en 2 le sequent
 * d'un cote les atomes et de l'autre les trucs un peu complique
 * 
 * Algo: 
 * chazarain, programmer avec scheme, P507
 * 
 * critique: 
 *  - dont like his lsequent, just use or and && of caml/scheme when 
 *    multiple path
 *  - dont like his sequent, with atomg atomd signedformule, 
 *    dont like signedformule
 *  - dont like his side effect, ses sequent sont imperative struct
 *     => he use copy-sequent 
 * 
 * sometimes je vois pas ou il traite les true et false, il a pas les
 * axiomes pour ca, 
 * 
 * tp2 of programming in lambda prolog at irisa in  DEA with bekkers
 *)

type sequent = ((atom list) * (formula list)) * ((atom list) * (formula list))

let (make_sequent: formula * formula -> sequent) = function
  | (Atom x, Atom y) -> ([x], []) , ([y], [])
  | (x,      Atom y) -> ([],  [x]), ([y], [])
  | (Atom x,  y)     -> ([x], []) , ([],  [y])
  | (x,       y)     -> ([],  [x]), ([],  [y])

let (add: formula * (atom list * formula list) -> (atom list * formula list)) = function
  | (Atom x, (y, z)) -> (x::y, z)
  | (x,      (y, z)) -> (y, x::z)

let rec (proof: sequent -> bool) = function
  | ((atoml, fl), (atomr, fr)) when (Common2.inter_set atoml atomr != []) -> true (* axiom *)
  | ((atoml, []), (atomr, [])) -> false  (* irreducible *)
  | ((atoml, x::xs), r) -> 
      let l = (atoml, xs) in
      (match x with
      |	(Not f)       -> proof (l,                   add (f,r))
      |	(Or (f1,f2))  -> proof (add (f1,l),          r) && 
	                 proof (add (f2,l),          r)
      |	(And (f1,f2)) -> proof (add (f2,add (f1,l)), r)
      |	(Imp (f1,f2)) -> proof (l,                   add(f1,r)) &&
	                 proof (add(f2,l),           r)
      |	Top           -> proof (l,r)
      |	Bottom        -> true
      |	_ -> failwith "internal error: cant have atom in right part of sequent"
      )
  | (l,(atomr,x::xs)) ->
      let r = (atomr, xs) in
      (match x with
      |	(Not f)       -> proof (add(f,l),            r)
      |	(Or (f1,f2))  -> proof (l,                   add(f2,add (f1,r)))
      |	(And (f1,f2)) -> proof (l,                   add (f1,r)) &&
	                 proof (l,                   add (f2,r))
      |	(Imp (f1,f2)) -> proof (add(f1,l),           add(f2,r))
      |	Top           -> true
      |	Bottom        -> proof (l,r)
      |	_ -> failwith "internal error: cant have atom in right part of sequent"
      )

(*****************************************************************************)
(* Now use parser combinators so can put all the code in the same file. No
 * need to split in a proptype.ml, proplexer.mll, propparser.mly and 
 * prop_logic.ml.
 *
 * old:
 *   let parse = fun s -> 
 *      Lexing.from_string s +> Propparser.main Proplexer.token
 *)

(* ---------------------------------------------------------------------- *)
(* lexer *)
(* ---------------------------------------------------------------------- *)
let kwds = ["AND";"OR";"NOT";"IMP";  "TOP";"BOTTOM"]
let symbols = ["(";")"]

let mykeyword = 
  PC.pred PC.alpha +++ PC.several PC.alpha >| (fun x -> 
    let s = (PC.collect x) in 
    if List.mem s kwds
    then PC.KWD s
    else raise Not_found
  )

let mysymbols = 
  PC.pred PC.symbol +++ PC.several PC.symbol >| (fun x -> 
    let s = (PC.collect x) in 
    if List.mem s symbols
    then PC.SYM s
    else raise Not_found
  )
  

(* the order is important if some "rules" overlap, as in ocamllex *)
let token =
  (mykeyword ||| mysymbols ||| PC.rawident) +++ PC.several PC.space >| fst

let lexer s = PC.lex_gen token s

(* ---------------------------------------------------------------------- *)
(* grammar *)
(* ---------------------------------------------------------------------- *)
let rec atom s =
 (
  (PC.ident           >| (fun x -> Atom x))
  |||
  (PC.a (PC.KWD "NOT") +++ term >| (fun (_, x) -> Not x))
  |||
  (PC.a (PC.KWD "TOP" ) >| (fun _ -> Top))
  |||
  (PC.a (PC.KWD "BOTTOM" ) >| (fun _ -> Bottom))
  |||
  (PC.a (PC.SYM "(") +++ term +++ PC.a (PC.SYM ")")  >| fun ((_, e), _) -> e)
  ) s
and factor s =
 (
  (atom +++ PC.a (PC.KWD "AND") +++ factor   >| fun ((f, _), g) -> And (f,g)) 
  |||
  (atom +++ PC.a (PC.KWD "IMP") +++ factor   >| fun ((f, _), g) -> And (f,g)) 
  |||
  atom
 ) s
and term s =
  (
   (factor +++ PC.a (PC.KWD "OR") +++ term     >| fun ((f, _), g) -> Or (f,g)) 
   |||
   factor
  ) s


let expr =
    term +++ PC.fin >| fst

(* ---------------------------------------------------------------------- *)
(* main *)
(* ---------------------------------------------------------------------- *)
let parse string =
    PC.val_of_parser (expr (lexer string))



(* ---------------------------------------------------------------------- *)
(* test *)
(* ---------------------------------------------------------------------- *)

let _ = Common2.example 
  (lexer "(a AND b)" 
  = 
  [PC.SYM "("; PC.IDENT "a"; PC.KWD "AND";PC.IDENT "b";PC.SYM ")"]
  )

(*let _ =   lexer "(a AND b)" +> List.map PC.string_of_token +> List.iter pr2*)

  
let ex1 =  parse "NOT (a AND b)"
let _ = Common2.example (ex1 = Not (And (Atom "a", Atom "b")))



(*****************************************************************************)
let prop_logic = fun (Prop s1) (Prop s2) -> 
  let (f1, f2) = (parse s1, parse s2) in
  proof (make_sequent (f1, f2))

(* As we want have a bottom, even prop:aa is a formula and must 
 * be inserted carefully. 
 *)
let is_formula_prop (Prop s) = true 

(*****************************************************************************)

(* fake mp3 transducer, just to test *)
let mp3_transducer = fun s -> 
  Common2.set 
    [ Prop ("genre:"  ^ (Common2.regexp_match s "tag_genre=\\(.+\\)"));
      Prop ("artist:" ^ (Common2.regexp_match s ".*\ntag_artist=\\(.+\\)"))
    ] 



(* very rudimentary, just to test *)
let c_adv_transducer = fun xs -> 
  let ($+$) = Common2.($+$) in
  let state = ref None in
  xs |> Common2.map (fun s -> 
    let _ = 
      if s =~ "int [a-zA-Z0-9]+(int [a-zA-Z0-9]+) {"
      then state := Some (Prop ("function:" ^ (Common2.regexp_match s "int \\([a-zA-Z0-9]+\\)("))) 
    in
    (match !state with None -> Common2.empty_list | Some x -> Common2.set [x]) $+$
    (if s =~ "int [a-zA-Z0-9]+(int [a-zA-Z0-9]+) {" then Common2.set [Prop "synchro"] else Common2.empty_list) $+$
    (if s =~ ".*fprintf(stderr" then Common2.set [Prop "debugging"] else Common2.empty_list) $+$
    (if s =~ ".*assert"           then Common2.set [Prop "error"] else Common2.empty_list) $+$
    (Common2.words s |> Common2.set |> Common2.filter (fun s -> Common2.member s ["x";"y";"z";"w";"i";"j"])
       |> Common2.map (fun s -> Prop ("var:" ^ s)))
  )



(* TODO better use of the keyword=  field cos can be on more than one lines, 
   in fact same pb for other entries => perhaps better pack pre processing (gather some lines)

 bug: if author is 'michand Brandt' => je vois le and :) faut des \b dans le split
*)
(* better domain: 
    use classification=  (cf 100_000.bib for example) 
    try do as in bibtex (suppr a/of/in/for/with/on, try make complex domain), split [ \t:], or have a dictionnary? (take acm classification) 
    inspire bibtex2html ? have tricks to infer from title ?
*)


let bibtex_adv_transducer = fun xs -> 
  let state = ref Common2.empty_list in     (* THE INTERNAL STATE *)
  let rec aux = function
    | [] -> []
    | s::xs -> 
	let props = 
	  if s =~ "[ \t]*@\\(.*\\){\\(.*\\)," then  (* @ symbol => we've got a new entry, such as @book{... *)
	    let (typ, ref) = matched2 s in
	    let xs' = Common2.take_until (fun s -> s =~ "[ \t]*}") xs in 
	    let props = [Prop ("typeref:" ^ typ); Prop ("ref:" ^ ref)] in
	    let _ = state :=
	      xs' |> Common2.fold (fun a s -> 
		(
		 match () with
		 | _ when s =~ "[ \t]*author[ \t]*=[ \t]*[\"{]\\(.*\\)[\"}]," -> 
		     Common2.matched 1 s |> Common2.split "[ \t]*and[ \t]*" |> Common2.map (fun s -> Prop ("author:" ^ s))
		 | _ when s =~ "[ \t]*year[ \t]*=[ \t]*\"?\\([0-9]+\\)" -> 
		     [Prop ("year:" ^ Common2.matched 1 s)]
		 | _ when s =~ "[ \t]*institution[ \t]*=[ \t]*\"\\(.*\\)\"," -> 
		     [Prop ("institution:" ^ Common2.matched 1 s)]
		 | _ when s =~ "[ \t]*title[ \t]*=[ \t]*[\"{]\\(.*\\)[\"}]," -> 
		     [Prop ("title:" ^ Common2.matched 1 s)]
                 | _ when s =~ "[ \t]*keywords[ \t]*=[ \t]*[\"{]\\([^\"]*\\)" -> 
		     Common2.matched 1 s |> split "[ \t]*[,;][ \t]*" |> Common2.map (fun s -> Prop ("domain:" ^ s))

                  (* for irisa.bib *)
		 | _ when s =~ "[ \t]*typededocument[ \t]*=[ \t]*[\"{]\\(.*\\)[\"}]," -> 
		     [Prop ("typeref:" ^ Common2.matched 1 s)]
		 | _ when s =~ "[ \t]*titre[ \t]*=[ \t]*[\"{]\\(.*\\)[\"}]," -> 
		     [Prop ("title:" ^ Common2.matched 1 s)]
		 | _ when s =~ "[ \t]*auteur[ \t]*=[ \t]*[\"{]\\(.*\\)[\"}]," -> 
		     [Prop ("author:" ^ Common2.matched 1 s)]
		 | _ when s =~ "[ \t]*motclef[ \t]*=[ \t]*[\"{]\\(.*\\)[\"}]," -> 
		     [Prop ("domain:" ^ Common2.matched 1 s)]
		 | _ when s =~ "[ \t]*datededition[ \t]*=[ \t]*[\"{][0-9][0-9]-\\([0-9]+\\)[\"}]," -> 
		     [Prop ("year:" ^ Common2.matched 1 s)]
		 | _ when s =~ "[ \t]*datededition[ \t]*=[ \t]*[\"{]\\([0-9]+\\)[\"}]," -> 
		     [Prop ("year:" ^ Common2.matched 1 s)]


		 | _ -> []
		) @ a
			  ) props in
	    (Prop "synchro")::!state (* SYNCHRONISATION POINT,  a new bibtex entry start a new unit *)
	  else !state in
	props::aux xs
  in
  aux xs




let ml_adv_transducer = fun xs -> 
  let ($+$) = Common2.($+$) in
  let _state = ref None in
  xs |> Common2.map (fun s -> 
    (if s =~ ".*assert"  then Common2.set [Prop "error"] else Common2.empty_list) $+$
    (if s =~ ".*example"  then Common2.set [Prop "example"] else Common2.empty_list) $+$
    Common2.set [Prop "true"]
  )
