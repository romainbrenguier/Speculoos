open Camlp4.PreCast
open Syntax

let add_to_decl _loc x name typ remaining = 
       let y = <:expr< [_decl_1 :: _declarations] >> in
       match typ with 
       | None ->
	 <:expr< let (_decl_1, $lid:name$) = $x$ in let _declarations = $y$ in $remaining$ >>
       | Some t ->
	 <:expr< let (_decl_1, ($lid:name$:$t$)) = $x$ in let _declarations = $y$ in $remaining$ >>



let () =
  EXTEND Gram
    expr: LEVEL "top"
    [ [
      "output"; v = a_LIDENT; ":"; t = ctyp ; m = expr; "in"; e = expr ->
    let x = <:expr< (output $str:v$ $m$) >> in
       add_to_decl _loc x v (Some t) e
    | "output"; name = a_LIDENT; m = expr; "in"; e = expr ->
       let x = <:expr< (output $str:name$  $m$) >> in
       add_to_decl _loc x name None e
    | "input"; v = a_LIDENT; ":"; t = ctyp ; m = expr; "in"; e = expr ->
    let x = <:expr< (input $str:v$  $m$) >> in 
    add_to_decl _loc x v (Some t) e
    | "input"; v = a_LIDENT; m = expr; "in"; e = expr ->
    let x = <:expr< (input $str:v$  $m$) >> in 
    add_to_decl _loc x v None e
    | "reg"; v = a_LIDENT; ":"; t = ctyp ; m = expr; "in"; e = expr ->
    let x = <:expr< (reg $str:v$  $m$) >> in 
    add_to_decl _loc x v (Some t) e
    | "reg"; v = a_LIDENT; m = expr; "in"; e = expr ->
    let x = <:expr< (reg $str:v$  $m$) >> in 
    add_to_decl _loc x v None e
    | "wire"; v = a_LIDENT; ":"; t = ctyp ; m = expr; "in"; e = expr ->
    let x = <:expr< (wire $str:v$  $m$) >> in 
    add_to_decl _loc x v (Some t) e
    | "wire"; v = a_LIDENT; m = expr; "in"; e = expr ->
    let x = <:expr< (wire $str:v$  $m$) >> in 
    add_to_decl _loc x v None e
      
    | "SPEC"; e = expr ->
	 <:expr< Cudd.init 100; let aiger = synthesize _declarations $e$ in (*Cudd.quit();*) aiger >> 

      | "for_each"; v = a_LIDENT; "in"; e = expr; "do"; f = expr ->
	 <:expr< for_each $e$ (fun $lid:v$ -> $f$) >> 

      ] ];
  END

