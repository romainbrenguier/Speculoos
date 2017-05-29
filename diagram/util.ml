let reduce f l = match l with
  | a :: b :: tail -> List.fold_left f (f a b) tail
  | _ -> List.hd l
