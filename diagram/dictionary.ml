open Key
open Vector

class ['a] dictionary default_value =
object(self)

  (* Correspondance between key ids and fields *)
  val table = (Hashtbl.create 8 : (int, int) Hashtbl.t)

  (* Data stores both the key and value in the order the fields were added *)
  val data = new vector (new key "", default_value)

  method get (field:key) =
    let index = Hashtbl.find table field#get_id in
    snd (data#get index)

  method set (field:key) (value:'a) =
    let index = Hashtbl.find table field#get_id in
    data#set index (field, value)

  method add_field field value =
    let index = data#size in
    data#push_back (field, value);
    Hashtbl.add table field#get_id index

  method iter f =
    data#iter (fun (a, b) -> f a b)
      
end
