
class ['a] vector (default_value:'a) =
object(self)

  val mutable capacity = 16
  val mutable array = Array.make 16 default_value
  val mutable front_pos = 0
  val mutable back_pos = -1

  method front =
    array.(front_pos)

  method back =
    array.(back_pos)
      
  method get i =
    array.(front_pos+i mod capacity)

  method set i elt =
    array.(front_pos+i mod capacity) <- elt

  method size =
    let diff = back_pos - front_pos + 1 in
    if diff < 0
    then
      capacity + diff
    else
      diff

  method resize size =
    let arr = Array.make size self#front in
    (* copy elements between front_pos and capacity *)
    Array.blit array front_pos arr 0 (capacity - front_pos);
    (* copy elements between 0 and front_pos *)
    Array.blit array 0 arr (capacity - front_pos) front_pos;
    back_pos <- size - 1;
    front_pos <- 0;
    array <- arr;
    capacity <- size

  method push_front elt =
    front_pos <- (capacity + front_pos - 1) mod capacity;
    array.(front_pos) <- elt;
    if front_pos = back_pos + 1
    then
      self#resize (capacity * 2)
	
  method push_back elt =
    back_pos <- (back_pos + 1) mod capacity;
    array.(back_pos) <- elt;
    if front_pos = back_pos + 1
    then
      self#resize (capacity * 2)

  method pop_front =
    let ret = self#front in
    front_pos <- front_pos + 1;
    ret

  method pop_back =
    let ret = self#back in
    back_pos <- back_pos - 1;
    ret
    
end 
