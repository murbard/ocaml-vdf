type t
type elt

(** Generate a proof following Wesolowski's paper
    @param x input of the VDF, element of the group
    @param y output of the VDF, also an element of the group
    @param t number of squarings to perform
    @param k some integer thing for processing chunks of bits, I don't quite understand
    @param l a security parameter, e.g. 256

*)
val generate: x:elt -> y:elt -> t:Z.t -> k:int -> l
val verify: ~x -> ~y -> ~proof -> ~t:Z.t -> bool
