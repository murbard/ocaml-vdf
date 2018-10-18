type t
val of_coefs : Z.t -> Z.t -> Z.t -> t
val discriminant: t -> Z.t
val identity_for_discriminant : Z.t -> t
val from_ab_discriminant: Z.t -> Z.t -> Z.t -> t
val pp: t -> String.t
val reduce: t -> t
val normalize: t -> t
val inverse: t -> t
val (^): t -> Z.t -> t
val ( * ): t -> t -> t
val (=): t -> t -> bool
val square: t -> t
