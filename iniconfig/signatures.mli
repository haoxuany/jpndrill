(* The only reason this is here is to avoid ocaml compiler bullshit on file naming. *)
(* These are just signatures, they're supposed to just compile. *)

module type REFERENCE = sig
  type 'a t
  val set : 'a t -> 'a -> unit
  val get : 'a t -> 'a
end

module type VALUE = sig
  include REFERENCE

  type rep
  val reference : rep t
  val serialize : rep -> string
  val deserialize : string -> rep
end

