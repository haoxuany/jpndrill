open Signatures;;

module type SERIALIZER = sig
  type rep
  val serialize : rep -> string
  val deserialize : string -> rep
end

module type REFENTRY = sig
  type t
  val reference : t ref
end

module RefReference =
  functor (R : sig
    module S : SERIALIZER
    module E : REFENTRY
    with type t = S.rep
  end
  ) -> (struct
  open R

  type 'a t = 'a ref
  let set r v = r := v
  let get r = !r

  type rep = E.t
  let reference = E.reference
  let serialize = S.serialize
  let deserialize = S.deserialize
end : VALUE)


