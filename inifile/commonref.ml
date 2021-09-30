open Refreference;;

module StringRefReference =
  functor (
    E : sig val value : string ref end
  ) -> RefReference(
    struct
      module S = struct
        type rep = string
        let serialize s = s
        let deserialize s = s
      end
      module E = struct
        type t = string
        let reference = E.value
      end
    end
  )

module IntRefReference =
  functor (
    E : sig val value : int ref end
  ) -> RefReference(
    struct
      module S = struct
        type rep = int
        let serialize = Int.to_string
        let deserialize = int_of_string
      end
      module E = struct
        type t = int
        let reference = E.value
      end
    end
  )

module BoolRefReference =
  functor (
    E : sig val value : bool ref end
  ) -> RefReference(
    struct
      module S = struct
        type rep = bool
        let serialize = Bool.to_string
        let deserialize = bool_of_string
      end
      module E = struct
        type t = bool
        let reference = E.value
      end
    end
  )
