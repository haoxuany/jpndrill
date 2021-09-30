open Refreference;;

module StringRefReference =
  functor (
    E : REFENTRY
    with type t = string
  ) -> RefReference(
    struct
      module S = struct
        type rep = string
        let serialize s = s
        let deserialize s = s
      end
      module E = E
    end
  )

module IntRefReference =
  functor (
    E : REFENTRY
    with type t = int
  ) -> RefReference(
    struct
      module S = struct
        type rep = int
        let serialize = Int.to_string
        let deserialize = int_of_string
      end
      module E = E
    end
  )

module BoolRefReference =
  functor (
    E : REFENTRY
    with type t = bool
  ) -> RefReference(
    struct
      module S = struct
        type rep = bool
        let serialize = Bool.to_string
        let deserialize = bool_of_string
      end
      module E = E
    end
  )
