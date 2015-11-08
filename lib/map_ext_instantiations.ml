
open Printf

open Bin_prot.Std

module Int_map = struct
  include Map_ext.Make(
    struct 
      type t = int with compare, bin_io
      let to_string t = string_of_int t
    end)
end

module Reverse_int_map = struct
  include Map_ext.Make(
    struct
      type t = int with compare, bin_io
      let compare t1 t2 = compare t1 t2 * -1
      let to_string t = string_of_int t
    end)
end

module Int32_map = struct
  include Map_ext.Make(
    struct
      type t = Int32.t with compare
      let compare = Int32.compare
      let to_string t = Int32.to_string t
    end)
end

module Int_tuple_map = struct
  include Map_ext.Make(
    struct
      type t = int * int with compare, bin_io
      let to_string (int_1, int_2) =
        sprintf
          "%d %d"
          int_1
          int_2
    end)
end

module Int_set_map = struct
  include Map_ext.Make(struct
      type t = Set_ext_instantiations.Int_set.t with compare
      let to_string int_set =
        Set_ext_instantiations.Int_set.to_string
          int_set
    end)
end

module String_map = struct
  include Map_ext.Make(
    struct
      type t = string
      let compare = String.compare
      let to_string t = t
    end)
end

module Float_map = struct
  include Map_ext.Make(BatFloat)
end

module Reverse_float_map = struct
  include Map_ext.Make(
    struct
      type t = float with compare, bin_io
      let compare t1 t2 = compare t1 t2 * -1
      let to_string t = string_of_float t
    end)
end
