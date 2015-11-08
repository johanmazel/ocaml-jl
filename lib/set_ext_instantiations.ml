
open Bin_prot.Std

module Int_set = Set_ext.Make(BatInt)

module Int_set_set = 
  Set_ext.Make(
    struct
      type t = Int_set.t
      let compare = Int_set.compare
      let to_string t =
        Int_set.to_string
          ~sep: " "
          t
    end)

module Int32_set = struct
  include Set_ext.Make(Int32)
end
	
module String_set = struct
  include Set_ext.Make(
    struct
      type t = string
      let compare = String.compare
      let to_string t = t
    end)
end

module Float_set = struct
  include Set.Make(BatFloat)
end

module Int64_set = struct
  include Set_ext.Make(Int64)
end
