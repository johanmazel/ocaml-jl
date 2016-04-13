
module L = BatList
module HT = BatHashtbl

module BatHashtbl = struct

  include HT
    
  let to_list t = L.of_enum (HT.enum t)
      
end

module BatHashtbl_utils = Hashtbl_utils.Make(BatHashtbl)


module Core_hashtbl_type = struct
  type ('a, 'b) t = ('a, 'b) Core_kernel.Core_hashtbl.t

  let add t key data = Core_kernel.Core_hashtbl.add_exn t ~key: key ~data: data
               
  let find = Core_kernel.Core_hashtbl.find_exn

  let fold f t acc =
    Core_kernel.Core_hashtbl.fold
      t
      ~f: (fun ~key: key ~data: data acc ->
          f key data acc
        )
      ~init: acc
      
  let iter f t =
    Core_kernel.Core_hashtbl.iter
      t
      ~f: (fun ~key: key ~data: data ->
          f key data
      )

  let to_list t = Core_kernel.Core_hashtbl.to_alist t
    
end

module Core_hashtbl_utils = Hashtbl_utils.Make(Core_hashtbl_type)
    
