module type R = sig
  type ('a,'e)r_  (* result *)
end

module R_is_result = struct
  type ('a,'e)r_ = ('a,'e)result
end
