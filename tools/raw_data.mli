open Core
open Protocol.Shadow_fyp_compiler_lib

module Reward : sig
  type dual_reward =
    { immediate : float;
      long_term : float;
    }
  [@@deriving sexp]

  type t =
    { path             : Protocol.Absolute_path.t;
      inline_reward    : dual_reward option;
      no_inline_reward : float option
    }
  [@@deriving sexp, fields]
end

type target = (Reward.dual_reward option * float option)
type example = (Feature_extractor.t * target)
