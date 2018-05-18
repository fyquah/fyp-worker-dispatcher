open Core
open Protocol.Shadow_fyp_compiler_lib
open Common

module Reward = struct
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
type 'a example = ('a Features.t * target)
