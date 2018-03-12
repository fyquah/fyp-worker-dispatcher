open Core
open Protocol.Shadow_fyp_compiler_lib
open Protocol.Inlining_tree.V1

module Compilation_unit = Fyp_compiler_lib.Compilation_unit
module Variable = Fyp_compiler_lib.Variable
module Function_metadata = Data_collector.Function_metadata

let () =
  let ident = Fyp_compiler_lib.Ident.create_persistent "test" in
  let linkage_name = Fyp_compiler_lib.Linkage_name.create "test" in
  let compilation_unit = Compilation_unit.create ident linkage_name in
  Compilation_unit.set_current compilation_unit
;;

let mk_fn s =
  let closure_id = Closure_id.wrap (Variable.create s) in
  let closure_origin = Closure_origin.create closure_id in
  { Function_metadata.
    closure_id = None;
    set_of_closures_id = None;
    closure_origin;
  }
;;

let f_a = mk_fn "a"
let f_b = mk_fn "b"
let f_c = mk_fn "c"
let f_d = mk_fn "d"
let f_e = mk_fn "e"

let mk_apply_id () = Apply_id.create `Plain_apply

let apply_a = mk_apply_id ()
let apply_b = mk_apply_id ()
let apply_c = mk_apply_id ()
let apply_d = mk_apply_id ()
let apply_e = mk_apply_id ()

(*
 * input 1:
 *   TOP_LEVEL
 *   | {a}
 *   | -- <b>
 *   | -- <c>
 *   | ---- <d|>
 *   | <a>
 *   | -- <d>
 *   | ---- <e|>
 *
 *  output 1:
 *   TOP_LEVEL
 *   | {a}
 *   | -- <b>
 *   | -- <c>
 *   | ---- <d|>
 *   | <a>
 *   | -- <b>
 *   | -- <c>
 *   | ---- <d>
 *   | ------ <e|>
 *)
let input_1 = [
  Declaration {
    declared = f_a;
    children = [
      Apply_inlined_function {
        applied  = f_b;
        apply_id = apply_b;
        children = [];
      };
      Apply_inlined_function {
        applied  = f_c;
        apply_id = apply_c;
        children = [
          Apply_non_inlined_function {
            applied  = f_d;
            apply_id = apply_d;
          }
        ]
      };
    ]
  };
  Apply_inlined_function {
    applied = f_a;
    apply_id = apply_a;
    children = [
      Apply_inlined_function {
        apply_id = apply_d;
        applied = f_d;
        children = [
          Apply_non_inlined_function {
            applied = f_e;
            apply_id = apply_e;
          }
        ]
      }
    ]
  }
]
;;

let output_1 = [
  Declaration {
    declared = f_a;
    children = [
      Apply_inlined_function {
        applied  = f_b;
        apply_id = apply_b;
        children = [];
      };
      Apply_inlined_function {
        applied  = f_c;
        apply_id = apply_c;
        children = [
          Apply_non_inlined_function {
            applied  = f_d;
            apply_id = apply_d;
          }
        ]
      };
    ]
  };
  Apply_inlined_function {
    applied = f_a;
    apply_id = apply_a;
    children = [
      Apply_inlined_function {
        applied  = f_b;
        apply_id = apply_b;
        children = [];
      };
      Apply_inlined_function {
        applied  = f_c;
        apply_id = apply_c;
        children = [
          Apply_inlined_function {
            apply_id = apply_d;
            applied = f_d;
            children = [
              Apply_non_inlined_function {
                applied = f_e;
                apply_id = apply_e;
              }
            ]
          }
        ]
      };
    ]
  }
]
;;

let input_2 = []
let output_2 = []

let f_g = mk_fn "g"
let f_h = mk_fn "h"
let f_i = mk_fn "i"

let apply_g1 = mk_apply_id ()
let apply_g2 = mk_apply_id ()
let apply_h1 = mk_apply_id ()
let apply_i1 = mk_apply_id ()

(* Declaration in declaration should expand recursively. *)
(*
 * input 3:
 *   TOP_LEVEL
 *   | {a}
 *   | -- <b>
 *   | -- <c>
 *   | ---- <d|>
 *   | -- {g}
 *   | ---- <h>
 *   | ------ <i|>
 *   | -- <g>
 *   | ---- <i|>
 *   | -- <g|>
 *   | <a>
 *   | -- <d>
 *   | ---- <e>
 *   | -- <i|>
 *   | -- <g>
 *   | ---- <h|>
 *
 *  output 3:
 *   TOP_LEVEL
 *   | {a}
 *   | -- <b>
 *   | -- <c>
 *   | ---- <d|>
 *   | -- {g}
 *   | ---- <h>
 *   | ------ <i|>
 *   | -- <g>
 *   | ---- <h>
 *   | ------ <i|>
 *   | -- <g|>
 *   | <a>
 *   | -- <b>
 *   | -- <c>
 *   | ---- <d>
 *   | ------ <e|>
 *   | -- <g>
 *   | ---- <h>
 *   | ------ <i|>
 *   | -- <g>
 *   | ---- <h|>
 *)

let input_3 = [
  Declaration {
    declared = f_a;
    children = [
      Apply_inlined_function {
        applied  = f_b;
        apply_id = apply_b;
        children = [];
      };
      Apply_inlined_function {
        applied  = f_c;
        apply_id = apply_c;
        children = [
          Apply_non_inlined_function {
            applied  = f_d;
            apply_id = apply_d;
          }
        ]
      };
      Declaration {
        declared = f_g;
        children = [
          Apply_inlined_function {
            applied = f_h;
            apply_id = apply_h1;
            children = [
              Apply_non_inlined_function {
                applied = f_i;
                apply_id = apply_i1;
              }
            ]
          }
        ]
      };
      Apply_inlined_function {
        apply_id = apply_g1;
        applied = f_g;
        children = [
          Apply_non_inlined_function {
            applied = f_i;
            apply_id = apply_i1;
          }
        ]
      };
      Apply_non_inlined_function {
        applied = f_g;
        apply_id = apply_g2;
      }
    ]
  };
  Apply_inlined_function {
    applied = f_a;
    apply_id = apply_a;
    children = [
      Apply_inlined_function {
        apply_id = apply_d;
        applied = f_d;
        children = [
          Apply_non_inlined_function {
            applied = f_e;
            apply_id = apply_e;
          }
        ]
      };
      Apply_non_inlined_function {
        applied = f_i;
        apply_id = apply_i1;
      };
      Apply_inlined_function {
        applied = f_g;
        apply_id = apply_g2;
        children = [
          Apply_non_inlined_function {
            applied = f_h;
            apply_id = apply_h1;
          };
        ]
      }
    ]
  };
]
;;

let output_3 = [
  Declaration {
    declared = f_a;
    children = [
      Apply_inlined_function {
        applied  = f_b;
        apply_id = apply_b;
        children = [];
      };
      Apply_inlined_function {
        applied  = f_c;
        apply_id = apply_c;
        children = [
          Apply_non_inlined_function {
            applied  = f_d;
            apply_id = apply_d;
          }
        ]
      };
      Declaration {
        declared = f_g;
        children = [
          Apply_inlined_function {
            applied = f_h;
            apply_id = apply_h1;
            children = [
              Apply_non_inlined_function {
                applied = f_i;
                apply_id = apply_i1;
              }
            ]
          }
        ]
      };
      Apply_inlined_function {
        apply_id = apply_g1;
        applied = f_g;
        children = [
          Apply_inlined_function {
            applied = f_h;
            apply_id = apply_h1;
            children = [
              Apply_non_inlined_function {
                applied = f_i;
                apply_id = apply_i1;
              }
            ]
          }
        ]
      };
      Apply_non_inlined_function {
        applied = f_g;
        apply_id = apply_g2;
      }
    ]
  };
  Apply_inlined_function {
    applied = f_a;
    apply_id = apply_a;
    children = [
      Apply_inlined_function {
        apply_id = apply_b;
        applied = f_b;
        children = [];
      };
      Apply_inlined_function {
        apply_id = apply_c;
        applied = f_c;
        children = [
          Apply_inlined_function {
            apply_id = apply_d;
            applied = f_d;
            children = [
              Apply_non_inlined_function {
                applied = f_e;
                apply_id = apply_e;
              }
            ]
          };
        ]
      };

      Declaration {
        declared = f_g;
        children = [
          Apply_inlined_function {
            applied = f_h;
            apply_id = apply_h1;
            children = [
              Apply_non_inlined_function {
                applied = f_i;
                apply_id = apply_i1;
              }
            ]
          }
        ]
      };

      Apply_inlined_function {
        applied = f_g;
        apply_id = apply_g1;
        children = [
          Apply_inlined_function {
            applied = f_h;
            apply_id = apply_h1;
            children = [
              Apply_non_inlined_function {
                applied = f_i;
                apply_id = apply_i1;
              };
            ]
          };
        ]
      };

      Apply_inlined_function {
        applied = f_g;
        apply_id = apply_g2;
        children = [
          Apply_non_inlined_function {
            applied = f_h;
            apply_id = apply_h1;
          }
        ]
      };
    ]
  };
]

let apply_c1 = mk_apply_id ()
let apply_c2 = mk_apply_id ()

let input_fully_inlined = [
  Declaration  {
    declared = f_a;
    children = [
      Apply_inlined_function  {
        applied = f_b;
        apply_id = apply_b;
        children = [
          Apply_inlined_function {
            applied = f_c;
            apply_id = apply_c1;
            children = [];
          }
        ]
      };

      Apply_inlined_function  {
        applied = f_d;
        apply_id = apply_d;
        children = [
          Apply_inlined_function {
            applied = f_c;
            apply_id = apply_c2;
            children = [];
          }
        ]
      };
    ]
  };

  Apply_inlined_function {
    applied = f_a;
    apply_id = apply_a;
    children = [];
  };
]
;;

let output_fully_inlined = [
  Declaration  {
    declared = f_a;
    children = [
      Apply_inlined_function  {
        applied = f_b;
        apply_id = apply_b;
        children = [
          Apply_inlined_function {
            applied = f_c;
            apply_id = apply_c1;
            children = [];
          }
        ]
      };

      Apply_inlined_function  {
        applied = f_d;
        apply_id = apply_d;
        children = [
          Apply_inlined_function {
            applied = f_c;
            apply_id = apply_c2;
            children = [];
          }
        ]
      };
    ]
  };

  Apply_inlined_function {
    apply_id = apply_a;
    applied  = f_a;
    children = [
      Apply_inlined_function  {
        applied = f_b;
        apply_id = apply_b;
        children = [
          Apply_inlined_function {
            applied = f_c;
            apply_id = apply_c1;
            children = [];
          }
        ]
      };

      Apply_inlined_function  {
        applied = f_d;
        apply_id = apply_d;
        children = [
          Apply_inlined_function {
            applied = f_c;
            apply_id = apply_c2;
            children = [];
          }
        ]
      };
    ]
  };
]
;;

let apply_a1 = mk_apply_id ()
let apply_a2 = mk_apply_id ()
let apply_a3 = mk_apply_id ()
let f_x      = mk_fn "x"

let input_shadowing = [
  Declaration {
    declared = f_a;
    children = [
      Apply_inlined_function {
        applied = f_b;
        apply_id = apply_b;
        children = [];
      }
    ]
  };

  Declaration {
    declared = f_x;
    children = [
      Apply_inlined_function {
        applied = f_a;
        apply_id = apply_a1;
        children = []
      };

      Declaration {
        declared = f_a;
        children = [
          Apply_inlined_function {
            applied = f_c;
            apply_id = apply_c;
            children = [];
          };

          Apply_inlined_function {
            applied = f_d;
            apply_id = apply_d;
            children = [];
          };
        ]
      };

      Apply_inlined_function {
        applied = f_a;
        apply_id = apply_a2;
        children = []
      };
    ];
  };

  Apply_inlined_function {
    applied = f_a;
    apply_id = apply_a3;
    children = []
  }
]
;;

let output_shadowing = [
  Declaration {
    declared = f_a;
    children = [
      Apply_inlined_function {
        applied = f_b;
        apply_id = apply_b;
        children = [];
      }
    ]
  };

  Declaration {
    declared = f_x;
    children = [
      Apply_inlined_function {
        applied = f_a;
        apply_id = apply_a1;
        children = [
          Apply_inlined_function {
            applied = f_b;
            apply_id = apply_b;
            children = [];
          }
        ]
      };

      Declaration {
        declared = f_a;
        children = [
          Apply_inlined_function {
            applied = f_c;
            apply_id = apply_c;
            children = [];
          };

          Apply_inlined_function {
            applied = f_d;
            apply_id = apply_d;
            children = [];
          };
        ]
      };

      Apply_inlined_function {
        applied = f_a;
        apply_id = apply_a2;
        children = [
          Apply_inlined_function {
            applied = f_c;
            apply_id = apply_c;
            children = [];
          };

          Apply_inlined_function {
            applied = f_d;
            apply_id = apply_d;
            children = [];
          };
        ]
      };
    ];
  };

  Apply_inlined_function {
    applied = f_a;
    apply_id = apply_a3;
    children = [
      Apply_inlined_function {
        applied = f_b;
        apply_id = apply_b;
        children = [];
      }
    ]
  }
]
;;

let input_unroll_function_simple = [

  Declaration {
    declared = f_a;
    children = [
      Apply_non_inlined_function {
        applied = f_a;
        apply_id = apply_a1;
      }
    ];
  };

  Apply_inlined_function {
    applied = f_a;
    apply_id = apply_a3;
    children = [
      Apply_inlined_function {
        applied = f_a;
        apply_id = apply_a1;
        children = [
          Apply_non_inlined_function {
            applied = f_a;
            apply_id = apply_a1;
          }
        ];
      }
    ]
  }
]

let output_unroll_function_simple = [

  Declaration {
    declared = f_a;
    children = [
      Apply_non_inlined_function {
        applied = f_a;
        apply_id = apply_a1;
      }
    ]
  };

  Apply_inlined_function {
    applied = f_a;
    apply_id = apply_a3;
    children = [
      Apply_inlined_function {
        applied = f_a;
        apply_id = apply_a1;
        children = [
          Apply_non_inlined_function {
            applied = f_a;
            apply_id = apply_a1;
          }
        ];
      }
    ];
  };
]

let input_unroll_function_double = [

  Declaration {
    declared = f_a;
    children = [
      Apply_inlined_function {
        applied = f_a;
        apply_id = apply_a1;
        children = [
          Apply_non_inlined_function {
            applied = f_a;
            apply_id = apply_a1;
          }
        ]
      }
    ]
  };

  Apply_inlined_function {
    applied = f_a;
    apply_id = apply_a3;
    children = [
      Apply_non_inlined_function {
        applied = f_a;
        apply_id = apply_a1;
      }
    ];
  };
]
;;


let output_unroll_function_double = [

  Declaration {
    declared = f_a;
    children = [
      Apply_inlined_function {
        applied = f_a;
        apply_id = apply_a1;
        children = [
          Apply_inlined_function {
            applied = f_a;
            apply_id = apply_a1;
            children = [
              Apply_non_inlined_function {
                applied = f_a;
                apply_id = apply_a1;
              }
            ]
          }
        ]
      }
    ]
  };

  Apply_inlined_function {
    applied = f_a;
    apply_id = apply_a3;
    children = [
      Apply_inlined_function {
        applied = f_a;
        apply_id = apply_a1;
        children = [
          Apply_inlined_function {
            applied = f_a;
            apply_id = apply_a1;
            children = [
              Apply_non_inlined_function {
                applied = f_a;
                apply_id = apply_a1;
              }
            ]
          }
        ]
      }
    ];
  };
]
;;

let examples = [
  (input_1, output_1);
  (input_2, output_2);
  (input_3, output_3);
  (input_fully_inlined, output_fully_inlined);
  (input_shadowing, output_shadowing);
  (input_unroll_function_simple, output_unroll_function_simple);
  (input_unroll_function_double, output_unroll_function_double);
]
