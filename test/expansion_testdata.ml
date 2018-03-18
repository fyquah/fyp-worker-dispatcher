open Core
open Protocol.Shadow_fyp_compiler_lib
open Protocol.Inlining_tree.V1

module Compilation_unit = Fyp_compiler_lib.Compilation_unit
module Variable = Fyp_compiler_lib.Variable
module Function_metadata = Data_collector.Function_metadata
module E = Top_level.Expanded

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
    closure_id = Some closure_id;
    set_of_closures_id = None;
    closure_origin;
    opt_closure_origin = None;
    specialised_for = None;
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

let f_g = mk_fn "g"
let f_h = mk_fn "h"
let f_i = mk_fn "i"

let apply_g1 = mk_apply_id ()
let apply_g2 = mk_apply_id ()
let apply_h1 = mk_apply_id ()
let apply_i1 = mk_apply_id ()


let input_2 : Top_level.t = []
let output_2 = E.of_list []


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

let apply_c_in_b = Apply_id.inline ~caller:apply_b ~inlined:apply_c
let apply_d_in_c_in_b = Apply_id.inline ~caller:apply_c_in_b ~inlined:apply_d
let apply_i1_in_h1 = Apply_id.inline ~caller:apply_h1 ~inlined:apply_i1
let apply_h1_in_g1 = Apply_id.inline ~caller:apply_g1 ~inlined:apply_h1
let apply_i1_in_h1_in_g1 =
  Apply_id.inline ~caller:apply_h1_in_g1 ~inlined:apply_i1
;;
let apply_c_in_a =
  Apply_id.inline ~caller:apply_a ~inlined:apply_c
;;
let apply_b_in_a =
  Apply_id.inline ~caller:apply_a ~inlined:apply_b
;;
let apply_d_in_c =
  Apply_id.inline ~caller:apply_c ~inlined:apply_d
;;
let apply_d_in_c_in_a =
  Apply_id.inline ~caller:apply_c_in_a ~inlined:apply_d
;;
let apply_e_in_d_in_c_in_a =
  Apply_id.inline ~caller:apply_d_in_c_in_a ~inlined:apply_e
;;
let apply_g1_in_a = Apply_id.inline ~caller:apply_a ~inlined:apply_g1
let apply_h1_in_g1_in_a =
  Apply_id.inline ~caller:apply_g1_in_a ~inlined:apply_h1
;;
let apply_i1_in_h1_in_g1 =
  Apply_id.inline ~caller:apply_h1_in_g1 ~inlined:apply_i1
;;
let apply_i1_in_h1_in_g1_in_a =
  Apply_id.inline ~caller:apply_h1_in_g1_in_a ~inlined:apply_i1
;;
let apply_g2_in_a =
  Apply_id.inline ~caller:apply_a ~inlined:apply_g2
;;
let apply_h1_in_g2_in_a =
  Apply_id.inline ~caller:apply_g2_in_a ~inlined:apply_h1
;;
let apply_i1_in_h1_in_g2_in_a =
  Apply_id.inline ~caller:apply_h1_in_g2_in_a ~inlined:apply_i1
;;

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
            apply_id = apply_d_in_c;
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
                apply_id = apply_i1_in_h1;
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
            apply_id = apply_i1_in_h1_in_g1;
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
        apply_id = apply_d_in_c_in_a;
        applied = f_d;
        children = [
          Apply_non_inlined_function {
            applied = f_e;
            apply_id = apply_e_in_d_in_c_in_a;
          }
        ]
      };
      Apply_non_inlined_function {
        applied = f_i;
        apply_id = apply_i1_in_h1_in_g1_in_a;
      };
      Apply_inlined_function {
        applied = f_g;
        apply_id = apply_g2_in_a;
        children = [
          Apply_non_inlined_function {
            applied = f_i;
            apply_id = apply_i1_in_h1_in_g2_in_a;
          };
        ]
      }
    ]
  };
]
;;


let output_3 = E.of_list [
  E.Decl {
    func = f_a;
    children = [
      E.Inlined {
        func  = f_b;
        path = Apply_id.to_path apply_b;
        children = [];
      };
      E.Inlined {
        func  = f_c;
        path = Apply_id.to_path apply_c;
        children = [
          E.Apply {
            func  = f_d;
            path = Apply_id.to_path apply_d_in_c;
          }
        ]
      };
      E.Decl {
        func = f_g;
        children = [
          E.Inlined {
            func = f_h;
            path = Apply_id.to_path apply_h1;
            children = [
              E.Apply {
                func = f_i;
                path = Apply_id.to_path apply_i1_in_h1;
              }
            ]
          }
        ]
      };
      E.Inlined {
        path = Apply_id.to_path apply_g1;
        func = f_g;
        children = [
          E.Inlined {
            func = f_h;
            path = Apply_id.to_path apply_h1_in_g1;
            children = [
              E.Apply {
                func = f_i;
                path = Apply_id.to_path apply_i1_in_h1_in_g1;
              }
            ]
          }
        ]
      };
      E.Apply {
        func = f_g;
        path = Apply_id.to_path apply_g2;
      }
    ]
  };
  E.Inlined {
    func = f_a;
    path = Apply_id.to_path apply_a;
    children = [
      E.Inlined {
        path = Apply_id.to_path apply_b_in_a;
        func = f_b;
        children = [];
      };
      E.Inlined {
        path = Apply_id.to_path apply_c_in_a;
        func = E.expanded_function_metadata;
        children = [
          E.Inlined {
            path = Apply_id.to_path apply_d_in_c_in_a;
            func = f_d;
            children = [
              E.Apply {
                func = f_e;
                path = Apply_id.to_path apply_e_in_d_in_c_in_a;
              }
            ]
          };
        ]
      };

      E.Inlined {
        func = E.expanded_function_metadata;
        path = Apply_id.to_path apply_g1_in_a;
        children = [
          E.Inlined {
            func = E.expanded_function_metadata;
            path = Apply_id.to_path apply_h1_in_g1_in_a;
            children = [
              E.Apply {
                func = f_i;
                path = Apply_id.to_path apply_i1_in_h1_in_g1_in_a;
              };
            ]
          };
        ]
      };

      E.Inlined {
        func = f_g;
        path = Apply_id.to_path apply_g2_in_a;
        children = [
          E.Inlined {
            func = f_h;
            path = Apply_id.to_path apply_h1_in_g2_in_a;
            children = [
              E.Apply {
                func = f_i;
                path = Apply_id.to_path apply_i1_in_h1_in_g2_in_a;
              }
            ]
          }
        ]
      };
    ]
  };
]

let apply_c1 = mk_apply_id ()
let apply_c2 = mk_apply_id ()
let apply_c1_in_b = 
  Apply_id.inline ~caller:apply_b ~inlined:apply_c1
;;
let apply_c2_in_b = 
  Apply_id.inline ~caller:apply_b ~inlined:apply_c2
;;
let apply_c2_in_d = 
  Apply_id.inline ~caller:apply_d ~inlined:apply_c2
;;
let apply_b_in_a = Apply_id.inline ~caller:apply_a ~inlined:apply_b
let apply_c1_in_b_in_a = Apply_id.inline ~caller:apply_b_in_a ~inlined:apply_c1
let apply_d_in_a = Apply_id.inline ~caller:apply_a ~inlined:apply_d
let apply_c2_in_d_in_a = Apply_id.inline ~caller:apply_d_in_a ~inlined:apply_c2

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
            apply_id = apply_c1_in_b;
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
            apply_id = apply_c2_in_d;
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

let output_fully_inlined = E.of_list [
  E.Decl  {
    func = f_a;
    children = [
      E.Inlined  {
        func = f_b;
        path = Apply_id.to_path apply_b;
        children = [
          E.Inlined {
            func = f_c;
            path = Apply_id.to_path apply_c1_in_b;
            children = [];
          }
        ]
      };

      E.Inlined  {
        func = f_d;
        path = Apply_id.to_path apply_d;
        children = [
          E.Inlined {
            func = f_c;
            path = Apply_id.to_path apply_c2_in_d;
            children = [];
          }
        ]
      };
    ]
  };

  E.Inlined {
    path = Apply_id.to_path apply_a;
    func  = f_a;
    children = [
      E.Inlined  {
        func = f_d;
        path = Apply_id.to_path apply_d_in_a;
        children = [
          E.Inlined {
            func = f_c;
            path = Apply_id.to_path apply_c2_in_d_in_a;
            children = [];
          }
        ]
      };

      E.Inlined  {
        func = f_b;
        path = Apply_id.to_path apply_b_in_a;
        children = [
          E.Inlined {
            func = f_c;
            path = Apply_id.to_path apply_c1_in_b_in_a;
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

let _examples = [
  (input_shadowing, output_shadowing);
  (input_unroll_function_simple, output_unroll_function_simple);
  (input_unroll_function_double, output_unroll_function_double);
]


let apply_b = Apply_id.create `Plain_apply

let mk_apply_id =
  let ctr = ref (-1) in
  let compilation_unit = apply_b.compilation_unit in
  fun parents ->
    ctr := !ctr + 1;
    Apply_id.build_directly compilation_unit (Apply_id.Plain_apply !ctr)
      parents

let apply_c = mk_apply_id []
let apply_d = mk_apply_id [ apply_c ]
let apply_a = mk_apply_id []
let apply_e = mk_apply_id []

let apply_d_in_a =
  Apply_id.inline ~caller:apply_a  ~inlined:apply_d
;;

let apply_e_in_d_in_a =
  Apply_id.inline ~caller:apply_d_in_a ~inlined:apply_e
;;


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
        apply_id = apply_d_in_a;
        applied = f_d;
        children = [
          Apply_non_inlined_function {
            applied = f_e;
            apply_id = apply_e_in_d_in_a;
          }
        ]
      }
    ]
  }
]
;;

let apply_b_in_a =
  Apply_id.inline ~caller:apply_a ~inlined:apply_b
;;

let apply_c_in_a =
  Apply_id.inline ~caller:apply_a ~inlined:apply_c
;;

let to_path = Apply_id.to_path

let output_1 = E.of_list [
  E.Decl {
    func = f_a;
    children = [
      E.Inlined {
        func  = f_b;
        path  = Apply_id.to_path apply_b;
        children = [];
      };
      E.Inlined {
        func  = f_c;
        path = to_path apply_c;
        children = [
          E.Apply {
            func  = f_d;
            path = Apply_id.to_path apply_d;
          }
        ]
      };
    ]
  };
  E.Inlined {
    func = f_a;
    path = to_path apply_a;
    children = [
      E.Inlined {
        func     = f_b;
        path = to_path apply_b_in_a;
        children = [];
      };
      E.Inlined {
        func  = E.expanded_function_metadata;
        path = to_path apply_c_in_a;
        children = [
          E.Inlined {
            path = to_path apply_d_in_a;
            func = f_d;
            children = [
              E.Apply {
                func = f_e;
                path = to_path apply_e_in_d_in_a;
              }
            ]
          }
        ]
      };
    ]
  }
]
;;

let (examples : (Top_level.t * Top_level.Expanded.t) list) =
  [(input_1, output_1);
   (input_2, output_2);
   (input_3, output_3);
   (input_fully_inlined, output_fully_inlined);
  ]
;;
