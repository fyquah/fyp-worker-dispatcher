open Core
open Async
open Tensorflow
open Protocol.Shadow_fyp_compiler_lib
open Common

module Data :
  sig
    type classification =
      { features : Owl.Mat.mat;
        labels   : int array;
        targets  : Owl.Mat.mat;
      }
  
    type regression =
      { features : Owl.Mat.mat;
        targets  : Owl.Mat.mat;
      }
  
    type _ t =
      | Classification : classification -> [ `classification ] t
      | Regression     : regression     -> [ `regression ] t
  
    val features : _ t -> Owl.Mat.mat
    val targets  : _ t -> Owl.Mat.mat
    val labels   : [ `classification ] t -> int array
    val num_classes   : [ `classification ] t -> int
  end


module Epoch_snapshot :
  sig
    type entry = {
      accuracy : float sexp_option;
      loss     : float;
    }
    [@@deriving sexp]

    type t = {
      epoch      : int;
      training   : entry option;
      validation : entry option;
      test       : entry option;
    }
    [@@deriving sexp]
  end

type hyperparams =
  { l2_reg  : float sexp_option;
    dropout_keep_prob : float sexp_option;
  }
[@@deriving sexp]

type node = [ `float ] Tensorflow.Node.t
type placeholder = [ `float ] Ops.Placeholder.t
type tf_model = 
  { input_placeholder       : placeholder;
    target_placeholder      : placeholder;
    is_training_placeholder : [ `int32 ] Ops.Placeholder.t;
    output                  : node;
    vars                    : node list;
    loss                    : node;
    gd                      : Tensorflow.Node.p list;
  }

type 'a model = 
  { tf_model                         : tf_model;
    create_normalised_feature_vector : [ `raw ] Features.t -> Owl.Mat.mat;
    training                         : 'a Data.t;
  }

type prediction =
  { network_output: Owl.Mat.mat;
    loss : float;
  }

val predict
   : tf_model
  -> session: Session.t
  -> target_matrix: Owl.Mat.mat
  -> Owl.Mat.mat
  -> prediction

val gen_snapshot_entry
   : session: Session.t
  -> 'a model
  -> 'a Data.t
  -> Epoch_snapshot.entry

val train_model
   : model: 'a model
  -> epochs: int
  -> test_data: 'a Data.t
  -> validation_data: 'a Data.t
  -> dump_graph: string option
  -> checkpoint: string option
  -> unit Deferred.t


val target_matrix_of_labels : num_classes: int -> int array -> Owl.Mat.mat
