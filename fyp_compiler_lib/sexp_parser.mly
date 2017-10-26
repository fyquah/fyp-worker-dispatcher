%token <string> STRING
%token LEFT_BRACE
%token RIGHT_BRACE
%token EOF

%start <Sexp.t option> prog
%%

prog:
  | EOF { None }
  | v = value { Some v }
  ;

value:
  | LEFT_BRACE; obj = sexp_list; RIGHT_BRACE { Sexp.List obj }
  | s = STRING { Sexp.Atom s }
  ;

sexp_list: obj = rev_sexp_list { List.rev obj };

rev_sexp_list:
  | (* empty *) { [] }
  | tl = rev_sexp_list; hd = value { hd :: tl }
  ;
