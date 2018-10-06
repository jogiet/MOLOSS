(** CmdLine parser module *) 

  
  let axs = ref []

  let toto () = ()

  let add_axs ax () = 
    axs := ax::(!axs)

  let optAll = ref false
  let optZ3 = ref false
  let optmSAT = ref false
  let optGetAssert = ref false
  let optGetModel = ref false
  let optDirect = ref false
  let optGetLog = ref false
  let optSoft = ref false
  let optSoftIgnore = ref false
  let optGetSimplify = ref false
  let optNoSimplify = ref true
  let optTime = ref false

  let args = 
      (* For axioms *)
      ["-S", Arg.Unit (add_axs Ast_modal.AxS ), " Use reflexivity axiom"; 
       "-B", Arg.Unit (add_axs Ast_modal.AxB ), " Use symmetry axiom"; 
       "-4", Arg.Unit (add_axs Ast_modal.Ax4 ), " Use transitivity axiom"; 
       "-5", Arg.Unit (add_axs Ast_modal.Ax5 ), " Use euclidian axiom"; 
       "-CD", Arg.Unit (add_axs Ast_modal.AxCD ), " Use functionnal axiom";
       (* For Options *)
       "--all", Arg.Set optAll, 
          " Use all SAT oracles (minisat, z3, mSAT";
       "--z3", Arg.Set optZ3, 
          " Use z3 as SAT oracle";
       "--mSAT", Arg.Set optmSAT, 
          " Use mSAT as SAT oracle";
       "--get-model", Arg.Set optGetModel, 
          " Print the Kripke model \
          if the formula is satisfiable \
          in the Flat Kripke Model (FKM) format";
       "--get-assert", Arg.Set optGetAssert, 
          " Print the assertions made by the solver \
          and the final model if the formule is satisfiable";
       "--direct", Arg.Set optDirect, 
          " Use Z3 as a first-order solver \
          on the translation of modal formulas \
          into first-order formulas (no instanciation \
          procedures are used)";
       "--get-log", Arg.Set optGetLog, 
          " Print the log of interaction \
          with the z3 SMT solver (only for `direct` mode)";
       "--time", Arg.Set optTime, 
          "Print the time needed to resolve the problem";
      "--soft", Arg.Set optSoft, 
          " Use `assert-soft` constraints, even if not needed by \
          the considered modal logic";
      "--soft-ignore", Arg.Set optSoftIgnore,
          " Do not use `assert-soft` constraints, \
          even if needed (beware, infinite loop possible in this case!)" ] 

  let usage = "usage : moloss.native file.intohylo [axioms] [options]"

  let print_usage () = Arg.usage args usage

  let file = ref None
  
  let anon_fun s =
    (* We assume that filename does not begin with '-' !*)
    if String.get s 0 = '-' then
    begin
      print_usage ();
      exit 1;
    end
    else
      file := Some s

  let init () = Arg.parse args anon_fun usage

