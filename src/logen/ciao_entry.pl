:- module(ciao_entry,_).

/* Main entry file for Logen */
/* use ciaoc -S ciao_entry.pl  to create a stand-alone version of Logen */

:- use_module('tools/error_manager.pl', [add_error/3, add_message/4,add_exception/4,
             count_errors_occured/1,set_verbosity_level/2]).
:- use_module('cogen.pl', [cogen_run/3, set_cogen_relative_dir/1, check_specialisation_query/1]).
:- use_module('tools/ciao_tools.pl', _).

:- use_module('bta/simple_bta.pl').

:- use_module('old_logen/logen_messages').

:- use_module(library(pathnames),[path_basename/2]).

:- include(runtime_checks_perform).

/* ------------------- */
/* main/1: ENTRY POINT */
/* ------------------- */

main(Args) :-
	catch(go(Args),E,
	     (add_exception(ciao_entry, "Toplevel exception: ~n",[],E),
	      halt(1))).

%% Simple bta entry point

go(ArgV) :-
	get_options(ArgV,Opts,_),
	(member(verbose_mode(Level),Opts)
	  -> set_verbosity_level(Level,_)
	  ;  set_verbosity_level(1,_) %% default print
	),
	(member(logen_dir(Dir),Opts) -> set_cogen_relative_dir(Dir); true),
	fail.
  
go(ArgV) :-
	get_options(ArgV,Opts,[]),
	(member(help,Opts) -> fail; true),
	member(simplebta(Type, PlFile,AnnFile),Opts),
	add_message(ciao_entry,2, "Annotate Options: ~w~n",[Opts]),
	!,
	annotate_file(Type, PlFile, AnnFile,[]).

	

go(ArgV) :-	
	get_options(ArgV,Opts,[FileName|AX]),
	add_message(ciao_entry,2, "Options: ~w",Opts),
	(member(help,Opts) -> fail; true),
	generate_gx_file(FileName,Opts,GXFile),
	
	(member(cogen_only, Opts) ->
	 true
	;
	   ( (AX = [Query|RestArgv] ->	     
	     get_adapted_query(Query,AQ,RestArgv)
	    ;
	     (print(user, 'Query to specialise =>'), 
	      read(AQ)
             )
         ),
         check_specialisation_query(AQ),
	     (delete(Opts,single_process,ROpts1) ->
	       specialise_using_gx_from_module(GXFile, AQ,ROpts1)
	     ;
	       specialize_using_gx(GXFile,AQ,Opts)
	     )
        )
    ),
	count_errors_occured(NrOfErrors),
	(NrOfErrors>0
	 -> (nl,print('*** Warning: '), print(NrOfErrors), print(' Error(s) occurred!'),nl,
	     halt(1))
	 ; true).		      		
go(_) :- print_usage.



get_adapted_query(Query,AQ,RestArgv) :-
  catch(read_from_chars(Query,Q),E,
        (format(user_error,"*** Specialization query has a syntax error.~n*** Query: '~w'~n",[Query]),
         add_exception(ciao_entry, "Exception while parsing specialization goal: ~n",[],E),
	     halt(1))),
  adapt_query(Q,AQ,RestArgv).
  
 
 

print_usage :-
	usage(Msg),
	print(user_error,Msg),nl,
    print(user_error,'  Possible Options are:'),nl,
    print_options.           
         %print(user_error,'  Note: the .pl and the . after Atom are optional'),nl.

print_options :- 
   recognised_option(Opt,_,Args,Msg),
   print(user_error,'      '), print(user_error,Opt),
   print_option_args(Args,1),
   print(user_error,': '), print(user_error,Msg),nl,
   fail.    
print_options.

print_option_args([],_).
print_option_args([_|T],N) :- print(user_error,' ARG'),print(user_error,N),
  N1 is N+1, print_option_args(T,N1).


/* ------------------- */


get_ann_filename(File, AnnFile) :-
	atom_concat(File, '.ann', AnnFile).

get_gx_filename(File, GxFile) :-
	atom_concat(File, '.gx', GxFile).


generate_gx_file(File,Options,GXFile) :- 
	add_message('ciao_entry', 2,"Generating GX File~n", []),
	%% Get ann and gx name
	get_ann_filename(File, AnnFile),
	
	(member(gx_file(GXFile), Options) ->
	 true
	;
	    get_gx_filename(File,  GXFile)
	),	
	statistics(runtime,[T1,_]),	
	mnf(cogen_run(AnnFile,GXFile,Options)),
	add_message('ciao_entry', 2,"done~n", []),
	statistics(runtime,[T2,_]),
	Time is T2-T1,
	add_message(cogen_entry,2, "Finished running cogen in ~w ms (runtime)",[Time]),
	%% compile by default now ciao and compiled ciao behaviour can differ!!
	
        (member(single_process,Options) ->
	 add_message(ciao_entry,2, "No compilation - single process mode",[])
	;(
          add_message(ciao_entry,2, "Compiling GX File",[]),
	  (member(ciao_path(CiaoPath), Options) ->
		atom_concat(CiaoPath, '/ciaoc', Ciaoc)
	  ;
	      Ciaoc = 'ciaoc'
	  ),	
	  format_to_chars("~w -o ~w.cpx ~w", [Ciaoc,GXFile,GXFile], CompileCmdS),
	  add_message(ciao_entry,3, "cmdLine: ~s",[CompileCmdS]),
	  name(CompileCmd, CompileCmdS),
	  system(CompileCmd,R1),
	  (R1 = 0 ->
	    true
	  ;
	    add_error(ciao_entry, "Compiling gx file failed",[]),
	    fail
	 ))).
generate_gx_file(File,Options,GXFile) :-
	add_error(ciao_entry, "Generation of GX File failed", []),
	add_error(ciao_entry, " GX call: ~w", [generate_gx_file(File,Options,GXFile)]),
	halt.


specialise_using_gx_from_module(GXFile, Query,Opts) :-
	use_module(GXFile,[main_gx/1]),
	path_basename(GXFile,Module),
	 (member(spec_file(File),Opts) ->
	     Args = [Query,'-o',File|Opts]
	;
	    Args = [Query|Opts]
	),	
	
	copy_term(Query, QueryC),
	prettyvars(QueryC),
	add_message(ciao_entry,2, "Calling Module directly",[]),
	add_message(ciao_entry,2, "Specialising for ~w",[QueryC]),
	
	(Module:main_gx(Args)->
	 true;
	 (add_error(ciao_entry,"Executing gx module file failed",[]),
	          halt(1))
	).
	
	

%%% move the compile gx code out of here, for the compile when not spec
specialize_using_gx(GXFile,Query,Opts) :-	
	%(member(compile_gx, Opts) ->
	%% now always compile!
	name(GXFile,GXString),
	(GXString=[47|_]  /* starts with /: no need to prefix with . */
	 -> format_to_chars("~w.cpx '~w'", [GXFile,Query], SpecCmdS)
	 ;  format_to_chars("./~w.cpx '~w'", [GXFile,Query], SpecCmdS)
	),

	 (member(spec_file(File),Opts) ->
	 format_to_chars("~s -o ~w", [SpecCmdS, File],CmdS)
	;
	    CmdS = SpecCmdS
	),
	name(Cmd, CmdS),
	copy_term(Query, QueryC),
	prettyvars(QueryC),
	add_message(ciao_entry,2, "Calling Compiled File",[]),
	add_message(ciao_entry,2, "Specialising for ~w",[QueryC]),
	add_message(ciao_entry,3, "Cmd line: ~w",[Cmd]),	
	system(Cmd, R),
	(R==0 -> true
	      ;  (add_error(ciao_entry,"Executing gx file failed with error: ~w~n",[R]),
	          halt(1))
	).




%	popen('bash', write, Cmd),
	
%	format(Cmd, "ciao ~w '~w'~Nexit", [GXFile, Query]),
%	close(Cmd).
%   build_specialization_entry_call(Query,_RenamedQuery,Call),
%   translate_term_into_atom(Call,CallString),
%   string_concatenate(CallString,',halt."',C1),
%   string_concatenate(' --goal "',C1,C2),
%   string_concatenate(GXFile,C2,C3),
%   string_concatenate('sicstus -l ',C3,RUNGX),
%   print('sicstus -l '), print(GXFile), 
%   print(' --goal "'), print(Call), print('."'),nl,
%   statistics(walltime,[T1,_]),
%   system(RUNGX),
%   statistics(walltime,[T2,_]),
%   (member(silent,Opts) -> true ; (print('Finished specialization in '),Time is T2-T1,print(Time), print(' ms (walltime).'),nl)),!.
%specialize_using_gx(GXFile,Query,Opts) :-
%   print('*** ERROR: Specialization failed!'),nl,
%   print('***        call: '), print(specialize_using_gx(GXFile,Query,Opts)), nl.
   
   
% add a dot at the end; in case user forgets
add_dot([],".").
add_dot(".",".") :- !.
add_dot([A|T],[A|R]) :- add_dot(T,R).

get_options([],[],[]).
get_options([X|T],Options,OtherArgs) :-
   (recognised_option(X,Opt,Values,_) ->       
       ( append(Values, Rest, T), RT = Rest,  /* extract args and insert into Opt by shared var*/
	     Options = [Opt|OT], OtherArgs = AT )   
   ;
       ( Options = OT,     OtherArgs = [X|AT],
	     RT = T )
   ),
   get_options(RT,OT,AT).


usage('Usage: logen [Options] File.pl ["Atom."]').
recognised_option('--help',help,[],'Prints this message').
recognised_option('-w',watch(default),[],'watch mode (supervise specialization); equivalent to -wb -wu -wm -wc').
recognised_option('-W',watch_once(default),[],'watch mode; like -w option but halt at first alarm (-w -w1)').
recognised_option('-wb',watch(builtins),[],'  watch built-ins').
recognised_option('-wp',watch(backpropagations),[],'  watch for back-propagation of bindings onto non-declarative builtins').
recognised_option('-wc',watch(connectives),[],'  watch connectives (if,or) for back-propagation of bindings').
recognised_option('-wu',watch(unfold),[],'  watch out for infinite unfoldings').
recognised_option('-wm',watch(memo),[],'  watch out for infinite memoisations').
recognised_option('-w1',watch(once),[],'  force halt at first watchdog alarm').
recognised_option('-c',cogen_only,[],'run cogen only (not the .gx file)').
recognised_option('-s',verbose_mode(0),[],'run silently (do not display specialized program,...)').
recognised_option('--safe',sandbox_mode,[],'run gx in safe sandbox mode').
recognised_option('-v',verbose_mode(2),[],'print debugging messages').
recognised_option('-vv',verbose_mode(3),[],'print more debugging messages').
recognised_option('-vvv',verbose_mode(4),[],'print even more debugging messages').
recognised_option('--compile_gx',compile_gx,[],'compile the gx file using ciaoc').
recognised_option('-m',memo_table,[],'display memo table').
recognised_option('-np',no_post_unfold,[],'no post-unfolding').
recognised_option('-ap',aggressive_post_unfold,[],'aggressive post-unfolding').
recognised_option('-g',display_gx_file,[],'display gx file').
recognised_option('--logen_dir', logen_dir(Dir),[Dir],'path to logen files'). 
recognised_option('-o', gx_file(Dir),[Dir],'GX filename'). 
recognised_option('--spec_file', spec_file(File),[File],'Spec filename'). 
recognised_option('--ciao_path', ciao_path(CiaoPath),[CiaoPath],'Ciao binary directory'). 
%recognised_option('--prolog', prolog(System),[System],'Target Prolog for GX file (sicstus,xsb,ciao)'). 
recognised_option('--simple_bta',simplebta(Type,Pl,Ann),[Type,Pl,Ann],'Run simple bta: --simple_bta unfold/memo PLfile AnnFile').
%recognised_option('-k',keep_memo,[]).
%recognised_option('-r',rebuild_everything,[]).
%recognised_option('-i',no_interface_clauses,[]).
recognised_option('-d',debug_mode,[],'debug mode for GX file').

recognised_option('-d2',debug_mode_full,[],'even more debugging messages in GX file').

recognised_option('--single_process',single_process,[],'run cogen and gx in a single process - faster').
recognised_option('--xml',xml_report_mode,[],'generate (some) diagnostic messages in xml').


%recognised_option('-xsb',xsb_mode,[]).
%recognised_option('-t', timeout(T), [T]).
%recognised_option('-errorlog', error_log(File), [File]).



/* replace argv(Nr) by argument from command line */
adapt_query(X,V,_Argv) :- var(X),!,X=V.
adapt_query(argv(Nr),Res,Argv) :- (nth(Nr,Argv,Res) -> true
                                                    ; (print_debug_message('Unspecified argument'),Res=argv(Nr))),!.
adapt_query(X,Res,Argv) :- nonvar(X), X=..[Func|Args],!,l_adapt_query(Args,AArgs,Argv), Res =..[Func|AArgs].
adapt_query(X,X,_).

l_adapt_query([],[],_).
l_adapt_query([A|T],[AA|AT],Argv) :- adapt_query(A,AA,Argv), l_adapt_query(T,AT,Argv).



show_file(File) :-
      open(File, read, Stream),
      (show_stream(Stream) -> true ; true),
      close(Stream).
show_stream(Stream) :- get_char(Stream,Char),
    ((Char = end_of_file)
      -> true
      ; (put_char(Char),
         show_stream(Stream)
        )
    ).




%% Needs to be mapped back to cogen..
%build_specialization_entry_call(Query,_RenamedQuery,Call).


%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

