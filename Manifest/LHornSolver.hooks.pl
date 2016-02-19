:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for LHornSolver").

'$builder_hook'(desc_name('LHornSolver')).

% ============================================================================

:- use_module(ciaobld(ciaoc_aux), [build_libs/2]).

'$builder_hook'(build_libraries) :-
	build_libs('LHornSolver', 'src').

'$builder_hook'(build_bin) :-
	bundleitem_do(lhornsolvercl, 'LHornSolver', build_nodocs).

% TODO: just say cmd('cmds/lhornsolvercl', [...])
'$builder_hook'(lhornsolvercl:item_def( 
    cmds_list('LHornSolver', bundle_src('LHornSolver')/'src', [
        'lhornsolver'-[
          output='lhornsolver', % (executable will be called 'lhornsolver')
	  plexe,
	  final_ciaoc
	]
    ]))).

'$builder_hook'(install) :- bundleitem_do(only_global_ins(~desc), 'LHornSolver', install).

'$builder_hook'(uninstall) :- bundleitem_do(only_global_ins(~desc), 'LHornSolver', uninstall).

desc := [
  lhornsolvercl,
  lib('LHornSolver', 'src')
].
