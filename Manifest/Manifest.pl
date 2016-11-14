:- bundle('LHornSolver').
version('1.0').
depends([
    core,
    chclibs,
    ciao_yices,
    logen
]).
alias_paths([
    lhornsolver = 'src'
]).
%
cmd('src/lhornsolver').
%
lib('src').


