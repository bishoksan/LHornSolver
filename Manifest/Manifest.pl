:- bundle('LHornSolver').
version('1.0').
depends([
    core-[version>='1.18'],
    chclibs,
    'github.com/jfmc/ciao_yices',
    'github.com/jfmc/logen'
]).
alias_paths([
    lhornsolver = 'src'
]).
%
cmd('src/lhornsolver').
%
lib('src').


