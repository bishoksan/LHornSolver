% Manifest file for LHornSolver
bundle_name('LHornSolver').
bundle_packname('LHornSolver').
bundle_requires([
    core,
    chclibs,
    ciao_yices
]).
bundle_alias_paths([
    lhornsolver = 'src'
]).


