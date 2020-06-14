# Test environments

* local OS X install, R 4.0.0
* ubuntu 16.04.6 (on travis-ci), 3.6.3, 4.0.0, and R-devel
* win-builder (with `devtools::check_win_release()`)

# R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

# Downstream dependencies

R CMD check run on downstream dependencies with `revdepcheck::revdep_check()`.
Two reverse dependencies (metacoder and ribosomeProfilingQC) generated errors
but these do not seem to be related to changes to ggfittext.
