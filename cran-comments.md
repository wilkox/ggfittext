# Test environments

* local OS X install, R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.1.3, 3.2.5, 3.4.4, 3.5.0 and R-devel
* win-builder (with `devtools::build_win`)

# R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

# Downstream dependencies

R CMD check run on downstream dependencies with `devtools::revdep_check`, no
problems found except for building package 'breathtestcore' which required
external libraries unavailable at the time of testing.
