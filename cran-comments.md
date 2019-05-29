# Test environments

* local OS X install, R 3.6.0
* ubuntu 16.04.6 (on travis-ci), R 3.2.5, 3.5.3, 3.6.0, and R-devel
* win-builder (with `devtools::check_win_release()`)

# R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

# Downstream dependencies

R CMD check run on downstream dependencies with `devtools::revdep_check()`, no
problems found.
