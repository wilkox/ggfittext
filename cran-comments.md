# R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Test environments

- local install: macOS 15.7.2, R 4.5.2
- win-builder:
  - with `devtools::check_win_release()`: R 4.5.2
  - with `devtools::check_win_devel()`): R unstable 2025-12-10 r89137 ucrt
- Using GitHub Actions:
  - macos-latest: macOS 15.6.1, R 4.5.1
  - windows-latest: Microsoft Windows Server 2025 10.0.26100, R 4.5.1
  - ubuntu-latest (devel): Ubuntu 24.04.3 LTS, R unstable 2025-09-30 r88885
  - ubuntu-latest (release): Ubuntu 24.04.3 LTS, R 4.5.1
  - ubuntu-latest (oldrel-1): Ubuntu 24.04.3 LTS, R 4.4.3

## revdepcheck results

We checked 25 reverse dependencies (23 from CRAN + 2 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

- We saw 0 new problems
- We failed to check 0 packages
