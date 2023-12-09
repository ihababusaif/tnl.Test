## R CMD check results 

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... [12s] NOTE
  Maintainer: 'Ihab Abusaif <censtat@gmail.com>'
  
  New submission

❯ On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking math rendering: package 'V8' unavailable

❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

❯ On ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... [6s/16s] NOTE
  Maintainer: ‘Ihab Abusaif <censtat@gmail.com>’
  
  New submission
  
  Found the following (possibly) invalid URLs:
    URL: https://www.researchgate.net/profile/Ihab-Abusaif
      From: inst/doc/tnl_Test.html
            README.md
      Status: 403
      Message: Forbidden

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [7s/17s] NOTE
  Maintainer: ‘Ihab Abusaif <censtat@gmail.com>’
  
  New submission

0 errors ✔ | 0 warnings ✔ | 6 notes ✖
