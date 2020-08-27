## New package notice
This is a new package submission

## Test environments
* windows-latest (release) - github actions
* macOS-latest (release) - github actions
* ubuntu-20.04 (release and devel) - github actions
* Windows Server 2008 R2 SP1 (devel) - rhub
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs. 

There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
   Maintainer: 'Spencer Nystrom <nystromdev@gmail.com>'
   New submission
   
   This is a new package submission

* checking for future file timestamps ... NOTE
  unable to verify current time
  
  This seems to be a duplicate of: https://stat.ethz.ch/pipermail/r-package-devel/2019q1/003577.html
  which suggests this is a bug in R-core in how it contacts the world time API (the time API likely changed).
  Setting _R_CHECK_SYSTEM_CLOCK_=0 removes the note on all tested platforms.

## Downstream dependencies
There are no downstream dependencies of this package
