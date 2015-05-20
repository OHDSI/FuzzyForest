## Test environments
* local OS X install, R version 3.2.0 (2014-10-31)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTES:
* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Daniel Conn <djconn17@gmail.com>'
  New submission
* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped

After changing my ~/.Rprofile to include 
"options(repos = c(CRAN="http://cran.r-project.org"))"
the second note went away for the local test 
environment.
