# Contributing to lczexplore
This explains how to propose a change to lczexplore.

### Fixing Typos
Small typos or grammatical errors in documentation may be edited directly using
the GitHub web interface, so long as the changes are made in the _source_ file.

*  YES: you edit a roxygen comment in a `.R` file below `R/`.
*  NO: you edit an `.Rd` file below `man/`.

### Before a Pull requests
Before you make a substantial pull request, you should always file an issue and
make sure someone from the team agrees that it’s a problem. If you’ve found a
bug, create an associated issue and illustrate the bug with a reproducible example. 

### Pull request process

*  We recommend that you create a Git branch for each pull request (PR).  
* We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html), 
for documentation. 
* We use tinytest https://github.com/markvanderloo/tinytest for tests. Contributions
with test cases included are easier to accept.

### Code of Conduct
Please note that the lczexplore package is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.