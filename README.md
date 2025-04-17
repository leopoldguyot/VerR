<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/leopoldguyot/VerR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/leopoldguyot/VerR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# VerR

VerR offers tools to run expression or scripts inside custom R environments.
These environments are created with the help of the `renv` package.
Using these environments, it is therefore possible to easily compare 
the behavior of different versions of a package.

## Installation

Get the package:

```r
# Check if remotes is installed. Otherwise install it.
if (!require("remotes", quietly = TRUE)){
    install.packages("remotes")
}
# Install the package
remotes::install_github("leopoldguyot/VerR",
    build_manual = TRUE,
    build_vignettes = TRUE
)
# Load the package
library(VerR)
```

## Usage

### Create Environments

When a new environment is initialized, a new directory is created inside the
`.envs/` directory. This new directory will contain all the installed packages 
specific to this environment. The creation of a new environment goes like this:

``` r
envCreate(envName = "env_name",
          packages = c("tidyr", # CRAN
                       "digest@0.6.36", # CRAN with specific ver.
                       "bioc::BioBase", # BioConductor
                       "leopoldguyot/VerR" # GitHub
                       )    
)
```

To specify the packages you want to install within the environment you can use
different syntax:

- `pkg`: install latest version of pkg from CRAN.

- `pkg@version`: install specified version of pkg from CRAN.

- `username/repo`: install package from GitHub. You can also specify the commit
 with `username/repo@commitId`

- `bioc::pkg`: install pkg from Bioconductor.

Note that to install a specific version of a Bioconductor package it is
recommended to install it via GitHub.

You can find more information on package installation within the `renv`
documentation:

``` r
?renv::install
```

### Manage your environments

``` r
# get the names of your environments
envList()

# remove an environment
envDelete(envName = "env_name")

# remove all your environments
envDelete()

# copy a file/dir from your working directory to environments
envCopyTo(sourcePath = "path",
           envName = envList(),
           targetPath = "" # Where in the env should the file/dir be copied 
)

# remove a file/dir from your environments
envRemoveFrom(targetPath = "path",
               envName = envList()
)
```
### Run expression within environments

To run an expression within an environment you can directly pass your 
expression to the `runInEnv` function. Note that you need to call `library()`
within your expression if you want to use packages installed in your 
environment.

``` r
runInEnv(expr = {
        packageVersion("digest")
    },
    envName = "env_name"
)
```

If you run this expression within the environment created earlier 
you should get:

``` r
"0.6.18"
```

You can also use `runInEnv` to evaluate your expression
within multiple environments.

``` r
runInEnv(expr = {
        packageVersion("digest")
    },
    envName = envList()
)
```

This will return a named list with the return value for each 
environment.
