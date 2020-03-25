# lie to pkgbuild
pkgbuild:::cache_set("has_compiler", TRUE)

# from CRAN
install.packages(c("here", "tidyverse", "remotes", "future", "future.apply",
                   "doFuture", "foreach", "data.table", "Rsolnp", "nnls",
                   "origami", "speedglm", "glmnet", "ranger", "xgboost",
                   "Rcpp"),
                 lib = "/global/scratch/nhejazi/R")

# use remotes to install from GitHub
remotes::install_github(c("osofr/simcausal@master",
                          "osofr/condensier@master",
                          "tlverse/sl3@ceba727",
                          "tlverse/hal9001@783d3aa",
                          "nhejazi/medshift@jrssb"),
                        lib = "/global/scratch/nhejazi/R")

# update all packages
update.packages(lib.loc = "/global/scratch/nhejazi/R")
