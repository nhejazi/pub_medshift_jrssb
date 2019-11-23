# lie to pkgbuild, as per Jeremy
pkgbuild:::cache_set("has_compiler", TRUE)

# from CRAN
install.packages(c("here", "tidyverse", "remotes", "future", "future.apply",
                   "doFuture", "foreach", "data.table", "Rsolnp", "nnls",
                   "speedglm", "glmnet", "ranger", "xgboost", "Rcpp"),
                 lib = "/global/scratch/nhejazi/R")

# use remotes to install from GitHub
remotes::install_github(c("osofr/simcausal@master",
                          "osofr/condensier@master",
                          "tlverse/origami@master",
                          "tlverse/sl3@master",
                          "tlverse/tmle3@master",
                          "tlverse/hal9001@master",
                          "nhejazi/medshift@master"),
                        lib = "/global/scratch/nhejazi/R")

# update all packages
update.packages(lib.loc = "/global/scratch/nhejazi/R")
