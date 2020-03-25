# from CRAN
install.packages(c("here", "tidyverse", "remotes", "future", "future.apply",
                   "doFuture", "data.table", "Rsolnp", "nnls", "speedglm",
                   "glmnet", "ranger", "xgboost", "Rcpp", "mma", "origami"))

# use remotes to install from GitHub
remotes::install_github(c("osofr/simcausal@master",
                          "osofr/condensier@master",
                          "nhejazi/npcausal@master",
                          "tlverse/sl3@ceba727",
                          "tlverse/hal9001@783d3aa",
                          "nhejazi/medshift@jrssb"))
