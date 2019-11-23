# from CRAN
install.packages(c("here", "tidyverse", "remotes", "future", "future.apply",
                   "doFuture", "data.table", "Rsolnp", "nnls", "speedglm",
                   "glmnet", "ranger", "xgboost", "Rcpp", "mma"))

# use Bioconductor's biocLite utility to install from GitHub
source("https://bioconductor.org/biocLite.R")
biocLite(c("osofr/simcausal@master",
           "osofr/condensier@master",
           "tlverse/origami@master",
           "tlverse/sl3@master",
           "tlverse/hal9001@master",
           "osofr/condensier@master",
           "nhejazi/npcausal@master"
         ))

# install medshift package from tarball
install.packages("~/medshift_0.0.8.tar.gz")

