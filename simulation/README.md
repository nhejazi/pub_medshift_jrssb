To reproduce the results of the simulation study reported in the manuscript,
examine the batch scripts in the `slurm` subdirectory. Equivalently, the
following may be run from an R session started from the same directory in which
this file is located

```r
install.packages("here")
library(here)
source(here("R", "00_install_pkgs.R"))
source(here("R", "03_run_simulation_param.R"))
```

The simulation study was originally run on 15 December 2018, prior to posting of
the preprint and submission to JRSSB. At that time, R version 3.5.1 was used on
[UC Berkeley's Savio
cluster](https://research-it.berkeley.edu/services/high-performance-computing/system-overview).
