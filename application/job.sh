#!/bin/bash
# Job name:
#SBATCH --job-name=mediation_example
#
# Working directory:
#SBATCH --workdir=/global/home/users/nhejazi/
#
# Partition:
#SBATCH --partition=savio2

# Account:
#SBATCH --account=co_biostat
#
# Processors (1 node = 20 cores):
#SBATCH --nodes=1
#SBATCH --exclusive
#
# Wall clock limit ('0' for unlimited):
#SBATCH --time=24:00:00
#
# Mail type:
#SBATCH --mail-type=all
#
# Mail user:
#SBATCH --mail-user=nhejazi@berkeley.edu
#
# Job output:
#SBATCH --output=slurm.out
#SBATCH --error=slurm.out
#

## Command(s) to run:
module load gcc/6.3.0 r r-packages
cd ~/medshift-meta/application/
R CMD BATCH R/install_pkgs.R logs/install_pkgs.Rout
R CMD BATCH R/simple_example.R logs/simple_example.Rout
