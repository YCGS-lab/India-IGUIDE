#!/bin/sh -l
#SBATCH --job-name=zonal_jm
#SBATCH --output=log/zonal_stats_%j.txt
#SBATCH --error=log/zonal_stats_%j.txt
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=01:00:00
#SBATCH --mem=8G
#SBATCH -p shared
#SBATCH -A cis250634

# Ensure log directories exist
mkdir -p logs_out logs_err

# Load environment
source ~/.bashrc
mamba activate ptenv

# Run the Python script
python zonal_stats_jm.py

mamba deactivate
