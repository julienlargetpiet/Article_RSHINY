#!/usr/bin/env bash

set -e

rm -rf plots

mkdir plots

mkdir plots/datatable_fread_plots      
mkdir plots/datatable_mem_readr_plots  
mkdir plots/datatable_readr_plots  
mkdir plots/dplyr_fread_plots      
mkdir plots/dplyr_mem_readr_plots  
mkdir plots/dplyr_readr_plots
mkdir plots/datatable_mem_fread_plots  
mkdir plots/datatable_mem_vroom_plots  
mkdir plots/datatable_vroom_plots  
mkdir plots/dplyr_mem_fread_plots  
mkdir plots/dplyr_mem_vroom_plots  
mkdir plots/dplyr_vroom_plots

echo "datatable readr"

export BENCH_PLOTS="plots_datatable_readr_var.R"

Rscript bench_analysis.R

echo "datatable fread"

export BENCH_PLOTS="plots_datatable_fread_var.R"

Rscript bench_analysis.R

echo "datatable vroom"

export BENCH_PLOTS="plots_datatable_vroom_var.R"

Rscript bench_analysis.R



echo "dplyr readr"

export BENCH_PLOTS="dplyr_variant/plots_dplyr_readr_var.R"

Rscript bench_analysis.R

echo "dplyr fread"

export BENCH_PLOTS="dplyr_variant/plots_dplyr_fread_var.R"

Rscript bench_analysis.R

echo "dplyr vroom"

export BENCH_PLOTS="dplyr_variant/plots_dplyr_vroom_var.R"

Rscript bench_analysis.R



