#!/usr/bin/env bash

set -e

rm -rf plots/dplyr_vroom_plots
rm -rf plots/dplyr_mem_vroom_plots

mkdir plots/dplyr_vroom_plots
mkdir plots/dplyr_mem_vroom_plots


echo "dplyr vroom"

export BENCH_PLOTS="dplyr_variant/plots_dplyr_vroom_var.R"

Rscript bench_analysis.R



