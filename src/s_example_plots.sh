#!/bin/bash

chmod +x *.R
chmod +x *.sh

current_directory="~/Desktop/pints-changepoints/src"
data_directory="../cleaned"
output_directory="../outputs"

# makes raw plot have logged y axis
plot_raw_logged=1
# plots estimated changepoint levels on raw plot
plot_raw_changepoints=1
# adds smoothed regression line to each plot
plot_smooth=1
# minimum number of tests between which a changepoint can occur
changepoints_run_length=10
# number of tests either side of identified recent changepoint to search for relevant commits
changepoints_num_tests_commits=10

# threshold at which (below) a test score is considered a pass
plot_pass_threshold=1

data_filename="mcmc_normal_HamiltonianMCMC_1"
output_filename_base="hmc_test"
Rscript s_plot_and_changepoint_test.R $current_directory $data_directory $data_filename $output_directory \
$output_filename_base $plot_raw_logged $plot_raw_changepoints $plot_smooth \
$plot_pass_threshold $changepoints_run_length $changepoints_num_tests_commits

data_filename="mcmc_banana_AdaptiveCovarianceMCMC_1"
output_filename_base="acmcmc_test"
Rscript s_plot_and_changepoint_test.R $current_directory $data_directory $data_filename $output_directory \
$output_filename_base $plot_raw_logged $plot_raw_changepoints $plot_smooth \
$plot_pass_threshold $changepoints_run_length $changepoints_num_tests_commits

data_filename="mcmc_normal_PopulationMCMC_1"
output_filename_base="populationmcmc_test"
Rscript s_plot_and_changepoint_test.R $current_directory $data_directory $data_filename $output_directory \
$output_filename_base $plot_raw_logged $plot_raw_changepoints $plot_smooth \
$plot_pass_threshold $changepoints_run_length $changepoints_num_tests_commits

changepoints_run_length=100
data_filename="mcmc_banana_DreamMCMC_3"
output_filename_base="dream_test"
Rscript s_plot_and_changepoint_test.R $current_directory $data_directory $data_filename $output_directory \
$output_filename_base $plot_raw_logged $plot_raw_changepoints $plot_smooth \
$plot_pass_threshold $changepoints_run_length $changepoints_num_tests_commits

changepoints_run_length=150
plot_pass_threshold=4
data_filename="opt_fn_SNES"
output_filename_base="snes_test"
Rscript s_plot_and_changepoint_test.R $current_directory $data_directory $data_filename $output_directory \
$output_filename_base $plot_raw_logged $plot_raw_changepoints $plot_smooth \
$plot_pass_threshold $changepoints_run_length $changepoints_num_tests_commits

data_filename="opt_fn_XNES"
output_filename_base="xnes_fn_test"
Rscript s_plot_and_changepoint_test.R $current_directory $data_directory $data_filename $output_directory \
$output_filename_base $plot_raw_logged $plot_raw_changepoints $plot_smooth \
$plot_pass_threshold $changepoints_run_length $changepoints_num_tests_commits

data_filename="opt_logistic_SNES"
output_filename_base="xnes_logistic_test"
Rscript s_plot_and_changepoint_test.R $current_directory $data_directory $data_filename $output_directory \
$output_filename_base $plot_raw_logged $plot_raw_changepoints $plot_smooth \
$plot_pass_threshold $changepoints_run_length $changepoints_num_tests_commits