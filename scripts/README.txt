This file contains scripts for basic analysis and visualization of the data.

All of these scripts require a preprocessed dataset containing demographic and results data for CPS students, as well as indicators of whether those students were involved with collaboration partners (and how).

The script `scramble_data.R` is designed for demonstrations and other purposes where results should be masked.  This script takes the true data and switches treatment/control status for a random subset of the population.  The script also masks site names.  The resulting data is not useful for drawing conclusions about program effectiveness, but can be used to demonstrate analysis tools without sharing sensitive results.

`create_summary_stats.R` synthesizes the available data points into subgroup averages (for continuous variables) or proportional frequencies (for categorical variables).  These calculated values are used in subsequent analysis.

The final script, `analyze-and-report.R`, contains most of the analysis, including scripts to generate an array of graphs (using ggplot2) and some basic regression estimates.