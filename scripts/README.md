This file contains scripts for basic analysis and visualization of the data.

All of these scripts require a preprocessed dataset containing demographic and results data for CPS students, as well as indicators of whether those students were involved with collaboration partners (and how).

The script `scramble_data.R` is designed for demonstrations and other purposes where results should be masked.  This script takes the true data and switches treatment/control status for a random subset of the population.  The script also masks site names.  The resulting data is not useful for drawing conclusions about program effectiveness, but can be used to demonstrate analysis tools without sharing sensitive results.

`create_summary_stats.R` synthesizes the available data points into subgroup averages.  These calculated values are used in subsequent analysis. This script is relatively slow, and we have considered optimizing the code using the data table package (`sandbox-for-data-table-for-summary-stats.R` includes our explorations of that option).

`gen-viz.R` generates a suite of standardized visualizations by organization and site.  It also contains a function that will generate a wide variety of other visuals with just a few parameters.

The final script, `run_regs.R`, contains the code to run and graph some basic regression analyses.