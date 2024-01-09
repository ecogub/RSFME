# An Assessment of Annual Load Estimation Methods in Small Watersheds for Cross Site Comparisons

## Structure
All scripts used in the paper are found in the /paper subfolder. Background scripts describing the flux estimation methods are present in the /src folder. The run order of the /paper subfolder is as follows:

### ts_simulation
Performs ARIMA fitting and time series simulation experiments.

### coarsen_plot
Perfroms data coarsening experiments on HBEF data.

### plynlimon_discussion
Reruns coarsen_plot experiements on Plynlimon dataset.

### macrosheds_application
Estimates loads for all appropriate MacroSheds sites.

### hbef_corr_exploration
Investigation and expoloration of HBEF data. Data underlying conversion of specific conductivity to calcium is found here.

### hbef_comparison_fig
Applies each method to HBEF dataset with high-frequency 'truth' as benchmark. 

### misc_figure_creation
Creates context, supporting plots, and CQ relationship plots for both HBEF and Plynlimon.

