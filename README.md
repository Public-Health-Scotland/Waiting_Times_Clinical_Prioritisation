# Clinical Prioritisation

This repository contains the Reproducible Analytical Pipeline (RAP) to produce the quarterly statistics on clinical prioritisation, part of the [Stage of Treatment (SoT) publication](https://publichealthscotland.scot/publications/nhs-waiting-times-stage-of-treatment/).

The first release of the publication is scheduled for 06 September 2022. This page will be updated as the development of the outputs continues.

## Source files

Data from BOXI must be processed for use in the shiny app and Excel output, with the data being saved as .csv in [/data/processed data]. Currently this process is done at the start of the `CP-publication-prep.R` script but it should be transferred to a separate script. Note that the filenames match the df names used in section 3 of `CP-publication-prep.R` but should be made more user-friendly as part of the planned script updates. The csv files are:

* addrem_perf
    + Combined data containly monthly additions, completed and ongoing waits, plus the 2019 monthly averages for these metrics.
    + Used for plotting the monthly trends in activity.
    
* perf_qtr_split
    + Quarterly performance data, with calculated proportions in each CP category.

* dow_4wk_qtr_pub    
    + Distribution of waits data, split into 4-week bands up to 52 weeks, then 13-week bands
    + Used for distribution of waits graphs/tables only.
    
* addhbr
    + Monthly additions by HBR, specialty and urgency
    
* add_simd
    + Quarterly age-sex-SIMD standardised addition rates for funnel plots.
    + Contains upper and lower CI data points


