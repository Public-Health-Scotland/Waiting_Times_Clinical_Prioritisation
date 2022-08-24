# Clinical Prioritisation

This repository contains the Reproducible Analytical Pipeline (RAP) to produce the quarterly statistics on clinical prioritisation, part of the [Stage of Treatment (SoT) publication](https://publichealthscotland.scot/publications/nhs-waiting-times-stage-of-treatment/).

The repository also contains the necessary code to run and deploy the accompanying shiny app.

The first release of the publication is scheduled for 06 September 2022. This page will be updated as the development of the outputs continues.

## Source files

Data from BOXI must be processed for use in the shiny app and Excel output, with the data being saved as .csv in [/data/processed data]. Currently this process is done at the start of the `CP-publication-prep.R` script but it should be transferred to a separate script. Note that the filenames match the df names used in section 3 of `CP-publication-prep.R` but should be made more user-friendly as part of the planned script updates. The csv files are:

* addrem_perf
    + Combined data containly monthly additions, completed and ongoing waits, plus the 2019 monthly averages for these metrics.
    + Used for plotting the monthly trends in activity.
    
* perf_qtr_split
    + Quarterly performance data, with calculated proportions in each CP category.

* topsix_specs
    + List of top six specialties based on ongoing waits, per HBT and quarter
    + Used to filter data for fig. 3 in draft report

* hb_plotdata
    + Quarterly additions, admissions and ongoing waits by HBT, specialty and urgency, with columns for total within each HBT/specialty/date/indicator group and proportion of indicator that is in category P2
    + Used for producing facetted graphs fig. 5 and 6 in draft report.

* dow_4wk_qtr_pub    
    + Distribution of waits data, split into 4-week bands up to 52 weeks, then 13-week bands
    + Used for distribution of waits graphs/tables only.
    
* addhbr
    + Monthly additions by HBR, specialty and urgency
    
* add_simd
    + Quarterly age-sex-SIMD standardised addition rates for funnel plots.
    + Contains upper and lower CI data points

## shiny app

The code for the shiny app can be found in the `shiny_app` folder

### Running the shiny app

* Make sure that you have all the necessary csv files obtained from running `CP-publication-prep.R`. These can be found in `data/processed data`

* Run the script `app_data_preparation.R`, which will copy the csv files over to the `shiny_app/data` folder as rds files

* Open `shiny_app/app.R` and select "run app"

### Password protect the shiny app (PRA)

You can password protect the app for pre-release access.

* Open `admin/create_crententials.R` (hidden file not pushed to Github - obtain this separately from collaborators).

* Edit the script with the chosen username and password for pre-realease access

    credentials_df <- data.frame(
    user = c("username"), # insert username here
    password = c("password"), # insert password here
    stringsAsFactors = FALSE)

* Save your changes and run the script. The script creates a folder called `admin` (if it doesn't already exist). Check that the `credentials.rds` file is saved to your admin folder.

* Go to `shiny_app/app.R`, uncomment the `secure_app()` function in the ui. (make sure to uncomment both opening and closing brackets)

* Run the app to check that the login page is active and test your credentials.

* Note: the `admin` folder is ignored but git to prevent the sharing credentials on Github.

### Deploying the shiny app

* Open `shiny_app/AppDeployment.R` (hidden file not committed to Github - obtain this separately) and edit the paths to point to your local `shiny_app` folder

* Run the script to deploy the app

### Developing the shiny app

#### App code layout

* `data` contains all the data needed for the shiny app. It is populated using the `app_data_preparation.R` script

* `app.R` is the main app file

* `AppDeployment.R` is for deploying the app

* `setup.R` contains the necessary packages and some settings. Loads all the data in `shiny_app/data` into a list called `app_data`. Initialises two lists of reactive values: `plots`, which will contain all the rendered plots, and `numbers` which will contain all the rendered numbers and tables.

* `www` contains the css stylesheet and images

* `pages` contains separate R scripts of app content, one for each page of the app

* `functions` contains R scripts with the functions corresponding to each of the app pages. Additionally `core_functions.R` has some useful centralised functions, `navigation_buttons.R` has the links for the navigation buttons and `modals.R` defines information modals which pop up with information when you click on them





