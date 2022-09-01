# Clinical Prioritisation

This repository contains the Reproducible Analytical Pipeline (RAP) to produce the quarterly statistics on clinical prioritisation, part of the [Stage of Treatment (SoT) publication](https://publichealthscotland.scot/publications/nhs-waiting-times-stage-of-treatment/).

The repository also contains the necessary code to run and deploy the accompanying shiny app.

The publication, released on 06 September 2022, is a one-off retrospective analysis of data relating to the Scottish Government's clinical prioritisation Framework, and covers the period July 2021 to June 2022. The Framework became no longer applicable as of 22 July 2022.  

## Workflow

* Create a `data` folder with subfolder `processed_data`

1. Transfer the necessary input files to the `data` folder. You can use the helper function in `functions/admin_functions.R` to do this. See "Input data files" below for the files you need.

2. Run `code/Main.R` (takes ~ 5 minutes). This 

    * Processes the data in `data` and saves it out in `data/processed data`
    
    * Creates plots and saves them out in `plots` (the code will create this folder if it doesn't already exist)
    
    * Transfers the processed data from `data/processed data` to `shiny_app/data` as .rds files so that the shiny app can run
    
3. Launch shiny app by running `shiny_app/app.R`


## Input data files

You will need the following data in your `data` folder - obtain from colleagues

* Distribution of Waits 4 week bands.xlsx
* Distribution of Waits larger time bands.xlsx
* dq_summaries.csv
* Performance excl. Lothian Dental Monthly.xlsx
* Performance excl. Lothian Dental Quarterly.xlsx
* Removal Reason excl. Lothian Dental.xlsx
* Spec Exclusions.xlsx

    
    
## shiny app

The code for the shiny app can be found in the `shiny_app` folder

### Password protect the shiny app (PRA)

You can password protect the app for pre-release access.

* Open `shiny_app/admin/create_crententials.R` (hidden file not pushed to Github - obtain this separately from collaborators)

* Edit the script with the chosen username and password for pre-realease access


```
  credentials_df <- data.frame(
  user = c("username"), # insert username here
  password = c("password"), # insert password here
  stringsAsFactors = FALSE)
```

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


