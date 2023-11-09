# hrtR

This code is published as part of the NHSBSA Official Statistics team's commitment to open code and transparency in how we produce our publications. The Hormone Replacement Therapy (HRT) reproducible analytical pipeline (RAP) is owned and maintained by the Official Statistics team.

# Introduction

This RAP aims to bring together all code needed to run a pipeline in R to produce the HRT publication. It includes accompanying documentation in line with RAP best practice. 

The RAP includes a `functions` folder containing several files with functions specific to this publication. The RAP will produce an HTML report and accompanying HTML background and methodology document. This RAP makes use of many R packages, including several produced internally at the NHSBSA. Therefore, some of these packages cannot be accessed by external users. 

This RAP cannot be run in it's entirety by external users. However it should provide information on how the Official Statistics team extract the data from the NHSBSA data warehouse, analyse the data, and produce the outputs released on the NHSBSA website as part of this publication.

This RAP is a work in progress and may be replaced as part of updates and improvements for each new release of the HRT publication. The functions in the `functions` folder do not contain unit testing, although we will investigate adding this in future.

## Getting started

You can clone the repository containing the RAP through [GitHub](https://github.com/) using the following steps.

In RStudio, click on "New project", then click "Version Control" and select the "Git" option.

Click "Clone Git Repository" then enter the URL of the HRT GitHub repository (https://github.com/nhsbsa-data-analytics/Prescribing-for-Diabetes). You can click "Browse" to control where you want the cloned repository to be saved in your computer.

You will also need to create a [PAT key](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens).

You can view the [source code for the HRT RAP](https://github.com/nhsbsa-data-analytics/Prescribing-for-Diabetes) on GitHub.


## Functions guide

Functions used specifically for this RAP can be found in the [functions folder](https://github.com/nhsbsa-data-analytics/Prescribing-for-Diabetes/tree/main/functions). The RAP also makes use of functions from a range of packages. A list of packages used is included at the beginning of the `pipeline.R` file, and installed and loaded within the pipeline code. Some functions contained in the functions folder may be placed into the internal NHSBSA packages in future.

Functions written directly for HRT have been split into several R script files. Below is a guide to the functions each file contains.

1. `extract_functions.R` contains functions for getting the required data out of the fact table. They generally use the dbplyr package to interact with the NHSBSA data warehouse. 

Functions include `national_extract()`, `paragraph_extract()`, `child_adult_extract()`, `imd_extract()`, `imd_paragraph_extract()`, `ageband_extract()`, `ageband_paragraph_extract()`, `gender_extract()`, `gender_paragraph_extract()`, `age_gender_extract()`, `age_gender_paragraph_extract()`, `national_presentation()`, `capture_rate_extract()`, `capture_rate_extract_dt()`, `costpericb_extract()`, and `costper_patient_extract()`.

2. `vis_functions.R` contains functions for use in data visualisation for HRT outputs, such as creating charts and formatting in markdown outputs. 

Functions include `infoBox_border()`, `infoBox_no_border()`, `age_gender_chart()`, `get_download_button()`, and `group_chart_hc_new()`.

3. `sdc_function.R` contains the `apply_sdc()` function to apply statistical disclosure control (SDC) to data in HRT spreadsheet outputs. This is done in line with our [statistical disclosure control protocol](https://www.nhsbsa.nhs.uk/policies-and-procedures). 


# Contributing

Contributions are not currently being accepted for this RAP. If this changes, a contributing guide will be made available.

# License

The `hrtR` package, including associated documentation, is released under the MIT license. Details can be found in the `LICENSE` file.
