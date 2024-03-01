# Mobile recordings preprocessing

This repository contains code and sample data for demonstration purposes on how mobile data can be pre-processed in order to use it in modeling later on. The modeling process itself is out of the scope of this project.

## Installation

We assume users is familiar with RStudio and it's installed on her machine.

1. Clone the repository

```
git clone <url-of-the-final-repository>
cd bipolar
```

2. Build the package

We recommend to use `devtools` to install the package.
If not already installed in Your R library, install it:

- open the project in RStudio
- run the following command in the console:
```
install.packages("devtools")
```
- finally, install the _ssfclus_ and _bipolar_ packages:
```
devtools::install_github("ITPsychiatry/ssfclust@refactor")
devtools::install()
```

### Install directly from github

In Your R terminal:

```
remotes::install_github("https://github.com/ITPsychiatry/bipolar",
                        auth_token = <your_token>)
```

## Easy to run examples (vignettes) 


There are the following rmarkdown files that show selected basic operations that can be performed and the sample data which is attached to the package:

_1_preprocessing_of_sensor_and_psychiatric_data.Rmd_

_2_aggregating_voice_recordings.Rmd_

_3_uncertainty_aware_feature_selection.Rmd_

_4_validation_of_semi_sueprvised_fuzzy_clustering.Rmd_

_5_view_dashboards_with_modeling_outputs.Rmd_

In the _Vignettes_ directory of the package, find these files and open them.
