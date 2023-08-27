# Mobile recordings preprocessing

This repository contains code and sample data for demonstration purposes on how mobile data can be pre-processed in order to use it in modeling later on. The modeling process itself is out of the scope of this project.

## Installation

We assume users is familiar with RStudio and it's installed on her machine.

1. Clone the repository

```
git clone <url-of-the-final-repository>
cd bipolar-preprocessing
```

2. Build the package

We recommend to use `devtools` to install the package.
If not already installed in Your R library, install it:

- open the project in RStudio
- run the following command in the console:
```
install.packages("devtools")
```
- finally, install the _bipolar-preprocessing_ package
```
devtools::install()
```

### Install directly from github

In Your R terminal:

```
remotes::install_github("https://github.com/ITPsychiatry/bipolar-preprocessing",
                        auth_token = <your_token>)
```

## Example use case

There's a rmarkdown file that shows what operations can be performed on the sample data which is attached to the package. In the root directory of the package find the _example_use_case.Rmd_ file and open it.
