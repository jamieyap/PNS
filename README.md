# 1. Getting Started
Getting started with using the code in this repository requires installing [R Studio](https://rstudio.com/products/rstudio/) on your machine. After this step has been completed, perform the following steps:

1. Clone the MARS repository
2. In R Studio, open the MARS.Rproj file
3. Execute the following commands in sequence on the console: 
   * `renv::activate()`
   * `renv::restore()`
   
After executing these commands on the console, you may be prompted with `Do you want to proceed? (y/N)` to install specific versions of R packages used by this repository that are unavailable on your machine. Responding `y` on the console initiates this process. 

The integration of the [`renv`](https://rstudio.github.io/renv/articles/renv.html) R package with this repository enables users to execute code using *the same collection and versions of R packages used by developers of this repository*, anticipating the release of newer versions of these packages which may present possible inter-package compatibility issues.

4. Run the script [`create-user-specific-environ.R`](https://github.com/jamieyap/MARS/blob/master/scripts-shared/create-user-specific-environ.R). This will create an `.Renviron` file in your current working directory and the file will be opened in R Studio. Place contents of [paths.txt](https://github.com/jamieyap/MARS/blob/master/paths.txt) into this `.Renviron` file, replacing the string `...` in `paths.txt` with relevant file paths on your machine.

# 2. How this Repository is Organized
This repository contains code for preparing, curating, and integrating multi-modal intensive longitudinal data, with the implementation of these tasks in R. Data come from multiple studies: the [`scripts-shared`](https://github.com/jamieyap/MARS/tree/master/scripts-shared) folder contains scripts that could be used across studies while the [`scripts-studies`](https://github.com/jamieyap/MARS/tree/master/scripts-studies) folder contains scripts that are specific to individual studies. Within each individual study, tasks are organized by type of data stream. The studies are briefly described below.

| Study | Brief Description|
|:-------:|:--------------------------------|
| Por Nuestra Salud | The Por Nuestra Salud (PNS) study examines intrapersonal and contextual determinants of smoking cessation among Spanish-speaking Mexican American smokers attempting to quit. |

# 3. Por Nuestra Salud

# 3.1 Essential

| File | Description|
|:-------:|:--------------------------------|
| TBA | TBA |

# 3.2 More Details

## 3.2.1 Data Curation

| File | Description|
|:-------:|:--------------------------------|
| [`pns-engage.R`](https://github.com/jamieyap/MARS/tree/master/scripts-studies/PNS/pns-engage.R) | Creates analytic dataset with engagement as outcome |
| [`pns-smoking.R`](https://github.com/jamieyap/MARS/tree/master/scripts-studies/PNS/pns-smoking.R) | Creates analytic dataset with self-reported smoking events as outcome |
| [`pns-urge.R`](https://github.com/jamieyap/MARS/tree/master/scripts-studies/PNS/pns-urge.R) | Creates analytic dataset with self-reported timestamps when an urge to smoke occurred as outcome |

## 3.2.2 Tests on Output Data

| File | Description|
|:-------:|:--------------------------------|
| [`pns-sanity-checks.Rmd`](https://github.com/jamieyap/MARS/tree/master/checks/pns-sanity-checks.Rmd) | Runs and collects results from `pns-test-file-01.R` and `pns-test-file-02.R` in a PDF file [`pns-sanity-checks.pdf`](https://github.com/jamieyap/MARS/tree/master/checks/pns-sanity-checks.pdf)|
| [`pns-test-file-01.R`](https://github.com/jamieyap/MARS/tree/master/scripts-studies/PNS/pns-test-file-01.R) | A collection of checks on the construction of self-reported smoking events as outcome |
| [`pns-test-file-02.R`](https://github.com/jamieyap/MARS/tree/master/scripts-studies/PNS/pns-test-file-02.R) | A collection of checks on the construction of Post-Quit Random or Urge EMA curated datasets |

# 3.3 Other

| File | Description|
|:-------:|:--------------------------------|
| [`pns-data-manip-utils.R`](https://github.com/jamieyap/MARS/tree/master/scripts-studies/PNS/pns-data-manip-utils.R) | A collection of functions to execute data preprocessing tasks specific to PNS raw data |
| [`pns-plot-utils.R`](https://github.com/jamieyap/MARS/tree/master/scripts-studies/PNS/pns-plot-utils.R) | A collection of functions to execute data visualization tasks specific to PNS curated data |


