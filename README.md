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
This repository contains code for preparing, curating, and integrating multi-modal intensive longitudinal data, with the implementation of these tasks in R. Data come from multiple studies: the [`scripts-shared`](https://github.com/jamieyap/MARS/tree/master/scripts-shared) folder contains scripts that could be used across studies while the [`scripts-studies`](https://github.com/jamieyap/MARS/tree/master/scripts-studies) folder contains scripts that are specific to individual studies. Within each individual study, tasks are organized by type of data stream.
