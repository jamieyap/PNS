# About this script: In this script, we perform a series of data curation tasks 
# specific to data from the PNS study

#------------------------------------------------------------------------------
# Read in libraries, file paths, and functions
#------------------------------------------------------------------------------

library(assertthat)
library(dplyr)

path.code <- Sys.getenv("path.code")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
source(file.path(path.code,"data-manip-utils.R"))

#------------------------------------------------------------------------------
# Setup PNS datasets. These are common data manipulation tasks for all
# PNS datasets for analyses
#------------------------------------------------------------------------------
source(file.path(path.code,"pns-run-curation/pns-setup.R"))

#------------------------------------------------------------------------------
# Specify columns to select
#------------------------------------------------------------------------------
# Columns with reference information
cols.ref <- c("id","time.unixts.scaled","with.any.response","engaged.yes")

# These are questions framed in terms of the present time
cols.items.now <- c("Urge1","Urge2","Urge3",
                    "Affect1","Affect2","Affect3","Affect4","Affect5",
                    "Affect6","Affect7","Affect8","Affect9","Affect10",
                    "CigAv","SSPostQR1","SocialSet2","SSPostQR3","SocialSet4",
                    "Restriction",
                    "Location_1","Location_2",
                    "Location_3","Location_4",
                    "Location_5","Location_6","Location_7",
                    "Expect1","Expect2","AbsSelfEff",
                    "Motive1","Motive2",
                    "Consume1","C2PostQR",
                    "Consume3_1","Consume3_2",
                    "Consume3_3","Consume3_4","Consume3_5")

variables.df <- post.quit.random %>% select(c(cols.ref, cols.items.now))

