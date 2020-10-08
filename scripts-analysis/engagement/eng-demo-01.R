# -----------------------------------------------------------------------------
# Preparatory steps
# -----------------------------------------------------------------------------

# Change path.pns.output_data to relevant working directory
# For example,
# path.pns.output_data <- "C:/Users/user/Desktop"
# note that above, a backslash instead of a forward slash is used

path.pns.output_data <- Sys.getenv("path.pns.output_data")  # location of curated_database_engagement.csv
path.pns.demo.engagement <- Sys.getenv("path.pns.demo.engagement")  # location of eng-functions.R
path.analysis <- Sys.getenv("path.pns.output_data") # location of the output csv file of this script; can differ from path.pns.output_data

# Load R packages we'll use
library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

# Read in eng-demo-functions.R
source(file.path(path.pns.demo.engagement, "eng-functions.R"))

# Read in contents of curated_database_engagement.csv
# In R version 3.6.3, by default, read.csv automatically converts strings to
# factors. Both data types are handled differently in R. 
# Hence, we set the argument stringsAsFactors=FALSE to prevent this behavior;
# strings will remain strings
data.for.analysis <- read.csv(file.path(path.pns.output_data, "curated_database_engagement.csv"), stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# Construct features in terms of past history
# -----------------------------------------------------------------------------

# Do for each row: count number of EMA delivered in the past 48 hours prior to time_unixts
data.for.analysis <- data.for.analysis %>% 
  group_by(id) %>%
  do(CountWithin(df.this.group=., H = 48, newvar.name = "count_delivered_past48"))

# Do for each row: 
# identify which EMAs were delivered in the past 48 hours prior to time_unixts
# take the mean of Affect1 across these EMAs
data.for.analysis <- data.for.analysis %>% 
  group_by(id) %>%
  do(MeanWithin(df.this.group=., H = 48, this.var = "Affect1", newvar.name = "mean_affect1_past48"))

# Do for each row: 
# identify which EMAs were delivered in the past 48 hours prior to time_unixts
# take the variance of Affect1 across these EMAs
data.for.analysis <- data.for.analysis %>% 
  group_by(id) %>%
  do(VarianceWithin(df.this.group=., H = 48, this.var = "Affect1", newvar.name = "var_affect1_past48"))

# At this point, data.for.analysis should have three additional columns
View(data.for.analysis)

# -----------------------------------------------------------------------------
# Construct features in terms of past history
# -----------------------------------------------------------------------------
# Note that the use of H=600 hours below effectively calculates counts,
# means, and variances since start of the study since PNS has a maximum of
# 21 days * 24 hours per day = 504 hours
# Any number greater than 504 hours will do

# Do for each row: count number of EMA delivered in the past 600 hours prior to time_unixts
data.for.analysis <- data.for.analysis %>% 
  group_by(id) %>%
  do(CountWithin(df.this.group=., H = 600, newvar.name = "count_delivered_since_baseline"))

# Do for each row: 
# identify which EMAs were delivered in the past 600 hours prior to time_unixts
# take the mean of Affect1 across these EMAs
data.for.analysis <- data.for.analysis %>% 
  group_by(id) %>%
  do(MeanWithin(df.this.group=., H = 600, this.var = "Affect1", newvar.name = "mean_affect1_since_baseline"))

# Do for each row: 
# identify which EMAs were delivered in the past 600 hours prior to time_unixts
# take the variance of Affect1 across these EMAs
data.for.analysis <- data.for.analysis %>% 
  group_by(id) %>%
  do(VarianceWithin(df.this.group=., H = 600, this.var = "Affect1", newvar.name = "var_affect1_since_baseline"))

# At this point, data.for.analysis should have another three additional columns
View(data.for.analysis)

# Note: 
# - the values of H, this.var, and newvar.name can be changed to
# consider other time periods and constructs
# - CountWithin, MeanWithin, and VarianceWithin all rely on data.for.analysis
# having the variable "time_unixts" as the basis of these calculations
# as their 'default' setting. This 'default setting' can be modified if needed
# in the script eng-functions.R

# -----------------------------------------------------------------------------
# Save output
# -----------------------------------------------------------------------------
write.csv(data.for.analysis, file.path(path.analysis, "data_for_analysis_engagement.csv"), row.names = FALSE, na="")


