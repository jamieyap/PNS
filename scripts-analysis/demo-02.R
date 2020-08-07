###############################################################################
# ABOUT:
# Sample code to
# - read curated data csv files
# - correctly identify subsets of the data to utilize in analysis
# - constuct features in terms of past history
# - fit a random effects model
###############################################################################

# Set current working directory to location of curated data
# Change path.pns.output_data to relevant working directory
# For example,
# path.pns.output_data <- "C:/Users/user/Desktop"
# note that above, a backslash instead of a forward slash is used
path.pns.output_data <- Sys.getenv("path.pns.output_data")
setwd(path.pns.output_data)

# Load R packages we'll use
library(dplyr)

# -----------------------------------------------------------------------------
# Read in curated data
# -----------------------------------------------------------------------------
# In R version 3.6.3, by default, read.csv automatically converts strings to
# factors. Both data types are handled differently in R. 
# Hence, we set the argument stringsAsFactors=FALSE to prevent this behavior;
# strings will remain strings
data.prequit.random <- read.csv("pre_quit_random_ema.csv", stringsAsFactors = FALSE)
data.postquit.random <- read.csv("post_quit_random_ema.csv", stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# Identify rows to be utilized in main analysis and sensitivity analysis
# -----------------------------------------------------------------------------
data.main.analysis.prequit.random <- data.prequit.random  # simply creating a copy and naming as such
data.main.analysis.postquit.random <- data.postquit.random  # simply creating a copy and naming as such

# Obtain subset of rows to be utilised in sensitivity analysis
# Individuals with sensitivity=1 are those with ambiguity in Quit Dates
# In sensitivity analysis, we only wish to include those individuals
# who did NOT have ambiguity in Quit Dates
data.sensitivity.analysis.prequit.random <- data.main.analysis.prequit.random %>% filter(sensitivity==0)
data.sensitivity.analysis.postquit.random <- data.main.analysis.postquit.random %>% filter(sensitivity==0)

# -----------------------------------------------------------------------------
# Pre-quit EMA dataset and post-quit EMA dataset have column names in common
# but also have column names that are specific to that dataset.
# Here, we first identify which column names are common to both.
# Next, we identify column names in the pre-quit EMA dataset which we wish
# to use in data analysis. 
# Finally, we identify column names in the postquit EMA dataset which we wish
# to use in data analysis
# -----------------------------------------------------------------------------

common.column.names <- c("id",
                         "callnumr",
                         "start_study_hrts",
                         "end_study_hrts",
                         "quit_hrts",
                         "start_study_unixts",
                         "end_study_unixts",
                         "quit_unixts",
                         "record_id",
                         "assessment_type",
                         "use_as_postquit",
                         "sensitivity",
                         "with_any_response",
                         "delivered_hrts",
                         "begin_hrts",
                         "end_hrts",
                         "time_hrts",
                         "delivered_unixts",
                         "begin_unixts",
                         "end_unixts",
                         "time_unixts")

# Note: refer to codebook to decide which column names to specify here
ema.items.prequit.random <- c("Affect1", "Affect2", "Affect3", "Affect4", "Affect5", "Affect6", "Affect7", "Affect8", "Affect9", "Affect10")
ema.items.postquit.random <- c("Affect1", "Affect2", "Affect3", "Affect4", "Affect5", "Affect6", "Affect7", "Affect8", "Affect9", "Affect10")

# -----------------------------------------------------------------------------
# Identify datasets to use in analysis
# -----------------------------------------------------------------------------
use.this.data.prequit.ema <- data.main.analysis.prequit.random # alternatively, use data.sensitivity.analysis.prequit.random  
use.this.data.postquit.ema <- data.main.analysis.postquit.random # alternatively, use data.sensitivity.analysis.postquit.random  

# -----------------------------------------------------------------------------
# Trim down data: exclude columns that will not be needed in data analysis
# -----------------------------------------------------------------------------
use.this.data.prequit.ema <- use.this.data.prequit.ema %>% select(common.column.names, ema.items.prequit.random)
use.this.data.postquit.ema <- use.this.data.postquit.ema %>% select(common.column.names, ema.items.postquit.random)
use.this.data <- rbind(use.this.data.prequit.ema, use.this.data.postquit.ema)

# Note: Some Pre-Quit Random EMAs are viewed as having occurred after Quit Date
# Similarly, some Post-Quit Random EMAs are veiwed as having occurred before Quit Date
# To identify which random EMAs are viewed as having occurred before Quit Date
# set use_as_postquit=0; to identify which random EMAs are viewed as having
# occurred after Quit Date set use_as_postquit=1

# Here, let's display what these rows look like for illustration
use.this.data %>% filter(use_as_postquit==0) %>% filter(assessment_type=="Pre-Quit Random") %>% head(., n=3)
use.this.data %>% filter(use_as_postquit==0) %>% filter(assessment_type=="Post-Quit Random") %>% head(., n=3)
use.this.data %>% filter(use_as_postquit==1) %>% filter(assessment_type=="Pre-Quit Random") %>% head(., n=3)
use.this.data %>% filter(use_as_postquit==1) %>% filter(assessment_type=="Post-Quit Random") %>% head(., n=3)

# -----------------------------------------------------------------------------
# Let's create new variables
# -----------------------------------------------------------------------------
# It's important that use.this.data is sorted according to increased values
# of (id, time_unixts) before proceeding
use.this.data <- use.this.data %>% arrange(id, time_unixts)

# Create more time variables
use.this.data <- use.this.data %>%
  mutate(hours_since_start = (time_unixts-start_study_unixts)/(60*60))

# -----------------------------------------------------------------------------
# From here onward, the new variables we will create will use functions in
# the script shared-data-manip-utils.R
# -----------------------------------------------------------------------------
# Change path.shared.code to the location of the file shared-data-manip-utils.R
# For example,
# path.shared.code <- "C:/Users/user/Desktop"
# note that above, a backslash instead of a forward slash is used

path.shared.code <- Sys.getenv("path.shared.code")
source(file.path(path.shared.code, "shared-data-manip-utils.R"))

# Many of the function in shared-data-manip-utils.R have data from one
# participant only as an input, specifically, the functions:
#
# GetFutureRecords
# GetPastRecords
# MeanWithin
# VarianceWithin
# MinWithin
# MaxWithin
# CountWithin
#
# For the above functions, remember that:
# 1. if data from more than one participant is used in the argument df.this.group,
# then the output will be incorrect
# 2. df.this.group needs to be sorted according to increased values (id, time_unixts) 

# Creates a new variable 
# with_any_response_shift_plus_1: does next EMA in the FUTURE have a response to at least one item?
use.this.data <- use.this.data %>% 
  group_by(id) %>%
  do(GetFutureRecords(df.this.group=., cols.today=c("with_any_response"), h=1, this.numeric=TRUE))

# Creates a new variable 
# time_unixts_shift_minus_1: what is time_unixts for the next EMA in the PAST?
use.this.data <- use.this.data %>% 
  group_by(id) %>%
  do(GetPastRecords(df.this.group=., cols.today=c("time_unixts"), h=1, this.numeric=TRUE))

# Now, let's create a new variable for time between two consecutive EMAs in hours
use.this.data <- use.this.data %>% mutate(hours_between = time_unixts - time_unixts_shift_minus_1)

# Inspect use.this.data
head(use.this.data)

# Now, let's create features in terms of past history

# The functions used next, defined in shared-data-manip-utils.R, also require that
# the data to have a column containing all 1's to be created; we'll do just that here
use.this.data <- use.this.data %>% mutate(ones=1)

# Get all id's in use.this.data
all.ids <- use.this.data %>% select(id) %>% unique(.)
all.ids <- all.ids$id
total.ids <- length(all.ids)

###############################################################################
# Create a new variable: mean in the past 48 hours of Affect1
###############################################################################

list.all <- list()

for(i in 1:total.ids){
  current.id <- all.ids[i]
  current.data <- use.this.data %>% filter(id==current.id)
  
  dat.newvar <- current.data %>%
    MeanWithin(df.this.group = ., ones.col = "ones", current.ts="time_unixts", H=48, this.var="Affect1")
  
  dat.newvar <- dat.newvar %>% rename(Affect1_mean_past48=V1)
  
  # dat.newvar is a data frame
  # Merge dat.newvar with current.data, another data frame
  # note that cbind() will not work here
  current.data <- cbind.data.frame(current.data, dat.newvar)
  
  list.all <- append(list.all, list(current.data))
}

use.this.data <- do.call(rbind, list.all)

###############################################################################
# Create a new variable: variance in the past 48 hours of Affect1
###############################################################################

list.all <- list()

for(i in 1:total.ids){
  current.id <- all.ids[i]
  current.data <- use.this.data %>% filter(id==current.id)
  
  dat.newvar <- current.data %>%
    VarianceWithin(df.this.group = ., ones.col = "ones", current.ts="time_unixts", H=48, this.var="Affect1")
  
  dat.newvar <- dat.newvar %>% rename(Affect1_variance_past48=V1)
  
  # dat.newvar is a data frame
  # Merge dat.newvar with current.data, another data frame
  # note that cbind() will not work here
  current.data <- cbind.data.frame(current.data, dat.newvar)
  
  list.all <- append(list.all, list(current.data))
}

use.this.data <- do.call(rbind, list.all)

# -----------------------------------------------------------------------------
# Now, let's fit a random effects model
# -----------------------------------------------------------------------------

# Load R packages we'll use
library(lme4)

# Plain vanilla model
# also try the following as covariates:
# hours_between, hours_since_start, use_as_postquit
model1 <- glmer(with_any_response_shift_plus_1 ~ with_any_response + (1|id), data=use.this.data, family=binomial)
summary(model1)

