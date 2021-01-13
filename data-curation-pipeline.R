###############################################################################
# ABOUT:
# * Documents the sequence at which different steps are performed;
#   dependency between different scripts in the scripts-studies and
#   scripts-shared directories are displayed
# * Utilizes the package ggdag to visualize data curation pipeline
###############################################################################

# -----------------------------------------------------------------------------
# Run pipeline
# -----------------------------------------------------------------------------

# Perform calculation to determine an individual's Quit Date for the purpose of
# data analysis
source(file.path(Sys.getenv("path.pns.code"), "calc-quit-dates.R"))
rm(list = ls())


# * Identify column names in raw data where responses to EMA items are recorded
# * For each of the original column names, new names are given;
#   the format used in name_new enables end-users to determine the particular 
#   kind of EMA in which the value in the merged dataset (having data from two
#   or more kinds of EMA; there are nine kinds of EMA) was originally provided
source(file.path(Sys.getenv("path.pns.code"), "create-dictionary.R"))
rm(list = ls())


# * Reads in EMA raw data files
# * Constuct variables for time when EMA was delivered, begun, completed
# * Construct variable indicating whether a response to any item was recorded
# * Save to intermediate file in preparation for further data processing
# * CALLS WITHIN SCRIPT: shared-data-manip-utils.R and data-manip-utils.R
source(file.path(Sys.getenv("path.pns.code"), "read-ema.R"))
rm(list = ls())


# * Complete preparaton by type of EMA
# * Focus of this script is not on individual items within an EMA but on
#   identifying which rows which are likely to result from one of the following
#   circumstances:
#      - EMAs (any type) which the software chose to launch and were 
#        successfully delivered/initiated but had indication of 
#        issues relating to the software
#      - EMAs (any type) which the software chose to launch and were 
#        successfully delivered/initiated with no indication of 
#        issues relating to the software
#      - self-initiated EMA button press where the software chose to NOT
#        launch an EMA
source(file.path(Sys.getenv("path.pns.code"), "clean-ema.R"))
rm(list = ls())

# Create database of smoking information using 'cleaned' EMA data
source(file.path(Sys.getenv("path.pns.code"), "create-database-smoking.R"))
rm(list = ls())

# Exclude rows corresponding to baseline participants who will not be
# included in any analysis
source(file.path(Sys.getenv("path.pns.code"), "clean-baseline.R"))
rm(list = ls())

# Format and save RData files into stand-alone or merged databases
source(file.path(Sys.getenv("path.pns.code"), "output-formatted-database.R"))
rm(list = ls())

# -----------------------------------------------------------------------------
# Visualize pipeline
# -----------------------------------------------------------------------------

library(ggdag)
theme_set(theme_dag_grid())

# Record dependencies using a directed acyclic graph
data_curation_dag <- dagify(output_formatted_database ~ calc_quit_dates,
                            output_formatted_database ~ clean_ema,
                            output_formatted_database ~ create_database_smoking,
                            output_formatted_database ~ clean_baseline,
                            output_formatted_database ~ create_dictionary,
                            clean_baseline ~ calc_quit_dates,
                            read_ema ~ create_dictionary,
                            read_ema ~ shared_data_manip_utils,
                            read_ema ~ data_manip_utils,
                            clean_ema ~ read_ema,
                            clean_ema ~ calc_quit_dates,
                            create_database_smoking ~ clean_ema,
                            create_database_smoking ~ identify_smoking_vars,
                            create_database_smoking ~ rules_smoking_quantity,
                            create_database_smoking ~ rules_smoking_indicator,
                            create_database_smoking ~ rules_smoking_timing,
                            labels = c("clean_baseline" = "clean-baseline.R",
                                       "read_ema" = "read-ema.R",
                                       "create_dictionary" = "create-dictionary.R",
                                       "shared_data_manip_utils" = "shared-data-manip-utils.R",
                                       "data_manip_utils" = "data-manip-utils.R",
                                       "clean_ema" = "clean-ema.R",
                                       "output_formatted_database" = "output-formatted-database.R",
                                       "calc_quit_dates" = "calc-quit-dates.R",
                                       "create_database_smoking" = "create-database-smoking.R",
                                       "identify_smoking_vars" = "identify-smoking-vars.R",
                                       "rules_smoking_quantity" = "rules-smoking-quantity.R",
                                       "rules_smoking_indicator" = "rules-smoking-indicator.R",
                                       "rules_smoking_timing" = "rules-smoking-timing.R"
                                       )) 

ggdag(data_curation_dag, text = FALSE, use_labels = "label", text_size = 2.88) 



