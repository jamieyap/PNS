###############################################################################
# ABOUT:
# * Identify individuals having at least 1 pre quit mode EMA and 
#   who self-reported no smoking all throughout all delivered EMAs
###############################################################################

library(dplyr)
library(magrittr)
library(purrr)
library(assertthat)

path.pns.input_data <- Sys.getenv("path.pns.input_data")
path.pns.output_data <- Sys.getenv("path.pns.output_data")
path.pns.staged_data <- Sys.getenv("path.pns.staged_data")
path.pns.code <- Sys.getenv("path.pns.code")
path.shared.code <- Sys.getenv("path.shared.code")

smokingdb <- readRDS(file.path(path.pns.staged_data, "curated_smoking_database.RData"))

# This calculation shows that each individual in smokingdb has at least 1 EMA
# delivered/initiated during the pre-quit period
tab.by.period <- smokingdb %>% 
  group_by(id, use.as.postquit) %>%
  summarise(total = n(),
            count.not.miss = sum(!is.na(smoking.indicator))) %>%
  arrange(use.as.postquit, count.not.miss)

# List down individuals who report 'NO' smoking throughout
tab.by.smk <- smokingdb %>%
  group_by(id) %>%
  summarise(total = n(), 
            count.not.miss = sum(!is.na(smoking.indicator)),
            subset.no = sum(smoking.indicator==0, na.rm=TRUE)) %>%
  mutate(is.all.no = if_else(count.not.miss == subset.no, 1, 0)) %>%
  arrange(desc(is.all.no))

# Identify individuals having at least 1 pre quit mode EMA and 
# who self-reported no smoking all EMA having any self-report
# of number of cigarettes smoked
tab.by.smk.subset <- tab.by.smk %>% 
  filter(is.all.no==1) %>%
  select(id, count.not.miss, is.all.no)

saveRDS(tab.by.smk.subset, file.path(path.pns.staged_data, "tabulate_smk.RData"))

