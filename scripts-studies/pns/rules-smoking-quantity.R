#------------------------------------------------------------------------------
# Clean up smoking counts variable:
# Apply decision rule for counts of cigarettes smoked to utilized 
# in data analysis
# Note: This set of rules differ from rules used to determine smoking.qty 
# for the purpose of inferring Quit Date when true Quit Date is ambiguous
#------------------------------------------------------------------------------

for(i in 1:length(list.all)){
  list.all[[i]]$smoking.qty = NA_real_
}

# Pre-Quit EMAs ###############################################################

list.all[["Pre-Quit Random"]] <- list.all[["Pre-Quit Random"]] %>% 
  mutate(smoking.qty = case_when(
    assessment.type=="Pre-Quit Random" & rawdata.qty==0 ~ 0,
    assessment.type=="Pre-Quit Random" & rawdata.qty==1 ~ 1-.5,
    assessment.type=="Pre-Quit Random" & rawdata.qty==2 ~ 2-.5,
    assessment.type=="Pre-Quit Random" & rawdata.qty==3 ~ 4-.5,
    assessment.type=="Pre-Quit Random" & rawdata.qty==4 ~ 6-.5,
    assessment.type=="Pre-Quit Random" & rawdata.qty==5 ~ 8-.5,
    assessment.type=="Pre-Quit Random" & rawdata.qty==6 ~ 10-.5,
    assessment.type=="Pre-Quit Random" & rawdata.qty==7 ~ 11-1,
    TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  # smoking.qty is missing if participant reported "No" smoking
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))

list.all[["Pre-Quit Urge"]] <- list.all[["Pre-Quit Urge"]] %>% 
  mutate(smoking.qty = case_when(
    assessment.type=="Pre-Quit Urge" & rawdata.qty==0 ~ 0,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==1 ~ 1-.5,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==2 ~ 2-.5,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==3 ~ 4-.5,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==4 ~ 6-.5,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==5 ~ 8-.5,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==6 ~ 10-.5,
    assessment.type=="Pre-Quit Urge" & rawdata.qty==7 ~ 11-1,
    TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  # smoking.qty is missing if participant reported "No" smoking
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))

list.all[["Pre-Quit Smoking Part Two"]] <- list.all[["Pre-Quit Smoking Part Two"]] %>% 
  mutate(rawdata.qty = case_when(
    # Clean up responses so that smoking.qty for all EMA types below are on the same scale
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==3 ~ 2,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==4 ~ 3,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==5 ~ 4,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==6 ~ 5,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==7 ~ 6,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==8 ~ 7,
    TRUE ~ as.numeric(rawdata.qty))) %>% 
  mutate(smoking.qty = case_when(
    # Fill in smoking.qty
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==0 ~ 0,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==1 ~ 1-.5,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==2 ~ 2-.5,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==3 ~ 4-.5,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==4 ~ 6-.5,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==5 ~ 8-.5,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==6 ~ 10-.5,
    assessment.type=="Pre-Quit Smoking Part Two" & rawdata.qty==7 ~ 11-1,
    TRUE ~ smoking.qty)) 

# Post-Quit EMAs ###############################################################
list.all[["Post-Quit Random"]] <- list.all[["Post-Quit Random"]] %>% 
  mutate(smoking.qty = case_when(
    assessment.type=="Post-Quit Random" & rawdata.qty==0 ~ 0,
    assessment.type=="Post-Quit Random" & rawdata.qty==1 ~ 1,
    assessment.type=="Post-Quit Random" & rawdata.qty==2 ~ 2,
    assessment.type=="Post-Quit Random" & rawdata.qty==3 ~ 4,
    assessment.type=="Post-Quit Random" & rawdata.qty==4 ~ 6,
    assessment.type=="Post-Quit Random" & rawdata.qty==5 ~ 8,
    assessment.type=="Post-Quit Random" & rawdata.qty==6 ~ 10,
    assessment.type=="Post-Quit Random" & rawdata.qty==7 ~ 11,
    TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  # smoking.qty is missing if participant reported "No" smoking
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))

list.all[["Post-Quit Urge"]] <- list.all[["Post-Quit Urge"]] %>% 
  mutate(smoking.qty = case_when(
    assessment.type=="Post-Quit Urge" & rawdata.qty==0 ~ 0,
    assessment.type=="Post-Quit Urge" & rawdata.qty==1 ~ 1-.5,
    assessment.type=="Post-Quit Urge" & rawdata.qty==2 ~ 2-.5,
    assessment.type=="Post-Quit Urge" & rawdata.qty==3 ~ 4-.5,
    assessment.type=="Post-Quit Urge" & rawdata.qty==4 ~ 6-.5,
    assessment.type=="Post-Quit Urge" & rawdata.qty==5 ~ 8-.5,
    assessment.type=="Post-Quit Urge" & rawdata.qty==6 ~ 10-.5,
    assessment.type=="Post-Quit Urge" & rawdata.qty==7 ~ 11-1,
    TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  # smoking.qty is missing if participant reported "No" smoking
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))

list.all[["Post-Quit About to Slip Part Two"]] <- list.all[["Post-Quit About to Slip Part Two"]] %>% 
  mutate(rawdata.qty = case_when(
    # Clean up responses so that smoking.qty for all EMA types below are on the same scale
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==3 ~ 2,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==4 ~ 3,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==5 ~ 4,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==6 ~ 5,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==7 ~ 6,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==8 ~ 7,
    TRUE ~ as.numeric(rawdata.qty))) %>% 
  mutate(smoking.qty = case_when(
    # Fill in smoking.qty
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==0 ~ 0,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==1 ~ 1-.5,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==2 ~ 2-.5,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==3 ~ 4-.5,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==4 ~ 6-.5,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==5 ~ 8-.5,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==6 ~ 10-.5,
    assessment.type=="Post-Quit About to Slip Part Two" & rawdata.qty==7 ~ 11-1,
    TRUE ~ smoking.qty))

list.all[["Post-Quit Already Slipped"]] <- list.all[["Post-Quit Already Slipped"]] %>% 
  mutate(smoking.qty = case_when(
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==0 ~ 0,
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==1 ~ 1-.5,
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==2 ~ 2-.5,
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==3 ~ 4-.5,
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==4 ~ 6-.5,
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==5 ~ 8-.5,
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==6 ~ 10-.5,
    assessment.type=="Post-Quit Already Slipped" & rawdata.qty==7 ~ 11-1,
    TRUE ~ smoking.qty)) %>% 
  # Finally, use info from rawdata.indicator
  # smoking.qty is missing if participant reported "No" smoking
  mutate(smoking.qty = if_else(is.na(smoking.qty) & (rawdata.indicator==0), 0, smoking.qty))


