#------------------------------------------------------------------------------
# Clean up smoking counts variable:
# Apply decision rule for counts of cigarettes smoked to utilized 
# in data analysis
# Note: This set of rules differ from rules used to determine smoking_qty 
# for the purpose of inferring Quit Date when true Quit Date is ambiguous
#------------------------------------------------------------------------------

for(i in 1:length(list.all)){
  list.all[[i]]$smoking_qty = NA_real_
}

# Pre-Quit EMAs ###############################################################

list.all[["Pre-Quit Random"]] <- list.all[["Pre-Quit Random"]] %>% 
  mutate(smoking_qty = case_when(
    assessment_type=="Pre-Quit Random" & rawdata_qty==0 ~ 0,
    assessment_type=="Pre-Quit Random" & rawdata_qty==1 ~ 1-.5,
    assessment_type=="Pre-Quit Random" & rawdata_qty==2 ~ 2-.5,
    assessment_type=="Pre-Quit Random" & rawdata_qty==3 ~ 4-.5,
    assessment_type=="Pre-Quit Random" & rawdata_qty==4 ~ 6-.5,
    assessment_type=="Pre-Quit Random" & rawdata_qty==5 ~ 8-.5,
    assessment_type=="Pre-Quit Random" & rawdata_qty==6 ~ 10-.5,
    assessment_type=="Pre-Quit Random" & rawdata_qty==7 ~ 11-1,
    TRUE ~ smoking_qty)) %>% 
  # Finally, use info from rawdata_indicator
  # smoking_qty is missing if participant reported "No" smoking
  mutate(smoking_qty = if_else(is.na(smoking_qty) & (rawdata_indicator==0), 0, smoking_qty))

list.all[["Pre-Quit Urge"]] <- list.all[["Pre-Quit Urge"]] %>% 
  mutate(smoking_qty = case_when(
    assessment_type=="Pre-Quit Urge" & rawdata_qty==0 ~ 0,
    assessment_type=="Pre-Quit Urge" & rawdata_qty==1 ~ 1-.5,
    assessment_type=="Pre-Quit Urge" & rawdata_qty==2 ~ 2-.5,
    assessment_type=="Pre-Quit Urge" & rawdata_qty==3 ~ 4-.5,
    assessment_type=="Pre-Quit Urge" & rawdata_qty==4 ~ 6-.5,
    assessment_type=="Pre-Quit Urge" & rawdata_qty==5 ~ 8-.5,
    assessment_type=="Pre-Quit Urge" & rawdata_qty==6 ~ 10-.5,
    assessment_type=="Pre-Quit Urge" & rawdata_qty==7 ~ 11-1,
    TRUE ~ smoking_qty)) %>% 
  # Finally, use info from rawdata_indicator
  # smoking_qty is missing if participant reported "No" smoking
  mutate(smoking_qty = if_else(is.na(smoking_qty) & (rawdata_indicator==0), 0, smoking_qty))

list.all[["Pre-Quit Smoking Part Two"]] <- list.all[["Pre-Quit Smoking Part Two"]] %>% 
  mutate(rawdata_qty = case_when(
    # Clean up responses so that smoking_qty for all EMA types below are on the same scale
    assessment_type=="Pre-Quit Smoking Part Two" & rawdata_qty==3 ~ 2,
    assessment_type=="Pre-Quit Smoking Part Two" & rawdata_qty==4 ~ 3,
    assessment_type=="Pre-Quit Smoking Part Two" & rawdata_qty==5 ~ 4,
    assessment_type=="Pre-Quit Smoking Part Two" & rawdata_qty==6 ~ 5,
    assessment_type=="Pre-Quit Smoking Part Two" & rawdata_qty==7 ~ 6,
    assessment_type=="Pre-Quit Smoking Part Two" & rawdata_qty==8 ~ 7,
    TRUE ~ as.numeric(rawdata_qty))) %>% 
  mutate(smoking_qty = case_when(
    # Fill in smoking_qty
    assessment_type=="Pre-Quit Smoking Part Two" & rawdata_qty==0 ~ 0,
    assessment_type=="Pre-Quit Smoking Part Two" & rawdata_qty==1 ~ 1-.5,
    assessment_type=="Pre-Quit Smoking Part Two" & rawdata_qty==2 ~ 2-.5,
    assessment_type=="Pre-Quit Smoking Part Two" & rawdata_qty==3 ~ 4-.5,
    assessment_type=="Pre-Quit Smoking Part Two" & rawdata_qty==4 ~ 6-.5,
    assessment_type=="Pre-Quit Smoking Part Two" & rawdata_qty==5 ~ 8-.5,
    assessment_type=="Pre-Quit Smoking Part Two" & rawdata_qty==6 ~ 10-.5,
    assessment_type=="Pre-Quit Smoking Part Two" & rawdata_qty==7 ~ 11-1,
    TRUE ~ smoking_qty)) 

# Post-Quit EMAs ###############################################################
list.all[["Post-Quit Random"]] <- list.all[["Post-Quit Random"]] %>% 
  mutate(smoking_qty = case_when(
    assessment_type=="Post-Quit Random" & rawdata_qty==0 ~ 0,
    assessment_type=="Post-Quit Random" & rawdata_qty==1 ~ 1-.5,
    assessment_type=="Post-Quit Random" & rawdata_qty==2 ~ 2-.5,
    assessment_type=="Post-Quit Random" & rawdata_qty==3 ~ 4-.5,
    assessment_type=="Post-Quit Random" & rawdata_qty==4 ~ 6-.5,
    assessment_type=="Post-Quit Random" & rawdata_qty==5 ~ 8-.5,
    assessment_type=="Post-Quit Random" & rawdata_qty==6 ~ 10-.5,
    assessment_type=="Post-Quit Random" & rawdata_qty==7 ~ 11-1,
    TRUE ~ smoking_qty)) %>% 
  # Finally, use info from rawdata_indicator
  # smoking_qty is missing if participant reported "No" smoking
  mutate(smoking_qty = if_else(is.na(smoking_qty) & (rawdata_indicator==0), 0, smoking_qty))

list.all[["Post-Quit Urge"]] <- list.all[["Post-Quit Urge"]] %>% 
  mutate(smoking_qty = case_when(
    assessment_type=="Post-Quit Urge" & rawdata_qty==0 ~ 0,
    assessment_type=="Post-Quit Urge" & rawdata_qty==1 ~ 1-.5,
    assessment_type=="Post-Quit Urge" & rawdata_qty==2 ~ 2-.5,
    assessment_type=="Post-Quit Urge" & rawdata_qty==3 ~ 4-.5,
    assessment_type=="Post-Quit Urge" & rawdata_qty==4 ~ 6-.5,
    assessment_type=="Post-Quit Urge" & rawdata_qty==5 ~ 8-.5,
    assessment_type=="Post-Quit Urge" & rawdata_qty==6 ~ 10-.5,
    assessment_type=="Post-Quit Urge" & rawdata_qty==7 ~ 11-1,
    TRUE ~ smoking_qty)) %>% 
  # Finally, use info from rawdata_indicator
  # smoking_qty is missing if participant reported "No" smoking
  mutate(smoking_qty = if_else(is.na(smoking_qty) & (rawdata_indicator==0), 0, smoking_qty))

list.all[["Post-Quit About to Slip Part Two"]] <- list.all[["Post-Quit About to Slip Part Two"]] %>% 
  mutate(rawdata_qty = case_when(
    # Clean up responses so that smoking_qty for all EMA types below are on the same scale
    assessment_type=="Post-Quit About to Slip Part Two" & rawdata_qty==3 ~ 2,
    assessment_type=="Post-Quit About to Slip Part Two" & rawdata_qty==4 ~ 3,
    assessment_type=="Post-Quit About to Slip Part Two" & rawdata_qty==5 ~ 4,
    assessment_type=="Post-Quit About to Slip Part Two" & rawdata_qty==6 ~ 5,
    assessment_type=="Post-Quit About to Slip Part Two" & rawdata_qty==7 ~ 6,
    assessment_type=="Post-Quit About to Slip Part Two" & rawdata_qty==8 ~ 7,
    TRUE ~ as.numeric(rawdata_qty))) %>% 
  mutate(smoking_qty = case_when(
    # Fill in smoking_qty
    assessment_type=="Post-Quit About to Slip Part Two" & rawdata_qty==0 ~ 0,
    assessment_type=="Post-Quit About to Slip Part Two" & rawdata_qty==1 ~ 1-.5,
    assessment_type=="Post-Quit About to Slip Part Two" & rawdata_qty==2 ~ 2-.5,
    assessment_type=="Post-Quit About to Slip Part Two" & rawdata_qty==3 ~ 4-.5,
    assessment_type=="Post-Quit About to Slip Part Two" & rawdata_qty==4 ~ 6-.5,
    assessment_type=="Post-Quit About to Slip Part Two" & rawdata_qty==5 ~ 8-.5,
    assessment_type=="Post-Quit About to Slip Part Two" & rawdata_qty==6 ~ 10-.5,
    assessment_type=="Post-Quit About to Slip Part Two" & rawdata_qty==7 ~ 11-1,
    TRUE ~ smoking_qty))

list.all[["Post-Quit Already Slipped"]] <- list.all[["Post-Quit Already Slipped"]] %>% 
  mutate(smoking_qty = case_when(
    assessment_type=="Post-Quit Already Slipped" & rawdata_qty==0 ~ 0,
    assessment_type=="Post-Quit Already Slipped" & rawdata_qty==1 ~ 1-.5,
    assessment_type=="Post-Quit Already Slipped" & rawdata_qty==2 ~ 2-.5,
    assessment_type=="Post-Quit Already Slipped" & rawdata_qty==3 ~ 4-.5,
    assessment_type=="Post-Quit Already Slipped" & rawdata_qty==4 ~ 6-.5,
    assessment_type=="Post-Quit Already Slipped" & rawdata_qty==5 ~ 8-.5,
    assessment_type=="Post-Quit Already Slipped" & rawdata_qty==6 ~ 10-.5,
    assessment_type=="Post-Quit Already Slipped" & rawdata_qty==7 ~ 11-1,
    TRUE ~ smoking_qty)) %>% 
  # Finally, use info from rawdata_indicator
  # smoking_qty is missing if participant reported "No" smoking
  mutate(smoking_qty = if_else(is.na(smoking_qty) & (rawdata_indicator==0), 0, smoking_qty))


