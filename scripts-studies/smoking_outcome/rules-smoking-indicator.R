#------------------------------------------------------------------------------
# Clean up smoking indicator variable
# This script is run after identify-smoking-vars.R and rules-smoking-quantity.R
#------------------------------------------------------------------------------

# Sanity check: if rawdata_indicator==0 then rawdata_qty must NOT be >0
for(i in 1:length(list.all)){
  tmp.summary.dat <- list.all[[i]] %>% select(id, rawdata_indicator, rawdata_qty)
  tmp.tab <- table(tmp.summary.dat$rawdata_indicator, tmp.summary.dat$rawdata_qty)
  if(nrow(tmp.tab)>0){
    if(sum(tmp.tab[1,])>0){
      print("CHECK: rawdata_indicator==0 but rawdata_qty>0")
    }else{
      next
    }
  }
}

# Proceed with data preparation
for(i in 1:length(list.all)){
  list.all[[i]]$smoking_indicator = NA_real_
  
  list.all[[i]] <- list.all[[i]] %>% 
    mutate(smoking_indicator = replace(smoking_indicator, rawdata_indicator==0, 0)) %>%
    mutate(smoking_indicator = replace(smoking_indicator, rawdata_indicator==1 & smoking_qty==0, 0)) %>%
    mutate(smoking_indicator = replace(smoking_indicator, rawdata_indicator==1 & smoking_qty>0, 1))
  
  list.all[[i]] <- list.all[[i]] %>% 
    # Rules for Part Two/Already Slipped EMAs
    mutate(smoking_indicator = replace(smoking_indicator, is.na(rawdata_indicator) & assessment_type=="Pre-Quit Smoking Part Two" & smoking_qty==0, 0)) %>%
    mutate(smoking_indicator = replace(smoking_indicator, is.na(rawdata_indicator) & assessment_type=="Post-Quit About to Slip Part Two" & smoking_qty==0, 0)) %>%
    mutate(smoking_indicator = replace(smoking_indicator, is.na(rawdata_indicator) & assessment_type=="Post-Quit Already Slipped" & smoking_qty==0, 0)) %>%
    # More rules for Part Two/Already Slipped EMAs
    mutate(smoking_indicator = replace(smoking_indicator, is.na(rawdata_indicator) & assessment_type=="Pre-Quit Smoking Part Two" & smoking_qty>0, 1)) %>%
    mutate(smoking_indicator = replace(smoking_indicator, is.na(rawdata_indicator) & assessment_type=="Post-Quit About to Slip Part Two" & smoking_qty>0, 1)) %>%
    mutate(smoking_indicator = replace(smoking_indicator, is.na(rawdata_indicator) & assessment_type=="Post-Quit Already Slipped" & smoking_qty>0, 1)) %>%
    # More rules for Part One EMAs
    mutate(smoking_indicator = replace(smoking_indicator, is.na(rawdata_indicator) & assessment_type=="Pre-Quit Smoking Part One", NA_real_)) %>%
    mutate(smoking_indicator = replace(smoking_indicator, is.na(rawdata_indicator) & assessment_type=="Post-Quit About to Slip Part One", NA_real_))
}


