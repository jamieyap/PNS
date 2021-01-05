#------------------------------------------------------------------------------
# About:
# * Clean up smoking indicator variable
# * This script is run after identify-smoking-vars.R and rules-smoking-quantity.R
#------------------------------------------------------------------------------

ApplyRulesSmokingIndicator <- function(list.all){
  
  # Sanity check: if rawdata.indicator==0 then rawdata.qty must NOT be >0
  for(i in 1:length(list.all)){
    tmp.summary.dat <- list.all[[i]] %>% select(id, rawdata.indicator, rawdata.qty)
    tmp.tab <- table(tmp.summary.dat$rawdata.indicator, tmp.summary.dat$rawdata.qty)
    if(nrow(tmp.tab)>0){
      if(sum(tmp.tab[1,])>0){
        print("CHECK: rawdata.indicator==0 but rawdata.qty>0")
      }else{
        next
      }
    }
  }
  
  # Proceed with data preparation
  for(i in 1:length(list.all)){
    list.all[[i]]$smoking.indicator = NA_real_
    
    list.all[[i]] <- list.all[[i]] %>% 
      mutate(smoking.indicator = replace(smoking.indicator, rawdata.indicator==0, 0)) %>%
      mutate(smoking.indicator = replace(smoking.indicator, rawdata.indicator==1 & smoking.qty==0, 0)) %>%
      mutate(smoking.indicator = replace(smoking.indicator, rawdata.indicator==1 & smoking.qty>0, 1))
    
    list.all[[i]] <- list.all[[i]] %>% 
      # Rules for Part Two/Already Slipped EMAs
      mutate(smoking.indicator = replace(smoking.indicator, is.na(rawdata.indicator) & assessment.type=="Pre-Quit Smoking Part Two" & smoking.qty==0, 0)) %>%
      mutate(smoking.indicator = replace(smoking.indicator, is.na(rawdata.indicator) & assessment.type=="Post-Quit About to Slip Part Two" & smoking.qty==0, 0)) %>%
      mutate(smoking.indicator = replace(smoking.indicator, is.na(rawdata.indicator) & assessment.type=="Post-Quit Already Slipped" & smoking.qty==0, 0)) %>%
      # More rules for Part Two/Already Slipped EMAs
      mutate(smoking.indicator = replace(smoking.indicator, is.na(rawdata.indicator) & assessment.type=="Pre-Quit Smoking Part Two" & smoking.qty>0, 1)) %>%
      mutate(smoking.indicator = replace(smoking.indicator, is.na(rawdata.indicator) & assessment.type=="Post-Quit About to Slip Part Two" & smoking.qty>0, 1)) %>%
      mutate(smoking.indicator = replace(smoking.indicator, is.na(rawdata.indicator) & assessment.type=="Post-Quit Already Slipped" & smoking.qty>0, 1)) %>%
      # More rules for Part One EMAs
      mutate(smoking.indicator = replace(smoking.indicator, is.na(rawdata.indicator) & assessment.type=="Pre-Quit Smoking Part One", NA_real_)) %>%
      mutate(smoking.indicator = replace(smoking.indicator, is.na(rawdata.indicator) & assessment.type=="Post-Quit About to Slip Part One", NA_real_))
  }
  
  return(list.all)
}


