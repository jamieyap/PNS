#------------------------------------------------------------------------------
# About: Identify smoking-related items in each EMA
#------------------------------------------------------------------------------

IdentifySmokingVars <- function(list.all){
  
  # Pre-Quit EMAs ###############################################################
  list.all[["Pre-Quit Random"]] <- list.all[["Pre-Quit Random"]] %>% 
    mutate(rawdata.indicator = PreQRSmoking1, 
           rawdata.qty = Smoking2_PreQ_Random, 
           rawdata.timing = Smoking3)
  
  list.all[["Pre-Quit Urge"]] <- list.all[["Pre-Quit Urge"]] %>% 
    mutate(rawdata.indicator = SmPQU1, 
           rawdata.qty = Smoking2_PreQ_Urge, 
           rawdata.timing = Smoking3)
  
  list.all[["Pre-Quit Smoking Part One"]] <- list.all[["Pre-Quit Smoking Part One"]] %>% 
    mutate(rawdata.indicator = NA, 
           rawdata.qty = NA, 
           rawdata.timing = NA)
  
  list.all[["Pre-Quit Smoking Part Two"]] <- list.all[["Pre-Quit Smoking Part Two"]] %>% 
    mutate(rawdata.qty = CigJustNow) %>%
    mutate(rawdata.indicator = NA, 
           rawdata.timing = NA)
  
  # Post-Quit EMAs ##############################################################
  list.all[["Post-Quit Random"]] <- list.all[["Post-Quit Random"]] %>% 
    mutate(rawdata.indicator = PostQRSmoking1, 
           rawdata.qty = Smoking2_PostQ_Random, 
           rawdata.timing = Smoking3)
  
  list.all[["Post-Quit Urge"]] <- list.all[["Post-Quit Urge"]] %>%  
    mutate(rawdata.indicator = SmPostQU1, 
           rawdata.qty = Smoking2_PostQ_Urge, 
           rawdata.timing = Smoking3)
  
  list.all[["Post-Quit About to Slip Part One"]] <- list.all[["Post-Quit About to Slip Part One"]] %>%  
    mutate(rawdata.indicator = NA, 
           rawdata.qty = NA, 
           rawdata.timing = NA)
  
  list.all[["Post-Quit About to Slip Part Two"]] <- list.all[["Post-Quit About to Slip Part Two"]] %>%  
    mutate(rawdata.qty = CigJustNow_PostQ_Slip2) %>%
    mutate(rawdata.indicator = NA, 
           rawdata.timing = NA)
  
  list.all[["Post-Quit Already Slipped"]] <- list.all[["Post-Quit Already Slipped"]] %>%  
    mutate(rawdata.qty = HowManyCig, 
           rawdata.timing = LastCig) %>%
    mutate(rawdata.indicator = NA)
  
  return(list.all)
}


