#------------------------------------------------------------------------------
# Identify smoking-related items in each EMA
#------------------------------------------------------------------------------

# Pre-Quit EMAs ###############################################################
list.all[["Pre-Quit Random"]] <- list.all[["Pre-Quit Random"]] %>% 
  mutate(rawdata_indicator = PreQRSmoking1, 
         rawdata_qty = Smoking2_PreQ_Random, 
         rawdata_timing = Smoking3)

list.all[["Pre-Quit Urge"]] <- list.all[["Pre-Quit Urge"]] %>% 
  mutate(rawdata_indicator = SmPQU1, 
         rawdata_qty = Smoking2_PreQ_Urge, 
         rawdata_timing = Smoking3)

list.all[["Pre-Quit Smoking Part One"]] <- list.all[["Pre-Quit Smoking Part One"]] %>% 
  mutate(rawdata_indicator = NA, 
         rawdata_qty = NA, 
         rawdata_timing = NA)

list.all[["Pre-Quit Smoking Part Two"]] <- list.all[["Pre-Quit Smoking Part Two"]] %>% 
  mutate(rawdata_qty = CigJustNow) %>%
  mutate(rawdata_indicator = NA, 
         rawdata_timing = NA)

# Post-Quit EMAs ##############################################################
list.all[["Post-Quit Random"]] <- list.all[["Post-Quit Random"]] %>% 
  mutate(rawdata_indicator = PostQRSmoking1, 
         rawdata_qty = Smoking2_PostQ_Random, 
         rawdata_timing = Smoking3)

list.all[["Post-Quit Urge"]] <- list.all[["Post-Quit Urge"]] %>%  
  mutate(rawdata_indicator = SmPostQU1, 
         rawdata_qty = Smoking2_PostQ_Urge, 
         rawdata_timing = Smoking3)

list.all[["Post-Quit About to Slip Part One"]] <- list.all[["Post-Quit About to Slip Part One"]] %>%  
  mutate(rawdata_indicator = NA, 
         rawdata_qty = NA, 
         rawdata_timing = NA)

list.all[["Post-Quit About to Slip Part Two"]] <- list.all[["Post-Quit About to Slip Part Two"]] %>%  
  mutate(rawdata_qty = CigJustNow_PostQ_Slip2) %>%
  mutate(rawdata_indicator = NA, 
         rawdata_timing = NA)

list.all[["Post-Quit Already Slipped"]] <- list.all[["Post-Quit Already Slipped"]] %>%  
  mutate(rawdata_qty = HowManyCig, 
         rawdata_timing = LastCig) %>%
  mutate(rawdata_indicator = NA)


