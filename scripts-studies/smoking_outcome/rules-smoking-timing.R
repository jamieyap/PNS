#------------------------------------------------------------------------------
# Clean up smoking time variable
#------------------------------------------------------------------------------

# Responses to timing of smoking in Post-Quit Already slipped (0-8) is on a 
# different scale than Post-Quit Random and Post-Quit Urge, both of 
# which are on the same scale (1-9). We shift responses to Post-Quit Random and
# Post-Quit Urge so that they are on a scale of 0-8.
#
# Similarly, Pre-Quit Random and Pre-Quit Urge are both on a scale of 1-9
# while the remaining types of Pre-Quit assessments are on a scale of 0-8
# Hence, we shift the scale of Pre-Quit Random and Pre-Quit Urge to be 0-8

df.all <- df.all %>%
  mutate(rawdata.timing = as.numeric(rawdata.timing)) %>%
  mutate(rawdata.timing = case_when(
    # Post-Quit assessments
    assessment.type == "Post-Quit Random" ~ rawdata.timing-1,
    assessment.type == "Post-Quit Urge" ~ rawdata.timing-1,
    # Pre-Quit assessments
    assessment.type == "Pre-Quit Random" ~ rawdata.timing-1,
    assessment.type == "Pre-Quit Urge" ~ rawdata.timing-1,
    # Else
    TRUE ~ rawdata.timing
  ))

df.all <- df.all %>%
  mutate(smoking.delta.minutes = case_when(
    # Post-Quit assessments
    assessment.type == "Post-Quit Random" & rawdata.timing!=8 ~ (15*rawdata.timing +15* (rawdata.timing+1))/2,
    assessment.type == "Post-Quit Urge" & rawdata.timing!=8 ~ (15*rawdata.timing +15* (rawdata.timing+1))/2,
    assessment.type == "Post-Quit Already Slipped" & rawdata.timing!=8 ~ (15*rawdata.timing +15* (rawdata.timing+1))/2,
    # Pre-Quit assessments
    assessment.type == "Pre-Quit Random" & rawdata.timing!=8 ~ (15*rawdata.timing +15* (rawdata.timing+1))/2,
    assessment.type == "Pre-Quit Urge" & rawdata.timing!=8 ~ (15*rawdata.timing +15* (rawdata.timing+1))/2,
    # Else
    TRUE ~ smoking.delta.minutes
  ))

df.all <- df.all %>%
  mutate(smoking.delta.minutes = case_when(
    # Post-Quit assessments
    assessment.type == "Post-Quit Random" & rawdata.timing==8 ~ 2*60 + (1/60)*((time.unixts-2*60*60) - time.unixts_shift_minus_1)/2,
    assessment.type == "Post-Quit Urge" & rawdata.timing==8 ~ 2*60 + (1/60)*((time.unixts-2*60*60) - time.unixts_shift_minus_1)/2,
    assessment.type == "Post-Quit Already Slipped" & rawdata.timing==8 ~ 2*60 + (1/60)*((time.unixts-2*60*60) - time.unixts_shift_minus_1)/2,
    # Pre-Quit assessments
    assessment.type == "Pre-Quit Random" & rawdata.timing==8 ~ 2*60 + (1/60)*((time.unixts-2*60*60) - time.unixts_shift_minus_1)/2,
    assessment.type == "Pre-Quit Urge" & rawdata.timing==8 ~ 2*60 + (1/60)*((time.unixts-2*60*60) - time.unixts_shift_minus_1)/2,
    # Else
    TRUE ~ smoking.delta.minutes
  ))

df.all <- df.all %>% 
  mutate(smoking.delta.minutes = if_else(smoking.delta.minutes > hours.between.past.and.present*60, hours.between.past.and.present*60/2, smoking.delta.minutes))

df.all <- df.all %>%
  mutate(smoking.delta.minutes = case_when(
    # Post-Quit assessments
    assessment.type == "Post-Quit About to Slip Part Two" & assessment.type_shift_minus_1 == "Post-Quit About to Slip Part One" ~ hours.between.past.and.present*60/2,
    # Pre-Quit assessments
    assessment.type == "Pre-Quit Smoking Part Two" & assessment.type_shift_minus_1 == "Pre-Quit Smoking Part One" ~ hours.between.past.and.present*60/2,
    # Else
    TRUE ~ smoking.delta.minutes
  ))

this.ave.hours.between.past.and.present <- df.all %>% filter(assessment.type == "Pre-Quit Smoking Part Two" & assessment.type_shift_minus_1 == "Pre-Quit Smoking Part One")
this.ave.hours.between.past.and.present <- mean(this.ave.hours.between.past.and.present$hours.between.past.and.present, na.rm=TRUE)

df.all <- df.all %>%
  mutate(smoking.delta.minutes = case_when(
    # Pre-Quit assessments
    (assessment.type == "Pre-Quit Smoking Part Two") & (assessment.type_shift_minus_1 != "Pre-Quit Smoking Part One") & (this.ave.hours.between.past.and.present <= hours.between.past.and.present) ~ this.ave.hours.between.past.and.present*60/2,
    (assessment.type == "Pre-Quit Smoking Part Two") & (assessment.type_shift_minus_1 != "Pre-Quit Smoking Part One") & (this.ave.hours.between.past.and.present > hours.between.past.and.present) ~ hours.between.past.and.present*60/2,
    # Else
    TRUE ~ smoking.delta.minutes
  ))

df.all <- df.all %>%
  mutate(smoking.delta.minutes = replace(smoking.delta.minutes, smoking.qty==0, NA_real_)) %>%
  mutate(smoking.delta.minutes = replace(smoking.delta.minutes, is.na(smoking.qty), NA_real_))


