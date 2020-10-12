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
  mutate(rawdata_timing = as.numeric(rawdata_timing)) %>%
  mutate(rawdata_timing = case_when(
    # Post-Quit assessments
    assessment_type == "Post-Quit Random" ~ rawdata_timing-1,
    assessment_type == "Post-Quit Urge" ~ rawdata_timing-1,
    # Pre-Quit assessments
    assessment_type == "Pre-Quit Random" ~ rawdata_timing-1,
    assessment_type == "Pre-Quit Urge" ~ rawdata_timing-1,
    # Else
    TRUE ~ rawdata_timing
  ))

df.all <- df.all %>%
  mutate(smoking_delta_minutes = case_when(
    # Post-Quit assessments
    assessment_type == "Post-Quit Random" & rawdata_timing!=8 ~ (15*rawdata_timing +15* (rawdata_timing+1))/2,
    assessment_type == "Post-Quit Urge" & rawdata_timing!=8 ~ (15*rawdata_timing +15* (rawdata_timing+1))/2,
    assessment_type == "Post-Quit Already Slipped" & rawdata_timing!=8 ~ (15*rawdata_timing +15* (rawdata_timing+1))/2,
    # Pre-Quit assessments
    assessment_type == "Pre-Quit Random" & rawdata_timing!=8 ~ (15*rawdata_timing +15* (rawdata_timing+1))/2,
    assessment_type == "Pre-Quit Urge" & rawdata_timing!=8 ~ (15*rawdata_timing +15* (rawdata_timing+1))/2,
    # Else
    TRUE ~ smoking_delta_minutes
  ))

df.all <- df.all %>%
  mutate(smoking_delta_minutes = case_when(
    # Post-Quit assessments
    assessment_type == "Post-Quit Random" & rawdata_timing==8 ~ 2*60 + (1/60)*((time_unixts-2*60*60) - time_unixts_shift_minus_1)/2,
    assessment_type == "Post-Quit Urge" & rawdata_timing==8 ~ 2*60 + (1/60)*((time_unixts-2*60*60) - time_unixts_shift_minus_1)/2,
    assessment_type == "Post-Quit Already Slipped" & rawdata_timing==8 ~ 2*60 + (1/60)*((time_unixts-2*60*60) - time_unixts_shift_minus_1)/2,
    # Pre-Quit assessments
    assessment_type == "Pre-Quit Random" & rawdata_timing==8 ~ 2*60 + (1/60)*((time_unixts-2*60*60) - time_unixts_shift_minus_1)/2,
    assessment_type == "Pre-Quit Urge" & rawdata_timing==8 ~ 2*60 + (1/60)*((time_unixts-2*60*60) - time_unixts_shift_minus_1)/2,
    # Else
    TRUE ~ smoking_delta_minutes
  ))

df.all <- df.all %>% 
  mutate(smoking_delta_minutes = if_else(smoking_delta_minutes > hours_between_past_and_present*60, hours_between_past_and_present*60/2, smoking_delta_minutes))

df.all <- df.all %>%
  mutate(smoking_delta_minutes = case_when(
    # Post-Quit assessments
    assessment_type == "Post-Quit About to Slip Part Two" & assessment_type_shift_minus_1 == "Post-Quit About to Slip Part One" ~ hours_between_past_and_present*60/2,
    # Pre-Quit assessments
    assessment_type == "Pre-Quit Smoking Part Two" & assessment_type_shift_minus_1 == "Pre-Quit Smoking Part One" ~ hours_between_past_and_present*60/2,
    # Else
    TRUE ~ smoking_delta_minutes
  ))

this_ave_hours_between_past_and_present <- df.all %>% filter(assessment_type == "Pre-Quit Smoking Part Two" & assessment_type_shift_minus_1 == "Pre-Quit Smoking Part One")
this_ave_hours_between_past_and_present <- mean(this_ave_hours_between_past_and_present$hours_between_past_and_present, na.rm=TRUE)

df.all <- df.all %>%
  mutate(smoking_delta_minutes = case_when(
    # Pre-Quit assessments
    (assessment_type == "Pre-Quit Smoking Part Two") & (assessment_type_shift_minus_1 != "Pre-Quit Smoking Part One") & (this_ave_hours_between_past_and_present <= hours_between_past_and_present) ~ this_ave_hours_between_past_and_present*60/2,
    (assessment_type == "Pre-Quit Smoking Part Two") & (assessment_type_shift_minus_1 != "Pre-Quit Smoking Part One") & (this_ave_hours_between_past_and_present > hours_between_past_and_present) ~ hours_between_past_and_present*60/2,
    # Else
    TRUE ~ smoking_delta_minutes
  ))

df.all <- df.all %>%
  mutate(smoking_delta_minutes = replace(smoking_delta_minutes, smoking_qty==0, NA_real_)) %>%
  mutate(smoking_delta_minutes = replace(smoking_delta_minutes, is.na(smoking_qty), NA_real_))


