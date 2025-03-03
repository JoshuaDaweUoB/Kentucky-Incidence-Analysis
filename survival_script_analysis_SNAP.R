# load packages
pacman::p_load(arsenal, survival, readxl, dplyr)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Emails to authors/SNAP data/Data")

# load data
analysis_data_hcv_clean <- read_excel("HCV_data_clean.xlsx")
head(analysis_data_hcv_clean)

## HCV analysis ##

# baseline characteristics sex work
analysis_data_hcv <- analysis_data_hcv %>%
  group_by(subject) %>%
  mutate(id_seq = row_number())

analysis_data_hcv_bl <- analysis_data_hcv_clean %>%
  group_by(subject) %>%
  mutate(id_seq = row_number(),
         recent_sw = ifelse(all(is.na(recent_sw)), NA_real_, max(recent_sw, na.rm = TRUE)),
         incarc_recent = ifelse(all(is.na(incarc_recent)), NA_real_, max(incarc_recent, na.rm = TRUE)),
         oat_ever = ifelse(all(is.na(oat_ever)), NA_real_, max(oat_ever, na.rm = TRUE)),
         msm_recent = ifelse(all(is.na(msm_recent)), NA_real_, max(msm_recent, na.rm = TRUE)),
         lifetime_msm = ifelse(all(is.na(lifetime_msm)), NA_real_, max(lifetime_msm, na.rm = TRUE)),
         homeless_recent = ifelse(all(is.na(homeless_recent)), NA_real_, max(homeless_recent, na.rm = TRUE)),
         gender = ifelse(all(is.na(gender)), NA_real_, max(gender, na.rm = TRUE)),
         daily_inj_6m = ifelse(all(is.na(daily_inj_6m)), NA_real_, max(daily_inj_6m, na.rm = TRUE)),
         years_inj = ifelse(all(is.na(years_inj)), NA_real_, max(years_inj, na.rm = TRUE))) %>%
  ungroup() %>%
  subset(id_seq == 1)

# convert numeric to factor variables
analysis_data_hcv_bl$gender <- factor(analysis_data_hcv_bl$gender)
analysis_data_hcv_bl$recent_sw <- factor(analysis_data_hcv_bl$recent_sw)
analysis_data_hcv_bl$incarc_recent <- factor(analysis_data_hcv_bl$incarc_recent)
analysis_data_hcv_bl$oat_ever <- factor(analysis_data_hcv_bl$oat_ever)
analysis_data_hcv_bl$msm_recent <- factor(analysis_data_hcv_bl$msm_recent)
analysis_data_hcv_bl$lifetime_msm <- factor(analysis_data_hcv_bl$lifetime_msm)
analysis_data_hcv_bl$homeless_recent <- factor(analysis_data_hcv_bl$homeless_recent)
analysis_data_hcv_bl$daily_inj_6m <- factor(analysis_data_hcv_bl$daily_inj_6m)

# table
table_bl_sw_msm_hcv <- tableby(gender ~ age + recent_sw + incarc_recent + oat_ever + homeless_recent + msm_recent + lifetime_msm + daily_inj_6m + years_inj, data = analysis_data_hcv_bl)
summary(table_bl_sw_msm_hcv, text=TRUE)

