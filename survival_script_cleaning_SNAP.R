## load packages
pacman::p_load(dplyr, haven, tidyr, haven, readxl, writexl, survival)

## set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/Publications/Sex work and risk of HIV and HCV/Emails to authors/SNAP data/Data")

## load data
hcv_data_wide <- read_dta('SNAP dataset 2-23-24.dta')

# save data
write_xlsx(hcv_data_wide,"HCV_data_wide.xlsx")

# remove empty rows
hcv_data_wide <- hcv_data_wide[!is.na(hcv_data_wide$dob0), ]

hcv_summary_bl <- table(hcv_data_wide$hcv0)
print(hcv_summary_bl) ## 216 positive, 287 negative

# List of column names to convert to character
columns_to_convert <- c("hcv0", "hcv6", "hcv12", "hcv18", "hcv24", "hcv30", "hcv36", "hcv42")

# Convert specified columns to character type
hcv_data_wide[columns_to_convert] <- lapply(hcv_data_wide[columns_to_convert], as.character)

# remove Ab positives at baseline
hcv_data_wide <- subset(hcv_data_wide, hcv0 == 0)

hcv_summary_bl <- table(hcv_data_wide$hcv0)
print(hcv_summary_bl) ## 0 positive, 287 negative

# create new lifetime msm variable
hcv_data_wide$lifetime_msm <- rowSums(hcv_data_wide[, c("sexmale0", "sexmale6", "sexmale12", "sexmale18", "sexmale24", "sexmale30", "sexmale36", "sexmale42")], na.rm = TRUE)

# Set lifetime_msm to 1 if any of the variables are greater than 0, otherwise set it to 0
hcv_data_wide$lifetime_msm <- ifelse(hcv_data_wide$lifetime_msm > 0, 1, 0)

# create new recent sex work variable

# split up study visits
hcv_data_bl <- subset(hcv_data_wide, select = c(subject, gender0, age1stin0, lifetime_msm))
hcv_data_v1 <- subset(hcv_data_wide, select = c(subject, today0, age0, living0, inj6m0, trtmntb0, conenva0, sex30m0, nocdmtr0, sxnocdme0, hcv0))
hcv_data_v2 <- subset(hcv_data_wide, select = c(subject, today6, age6, living6, inj6m6, trtmntb6, conenva6, sex30m6, nocdmtr6, sxnocdme6, hcv6))
hcv_data_v3 <- subset(hcv_data_wide, select = c(subject, today12, age12, living12, inj6m12, trtmntb12, conenva12, sex30m12, nocdmtr12, sxnocdme12, hcv12))
hcv_data_v4 <- subset(hcv_data_wide, select = c(subject, today18, age18, living18, inj6m18, trtmntb18, conenva18, sex30m18, nocdmtr18, sxnocdme18, hcv18))
hcv_data_v5 <- subset(hcv_data_wide, select = c(subject, today24, age24, living24, inj6m24, trtmntb24, conenva24, sex30m24, nocdmtr24, sxnocdme24, hcv24))
hcv_data_v6 <- subset(hcv_data_wide, select = c(subject, today30, age30, living30, inj6m30, trtmntb30, conenva30, sex30m30, nocdmtr30, sxnocdme30, hcv30))
hcv_data_v7 <- subset(hcv_data_wide, select = c(subject, today36, age36, living36, inj6m36, trtmntb36, conenva36, sex30m36, nocdmtr36, sxnocdme36, hcv36))
hcv_data_v8 <- subset(hcv_data_wide, select = c(subject, today42, age42, living42, inj6m42, trtmntb42, conenva42, sex30m42, nocdmtr42, sxnocdme42, hcv42))

# list of dataframe names
dataframe_names <- c("hcv_data_v1", "hcv_data_v2", "hcv_data_v3", "hcv_data_v4", "hcv_data_v5", "hcv_data_v6", "hcv_data_v7", "hcv_data_v8")

# loop through each dataframe to remove time stamps
for (df_name in dataframe_names) {
  # Remove numbers from the end of variable names
  assign(df_name, `names<-`(get(df_name), gsub("\\d+$", "", names(get(df_name)))))
}

# reshape study visits
hcv_data_long <- bind_rows(hcv_data_v1, hcv_data_v2, hcv_data_v3, hcv_data_v4, hcv_data_v5, hcv_data_v6, hcv_data_v7) %>%
  arrange(subject, today, hcv)

# Merge hcv_data_long with the baseline dataframe based on 'subject'
hcv_data_long <- left_join(hcv_data_long, hcv_data_bl, by = "subject")

# save long data
write_xlsx(hcv_data_long,"HCV_data_long.xlsx")

# remove missing rows
hcv_data_long <- hcv_data_long[!is.na(hcv_data_long$today), ]
hcv_data_long <- hcv_data_long[!is.na(hcv_data_long$hcv), ]

# find first positive hcv test
hcv_data_long <- hcv_data_long %>%
  filter(hcv == 1) %>%
  group_by(subject) %>%
  summarise(date_first_pos = min(today, na.rm = TRUE)) %>%
  left_join(hcv_data_long, ., by = "subject")

# remove all rows after first positive hcv test
hcv_data_long <- hcv_data_long %>%
  filter(is.na(date_first_pos) | today <= date_first_pos)

## testing data

# keep columns of interest
testing_df <- subset(hcv_data_long, select = c(subject, today, hcv)) 

# create lag of test (using lead function)
testing_df <- testing_df %>%
  arrange(subject, today) %>%  
  group_by(subject) %>%
  mutate(hcv_end = lead(today),
         hcv_rslt = lead(hcv))  

# rename start date to avoid confusion
testing_df <- testing_df %>%
  rename(hcv_start = today)

# remove Nth row for each participant (because of lead)
testing_df <- testing_df %>%
  group_by(subject) %>%
  filter(row_number() < n())

testing_df <- subset(testing_df, select = c(subject, hcv_start, hcv_end, hcv_rslt)) 

# calculate difference between end and start dates
testing_df <- testing_df %>%
  mutate(days_risk = hcv_end-hcv_start)

testing_df <- testing_df %>%
  arrange(subject) %>%
  group_by(subject) %>%
  mutate(days_risk_end = cumsum(as.numeric(days_risk, units = "days")))

testing_df <- testing_df %>%
  arrange(subject) %>%
  group_by(subject) %>%
  mutate(days_risk_start = lag(days_risk_end, default = 0))

testing_df <- subset(testing_df, select = c(subject, days_risk, days_risk_start, days_risk_end, hcv_rslt)) 

hcv_summary_long <- table(testing_df$hcv_rslt)
print(hcv_summary_long) ## 108 incident infections

write_xlsx(testing_df,"HCV_testing_data.xlsx")

## exposure data

# keep columns of interest
exposure_df <- subset(hcv_data_long, select = c(subject, gender, living, inj6m, trtmntb, conenva, sex30m, nocdmtr, sxnocdme, lifetime_msm, hcv)) 

exposure_df <- exposure_df %>%
  arrange(subject) %>%
  group_by(subject) %>%
  slice(-n())  

# Set recent_sw to 1 if any of the variables are greater than 0, otherwise set it to 0
exposure_df$recent_sw <- ifelse(exposure_df$nocdmtr > 0, 1, 0)
exposure_df$recent_sw_part <- ifelse(exposure_df$sxnocdme > 0, 1, 0)

# Recode sex30m so that any value above 1 is equal to 1
exposure_df <- exposure_df %>%
  mutate(sex30m = ifelse(sex30m > 1, 1, sex30m))

# Create new variable homeless_recent
exposure_df <- exposure_df %>%
  mutate(homeless_recent = ifelse(living == 9, 1, 0))

# rename variables
exposure_df <- exposure_df %>%
  rename(oat_ever = trtmntb) %>%
  rename(msm_recent = sex30m) %>%
  rename(incarc_recent = conenva)

# Recode msm_recent and lifetime_msm to 0 if gender equals 2
exposure_df <- exposure_df %>%
  mutate(msm_recent = ifelse(gender == 2, NA, msm_recent),
         lifetime_msm = ifelse(gender == 2, NA, lifetime_msm))

sw_summary <- table(exposure_df$recent_sw)
print(sw_summary) ## 108 incident infections

sw_summary_part <- table(exposure_df$recent_sw_part)
print(sw_summary_part) ## 108 incident infections

# create analysis df

# sequence by id for merge
exposure_df <- exposure_df %>%
  arrange(subject) %>%
  mutate(id_seq = row_number())

testing_df <- testing_df %>%
  arrange(subject) %>%
  mutate(id_seq = row_number())

# merge
analysis_data_hcv <- left_join(exposure_df, testing_df, by = c("subject", "id_seq"))

analysis_data_hcv$hcv_rslt <- as.numeric(analysis_data_hcv$hcv_rslt)
analysis_data_hcv$days_risk <- as.numeric(analysis_data_hcv$days_risk)

# save data
write_xlsx(analysis_data_hcv,"HCV_data_clean.xlsx")
