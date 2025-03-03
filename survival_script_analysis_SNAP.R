# load packages
pacman::p_load(dplyr, arsenal, survival)

## HCV analysis ##

# baseline characteristics sex work
analysis_data_hcv <- analysis_data_hcv %>%
  group_by(subject) %>%
  mutate(id_seq = row_number())

analysis_data_hcv_bl <- subset(analysis_data_hcv, id_seq == 1)


sw_summary_bl <- table(analysis_data_hcv_bl$sxnocdme)
print(sw_summary_bl) ## 216 positive, 287 negative

sw_summary_long <- table(analysis_data_hcv$sxnocdme)
print(sw_summary_long) ## 216 positive, 287 negative

## incidence rate calculations

# overall incidence rate
total_days_hcv <- sum(analysis_data_hcv$days_risk)
total_cases <- sum(analysis_data_hcv$hcv_rslt)
incidence_rate <- (total_cases / total_days_hcv) * 365.25 *100

cat("Incidence rate of HCV per 100 person years:", incidence_rate)

# selling sex work incidence rate
analysis_data_hcv$sw_time_bin <- analysis_data_hcv$recent_sw
analysis_data_hcv$sw_time_bin[is.na(analysis_data_hcv$sw_time_bin)] <- 0
total_days_hcv_sw <- sum(analysis_data_hcv$days_risk[analysis_data_hcv$sw_time_bin == 1])
total_cases_sw <- sum(analysis_data_hcv$hcv_rslt[analysis_data_hcv$sw_time_bin == 1])
incidence_rate_sw <- (total_cases_sw / total_days_hcv_sw) * 365.25 *100

cat("Incidence rate of HCV per 100 person years among sex workers:", incidence_rate_sw)

# no sex work incidence rate
analysis_data_hcv$sw_time_bin <- analysis_data_hcv$recent_sw
analysis_data_hcv$sw_time_bin <- ifelse(is.na(analysis_data_hcv$sw_time_bin), 1, analysis_data_hcv$sw_time_bin)
total_days_hcv_nosw <- sum(analysis_data_hcv$days_risk[analysis_data_hcv$sw_time_bin == 0])
total_cases_nosw <- sum(analysis_data_hcv$hcv_rslt[analysis_data_hcv$sw_time_bin == 0])
incidence_rate_nosw <- (total_cases_nosw / total_days_hcv_nosw) * 365.25 *100

cat("Incidence rate of HCV per 100 person years among non-sex workers:", incidence_rate_nosw)
