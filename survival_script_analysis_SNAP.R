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

### overall incidence rate calculations ###

total_days_hcv <- sum(analysis_data_hcv_clean$days_risk)
total_cases <- sum(analysis_data_hcv_clean$hcv_rslt)
incidence_rate <- (total_cases / total_days_hcv) * 365.25 * 100

# Calculate 95% confidence intervals
incidence_rate_se <- sqrt(total_cases) / total_days_hcv * 365.25 * 100
ci_lower <- incidence_rate - 1.96 * incidence_rate_se
ci_upper <- incidence_rate + 1.96 * incidence_rate_se

cat("Incidence rate of HCV per 100 person years:", incidence_rate, "\n")
cat("95% CI:", ci_lower, "-", ci_upper, "\n")

# Function to calculate incidence rates and rate ratios
calculate_incidence_and_rate_ratio <- function(data, time_bin, group_label) {
  # selling sex work incidence rate
  total_days_hcv_sw <- sum(data$days_risk[data[[time_bin]] == 1])
  total_cases_sw <- sum(data$hcv_rslt[data[[time_bin]] == 1])
  incidence_rate_sw <- (total_cases_sw / total_days_hcv_sw) * 365.25 * 100

  # Calculate 95% confidence intervals for sex workers
  incidence_rate_sw_se <- sqrt(total_cases_sw) / total_days_hcv_sw * 365.25 * 100
  ci_lower_sw <- incidence_rate_sw - 1.96 * incidence_rate_sw_se
  ci_upper_sw <- incidence_rate_sw + 1.96 * incidence_rate_sw_se

  cat("Incidence rate of HCV per 100 person years among sex workers (", group_label, "):", incidence_rate_sw, "\n")
  cat("95% CI:", ci_lower_sw, "-", ci_upper_sw, "\n")

  # no sex work incidence rate
  total_days_hcv_nosw <- sum(data$days_risk[data[[time_bin]] == 0])
  total_cases_nosw <- sum(data$hcv_rslt[data[[time_bin]] == 0])
  incidence_rate_nosw <- (total_cases_nosw / total_days_hcv_nosw) * 365.25 * 100

  # Calculate 95% confidence intervals for non-sex workers
  incidence_rate_nosw_se <- sqrt(total_cases_nosw) / total_days_hcv_nosw * 365.25 * 100
  ci_lower_nosw <- incidence_rate_nosw - 1.96 * incidence_rate_nosw_se
  ci_upper_nosw <- incidence_rate_nosw + 1.96 * incidence_rate_nosw_se

  cat("Incidence rate of HCV per 100 person years among non-sex workers (", group_label, "):", incidence_rate_nosw, "\n")
  cat("95% CI:", ci_lower_nosw, "-", ci_upper_nosw, "\n")

  # Calculate rate ratio and its 95% confidence interval
  rate_ratio <- incidence_rate_sw / incidence_rate_nosw
  rate_ratio_se <- sqrt((1 / total_cases_sw) + (1 / total_cases_nosw))
  ci_lower_rr <- exp(log(rate_ratio) - 1.96 * rate_ratio_se)
  ci_upper_rr <- exp(log(rate_ratio) + 1.96 * rate_ratio_se)

  cat("Rate ratio of HCV (sex workers vs non-sex workers) (", group_label, "):", rate_ratio, "\n")
  cat("95% CI:", ci_lower_rr, "-", ci_upper_rr, "\n")

  # Create a summary dataset for Poisson regression
  summary_data <- data %>%
    group_by(!!sym(time_bin), homeless_recent_analysis, incarc_recent_analysis, oat_ever_analysis, daily_inj_6m_analysis, years_inj_analysis) %>%
    summarise(
      total_cases = sum(hcv_rslt),
      total_days = sum(days_risk)
    ) %>%
    mutate(
      rate = total_cases / total_days * 365.25 * 100
    )

  # Fit Poisson regression model controlling for homeless_recent and incarc_recent
  poisson_model1 <- glm(total_cases ~ get(time_bin) + homeless_recent_analysis + incarc_recent_analysis + oat_ever_analysis + offset(log(total_days)), 
                        family = poisson(link = "log"), 
                        data = summary_data)

  # Extract rate ratio and confidence intervals
  rate_ratio1 <- exp(coef(poisson_model1)[2])
  ci1 <- exp(confint(poisson_model1)[2, ])

  cat("Rate ratio of HCV (sex workers vs non-sex workers) controlling for homeless_recent and incarc_recent (", group_label, "):", rate_ratio1, "\n")
  cat("95% CI:", ci1[1], "-", ci1[2], "\n")

  # Fit Poisson regression model controlling for homeless_recent, incarc_recent, daily_inj_6m, and years_inj
  poisson_model2 <- glm(total_cases ~ get(time_bin) + homeless_recent_analysis + incarc_recent_analysis + oat_ever_analysis + daily_inj_6m_analysis + years_inj_analysis + offset(log(total_days)), 
                        family = poisson(link = "log"), 
                        data = summary_data)

  # Extract rate ratio and confidence intervals
  rate_ratio2 <- exp(coef(poisson_model2)[2])
  ci2 <- exp(confint(poisson_model2)[2, ])

  cat("Rate ratio of HCV (sex workers vs non-sex workers) controlling for homeless_recent, incarc_recent, daily_inj_6m, and years_inj (", group_label, "):", rate_ratio2, "\n")
  cat("95% CI:", ci2[1], "-", ci2[2], "\n")
}

# Calculate for recent exposure
analysis_data_hcv_clean <- analysis_data_hcv_clean %>%
  mutate(sw_time_bin_recent = ifelse(is.na(recent_sw), 0, recent_sw))

calculate_incidence_and_rate_ratio(analysis_data_hcv_clean, "sw_time_bin_recent", "recent exposure")

# Function to calculate incidence rates and rate ratios
calculate_incidence_and_rate_ratio <- function(data, time_bin, group_label) {
  calculate_for_gender <- function(data, gender_label) {
    # selling sex work incidence rate
    total_days_hcv_sw <- sum(data$days_risk[data[[time_bin]] == 1])
    total_cases_sw <- sum(data$hcv_rslt[data[[time_bin]] == 1])
    incidence_rate_sw <- (total_cases_sw / total_days_hcv_sw) * 365.25 * 100

    # Calculate 95% confidence intervals for sex workers
    incidence_rate_sw_se <- sqrt(total_cases_sw) / total_days_hcv_sw * 365.25 * 100
    ci_lower_sw <- incidence_rate_sw - 1.96 * incidence_rate_sw_se
    ci_upper_sw <- incidence_rate_sw + 1.96 * incidence_rate_sw_se

    cat("Incidence rate of HCV per 100 person years among sex workers (", group_label, ", ", gender_label, "):", incidence_rate_sw, "\n")
    cat("95% CI:", ci_lower_sw, "-", ci_upper_sw, "\n")

    # no sex work incidence rate
    total_days_hcv_nosw <- sum(data$days_risk[data[[time_bin]] == 0])
    total_cases_nosw <- sum(data$hcv_rslt[data[[time_bin]] == 0])
    incidence_rate_nosw <- (total_cases_nosw / total_days_hcv_nosw) * 365.25 * 100

    # Calculate 95% confidence intervals for non-sex workers
    incidence_rate_nosw_se <- sqrt(total_cases_nosw) / total_days_hcv_nosw * 365.25 * 100
    ci_lower_nosw <- incidence_rate_nosw - 1.96 * incidence_rate_nosw_se
    ci_upper_nosw <- incidence_rate_nosw + 1.96 * incidence_rate_nosw_se

    cat("Incidence rate of HCV per 100 person years among non-sex workers (", group_label, ", ", gender_label, "):", incidence_rate_nosw, "\n")
    cat("95% CI:", ci_lower_nosw, "-", ci_upper_nosw, "\n")

    # Calculate rate ratio and its 95% confidence interval
    rate_ratio <- incidence_rate_sw / incidence_rate_nosw
    rate_ratio_se <- sqrt((1 / total_cases_sw) + (1 / total_cases_nosw))
    ci_lower_rr <- exp(log(rate_ratio) - 1.96 * rate_ratio_se)
    ci_upper_rr <- exp(log(rate_ratio) + 1.96 * rate_ratio_se)

    cat("Rate ratio of HCV (sex workers vs non-sex workers) (", group_label, ", ", gender_label, "):", rate_ratio, "\n")
    cat("95% CI:", ci_lower_rr, "-", ci_upper_rr, "\n")

    # Create a summary dataset for Poisson regression
    summary_data <- data %>%
      group_by(!!sym(time_bin), homeless_recent_analysis, incarc_recent_analysis, oat_ever_analysis, daily_inj_6m_analysis, years_inj_analysis) %>%
      summarise(
        total_cases = sum(hcv_rslt),
        total_days = sum(days_risk),
        .groups = 'drop'
      ) %>%
      mutate(
        rate = total_cases / total_days * 365.25 * 100
      )

    # Fit Poisson regression model controlling for homeless_recent and incarc_recent
    poisson_model1 <- tryCatch(
      glm(total_cases ~ get(time_bin) +  homeless_recent_analysis + incarc_recent_analysis + offset(log(total_days)), 
          family = poisson(link = "log"), 
          data = summary_data),
      error = function(e) NULL
    )

    if (!is.null(poisson_model1)) {
      # Extract rate ratio and confidence intervals
      rate_ratio1 <- exp(coef(poisson_model1)[2])
      ci1 <- exp(confint(poisson_model1)[2, ])

      cat("Rate ratio of HCV (sex workers vs non-sex workers) controlling for homeless_recent and incarc_recent (", group_label, ", ", gender_label, "):", rate_ratio1, "\n")
      cat("95% CI:", ci1[1], "-", ci1[2], "\n")
    } else {
      cat("Poisson regression model did not converge for homeless_recent and incarc_recent (", group_label, ", ", gender_label, ")\n")
    }

    # Fit Poisson regression model controlling for homeless_recent, incarc_recent, daily_inj_6m, and years_inj
    poisson_model2 <- tryCatch(
      glm(total_cases ~ get(time_bin) + homeless_recent_analysis + incarc_recent_analysis + daily_inj_6m_analysis + years_inj_analysis + offset(log(total_days)), 
          family = poisson(link = "log"), 
          data = summary_data),
      error = function(e) NULL
    )

    if (!is.null(poisson_model2)) {
      # Extract rate ratio and confidence intervals
      rate_ratio2 <- exp(coef(poisson_model2)[2])
      ci2 <- exp(confint(poisson_model2)[2, ])

      cat("Rate ratio of HCV (sex workers vs non-sex workers) controlling for homeless_recent, incarc_recent, daily_inj_6m, and years_inj (", group_label, ", ", gender_label, "):", rate_ratio2, "\n")
      cat("95% CI:", ci2[1], "-", ci2[2], "\n")
    } else {
      cat("Poisson regression model did not converge for homeless_recent, incarc_recent, daily_inj_6m, and years_inj (", group_label, ", ", gender_label, ")\n")
    }
  }

  # Calculate for women (gender == 2)
  calculate_for_gender(data %>% filter(gender == 2), "women")

  # Calculate for men (gender == 1)
  calculate_for_gender(data %>% filter(gender == 1), "men")
}

# Calculate for recent exposure
analysis_data_hcv_clean <- analysis_data_hcv_clean %>%
  mutate(sw_time_bin_recent = ifelse(is.na(recent_sw), 0, recent_sw))

calculate_incidence_and_rate_ratio(analysis_data_hcv_clean, "sw_time_bin_recent", "recent exposure")