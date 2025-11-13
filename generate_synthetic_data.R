#' Generate Synthetic Applicant Flow Data
#' 
#' Creates realistic hiring funnel data for demonstration purposes.
#' All data is entirely fictional - no real applicant or employer data included.
#' 
#' Pattern simulated: Late-stage disparity (common in real audits)
#' - Early stages show relative parity
#' - Disparities emerge at later decision points
#' 
#' File: data/generate_synthetic_data.R

# Install required packages
install_if_missing <- function(pkgs) {
  new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(new_pkgs)) install.packages(new_pkgs)
}

install_if_missing(c(
  "dplyr"
))

library(dplyr)

set.seed(23)  # Reproducible results

generate_applicant_flow_data <- function(n_applicants = 100, 
                                         role = "Software Engineer") {
  
  # Generate protected class distributions
  # Proportions based on typical tech sector demographics
  
  applicants <- data.frame(
    applicant_id = paste0("SYNTH_", sprintf("%05d", 1:n_applicants)),
    role = role
  )
  
  # Gender distribution
  applicants$gender <- sample(
    c("Female", "Male", "Other"),
    size = n_applicants,
    replace = TRUE,
    prob = c(0.25, 0.75, 0.02)
  )
  
  # Race/ethnicity distribution (EEOC EEO-1 categories)
  applicants$race_ethnicity <- sample(
    c("White", "Asian", "Black or African American", 
      "Hispanic or Latino", "Two or More Races", 
      "Native Hawaiian or Pacific Islander",
      "Native American or Alaska Native"),
    size = n_applicants,
    replace = TRUE,
    prob = c(0.43, 0.40, 0.06, 0.07, 0.03, 0.005, 0.005)
  )
  
  # Stage 1: Resume Screening (minimal disparity)
  applicants$screen_pass_prob <- case_when(
    applicants$gender == "Female" ~ 0.50,
    applicants$gender == "Male" ~ 0.52,
    applicants$gender == "Other" ~ 0.48
  )
  
  applicants$screen_passed <- rbinom(
    n = n_applicants, 
    size = 1, 
    prob = applicants$screen_pass_prob
  )
  
  # Stage 2: Interview (slight disparity begins)
  # Only those who passed screening advance
  applicants$interview_pass_prob <- case_when(
    applicants$screen_passed == 0 ~ 0,  # Didn't pass screen
    applicants$gender == "Female" ~ 0.65,
    applicants$gender == "Male" ~ 0.70,
    applicants$gender == "Other" ~ 0.60
  )
  
  applicants$interview_passed <- rbinom(
    n = n_applicants,
    size = 1,
    prob = applicants$interview_pass_prob
  )
  
  # Stage 3: Technical Assessment (disparity widens)
  applicants$assessment_pass_prob <- case_when(
    applicants$interview_passed == 0 ~ 0,  # Didn't pass interview
    applicants$gender == "Female" ~ 0.55,
    applicants$gender == "Male" ~ 0.65,
    applicants$gender == "Other" ~ 0.50
  )
  
  applicants$assessment_passed <- rbinom(
    n = n_applicants,
    size = 1,
    prob = applicants$assessment_pass_prob
  )
  
  # Stage 4: Final Selection (strongest disparity - offer stage)
  applicants$selection_prob <- case_when(
    applicants$assessment_passed == 0 ~ 0,  # Didn't pass assessment
    applicants$gender == "Female" ~ 0.35,
    applicants$gender == "Male" ~ 0.50,
    applicants$gender == "Other" ~ 0.30
  )
  
  applicants$selected <- rbinom(
    n = n_applicants,
    size = 1,
    prob = applicants$selection_prob
  )
  
  # Add some "Unknown" categories (realistic data quality issue)
  unknown_gender_mask <- sample(1:n_applicants, size = round(0.02 * n_applicants))
  applicants$gender[unknown_gender_mask] <- "Unknown"
  
  unknown_race_mask <- sample(1:n_applicants, size = round(0.05 * n_applicants))
  applicants$race_ethnicity[unknown_race_mask] <- "Unknown"
  
  # Clean up temporary probability columns
  applicants <- applicants %>%
    select(-contains("_prob"))
  
  return(applicants)
}

# Generate data
applicants_raw <- generate_applicant_flow_data(n_applicants = 100, 
                                               role = "Software Engineer")
# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
  cat("✅ Created 'data' directory\n")
}

# Save
write.csv(applicants_raw, "data/synthetic_applicants.csv", row.names = FALSE)

cat("✅ Generated", nrow(applicants_raw), "synthetic applicant records\n")
cat("Role:", unique(applicants_raw$role), "\n")
cat("\nGender distribution:\n")
print(table(applicants_raw$gender))
cat("\nRace/ethnicity distribution:\n")
print(table(applicants_raw$race_ethnicity))

cat("\n⚠️  CONFIDENTIALITY NOTE: This is entirely fictional data for demonstration only\n")
