#' Applicant Flow Analysis Functions
#' 
#' #' Methodology: UGESP 4/5ths rule, Fisher's exact test, CMH stratification testing
#' 
#' File: functions/applicant_flow_functions.R

# Install required packages
install_if_missing <- function(pkgs) {
  new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(new_pkgs)) install.packages(new_pkgs)
}

install_if_missing(c(
  "dplyr",
  "DescTools"
))

library(dplyr)
library(DescTools)

#' Create comprehensive applicant flow table with statistical tests
#' 
#' @param data Applicant-level data frame
#' @param group_var Protected class variable ("gender" or "race_ethnicity")
#' @param outcome_var Binary outcome variable (1 = passed, 0 = rejected)
#' @param stage_name Stage name for display
#' @param role_name Job title for display
#' @param min_threshold Minimum % to include (default = 0.02)
#' 
#' @return List with table, statistics, and interpretation
#' 
create_flow_analysis <- function(data,
                                 group_var,
                                 outcome_var,
                                 stage_name = "Stage",
                                 role_name = "Role",
                                 min_threshold = 0.02) {
  
  # Remove unknown categories for analysis
  data_clean <- data %>%
    filter(!!sym(group_var) != "Unknown")
  
  # Create outcome display variable
  data_clean <- data_clean %>%
    mutate(outcome_display = ifelse(!!sym(outcome_var) == 1, "Passed", "Rejected"))
  
  # Create contingency table
  cont_table <- table(data_clean[[group_var]], data_clean$outcome_display)
  
  # Ensure column order (Rejected, Passed)
  if (all(c("Rejected", "Passed") %in% colnames(cont_table))) {
    cont_table <- cont_table[, c("Rejected", "Passed")]
  }
  
  # Calculate expected values (chi-square)
  chi_test <- suppressWarnings(chisq.test(cont_table))
  expected <- chi_test$expected
  
  # Calculate deviations
  deviation <- cont_table - expected
  
  # Calculate percentages
  row_pct <- prop.table(cont_table, margin = 1) * 100
  col_pct <- prop.table(cont_table, margin = 2) * 100
  
  # Row totals
  row_totals <- rowSums(cont_table)
  total_n <- sum(cont_table)
  
  # Calculate selection rates
  selection_rates <- cont_table[, "Passed"] / row_totals
  
  # Identify highest selection rate (reference group per LL144)
  max_rate <- max(selection_rates)
  reference_group <- names(which.max(selection_rates))
  
  # Calculate impact ratios
  impact_ratios <- selection_rates / max_rate
  
  # Fisher's exact test
  fisher_result <- suppressWarnings(fisher.test(cont_table))
  fisher_p <- fisher_result$p.value
  
  # Identify groups below 2% threshold
  pct_of_total <- row_totals / total_n
  below_threshold <- pct_of_total < min_threshold
  
  # Identify adverse impact (IR < 0.80)
  adverse_impact <- impact_ratios < 0.80 & !below_threshold
  
  # Count unknowns
  n_unknown <- sum(data[[group_var]] == "Unknown")
  
  # Build formatted results table
  results_list <- list()
  
  for (i in 1:nrow(cont_table)) {
    group_name <- rownames(cont_table)[i]
    
    group_results <- list(
      group = group_name,
      n_rejected = cont_table[i, "Rejected"],
      n_passed = cont_table[i, "Passed"],
      n_total = row_totals[i],
      expected_rejected = expected[i, "Rejected"],
      expected_passed = expected[i, "Passed"],
      deviation_rejected = deviation[i, "Rejected"],
      deviation_passed = deviation[i, "Passed"],
      row_pct_rejected = row_pct[i, "Rejected"],
      row_pct_passed = row_pct[i, "Passed"],
      col_pct_rejected = col_pct[i, "Rejected"],
      col_pct_passed = col_pct[i, "Passed"],
      pct_of_total = pct_of_total[i] * 100,
      selection_rate = selection_rates[i] * 100,
      impact_ratio = impact_ratios[i],
      below_threshold = below_threshold[i],
      adverse_impact = adverse_impact[i]
    )
    
    results_list[[i]] <- group_results
  }
  
  # Create interpretation text
  ai_groups <- names(impact_ratios[adverse_impact])
  
  interpretation <- paste0(
    "Applicant Flow Analysis: ", role_name, " - ", stage_name, "\n",
    "Total Applicants (excl. Unknown): ", format(total_n, big.mark = ","), "\n",
    "Unknown ", group_var, ": ", n_unknown, " (excluded from calculations)\n\n",
    "Reference Group (Highest Selection Rate): ", reference_group, 
    " (", sprintf("%.1f%%", max_rate * 100), ")\n",
    "Fisher's Exact Test (2-sided): p = ", sprintf("%.4f", fisher_p),
    ifelse(fisher_p < 0.05, " *", ""), "\n",
    "Adverse Impact (IR < 0.80): ", ifelse(any(adverse_impact), "YES", "No"), "\n"
  )
  
  if (any(adverse_impact)) {
    interpretation <- paste0(
      interpretation,
      "\nGroups with Adverse Impact:\n"
    )
    
    for (grp in ai_groups) {
      shortfall <- expected[grp, "Passed"] - cont_table[grp, "Passed"]
      interpretation <- paste0(
        interpretation,
        sprintf("  • %s: IR = %.2f (shortfall of %.1f compared to expected)\n",
                grp, impact_ratios[grp], shortfall)
      )
    }
  }
  
  # Return comprehensive results
  return(list(
    results = results_list,
    contingency_table = cont_table,
    expected_values = expected,
    deviations = deviation,
    selection_rates = selection_rates,
    impact_ratios = impact_ratios,
    reference_group = reference_group,
    fisher_p = fisher_p,
    adverse_impact_found = any(adverse_impact),
    adverse_impact_groups = ai_groups,
    n_unknown = n_unknown,
    interpretation = interpretation,
    stage = stage_name,
    role = role_name
  ))
}


#' Test whether stratification is needed using CMH methods
#' 
#' Uses Breslow-Day and Mantel-Haenszel tests to determine if
#' relationship between protected class and outcome varies by role
#' 
#' @param data Applicant data with role variable
#' @param group_var Protected class variable
#' @param outcome_var Binary outcome variable
#' @param strata_var Stratification variable (e.g., "role", "department")
#' 
#' @return List with test results and recommendation
#' 
test_stratification <- function(data, group_var, outcome_var, strata_var) {
  
  # Remove unknowns
  data_clean <- data %>%
    filter(!!sym(group_var) != "Unknown")
  
  # Create 3-way contingency table
  cont_table <- table(
    data_clean[[outcome_var]],
    data_clean[[group_var]],
    data_clean[[strata_var]]
  )
  
  # Breslow-Day test for homogeneity of odds ratios
  bd_result <- tryCatch({
    BreslowDayTest(cont_table)
  }, error = function(e) {
    list(p.value = NA, 
         statistic = NA,
         message = "Unable to calculate (likely due to small n or zeros)")
  })
  
  # Mantel-Haenszel test
  mh_result <- tryCatch({
    mantelhaen.test(cont_table)
  }, error = function(e) {
    list(p.value = NA,
         statistic = NA,
         message = "Unable to calculate")
  })
  
  # Interpretation
  bd_p <- bd_result$p.value
  mh_p <- mh_result$p.value
  
  if (!is.na(bd_p) && !is.na(mh_p)) {
    if (bd_p < 0.05) {
      recommendation <- paste0(
        "Breslow-Day test suggests interaction (p = ", sprintf("%.4f", bd_p), "). ",
        "Consider separate analysis by ", strata_var, "."
      )
      stratify <- TRUE
    } else {
      recommendation <- paste0(
        "Breslow-Day test shows no interaction (p = ", sprintf("%.4f", bd_p), "). ",
        "Mantel-Haenszel p = ", sprintf("%.4f", mh_p),
        ifelse(mh_p < 0.05, 
               " suggests significant association controlling for strata.",
               " shows no significant association controlling for strata."),
        "\nPooled analysis across ", strata_var, " is appropriate."
      )
      stratify <- FALSE
    }
  } else {
    recommendation <- "Unable to perform stratification tests (insufficient data)."
    stratify <- NA
  }
  
  return(list(
    breslow_day_p = bd_p,
    breslow_day_stat = bd_result$statistic,
    mantel_haenszel_p = mh_p,
    mantel_haenszel_stat = mh_result$statistic,
    stratification_recommended = stratify,
    interpretation = recommendation
  ))
}


#' Format results as publication-ready table
#' 
#' @param analysis_results Output from create_flow_analysis()
#' @param format "html", "latex", or "markdown"
#' 
format_flow_table <- function(analysis_results, format = "html") {
  
  # Build data frame for table
  table_rows <- list()
  
  for (i in seq_along(analysis_results$results)) {
    res <- analysis_results$results[[i]]
    
    # Group header
    table_rows[[length(table_rows) + 1]] <- data.frame(
      Group = paste0("**", res$group, "**"),
      Measure = "",
      Rejected = "",
      Passed = "",
      Total = "",
      SelectionRate = "",
      ImpactRatio = "",
      Flag = ""
    )
    
    # Frequency
    table_rows[[length(table_rows) + 1]] <- data.frame(
      Group = "",
      Measure = "  Frequency",
      Rejected = format(res$n_rejected, big.mark = ","),
      Passed = format(res$n_passed, big.mark = ","),
      Total = format(res$n_total, big.mark = ","),
      SelectionRate = sprintf("%.1f%%", res$selection_rate),
      ImpactRatio = sprintf("%.2f", res$impact_ratio),
      Flag = ifelse(res$adverse_impact, "⚠️ AI", "")
    )
    
    # Expected
    table_rows[[length(table_rows) + 1]] <- data.frame(
      Group = "",
      Measure = "  Expected",
      Rejected = sprintf("%.1f", res$expected_rejected),
      Passed = sprintf("%.1f", res$expected_passed),
      Total = format(res$n_total, big.mark = ","),
      SelectionRate = "",
      ImpactRatio = "",
      Flag = ""
    )
    
    # Deviation
    table_rows[[length(table_rows) + 1]] <- data.frame(
      Group = "",
      Measure = "  Deviation",
      Rejected = sprintf("%+.1f", res$deviation_rejected),
      Passed = sprintf("%+.1f", res$deviation_passed),
      Total = "—",
      SelectionRate = "",
      ImpactRatio = "",
      Flag = ""
    )
    
    # Row %
    table_rows[[length(table_rows) + 1]] <- data.frame(
      Group = "",
      Measure = "  Row Pct",
      Rejected = sprintf("%.1f%%", res$row_pct_rejected),
      Passed = sprintf("%.1f%%", res$row_pct_passed),
      Total = "100.0%",
      SelectionRate = "",
      ImpactRatio = "",
      Flag = ""
    )
    
    # Col %
    table_rows[[length(table_rows) + 1]] <- data.frame(
      Group = "",
      Measure = "  Col Pct",
      Rejected = sprintf("%.1f%%", res$col_pct_rejected),
      Passed = sprintf("%.1f%%", res$col_pct_passed),
      Total = sprintf("%.1f%%", res$pct_of_total),
      SelectionRate = "",
      ImpactRatio = "",
      Flag = ifelse(res$below_threshold, "< 2%", "")
    )
  }
  
  # Combine into data frame
  table_df <- do.call(rbind, table_rows)
  
  # Add total row
  total_row <- data.frame(
    Group = "**Total**",
    Measure = "",
    Rejected = format(sum(analysis_results$contingency_table[, "Rejected"]), big.mark = ","),
    Passed = format(sum(analysis_results$contingency_table[, "Passed"]), big.mark = ","),
    Total = format(sum(analysis_results$contingency_table), big.mark = ","),
    SelectionRate = "—",
    ImpactRatio = "—",
    Flag = ""
  )
  
  table_df <- rbind(table_df, total_row)
  
  # Create kable
  kbl <- knitr::kable(
    table_df,
    format = format,
    col.names = c("Group", "Measure", "Rejected", "Passed", "Total",
                  "Selection Rate", "Impact Ratio", "Flag"),
    align = c("l", "l", "r", "r", "r", "r", "r", "c"),
    escape = FALSE,
    caption = paste0(analysis_results$role, " - ", analysis_results$stage)
  )
  
  if (format == "html") {
    
    if (!requireNamespace("kableExtra", quietly = TRUE)) {
      stop("The 'kableExtra' package is required for HTML table styling.")
    }
    
    kbl <- kbl %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE,
        font_size = 11
      ) %>%
      kableExtra::row_spec(0, bold = TRUE, background = "#e8e8e8")
  }
  
  return(kbl)
}

