# Applicant Flow Analysis 

## Overview

This repository demonstrates my applicant flow analysis methodology.

The analysis shown here uses **synthetic data** to protect client confidentiality while showcasing the statistical approach I apply to hiring discrimination cases.

## About This Work

My background includes:
- **EEOC Statistical Expert:** Conducted systemic investigations into hiring, promotion, and compensation discrimination
- **Law Firm Consultant:** Supported Title VII litigation with applicant flow and adverse impact analyses  
- **AI Fairness Auditor:** Evaluated algorithmic hiring tools for bias and regulatory compliance 

This demonstration replicates my analytical workflow without exposing confidential case information.

## Methodology

The analysis follows EEOC Uniform Guidelines on Employee Selection Procedures (UGESP):
- Contingency table analysis (passed vs. rejected by protected class)
- Fisher's exact test for statistical significance
- Expected values and deviation calculations (chi-square framework)
- Selection rate and impact ratio computations per the 4/5ths rule
- Cochran-Mantel-Haenszel tests for stratification decisions


## Technical Approach

- **Language:** R (primary tool for federal employment discrimination analysis)
- **Sample Size:** n=100 synthetic applicants
- **Statistical Tests:** Fisher's exact test, chi-square expected values, Breslow-Day, Mantel-Haenszel
- **Output Format:** Publication-quality tables matching EEOC investigation standards
- **Documentation:** Comprehensive R Markdown with reproducible workflow

## What's Demonstrated

- Multi-stage applicant flow analysis (Screen → Interview → Assessment → Selection)  
- Gender and race/ethnicity analysis per EEOC standards  
- Contingency tables with frequency/expected/deviation/percentages  
- Selection rate and impact ratio calculations  
- Statistical significance testing (Fisher's exact)  
- Adverse impact determination (4/5ths rule)  
- Stratification testing (Breslow-Day + Mantel-Haenszel)  
- Professional reporting format with interpretation

## Repository Structure

```
applicant-flow-demo/
├── README.md                              # This file
├── PROJECT_CHECKLIST.md                   # Detailed component list
├── data/
│   └── generate_synthetic_data.R          # Creates n=100 test records
├── functions/
│   └── applicant_flow_functions.R         # Reusable analysis functions
└── analysis/
    └── applicant_flow_analysis.Rmd        # Complete analytical workflow
```

## Quick Start

```r
# 1. Install required packages
install.packages(c("dplyr", "knitr", "kableExtra", "ggplot2", "tidyr", "DescTools"))

# 2. Generate synthetic data
source("data/generate_synthetic_data.R")

# 3. Run analysis
rmarkdown::render("analysis/applicant_flow_analysis.Rmd")
```

## Key Functions

### `create_flow_analysis()`
Comprehensive adverse impact analysis for a single stage:
- Contingency table construction
- Expected values & deviations
- Selection rates & impact ratios  
- Fisher's exact test
- Adverse impact flagging
- Shortfall calculations

### `test_stratification()`
Determines if stratified analysis is needed:
- Breslow-Day test (homogeneity of odds ratios across strata)
- Mantel-Haenszel test (association controlling for stratification variable)
- Interpretation & recommendations

### `format_flow_table()`
Publication-ready table formatting:
- Frequency, expected, deviation 
- HTML/LaTeX/Markdown output
- Automatic adverse impact highlighting

## Sample Output

The analysis produces tables in this format:

```
Applicant Flow Analysis: Software Engineer - Interview Stage

Group: Female
  Frequency:      120 passed,  180 rejected  (Total: 300)
  Expected:       135.0,       165.0
  Deviation:      -15.0,       +15.0
  Row Pct:        40.0%,       60.0%
  Col Pct:        44.4%,       52.6%
  Selection Rate: 40.0%
  Impact Ratio:   0.78  ⚠️ Adverse Impact

Reference Group: Male (51.4%)
Fisher's Exact Test: p = 0.032 *
Adverse Impact: YES

Shortfall: 15.0 female applicants compared to expected values
```

## Conclusions

This demonstration shows how multi-stage applicant flow analysis identifies where disparities emerge, quantifies both practical and statistical significance, and supports fair, defensible decision-making. The methodology aligns with EEOC and UGESP standards and applies across hiring, promotion, pay, and termination decisions.

## Confidentiality Note

All data in this repository is entirely synthetic and created for demonstration purposes only. No real applicant or employer records are included.

## Adaptability

This methodology applies to:
- Title VII hiring, promotion, discharge, and compensation analyses
- Algorithmic fairness assessments
- Pay equity analyses
- Promotion and termination analyses  
- Affirmative action plan assessments
- OFCCP compliance reviews

## Statistical Rigor

This analysis demonstrates:
- **Practical significance:** Impact ratios < 0.80 (4/5ths rule)
- **Statistical significance:** Fisher's exact test (appropriate for small samples)
- **Effect size emphasis:** Selection rate differentials and shortfall calculations
- **Stratification testing:** CMH methods to determine when pooling is appropriate
- **Highest group as reference:** EEOC standard (not pre-determined reference)

## Scope Note

This demonstration analyzes a single job role (Software Engineer). In actual engagements, I stratify analysis by job title, department, and location as appropriate to ensure valid comparisons among similarly situated applicants. The stratification testing functions demonstrate the methodology for determining when separate analysis is statistically warranted.

## Author

**Alicia Lee, PhD**  
  Employment discrimination and quantitative fairness specialist.

**Email:** alicialee53@gmail.com
**LinkedIn:** https://www.linkedin.com/in/aliciajlee/


## License

MIT License - This code is provided for demonstration and educational purposes.

