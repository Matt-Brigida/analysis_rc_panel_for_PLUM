### tables of summary statistics

library(plm)
library(stargazer)

call_all <- readRDS("./callall_pdata.rds")

## create MDI and non-MDI subsets-----------------

mdi_subset <- readRDS("../call_report_mdi_subset.rds")

not_mdi_subset <- readRDS("../call_report_not_mdi_subset.rds")

###

## variables

var <- c("CSBLTOT", "CNUMSBL", "SBLTOT_TA", "EQTA0", "ROA0", "NPA0", "LIQTA0", "CORETA0", "BCOMMITTAC0", "LNTA0", "BIGBANK", "BADBANK", "DENOVO")

mdi_vars <- data.frame(mdi_subset[, var])

not_mdi_vars <- data.frame(not_mdi_subset[, var])

## create tables with stargazer

stargazer(mdi_vars, type = "html", title="Descriptive Statistics: MDIs", digits=3, out="mdi_vars_summary_statistics.htm", covariate.labels = c("% Change Amt. S. Bus. Loans", "% Change Num. S. Bus. Loans", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "ln(Total Assets)", "Large Bank", "Troubled Bank", "De Novo"))

stargazer(not_mdi_vars, type = "html", title="Descriptive Statistics: Non-MDIs", digits=3, out="not_mdi_vars_summary_statistics.htm", covariate.labels = c("% Change Amt. S. Bus. Loans", "% Change Num. S. Bus. Loans", "Loans", "Total Equity", "ROA", "NPLs", "Liquid Assets", "Core Deposits", "Committments", "ln(Total Assets)", "Large Bank", "Troubled Bank", "De Novo"))
