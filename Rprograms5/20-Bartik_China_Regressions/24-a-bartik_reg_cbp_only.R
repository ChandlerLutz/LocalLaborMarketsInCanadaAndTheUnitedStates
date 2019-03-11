##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-02-04

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); })

##Load Packages
library(data.table);
library(magrittr);
library(lfe); library(car);
library(stargazer); library(starpolishr);

##Set to TRUE if you want latex headers in the tex output
headers.tex <- FALSE


all.data <- readRDS("../../Data5/_DataFinal_/10-bartik_panel_us_ca.rds")

data.us <- all.data[country == "US"]
data.ca <- all.data[country == "CA"]

## -- Second Stage -- ##

##The lhs vars
lhs.vars <- readRDS("../../Data5/_DataFinal_/10-barik_lhs_vars_df.rds")

lhs.line1 <- paste0(lhs.vars$lhs.line1, collapse = " & ") %>%
    paste0(" & ", ., " \\\\")
lhs.line2 <- paste0(lhs.vars$lhs.line2, collapse = " & ") %>%
    paste0(" & ", ., " \\\\")
lhs.line3 <- paste0(lhs.vars$lhs.line3, collapse = " & ") %>%
    paste0(" & ", ., " \\\\")

felm.rhs <- "~ 0 + start_end:country + CA_2 + CA_3 + CA_4 + CA_5 + US_3 + US_4 + US_5 + US_6 + US_7 + US_8 + US_9 | 0 | (diff_log_census_emp_us | diff_log_census_emp_ca ~ bartik_log_diff_asm_ca + bartik_log_diff_asm_us) | state_abb"

## ##test
## felm(diff_log_w ~ 0 + start_end:country +
##               CA_2 + CA_3 + CA_4 + CA_5 + US_3 + US_4 + US_5 + US_6 + US_7 + US_8 + US_9 | 0 |
##               (diff_log_census_emp_us | diff_log_census_emp_ca ~ bartik_log_diff_asm_ca +
##                  bartik_log_diff_asm_us) | state_abb,
##      all.data, weights = all.data$start_pop2459) %>% summary


bartik.2sls <- lapply(lhs.vars$lhs, function(lhs) {
  felm(as.formula(paste0(lhs, felm.rhs)), all.data, weights = all.data$start_pop2459)
}) %>% setNames(lhs.vars$lhs)

##US == CA pvalue
us.ca.pval <- lapply(bartik.2sls, function(mod) {
  linearHypothesis(mod, c("`diff_log_census_emp_us(fit)` = `diff_log_census_emp_ca(fit)`"),
                   test = "F") %>%
    .[2, 4] %>% sprintf("%.03f", .)
}) %>% paste0(collapse = " & ") %>%
  paste0("US = CA p-value & ", ., " \\\\")


star.out.2sls <- stargazer(bartik.2sls, type = "latex",
                           keep.stat = "n",
                           title = "Using only CBP Bartiks: 2SLS Estimates -- Local Labor Market Effects of Sectoral Shifts Predicted at the National Level (Bartik):  1990 to 2011",
                           label = "tab:bartik_cbp_2sls",
                           keep = c("fit")) %>%
  .[!grepl(" diff\\\\_", .)] %>%
  star_insert_row(c("\\\\[-2.0ex]", lhs.line1, lhs.line2, lhs.line3),
                  insert.after = 11) %>%
  ##Update the RHS variable names
  star_rhs_names(pattern = c("`diff\\\\_log\\\\_census\\\\_emp\\\\_us\\(fit\\)`",
                             "`diff\\\\_log\\\\_census\\\\_emp\\\\_ca\\(fit\\)`"),
                 line1 = c("$\\\\Delta$ Log Employment $\\\\times$",
                           "$\\\\Delta$ Log Employment $\\\\times$"),
                 line2 = c("United States", "Canada")) %>%
  star_insert_row(string = us.ca.pval, insert.after = 25) %>%
  sub("Dependent variable:", "Dependent variable: Difference in ", x = .) %>%
  star_notes_tex(note.type = "threeparttable", note = "See the notes for table \\ref{tab:gradients}. Sample consists of 264 metro areas in the United States and 82 in Canada observed  in  1990, 2000, 2007, and 2011. Not all dependent variables are available for all metro areas. The CBP/ASM Bartik instrument is calculated using County (US) Businesss Patterns or Canadian (CA) Business Patterns along with the ASM data, respectively. The Bartik instruments are calculated using the start of sample as the base year. Within each panel, controls include decadal and census division/region fixed effects. The p-value in the bottom row is associated with the null hypothesis of equality on the coefficients ($\\Delta$ Log Employment) across the United States and Canada.") %>%
  star_sidewaystable

star_tex_write(star.out.2sls, file = "TexFiles/24-a-cbp_bartik_2sls.tex",
               headers = headers.tex)


