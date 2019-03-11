##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-12-16


##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


##Load Packages
library(data.table);
library(magrittr);
library(lfe); library(car);
library(stargazer); library(starpolishr);

##Set to TRUE if you want latex headers in the tex output
headers.tex <- FALSE

##the tex font size
bartik.tex.font.size <- readRDS('../../Data5/_DataFinal_/10-bartik_tex_font_size.rds')

all.data <- readRDS("../../Data5/_DataFinal_/10-bartik_panel_us_ca.rds")

data.us <- all.data[country == "US"]
data.ca <- all.data[country == "CA"]


f_stage1 <- function(DT) {

  pooled.flag <- (length(unique(DT[["country"]])) == 2)

  if (isTRUE(pooled.flag)) {
    ##pooled regressions

    ##Only census bartik
    f1 <- diff_log_census_emp ~ 0 + census_bartik_ca + census_bartik_us +
      start_end:country +
      CA_2 + CA_3 + CA_4 + CA_5 + US_3 +
      US_4 + US_5 + US_6 + US_7 + US_8 + US_9 |
      0 | 0 | state_abb
    ##Only CBP/ASM Bartik
    f2 <- diff_log_census_emp ~ 0 + bartik_log_diff_asm_ca + bartik_log_diff_asm_us +
      start_end:country +
      CA_2 + CA_3 + CA_4 + CA_5 + US_3 +
      US_4 + US_5 + US_6 + US_7 + US_8 + US_9 |
      0 | 0 | state_abb
    ##Both instruments
    f3 <- diff_log_census_emp ~ 0 + census_bartik_ca + census_bartik_us +
      bartik_log_diff_asm_ca + bartik_log_diff_asm_us +
      start_end:country +
      CA_2 + CA_3 + CA_4 + CA_5 + US_3 + US_4 + US_5 + US_6 + US_7 + US_8 + US_9 |
      0 | 0 | state_abb
  } else {
    ##not pooled regressions
    f1 <- diff_log_census_emp ~ 0 + census_bartik | start_end + census_division | 0 | state_abb
    f2 <- diff_log_census_emp ~ 0 + bartik_log_diff_asm | start_end + census_division |
      0 | state_abb
    f3 <- diff_log_census_emp ~ 0 + census_bartik + bartik_log_diff_asm  |
      start_end + census_division | 0 | state_abb
  }

  mod1 <- felm(f1, data = DT, weights = DT$start_pop2459)
  mod2 <- felm(f2, data = DT, weights = DT$start_pop2459)
  mod3 <- felm(f3, data = DT, weights = DT$start_pop2459)


  mod.list <- list(mod1, mod2, mod3)
  f_fstat <- function(mod) {
    bartik.coefs <- coefficients(mod) %>% names %>% .[grepl("bartik", x = .)]
    linearHypothesis(mod, hypothesis.matrix = bartik.coefs, test = "F", vcov = mod$clustervcv) %>%
      .[2, 3] %>% sprintf("%.03f", .)

  }
  f.stat.stage1 <- sapply(mod.list, f_fstat) %>%
    paste0(., collapse = " & ") %>%
    paste0("First Stage F-Statistic & ", ., " \\\\")

  star.out <- stargazer(mod1, mod2, mod3,
                        title = "Two Stage Least Squares Estimates of Changes in Local Labor Market Outcomes due to Changes in Employment (Two Bartik Instrument Estimates): 1990 to 2011",
                        label = "tab:bartik_first",
                        dep.var.labels = "Log Census Employment",
                        keep = "bartik", keep.stat = c("rsq")) %>%
    star_insert_row(string = f.stat.stage1, insert.after = 21)

  return(star.out)
}

star.stage1.us <- f_stage1(data.us)
star.stage1.ca <- f_stage1(data.ca)
star.stage1.all <- f_stage1(all.data)

star.stage1.panel <- star_panel(star.stage1.us, star.stage1.ca, star.stage1.all,
                                panel.names = c("United States", "Canada",
                                                "United States and Canada"),
                                same.summary.stats = FALSE,
                                panel.label.fontface = "italic")


##US == CA pval
mod1.pval <- felm(diff_log_census_emp ~ census_bartik:country + start_end:country |
               census_division | 0 | state_abb,
             data = all.data, weights = all.data$start_pop2459) %>%
  linearHypothesis("census_bartik:countryCA = census_bartik:countryUS", test = "F") %>%
  .[2, 4] %>% sprintf("%.03f", .)
mod2.pval <- felm(diff_log_census_emp ~ bartik_log_diff_asm:country + start_end:country |
               census_division | 0 | state_abb,
               data = all.data, weights = all.data$start_pop2459) %>%
  linearHypothesis("bartik_log_diff_asm:countryCA = bartik_log_diff_asm:countryUS", test = "F") %>%
  .[2, 4] %>% sprintf("%.03f", .)
mod3.pval <- felm(diff_log_census_emp ~ census_bartik:country + bartik_log_diff_asm:country +
               start_end:country |
               census_division | 0 | state_abb,
               data = all.data, weights = all.data$start_pop2459) %>%
  linearHypothesis(c("census_bartik:countryCA = census_bartik:countryUS",
                     "countryCA:bartik_log_diff_asm = countryUS:bartik_log_diff_asm"),
                     test = "F") %>%
  .[2, 4] %>% sprintf("%.03f", .)


us.ca.pval <- c(mod1.pval, mod2.pval, mod3.pval) %>%
  paste0(collapse = " & ") %>%
  paste0("US Bartik = CA Bartik pval & ", ., " \\\\")

star.stage1.panel <- star_insert_row(
  star.stage1.panel,
  string = c(us.ca.pval,
             "Census Div/Region FE? & Yes & Yes & Yes \\\\",
             "Period FE? & Yes & Yes & Yes \\\\"),
  insert.after = 46) %>%
  ##update variable names
  sub("Dependent variable:", "Dependent variable: Difference in ", x = .) %>%
  gsub("census\\\\_bartik", "Census Bartik", x = .) %>%
  gsub("bartik\\\\_log\\\\_diff\\\\_asm", "CBP/ASM Bartik", x = .) %>%
  star_notes_tex(note.type = "threeparttable", note = "See the notes for table \\ref{tab:gradients}. Sample consists of 264 metro areas in the United States and 82 in  Canada observed  in  1990, 2000, 2007, and 2011. The Census and CBP/ASM Bartik instruments are calculated using census data and County (US) Business Patterns data or Canadian (CA) Business Patterns along with ASM data, respectively. Both the CBP/ASM and census Bartik instruments are calculated using the start of period as the base year. In panel C, decadal fixed effects are interacted with a country indicator. See the notes to table \\ref{tab:summ_stats} for data sources and definitions. Regressions are  weighted by start of period population 24-59. Heteroskedasticity robust standard errors clustered at the state/province level and are in parentheses. *, **, *** indicates statistical significance at the 10, 5, and 1 percent levels, respectively.")


star_tex_write(star.stage1.panel, file = "TexFiles/20-bartik_stage1.tex",
               headers = headers.tex)


## -- Second Stage -- ##

##The lhs vars
lhs.vars <- readRDS("../../Data5/_DataFinal_/10-barik_lhs_vars_df.rds")

lhs.line1 <- paste0(lhs.vars$lhs.line1, collapse = " & ") %>%
    paste0(" & ", ., " \\\\")
lhs.line2 <- paste0(lhs.vars$lhs.line2, collapse = " & ") %>%
    paste0(" & ", ., " \\\\")
lhs.line3 <- paste0(lhs.vars$lhs.line3, collapse = " & ") %>%
    paste0(" & ", ., " \\\\")

felm.rhs <- readRDS("../../Data5/_DataFinal_/10-bartik_felm_rhs.rds")

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
                           title = "2SLS Estimates -- Local Labor Market Effects of Sectoral Shifts Predicted at the National Level (Bartik):  1990 to 2011",
                           label = "tab:bartik_2sls",
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
  star_notes_tex(note.type = "threeparttable", note = "See the notes for table \\ref{tab:gradients}. Sample consists of 264 metro areas in the United States and 82 in Canada observed  in  1990, 2000, 2007, and 2011. Not all dependent variables are available for all metro areas. The CBP/ASM Bartik instrument is calculated using County (US) Businesss Patterns or Canadian (CA) Business Patterns along with the ASM data, respectively. The Bartik instruments are calculated using the start of sample as the base year. Within each panel, controls include decadal and census division/region fixed effects. The p-value in the bottom row is associated with the null hypothesis of equality on the coefficients ($\\Delta$ Log Employment) across the United States and Canada. See the notes to table \\ref{tab:summ_stats} for data sources and definitions. Regressions are  weighted by start of period population 24-59. Heteroskedasticity robust standard errors clustered at the state/province level and are in parentheses. *, **, *** indicates statistical significance at the 10, 5, and 1 percent levels, respectively.") %>%
  star_sidewaystable %>%
  ##insert the font size
  star_insert_row(bartik.tex.font.size, insert.after = 6)

star_tex_write(star.out.2sls, file = "TexFiles/20-bartik_2sls.tex",
               headers = headers.tex)
