##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-29

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe); library(car);
  library(stargazer); library(starpolishr)
})

headers.tex <- FALSE

##the tex font size
bartik.tex.font.size <- readRDS('../../Data5/_DataFinal_/10-bartik_tex_font_size.rds')

all.data <- readRDS("../../Data5/_DataFinal_/20-bartik_panel_us_ca_1980s.rds") %>%
  .[, census.division := paste0(country, census.division)] %>%
  ##Census interactions:
  .[, census_bartik_ca := ifelse(country == "CA", census_bartik, 0)] %>%
  .[, census_bartik_us := ifelse(country == "US", census_bartik, 0)] %>%
  ## diff_log_census_emp x ca interaction for canada
  .[, diff_log_census_emp_ca := ifelse(country == "CA", diff_log_census_emp, 0)] %>%
  .[, diff_log_census_emp_us := ifelse(country == "US", diff_log_census_emp, 0)]



all.data.us <- all.data[country == "US"]
all.data.ca <- all.data[country == "CA"]

##The lhs vars
lhs.vars <- readRDS("../../Data5/_DataFinal_/10-barik_lhs_vars_df.rds") %>%
  setDT %>%
  .[lhs != "diff_log_unemp_insurance"]

lhs.line1 <- paste0(lhs.vars$lhs.line1, collapse = " & ") %>%
    paste0(" & ", ., " \\\\")
lhs.line2 <- paste0(lhs.vars$lhs.line2, collapse = " & ") %>%
    paste0(" & ", ., " \\\\")
lhs.line3 <- paste0(lhs.vars$lhs.line3, collapse = " & ") %>%
  paste0(" & ", ., " \\\\")


f_first_stage_f <- function(temp.country) {

  f <- sprintf("diff_log_census_emp_%1$s ~ census_bartik_%1$s | census.division | 0 |
                      state.abb", tolower(temp.country))



  temp.counry <- toupper(temp.country)
  all.data.country <- all.data[country == c(temp.country)]

  f.out.tex <- felm(as.formula(f), data = all.data.country,
              weights = all.data.country[["start_pop2459"]]) %>%
    broom::tidy(.) %>%
    setDT %>% .[, statistic ^ 2] %>%
    sprintf("%.02f", x = .) %>%
    rep(., times = nrow(lhs.vars)) %>%
    paste0(collapse = " & ") %>%
    paste0(temp.country, " First-Stage F & ", ., " \\\\")

  return(f.out.tex)

}


## -- First Stage -- ##
stage1.us.f <- f_first_stage_f("US")

stage1.ca.f <- f_first_stage_f("CA")

## -- 2SLS -- ##
felm.rhs <- "~ 0  | census.division |
              (diff_log_census_emp_us | diff_log_census_emp_ca ~ census_bartik_ca + census_bartik_us) | state.abb"

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
                           title = "2SLS Estimates -- Local Labor Market Effects of Sectoral Shifts Predicted at the National Level (Bartik):  1980 to 1990",
                           label = "tab:bartik_2sls_80s",
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
  star_insert_row(string = c(stage1.us.f, stage1.ca.f), insert.after = 25) %>%
  star_notes_tex(note.type = "threeparttable", note = "See the notes for table \\ref{tab:gradients}. Sample consists of 264 metro areas in the United States and 82 in Canada observed  in  1980. Controls include census division/region fixed effects. The p-value in the bottom row is associated with the null hypothesis of equality on the coefficients ($\\Delta$ Log Employment) across the United States and Canada. See the notes to table \\ref{tab:summ_stats} for data sources and definitions. Regressions are  weighted by start of period population 24-59. Heteroskedasticity robust standard errors clustered at the state/province level and are in parentheses. *, **, *** indicates statistical significance at the 10, 5, and 1 percent levels, respectively.") %>%
  star_sidewaystable %>%
  star_insert_row(bartik.tex.font.size, insert.after = 6)

star_tex_write(star.out.2sls, file = "TexFiles/25-bartik_2sls_1980s.tex",
               headers = headers.tex)
