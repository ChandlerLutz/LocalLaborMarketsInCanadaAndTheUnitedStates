##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-29

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe);
  library(stargazer); library(starpolishr)})

##get the common city sizes
headers.tex = FALSE

##the stargazer dfl panel labels
dfl.labels <- readRDS("RdsFiles/10-dfl_panel_labels.rds")

##the tex font size
bartik.tex.font.size <- readRDS('../../Data5/_DataFinal_/10-bartik_tex_font_size.rds')



##get the common city sizes
common.support.city.ids <-
  readRDS("../20-Bartik_China_Regressions/RdsFiles/21-common_pop_support_city_ids.rds")

all.data <- readRDS("../../Data5/_DataFinal_/20-bartik_panel_us_ca_1980s.rds") %>%
  .[, census.division := paste0(country, census.division)] %>%
  ##Census interactions:
  .[, census_bartik_ca := ifelse(country == "CA", census_bartik, 0)] %>%
  .[, census_bartik_us := ifelse(country == "US", census_bartik, 0)] %>%
  ## diff_log_census_emp x ca interaction for canada
  .[, diff_log_census_emp_ca := ifelse(country == "CA", diff_log_census_emp, 0)] %>%
  .[, diff_log_census_emp_us := ifelse(country == "US", diff_log_census_emp, 0)]

##Import the odds ratio and merge and create the weight
DT.log.odds <- readRDS("RdsFiles/10-dfl_step1_odds_ratio.rds")
numeric.cols <- names(DT.log.odds) %>% .[!grepl("city_id", .)]


all.data <- merge(all.data, DT.log.odds, by = "city_id", all.x = TRUE)

data.us <- all.data[country == "US"]
data.ca <- all.data[country == "CA"]

##THe common city size support data
all.data.common.support <- all.data %>%
  .[city_id %chin% c(common.support.city.ids)]


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


## -- 2SLS -- ##
felm.rhs <- "~ 0  | census.division |
              (diff_log_census_emp | diff_log_census_emp_ca ~ census_bartik_ca + census_bartik_us) | state.abb"


f_stargazer <- function(mods) {

  out <- capture.output(
    stargazer(mods, type = "latex",
              title = "2SLS Bartik Estimates -- Difference between US and Canada in the 1980s",
              label = "tab:bartik_2sls_diff_all_dfl_80s",
              keep = "ca\\(fit\\)", keep.stat = "n")) %>%
    .[!grepl(" diff\\\\_", .)] %>%
    star_insert_row(c("\\\\[-2.0ex]", lhs.line1, lhs.line2, lhs.line3),
                    insert.after = 12) %>%
    star_rhs_names(pattern = c("`diff\\\\_log\\\\_census\\\\_emp\\\\_ca\\(fit\\)`"),
                   line1 = c("$\\\\Delta$ Log Employment"),
                   line2 = c("CA - US")
                   )


}


##original regressions
bartik.2sls.orig <- lapply(lhs.vars$lhs, function(lhs) {
  felm(as.formula(paste0(lhs, felm.rhs)), all.data, weights = all.data$start_pop2459)
}) %>% setNames(lhs.vars$lhs) %>% f_stargazer


##common support
bartik.2sls.common.support <- lapply(lhs.vars$lhs, function(lhs) {
  felm(as.formula(paste0(lhs, felm.rhs)), all.data.common.support,
       weights = all.data.common.support$start_pop2459)
}) %>% setNames(lhs.vars$lhs) %>% f_stargazer

##DFL weights 1
bartik.2sls.dfl1 <- lapply(lhs.vars$lhs, function(lhs) {
  felm(as.formula(paste0(lhs, felm.rhs)), all.data.common.support,
       weights = all.data.common.support[, start_pop2459 * odds.ratio1])
}) %>% setNames(lhs.vars$lhs) %>% f_stargazer


##DFL weights 2
bartik.2sls.dfl2 <- lapply(lhs.vars$lhs, function(lhs) {
  felm(as.formula(paste0(lhs, felm.rhs)), all.data.common.support,
       weights = all.data.common.support[, start_pop2459 * odds.ratio2])
}) %>% setNames(lhs.vars$lhs) %>% f_stargazer


star.panel <- star_panel(
  bartik.2sls.orig, bartik.2sls.common.support,
  bartik.2sls.dfl1, bartik.2sls.dfl2,
  panel.names = c(
    "Original Estimates",
    dfl.labels[1],
    dfl.labels[2],
    dfl.labels[3]
  ),
  same.summary.stats = FALSE, panel.label.fontface = "italic") %>%
  sub("Dependent variable:", "Dependent variable: Difference in ", x = .) %>%
star_notes_tex(note.type = "threeparttable",
               note = "See the notes for table \\ref{tab:gradients}. The difference between Canada and the U.S. 2SLS Bartik estimates. Panel A corresponds full sample data in the 1980s. Panel B uses common city size support. See table \\ref{tab:dfl_step1} for the reweigthing model specifications. Regressions are  weighted by start of period population 24-59. Heteroskedasticity robust standard errors clustered at the state/province level and are in parentheses. *, **, *** indicates statistical significance at the 10, 5, and 1 percent levels, respectively.") %>%
  star_sidewaystable %>%
  ##insert the font size
  star_insert_row(bartik.tex.font.size, insert.after = 6)




star_tex_write(star.panel, file = "TexFiles/25-bartik_2sls_all_diff.tex",
               headers = headers.tex)
