##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-13

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe); library(stargazer); library(starpolishr)})

##Set to TRUE if you want latex headers in the tex output
headers.tex <- FALSE

##the stargazer dfl panel labels
dfl.labels <- readRDS("RdsFiles/10-dfl_panel_labels.rds")


##get the common city sizes
common.support.city.ids <-
  readRDS("../20-Bartik_China_Regressions/RdsFiles/21-common_pop_support_city_ids.rds")


all.data <- readRDS("../../Data5/_DataFinal_/10-ipw_panel_us_ca.rds")



##Import the odds ratio and merge and create the weight
DT.log.odds <- readRDS("RdsFiles/10-dfl_step1_odds_ratio.rds")
numeric.cols <- names(DT.log.odds) %>% .[!grepl("city_id", .)]


all.data <- merge(all.data, DT.log.odds, by = "city_id", all.x = TRUE) %>%
  setnames(c("d_ipw_asm", "d_ipw_other_asm"), c("d.ipw", "d.ipw.instrument")) %>%
  .[, d.ipw.ca := ifelse(country == "CA", d.ipw, 0)] %>%
  .[, d.ipw.instrument.ca := ifelse(country == "CA", d.ipw.instrument, 0)] %>%
  .[, d.ipw.us := ifelse(country == "US", d.ipw, 0)] %>%
  .[, d.ipw.instrument.us := ifelse(country == "US", d.ipw.instrument, 0)]


data.us <- all.data[country == "US"]
data.ca <- all.data[country == "CA"]


all.data.common.support <- all.data %>%
  .[city_id %chin% c(common.support.city.ids)]

## -- Second Stage -- ##

##Read in the LHS VARS
lhs.vars <- readRDS("../../Data5/_DataFinal_/10-ipw_lhs_vars_df.rds")

lhs.line1 <- paste0(lhs.vars$lhs.line1, collapse = " & ") %>%
    paste0(" & ", ., " \\\\")
lhs.line2 <- paste0(lhs.vars$lhs.line2, collapse = " & ") %>%
    paste0(" & ", ., " \\\\")
lhs.line3 <- paste0(lhs.vars$lhs.line3, collapse = " & ") %>%
    paste0(" & ", ., " \\\\")

felm.rhs <- "~ country + start_census_manufac_share:country + start_end:country |
         census_division  |
         (d.ipw | d.ipw.ca ~ d.ipw.instrument.us + d.ipw.instrument.ca) |
         state_abb"



f_stargazer <- function(mods) {

  ## out <- stargazer(mods, type = "text",
  ##                  title = "2SLS IPW Estimates -- Difference between US and Canada",
  ##                  label = "tab:ipw_2sls_diff_all_dfl",
  ##                  ##keep = "ca\\(fit\\)",
  ##                  keep.stat = "n",
  ##                  font.size = "small")

  out <- capture.output(
    stargazer(mods, type = "latex",
              title = "Differences between the U.S. and Canada in Local Labor Market Outcomes due to Import Competition from China: 1990 to 2007",
              label = "tab:ipw_2sls_diff_all_dfl",
              keep = "ca\\(fit\\)", keep.stat = "n",
              font.size = "small")) %>%
    .[!grepl(" diff\\\\_", .)] %>%
    star_insert_row(c("\\\\[-2.0ex]", lhs.line1, lhs.line2, lhs.line3),
                    insert.after = 12) %>%
    star_rhs_names(pattern = c("`d.ipw.ca\\(fit\\)`"),
                   line1 = c("$\\\\Delta$ Imports from China "),
                   line2 = c("per worker (CA - US)")
                   )


}

##original regressions
ipw.2sls.orig <- lapply(lhs.vars$lhs, function(lhs) {
  felm(as.formula(paste0(lhs, felm.rhs)), all.data, weights = all.data$start_pop2459)
}) %>% setNames(lhs.vars$lhs) %>% f_stargazer

##common support
ipw.2sls.common.support <- lapply(lhs.vars$lhs, function(lhs) {
  felm(as.formula(paste0(lhs, felm.rhs)), all.data.common.support,
       weights = all.data.common.support$start_pop2459)
}) %>% setNames(lhs.vars$lhs) %>% f_stargazer

##DFL weights 1
ipw.2sls.dfl1 <- lapply(lhs.vars$lhs, function(lhs) {
  felm(as.formula(paste0(lhs, felm.rhs)), all.data.common.support,
       weights = all.data.common.support[, start_pop2459 * odds.ratio1])
}) %>% setNames(lhs.vars$lhs) %>% f_stargazer


##DFL weights 2
ipw.2sls.dfl2 <- lapply(lhs.vars$lhs, function(lhs) {
  felm(as.formula(paste0(lhs, felm.rhs)), all.data.common.support,
       weights = all.data.common.support[, start_pop2459 * odds.ratio2])
}) %>% setNames(lhs.vars$lhs) %>% f_stargazer


star.panel <- star_panel(
  ipw.2sls.orig, ipw.2sls.common.support,
  ipw.2sls.dfl1, ipw.2sls.dfl2,
  panel.names = c(
    "Original Estimates",
    dfl.labels[1],
    dfl.labels[2],
    dfl.labels[3]),
  same.summary.stats = FALSE, panel.label.fontface = "italic") %>%
  sub("Dependent variable:", "Dependent variable: Difference in ", x = .) %>%
star_notes_tex(note.type = "caption",
               note = "See the notes for table \\ref{tab:gradients}. The difference between Canada and the U.S. 2SLS Imports per worker estimates estimates. Panel A corresponds to our original, full sample data from table \\ref{tab:china_2sls}. Panel B uses common city size support as in table \\ref{tab:china_2sls_common}. See table \\ref{tab:dfl_step1} for the reweigthing model specifications. Regressions are  weighted by start of period population 24-59. Heteroskedasticity robust standard errors clustered at the state/province level and are in parentheses. *, **, *** indicates statistical significance at the 10, 5, and 1 percent levels, respectively.") %>%
  star_sidewaystable

star_tex_write(star.panel, file = "TexFiles/30-ipw_2sls_all_diff.tex",
               headers = headers.tex)
