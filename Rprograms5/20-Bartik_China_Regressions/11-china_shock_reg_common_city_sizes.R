##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-12-19

##Clear the workspace
rm(list = ls())

##Load Packages
library(data.table); ##library(dtplyr); library(dplyr);
library(magrittr); library(lubridate); library(lfe);
library(stargazer); library(starpolishr); library(AER);

##For the China shock regressions -- decadal difference


##Set to TRUE if you want latex headers in the tex output
headers.tex <- FALSE


all.data <- readRDS("../../Data5/_DataFinal_/10-ipw_panel_us_ca.rds") %>%
  setnames(c("d_ipw_asm", "d_ipw_other_asm"), c("d.ipw", "d.ipw.instrument")) %>%
  .[, d.ipw.ca := ifelse(country == "CA", d.ipw, 0)] %>%
  .[, d.ipw.instrument.ca := ifelse(country == "CA", d.ipw.instrument, 0)] %>%
  .[, d.ipw.us := ifelse(country == "US", d.ipw, 0)] %>%
  .[, d.ipw.instrument.us := ifelse(country == "US", d.ipw.instrument, 0)]

##update the city cizes for common city sizes
##get the max city size in Canada
## all.data <- all.data[start_end == min(start_end), start_sample_pop := start_pop2459] %>%
##   .[, start_sample_pop := mean(start_sample_pop, na.rm = TRUE), by = .(country, msa)]
## max.city.size <- all.data[country == "CA", max(start_sample_pop)]
## min.city.size <- all.data[country == "US", min(start_sample_pop)]
## all.data <- all.data[start_sample_pop >= min.city.size & start_sample_pop <= max.city.size]



##the common city size ids
common.city.ids <- readRDS("RdsFiles/21-common_pop_support_city_ids.rds")

all.data <- all.data[city_id %chin% c(common.city.ids)]

data.us <- all.data[country == "US"]
data.ca <- all.data[country == "CA"]

## -- first stage -- ##

data.ca.first <- copy(data.ca) %>%
    setnames("d.ipw.instrument", "d.ipw.instrument.ca")

first.us <- felm(d.ipw ~ d.ipw.instrument + start_census_manufac_share |
                   census_division + start_end | 0 | state_abb, data.us,
                 weights = data.us$start_pop2459)

##US first stage F-stat
first.us.f <- (first.us$ctval[["d.ipw.instrument"]] ^ 2) %>%
  sprintf("%.02f", x = .)

first.ca <- felm(d.ipw ~ d.ipw.instrument.ca + start_census_manufac_share |
                   census_division + start_end | 0 | state_abb, data.ca.first,
                 weights = data.ca.first$start_pop2459)
names(first.ca$coefficients)[1] <- c("d.ipw.instrument.ca")

##CA first stage F-stat
first.ca.f <- (first.ca$ctval[["d.ipw.instrument.ca"]] ^ 2) %>%
  sprintf("%.02f", x = .)

##The latex string for the first stage F-stat
f.stat.stage1.string <- paste(first.us.f, first.ca.f, sep = " & ") %>%
  paste0("First Stage F-statistic & ", ., " \\\\")

##The p-values with null of equality across countries
first.us.ca <- felm(d.ipw ~ d.ipw.instrument:country +
                        start_census_manufac_share:country +
                        census_division + start_end:country |
                        0 | 0 | state_abb, all.data,
                    weights = all.data$start_pop2459)
##Null: equality of d.IPW across countries
us.ca.pval.ipw <- linearHypothesis(
    first.us.ca,
    "d.ipw.instrument:countryCA = d.ipw.instrument:countryUS",
    test = "F")
us.ca.pval.ipw <- us.ca.pval.ipw[4][2, 1] %>%
    sprintf("%0.3f", .) %>%
    paste0("$\\Delta$IPW from China to Other: US = CA pval & \\multicolumn{2}{c}{", .,
           "} \\\\")
##Null: equality of start of period manufac share across countries
us.ca.pval.manufac <- linearHypothesis(
    first.us.ca,
    "countryCA:start_census_manufac_share = countryUS:start_census_manufac_share",
    test = "F")
us.ca.pval.manufac <- us.ca.pval.manufac[4][2, 1] %>%
    sprintf("%0.3f", .) %>%
    paste0("Start of Period Manufac Share: US = Canada pval & \\multicolumn{2}{c}{", .,
           "} \\\\")

##First stage stargazer
star.first <- stargazer(first.us, first.ca, type = "latex",
                        label = "tab:china_first_common",
                        title = "Common Metro Size First Stage Estimates -- Changes in Local Imports per Worker and Predicted Changes in Imports per Worker",
                        keep.stat = c("n", "f", "rsq")) %>%
    star_rhs_names(pattern = c("d.ipw.instrument ",
                               "d.ipw.instrument.ca",
                               "start\\\\_census\\\\_manufac\\\\_share"),
                   line1 = c("$\\\\Delta$ imports from China to Other Countries ",
                             "$\\\\Delta$ imports from China to Other Countries ",
                             "Start of period manufacturing share"),
                   line2 = c("per US worker", "per Canadian worker", "")
                   ) %>%
  gsub("\\\\multicolumn\\{2\\}\\{c\\}\\{d.ipw\\}", " US & Canada \\\\", .) %>%
  ##insert the first stage F-stat
  star_insert_row(string = f.stat.stage1.string, insert.after = 26)

saveRDS(star.first, "RdsFiles/11-china_shock_stargazer_stage1_common_city_sizes.rds")

star.string <- c("& \\multicolumn{2}{c}{$\\Delta$ imports from China } \\\\ ",
                 "& \\multicolumn{2}{c}{per worker} \\\\",
                 "\\\\[-1.8ex]  \\hline \\\\[-1.8ex]",
                 us.ca.pval.ipw, us.ca.pval.manufac)

saveRDS(star.string, "RdsFiles/11-china_shock_stage1_star_insert_string_common_city_sizes.rds")


star.first <- star.first %>%
    ##Update the LHS names
    star_insert_row(star.string,
                    insert.after = c(10, 10, 26, 26))





star.first <- star.first %>%
    star_notes_tex(note.type = "threeparttable",
                   note = "See the notes for table \\ref{tab:gradients}. Sample consists of 256 metro areas in the United States -- observed in 1990, 2000, and 2007 -- and 56 in  Canada -- observed  in  1991, 2000/1 and 2006/7. In both columns, controls include decadal fixed effects and census division (column (1), U.S.)  or region (column (2), Canada) fixed effects. Predicted imports per workers are constructed using imports from Australia, Denmark, Finland, Germany, Japan, New Zealand, Spain, and Switzerland (Other Countries). Imports from China to Other Countries for Canada are adjusted using the 1990 relative population between the U.S. and Canada (by multiplying by (27.79 / 249.6)). Cities are chosen so that 1990 24-59 metro population size has a common support. See the notes to table \\ref{tab:summ_stats} for data sources and definitions. Regressions are  weighted by start of period population 24-59. Heteroskedasticity robust standard errors clustered at the state/province level and are in parentheses. *, **, *** indicates statistical significance at the 10, 5, and 1 percent levels, respectively.")
star_tex_write(star.first,
               file = "TexFiles/11-china_shock_first_stage_common_city_sizes.tex",
               headers = headers.tex)


########################
## -- Second Stage -- ##
########################

##read in the LHS vars
lhs.vars <- readRDS("../../Data5/_DataFinal_/10-ipw_lhs_vars_df.rds")

lhs.line1 <- paste0(lhs.vars$lhs.line1, collapse = " & ") %>%
    paste0(" & ", ., " \\\\")
lhs.line2 <- paste0(lhs.vars$lhs.line2, collapse = " & ") %>%
    paste0(" & ", ., " \\\\")
lhs.line3 <- paste0(lhs.vars$lhs.line3, collapse = " & ") %>%
    paste0(" & ", ., " \\\\")

felm.rhs <- "start_census_manufac_share |
         census_division + start_end | (d.ipw ~ d.ipw.instrument) |
         state_abb"

##US second stage estimates
second.us <- lapply(lhs.vars$lhs, function(lhs) {
  felm(reformulate(felm.rhs, lhs), data = data.us,
       weights = data.us$start_pop2459)
}) %>%
  ##Send to stargaer
  stargazer(., type = "latex", keep.stat = "n",
            label = "tab:china_2sls_common",
            title = "Common City Size 2SLS Estimates -- the Impact of Import Competition on Local Labor Markets in the U.S. and Canada: 1990 to 2007",
            keep = c("`d.ipw\\(fit\\)`", "start_census_manufac_share")) %>%
  star_rhs_order(c("`d.ipw\\(fit\\)`", "start\\\\_census\\\\_manufac\\\\_share")) %>%
  ##Update the rhs names
  star_rhs_names(pattern = c("`d.ipw\\(fit\\)`", "start\\\\_census\\\\_manufac\\\\_share"),
                 line1 = c("$\\\\Delta$ imports from China", "Start of Period"),
                 line2 = c("to US per worker", "Manufac Share")) %>%
  ##Delete the line with the felm.rhs as the
  ##left hand side variable
  .[!grepl("diff\\\\_", .)] %>%
  star_insert_row(c("\\\\[-2.0ex]", lhs.line1, lhs.line2, lhs.line3),
                  insert.after = 11)

##Canada second stage estimates
second.ca <- lapply(lhs.vars$lhs, function(lhs) {
  felm(reformulate(felm.rhs, lhs), data = data.ca,
       weights = data.ca$start_pop2459)
}) %>%
  ##Send to stargaer
  stargazer(., type = "latex", keep.stat = "n",
            keep = c("`d.ipw\\(fit\\)`", "start_census_manufac_share")) %>%
  star_rhs_order(c("`d.ipw\\(fit\\)`", "start\\\\_census\\\\_manufac\\\\_share")) %>%
  ##Update the rhs names
  star_rhs_names(
    pattern = c("`d.ipw\\(fit\\)`", "start\\\\_census\\\\_manufac\\\\_share"),
    line1 = c("$\\\\Delta$ imports from China", "Start of Period"),
    line2 = c("to Canada per worker", "Manufac Share"))

second.ca.us <- star_panel(second.us, second.ca,
                           panel.names = c("United States", "Canada"),
                           same.summary.stats = FALSE,
                           panel.label.fontface = "italic")


felm.rhs.all <- "start_census_manufac_share:country + census_division + start_end:country |
          0 | (d.ipw.ca | d.ipw.us ~ d.ipw.instrument.us + d.ipw.instrument.ca ) |
         state_abb"

##Null 1: d.ipw CA = d.ipw US   Null 2: manufac share CA = manufac share US
us.ca.equal.pvals <- lapply(lhs.vars$lhs, function(lhs) {
    temp.mod <- felm(reformulate(felm.rhs.all, lhs),
                      data = all.data, weights = all.data$start_pop2459)
    c(linearHypothesis(temp.mod, "`d.ipw.ca(fit)` = `d.ipw.us(fit)`")[4][2,1],
      linearHypothesis(
          temp.mod, "start_census_manufac_share:countryCA = start_census_manufac_share:countryUS")[4][2,1])
})
##US d.ipw == CA d.ipw pval
us.ca.equal.pvals.ipw <- lapply(us.ca.equal.pvals, function(x) x[1]) %>%
    ##Format
    sprintf("%0.3f", .) %>%
    ##Convert to a string
    paste0(., collapse = " & ") %>%
    paste0("$\\Delta$ IPW US = Canada pval & ", ., " \\\\")
##us manfuc share == ca manufac share
us.ca.equal.pvals.manufac <- lapply(us.ca.equal.pvals, function(x) x[2]) %>%
    ##Format
    sprintf("%0.3f", .) %>%
    ##Convert to a string
    paste0(., collapse = " & ") %>%
    paste0("\\multicolumn{2}{@{} l}{Start of Period Manufac} \\\\ Share US = Canada pval & ", ., " \\\\")



##Add in the p-values
second.ca.us <- star_insert_row(second.ca.us,
                                c("\\hline \\\\[-2.0ex]",
                                  us.ca.equal.pvals.ipw## ,
                                  ## "\\\\[-2.0ex] \\hline \\\\[-2.0ex] ",
                                  ## us.ca.equal.pvals.manufac
                                  ),
                                insert.after = c(37, 37##, 37, 37
                                                 )) %>%
    ##Change the Dependent variables
    gsub("Dependent variable:", "Dependent variable: Decadal Change in ", .) %>%
    ##Add notes
    star_notes_tex(note.type = "threeparttable",
                   note = "See the notes for tables \\ref{tab:gradients} and \\ref{tab:china_first}. Sample consists of 255 metro areas in the United States and 54 in  Canada observed  in  1990, 2000, and 2007. Not all dependent variables are available for all metro areas. Controls include census division/region and decadal fixed effects. Metros are chosen so that 1990 24-59 metro population size has a common support. See the notes to table \\ref{tab:summ_stats} for data sources and definitions. Regressions are  weighted by start of period population 24-59. Heteroskedasticity robust standard errors clustered at the state/province level and are in parentheses. *, **, *** indicates statistical significance at the 10, 5, and 1 percent levels, respectively.") %>%
    star_sidewaystable

star_tex_write(second.ca.us, file = "TexFiles/11-china_shock_2sls_common_city_sizes.tex",
          headers = headers.tex)
