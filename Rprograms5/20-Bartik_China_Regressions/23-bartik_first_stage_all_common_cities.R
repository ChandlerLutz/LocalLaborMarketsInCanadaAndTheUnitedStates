##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-19

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe); library(car);
  library(stargazer); library(starpolishr);})


##Set to TRUE if you want latex headers in the tex output
headers.tex <- FALSE


all.data <- readRDS("../../Data5/_DataFinal_/10-bartik_panel_us_ca.rds")

data.us <- all.data[country == "US"]
data.ca <- all.data[country == "CA"]

##the common city size ids
common.city.ids <- readRDS("RdsFiles/21-common_pop_support_city_ids.rds")

all.data.common <- all.data[city_id %chin% c(common.city.ids)]

f_stage1 <- function(DT) {

  pooled.flag <- (length(unique(DT[["country"]])) == 2)

  DT.common.city <- DT[city_id %chin% c(common.city.ids)]

  if (isTRUE(pooled.flag)) {
    ##pooled regressions
    f1 <- diff_log_census_emp ~ 0 + census_bartik + start_end:country +
                  CA_2 + CA_3 + CA_4 + CA_5 + US_3 + US_4 + US_5 + US_6 + US_7 + US_8 + US_9 |
                   0 | 0 | state_abb

    f2 <- diff_log_census_emp ~ 0 + bartik_log_diff_asm + start_end:country +
      CA_2 + CA_3 + CA_4 + CA_5 + US_3 + US_4 + US_5 + US_6 + US_7 + US_8 + US_9 |
      0 | 0 | state_abb
    f3 <- diff_log_census_emp ~ 0 + census_bartik + bartik_log_diff_asm + start_end:country +
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
  mod4 <- felm(f1, data = DT.common.city, weights = DT.common.city$start_pop2459)
  mod5 <- felm(f2, data = DT.common.city, weights = DT.common.city$start_pop2459)
  mod6 <- felm(f3, data = DT.common.city, weights = DT.common.city$start_pop2459)

  mod.list <- list(mod1, mod2, mod3, mod4, mod5, mod6)
  f_fstat <- function(mod) {
    bartik.coefs <- coefficients(mod) %>% names %>% .[grepl("bartik", x = .)]
    linearHypothesis(mod, hypothesis.matrix = bartik.coefs, test = "F", vcov = mod$clustervcv) %>%
      .[2, 3] %>% sprintf("%.03f", .)

  }
  f.stat.stage1 <- sapply(mod.list, f_fstat) %>%
    paste0(., collapse = " & ") %>%
    paste0("First Stage F-Statistic & ", ., " \\\\")
  reg.sample1 <- c("Full", "Full", "Full", "Com. Size", "Com. Size", "Com. Size") %>%
    paste0(., collapse = " & ") %>%
    paste0("\\cline{2-7} \\\\[-2.5ex] Regression Sample & ", ., " \\\\")
  reg.sample2 <- c("Sample", "Sample", "Sample", "City Size", "City Size", "City Size") %>%
    paste0(., collapse = " & ") %>%
    paste0(" & ", ., " \\\\")

  star.out <- stargazer(mod1, mod2, mod3, mod4, mod5, mod6,
                        title = "First Stage ``Bartik'' Estimates: Changes in Local Employment Predicted by National Sectoral Shifts: 1990 to 2011",
                        label = "tab:bartik_first",
                        font.size = "small",
                        dep.var.labels = "Log Census Employment",
                        keep = "bartik", keep.stat = c("rsq")) %>%
    star_insert_row(string = c(f.stat.stage1, reg.sample1), insert.after = 23)

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



##US == CA pval all cities
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

##US == CA pval common cities
mod4.pval <- felm(diff_log_census_emp ~ census_bartik:country + start_end:country |
               census_division | 0 | state_abb,
             data = all.data.common, weights = all.data.common$start_pop2459) %>%
  linearHypothesis("census_bartik:countryCA = census_bartik:countryUS", test = "F") %>%
  .[2, 4] %>% sprintf("%.03f", .)
mod5.pval <- felm(diff_log_census_emp ~ bartik_log_diff_asm:country + start_end:country |
               census_division | 0 | state_abb,
               data = all.data.common, weights = all.data.common$start_pop2459) %>%
  linearHypothesis("bartik_log_diff_asm:countryCA = bartik_log_diff_asm:countryUS", test = "F") %>%
  .[2, 4] %>% sprintf("%.03f", .)
mod6.pval <- felm(diff_log_census_emp ~ census_bartik:country + bartik_log_diff_asm:country +
               start_end:country |
               census_division | 0 | state_abb,
               data = all.data.common, weights = all.data.common$start_pop2459) %>%
  linearHypothesis(c("census_bartik:countryCA = census_bartik:countryUS",
                     "countryCA:bartik_log_diff_asm = countryUS:bartik_log_diff_asm"),
                     test = "F") %>%
  .[2, 4] %>% sprintf("%.03f", .)


us.ca.pval <- c(mod1.pval, mod2.pval, mod3.pval, mod4.pval, mod5.pval, mod6.pval) %>%
  paste0(collapse = " & ") %>%
  paste0("US Bartik = Canada Bartik pval & ", ., " \\\\")

sample.string <- "\\\\[-2ex] & \\multicolumn{3}{c}{Full} & \\multicolumn{3}{c}{Com. Size} \\\\ \\cline{2-4} \\cline{5-7}"


star.stage1.panel <- star_insert_row(
  star.stage1.panel,
  string = c("\\\\[-2ex] \\hline \\\\[-2ex] ", us.ca.pval),
  insert.after = 50) %>%
  ##update variable names
  sub("Dependent variable:", "Dependent variable: Difference in", x = .) %>%
  gsub("census\\\\_bartik", "Census Bartik", x = .) %>%
  gsub("bartik\\\\_log\\\\_diff\\\\_asm", "CBP/ASM Bartik", x = .) %>%
  star_notes_tex(note.type = "caption", note = "See the notes for table \\ref{tab:gradients}. The Census and CBP/ASM Bartik instruments are calculated using census data and County (US) Business Patterns data or Canadian (CA) Business Patterns along with ASM data, respectively. All regressions include decadal and region fixed effects; Panel C interacts decadal fixed effects by country. For the full sample (columns 1 - 3), the sample consists of 264 metro areas in the United States and 82 in Canada observed  in  1990, 2000, 2007, and 2011. Canadian metro areas have population greater than 15,000 in 1990; U.S. metro areas have population greater than 50,000 in 1999. In the common city size sample (columns 4 - 6), cities are chosen so that 1990 24-59 city population size has a common support. See the notes to table \\ref{tab:summ_stats} for data sources and definitions. Regressions are  weighted by start of period population 24-59. Heteroskedasticity robust standard errors clustered at the state/province level and are in parentheses. *, **, *** indicates statistical significance at the 10, 5, and 1 percent levels, respectively.")




star_tex_write(star.stage1.panel, file = "TexFiles/23-bartik_stage1_all_common_pop.tex",
               headers = headers.tex)
