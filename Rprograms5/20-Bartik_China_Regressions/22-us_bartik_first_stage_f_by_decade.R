##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-12-30

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe); library(stargazer)})

data.us <- readRDS("../../Data5/_DataFinal_/10-us_all_data_diff.rds") %>%
   .[start.end %chin% c("1980_1990", "1990_2000", "2000_2007", "2007_2011")]

mod <- felm(diff_log_census_emp ~ 0 + cbp_bartik_80_base * start.end | census.division |
              0 | state.abb, data = data.us, weights = data.us$start_pop2459, data.us)

mod07 <- felm(diff_log_census_emp ~ 0 + cbp_bartik | census.division |
                 0 | state.abb, data = data.us[start.end == "2007_2011"],
              weights = data.us[start.end == "2007_2011", start_pop2459], data.us)

mod00 <- felm(diff_log_census_emp ~ 0 + cbp_bartik | census.division |
                 0 | state.abb, data = data.us[start.end == "2000_2007"],
               weights = data.us[start.end == "2000_2007", start_pop2459], data.us)

mod90 <- felm(diff_log_census_emp ~ 0 + cbp_bartik | census.division |
                 0 | state.abb, data = data.us[start.end == "1990_2000"],
               weights = data.us[start.end == "1990_2000", start_pop2459], data.us)

mod80 <- felm(diff_log_census_emp ~ 0 + cbp_bartik_80_base | census.division |
                 0 | state.abb, data = data.us[start.end == "1980_1990"],
               weights = data.us[start.end == "1980_1990", start_pop2459], data.us)


stargazer(mod80, mod90, mod00, mod07, type = "text")






