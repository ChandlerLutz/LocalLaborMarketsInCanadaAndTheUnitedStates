##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-19

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(stargazer); library(starpolishr)})

##Set to TRUE if you want latex headers in the tex output
headers.tex <- FALSE


## -- first stage all.data -- ##

star.first.all <- readRDS("RdsFiles/10-china_shock_stargazer_stage1.rds")
star.first.common <- readRDS("RdsFiles/11-china_shock_stargazer_stage1_common_city_sizes.rds")

star.string.all <- readRDS("RdsFiles/10-china_shock_stage1_star_insert_string.rds")
star.string.common <- readRDS("RdsFiles/11-china_shock_stage1_star_insert_string_common_city_sizes.rds")

star.out <- star_panel(star.first.all, star.first.common,
                       panel.names = c("Full Sample", "Common City Sizes"),
                       same.summary.stats = FALSE,
                       same.lhs.vars = TRUE,
                       panel.label.fontface = "italic") %>%
  star_insert_row(string = c(star.string.all, star.string.common[3:5]),
                  insert.after = c(10, 10, 28, 28, 28, 43, 43, 43)) %>%
  star_notes_tex(note.type = "caption", note = "See the notes for table \\ref{tab:gradients}. Panel A uses the full sample. In panel B, metros are chosen so that 1990 24-59 metro population size has a common support. Specifically, the minimum metro size within each country is equal to the minimum metro size in the U.S. and the maximum metro size within each country is equal to the maximum metro size in Canada using start of sample population.  Metros are observed in 1990, 2000, and 2007. In both columns, controls include decadal fixed effects and census division (column (1), U.S.)  or region (column (2), Canada) fixed effects. Predicted imports per workers are constructed using imports from Australia, Denmark, Finland, Germany, Japan, New Zealand, Spain, and Switzerland (Other Countries). Imports from China to Other Countries for Canada are adjusted using the 1990 relative manufacturing employment between the U.S. and Canada 0.1064. Regressions are  weighted by start of period population 24-59. Heteroskedasticity robust standard errors clustered at the state/province level and are in parentheses. *, **, *** indicates statistical significance at the 10, 5, and 1 percent levels, respectively.")

star_tex_write(star.out, file = "TexFiles/13-china_shock_stage1_all_common_pop.tex",
               headers = headers.tex)





