##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-22

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())



suppressMessages({library(CLmisc); library(lfe); library(broom);
  library(crossplotr); library(ggrepel);
  library(Hmisc); library(stargazer); library(starpolishr)})

##For latex headers
headers.tex <- FALSE

##save plots
save.plots <- TRUE


##the city ids used in the regressions
reg.city.ids <- readRDS("../../Data5/_DataFinal_/10-reg_city_ids.rds")
##the common city size ids
common.city.ids <- readRDS("RdsFiles/21-common_pop_support_city_ids.rds")

##fwrite(data.table(reg_city_id = reg.city.ids), file = "../city_ids.csv")

f_diff <- function(x) x[2] - x[1]

##For the weighted standard deviation
wtd.sd <- function(x, w) {
    out <- try(wtd.var(x, weights = w, na.rm = TRUE), silent = TRUE)
    if (inherits(out, "try-error")) {
        return(NA_real_)
    } else {
        return(sqrt(out))
    }
}

##the variables
vars <- list(
  ##Wages
  c("start_log_wkwage_2010usd", "start\\\\_log\\\\_wkwage\\\\_2010usd",
    "Log Weekly Wage (USD 2010)", "wages", 0, 0, 0, 0, 0, 0, 0, 0),
  c("start_w", "start\\\\_w ", "Wage Location Index",
    "wages", 0, 0, 0, 0, 0, 0, 0, 0),
  c("start_w_xb","start\\\\_w\\\\_xb", "Wage Skill Index",
    "wages", 0, 0, 0, 0, 0, 0, 0, 0),
  c("start_log_cd_hs_wkwage","start\\\\_log\\\\_cd\\\\_hs\\\\_wkwage",
    "Log Univ/HS Wage", "wages", 0, 0, 0, 0, 0, 0, 0, 0),
  ##Employment and population
  c("start_log_pop2459", "start\\\\_log\\\\_pop2459", "Log Pop",
    "emp", 6.85, 0.73, 6.85, 0.73, 6.8, -0.51, 6.85, 0),
  c("start_log_census_emp_pop", "start\\\\_log\\\\_census\\\\_emp\\\\_pop",
    "Log (Emp/Pop)", "emp", 0, 0, 0, 0, 0, 0, 0, 0),
  c("start_log_census_college2459_pop2459", "start\\\\_log\\\\_census\\\\_college2459\\\\_pop2459",
    "Log (Univ/Pop)", "emp", 0, 0, 0, 0, 0, 0, 0, 0),
  c("start_log_fb_pop2459", "start\\\\_log\\\\_fb\\\\_pop2459", "Log (Foreign/Pop)",
    "emp", 0, 0, 0, 0, 0, 0, 0, 0),
  c("start_log_census_manufac_share", "start\\\\_log\\\\_census\\\\_manufac\\\\_share",
    "Log Manufac Share", "emp", 0, 0, 0, 0, 0, 0, 0, 0),
  c("start_p", "start\\\\_p", "Local Housing Costs",
    "emp", 0, 0, 0, 0, 0, 0, 0, 0)## ,
  ## c("Union_Coverage", "Union\\\\_Coverage", "Union Coverage",
  ##   "emp", 0, 0, 0, 0, 0, 0, 0, 0)
)

vars <- do.call("rbind", vars) %>%
    as.data.frame(., stringsAsFactors = FALSE)
vars[] <- lapply(vars, function(x) unlist(x))
vars <- setNames(vars,
                 c("var", "pattern", "name", "category",
                   ##the text positions
                   "x.full.us", "y.full.us", "x.common.us", "y.common.us",
                   "x.full.ca", "y.full.ca", "x.common.ca", "y.common.ca"))
vars <- vars %>% setDT

##based on the category
vars.wages <- vars[category == "wages"]
vars.emp <- vars[category == "emp"]
##the categories as they'll be in the star panel
panel.categories <- c("wage", "emp")
panel.names <- c("Wages", "Population and Employment")


##helper function to clean the data
f_clean <- function(DT.cross) {

  DT.cross <- DT.cross %>%
    .[city_id %chin% c(reg.city.ids)] %>%
    .[start.year == 1991, start.year := 1990] %>%
    .[!is.na(pop1990)] %>%
    .[, start_emp := start_census_emp_pop * start_pop2459] %>%
    .[, start_log_emp := log(start_emp)] %>%
    .[, `:=`(start_census_unemp_rate = start_census_unemp_rate * 100,
             start_census_emp_pop = start_census_emp_pop * 100)] %>%
    ##log some variables
    .[, `:=`(start_log_wkwage_2010usd = log(start_wkwage_2010usd),
             start_log_census_emp_pop = log(start_census_emp_pop),
             start_log_census_manufac_share = log(start_census_manufac_share))] %>%
    .[start.year %in% c(1990, 2011)] %>%
    .[, mget(c("city_id", "shortname", "state.abb", "start.year", "pop1990", vars$var))]


  return(DT.cross)

}


DT.us.union <- readRDS("../../Data5/_DataFinal_/11-us_union_coverage.rds")
DT.us.cross <- readRDS("../../Data5/_DataFinal_/10-us_data_cross_section.rds") %>%
  merge(DT.us.union, by = c("city_id", "start.year"), all.x = TRUE) %>%
  f_clean

DT.us.cross.common <- DT.us.cross[city_id %chin% c(common.city.ids)]

DT.ca.union <- readRDS("../../Data5/_DataFinal_/11-ca_union_coverage.rds")
DT.ca.cross <- readRDS("../../Data5/_DataFinal_/10-ca_data_cross_section.rds") %>%
  merge(DT.ca.union, by = c("city_id", "start.year"),
        all.x = TRUE) %>%
  f_clean

DT.ca.cross.common <- DT.ca.cross[city_id %chin% c(common.city.ids)]

DT.all <- rbind(DT.us.cross, DT.ca.cross) %>%
  .[, country := stringr::str_extract(city_id, "^[US|CA]{2}")] %>%
  setcolorder(c("country"))


DT.all.common <- DT.all[city_id %chin% c(common.city.ids)]

f_sd <- function(DT.cross) {

  CA <- grepl("^CA", DT.cross$city_id[1])

  DT.sd <- DT.cross[, lapply(.SD, wtd.sd, pop1990), .SDcols = vars$var,
                    by = start.year] %>%
    melt(id.vars = "start.year") %>%
    .[, start.year := paste0("start.year", start.year)] %>%
    dcast(variable ~ start.year, value.var = "value") %>%
    .[, start.year2011 := (start.year2011 / start.year1990)] %>%
    .[, `:=`(start.year1990 = sprintf("%.02f", start.year1990),
             start.year2011 = sprintf("%.02f", start.year2011))]

  new.names <- ifelse(CA == TRUE,
                      list(c("CAsd1990", "CAsd2011")),
                      list(c("USsd1990", "USsd2011"))) %>%
    unlist
  ##update the names
  setnames(DT.sd, c("start.year1990", "start.year2011"), new.names)

  return(DT.sd)

}

f_slopes <- function(DT.cross) {

  DT.w.initial <- DT.cross[start.year == 1990,
                           .(city_id, wage.initial = start_log_wkwage_2010usd)]

  DT.slopes <- copy(DT.cross) %>%
    melt(id.vars = c("city_id", "shortname", "state.abb", "start.year", "pop1990")) %>%
    .[, start.year := paste0("start.year", start.year)] %>%
    dcast(city_id + shortname + state.abb + pop1990 + variable ~ start.year,
          value.var = "value") %>%
    .[, change := start.year2011 - start.year1990] %>%
    merge(DT.w.initial, by = "city_id")

  country <- ifelse(grepl("^US", DT.cross$city_id[[1]]), "US", "Canada")
  DT.slopes <- DT.slopes[, country := c(country)]

  ##Save the graph for each variabls
  if (save.plots == TRUE) {


    ##for the common city size regs
    unique.ids <- DT.cross[, uniqueN(city_id)]
    if ((country == "Canada" && unique.ids < 70) | (country == "US" && unique.ids < 260)) {
      data.sample <- "common_support"
    } else {
      data.sample <- "full_sample"
    }


    for (i in seq_along(vars$var)) {
      temp.var <- vars$var[i]
      temp.ylabel <- paste0("Change in ", vars$name[i], ", 1990-2011")

      ##the text position
      if (country == "US" & data.sample == "full_sample") {
        temp.text.pos.x <- vars$x.full.us
        temp.text.pos.y <- vars$y.full.us
      } else if (country == "US" & data.sample == "common_support") {
        temp.text.pos.x <- vars$x.common.us
        temp.text.pos.y <- vars$y.common.us
      } else if (country == "Canada" & data.sample == "full_sample") {
        temp.text.pos.x <- vars$x.full.ca
        temp.text.pos.y <- vars$y.full.ca
      } else if (country == "Canada" & data.sample == "common_support") {
        temp.text.pos.x <- vars$x.common.ca
        temp.text.pos.y <- vars$y.common.ca
      }

      temp.text.pos.x <- temp.text.pos.x[i] %>% as.numeric
      temp.text.pos.y <- temp.text.pos.y[i] %>% as.numeric

      DT.temp <- DT.slopes[variable == c(temp.var)]

      p.wage.initial <- crossplot(DT.temp,
                                  x.var = "wage.initial", y.var = "change",
                                  size.var = "pop1990",
                                  shapes.var = "country", label.var = "shortname",
                                  xlabel = "Initial Log Weekly Wage",
                                  ylabel = temp.ylabel,
                                  shapes = c(21),
                                  colors = c("blue"),
                                  points.alpha = 0.75) %>%
        crossplot_print_stats(text.pos = c(temp.text.pos.x, temp.text.pos.y),
                              weighted = TRUE)

      DT.wage.initial.outliers <- crossplot_outliers(p.wage.initial,
                                                     vars = c("wage.initial", "change"),
                                                     num.outliers = 10) %>% setDT
      DT.wage.initial.big <- DT.temp %>%
        .[shortname %chin% c("Los Angeles", "New York", "Detroit", "Chicago",
                             "Washington", "Houston", "Denver", "Seattle",
                             "Orlando", "Pittsburgh",
                             "Toronto", "Montreal", "Vancouver", "Windsor",
                             "Ottawa", "Hamilton", "Salaberry", "Kitchener",
                             "Calgary")]
      DT.wage.inital.labels <- rbind(DT.wage.initial.outliers, DT.wage.initial.big) %>%
        .[!duplicated(shortname)]

      p.wage.initial <- p.wage.initial +
        geom_text_repel(data = DT.wage.inital.labels,
                        fontface = "bold", size = 3.5, force = 1,
                        max.iter = 2e4)

      ##Save
      temp.file.name <- sprintf("05-plot_wage_initial/05-%s_%s_%s.pdf",
                                data.sample, country, temp.var)

      ggsave(filename = temp.file.name, plot = p.wage.initial)

      temp.rds.name <- sprintf("05-plot_wage_initial-rds/05-%s_%s_%s.rds",
                               data.sample, country, temp.var)

      saveRDS(p.wage.initial, temp.rds.name)







      ##clean up
      rm(temp.var, temp.ylabel, temp.file.name, temp.rds.name)

    } ## end of for loop for plots

  }##end of save plots IF



  ##nest by variable
  DT.slopes.regs <- DT.slopes[, list(DT.temp = list(.SD)), by = variable] %>%
    .[, mod.own := lapply(DT.temp, function(DT) felm(change ~ start.year1990 | 0 | 0 | state.abb,
                                                     DT, weights = DT$pop1990
                                                     ))] %>%
    .[, mod.wage := lapply(DT.temp, function(DT) felm(change ~ wage.initial | 0 | 0 | state.abb,
                                                      DT, weights = DT$pop1990))] %>%
    .[, coef.own := sapply(mod.own, function(x) sprintf("%.02f", x$coefficients[2]))] %>%
    .[, coef.wage := sapply(mod.wage, function(x) sprintf("%.02f", x$coefficients[2]))] %>%
    .[, pval.own := sapply(mod.own, function(x) x$cpval[2])] %>%
    .[, pval.wage := sapply(mod.wage, function(x) x$cpval[2])] %>%
    .[, coef.own.stars := ifelse(pval.own < 0.01, "***",
                          ifelse(pval.own < 0.05, "**",
                          ifelse(pval.own < 0.1, "*", "")))] %>%
    .[, coef.wage.stars := ifelse(pval.wage < 0.01, "***",
                           ifelse(pval.wage < 0.05, "**",
                           ifelse(pval.wage < 0.1, "*", "")))] %>%
    .[, coef.own := paste0(coef.own, coef.own.stars)] %>%
    .[, coef.wage := paste0(coef.wage, coef.wage.stars)] %>%
    select_by_ref(c("variable", "coef.own", "coef.wage"))

  CA <- grepl("^CA", DT.cross$city_id[1])
  new.names <- ifelse(CA == TRUE, list(c("CAownInitial", "CAwageInitial")),
                      list(c("USownInitial", "USwageInitial"))) %>%
    unlist
  setnames(DT.slopes.regs, c("coef.own", "coef.wage"), new.names)

  return(DT.slopes.regs)

}

f_diff_slopes <- function(DT.all) {

  DT.w.initial <- DT.all[start.year == 1990,
                         .(city_id, wage.initial = start_log_wkwage_2010usd)]

  DT.slopes.diff <- copy(DT.all) %>%
    melt(id.vars = c("country", "state.abb", "shortname", "city_id", "start.year", "pop1990")) %>%
    .[, start.year := paste0("start.year", start.year)] %>%
    dcast(country + state.abb + city_id + shortname + pop1990 + variable ~ start.year, value.var = "value") %>%
    .[, change := start.year2011 - start.year1990] %>%
    ##change country to a factor so it's CA relative to US
    .[, country := factor(country, levels = c("US", "CA"))] %>%
    ##merge in the initial wage
    merge(DT.w.initial, by = "city_id") %>%
    ##nest
    .[, list(DT.temp = list(.SD)), by = variable] %>%
    .[, mod.own := lapply(DT.temp, function(DT) felm(change ~ start.year1990 * country | 0 | 0 | state.abb,
                                                     DT, weights = DT$pop1990))] %>%
    .[, mod.wage := lapply(DT.temp, function(DT) felm(change ~ wage.initial * country | 0 | 0 | state.abb,
                                                      DT, weights = DT$pop1990))] %>%
    .[, coef.own := sapply(mod.own, function(x) sprintf("%.02f", x$coefficients[4]))] %>%
    .[, coef.wage := sapply(mod.wage, function(x) sprintf("%.02f", x$coefficients[4]))] %>%
    .[, pval.own := sapply(mod.own, function(x) x$cpval[4])] %>%
    .[, pval.wage := sapply(mod.wage, function(x) x$cpval[4])] %>%
    .[, coef.own.stars := ifelse(pval.own < 0.01, "***",
                          ifelse(pval.own < 0.05, "**",
                          ifelse(pval.own < 0.1, "*", "")))] %>%
    .[, coef.wage.stars := ifelse(pval.wage < 0.01, "***",
                           ifelse(pval.wage < 0.05, "**",
                           ifelse(pval.wage < 0.1, "*", "")))] %>%
    .[, coef.own := paste0(coef.own, coef.own.stars)] %>%
    .[, coef.wage := paste0(coef.wage, coef.wage.stars)] %>%
    select_by_ref(c("variable", "coef.own", "coef.wage"))

  return(DT.slopes.diff)


}

## -- Full sample -- ##
DT.us.sd <- f_sd(DT.us.cross)
DT.ca.sd <- f_sd(DT.ca.cross)
DT.us.slopes <- f_slopes(DT.us.cross)
DT.ca.slopes <- f_slopes(DT.ca.cross)
DT.diff.slopes <- f_diff_slopes(DT.all)


DT.results.all <- merge(DT.us.sd, DT.us.slopes, by = "variable") %>%
  merge(DT.ca.sd, by = "variable") %>%
  merge(DT.ca.slopes, by = "variable") %>%
  merge(DT.diff.slopes, by = "variable")


f_star <- function(x.mat) {

  star.out <- capture.output(stargazer(
    as.matrix(x.mat),
    label = "tab:sigma_beta",
    title = "Measures of Local Labor Market Dispersion, Divergence, and Mean Reversion in the U.S. and Canada: 1990 to 2011",
    font.size = "footnotesize")) %>%
    sub("ccccccccccc", "lcccccccccc", x = .) %>%
    ##udpate LHS names
    star_lhs_names(pattern = c("USsd1990", "USsd2011",
                               "USownInitial", "USwageInitial",
                               "CAsd1990", "CAsd2011",
                               "CAownInitial", "CAwageInitial",
                               "coef.own", "coef.wage"),
                   line1 = c("Std", "Sd(2011),", "Change", "Change",
                             "Std", "Sd(2011),", "Change", "Change",
                             "Change", "Change"),
                   line2 = c("Dev", "Sd(1990)", "On 1990", "On 1990",
                             "Dev", "Sd(1990)", "On 1990", "On 1990",
                             "On 1990", "On 1990"),
                   line3 = c("1990", "Ratio", "Own", "Wage",
                             "1990", "Ratio", "Own", "Wage",
                             "Own", "Wage"),
                   multicol = "c") %>%
    gsub("variable", "", x = .) %>%
    star_rhs_names(pattern = vars$pattern, line1 = vars$name)

  return(star.out)

}

DT.results.temp <- copy(DT.results.all) %>% .[, variable := as.character(variable)]
DT.results.wage <- DT.results.temp %>%
  .[variable %chin% c(vars.wages[["var"]])]
DT.results.emp <- DT.results.temp %>%
  .[variable %chin% c(vars.emp[["var"]])]

##check
stopifnot(
  nrow(rbind(DT.results.wage, DT.results.emp)) ==
    nrow(DT.results.temp))

star.results.list <- list(DT.results.wage, DT.results.emp) %>%
  lapply(as.matrix) %>%
  lapply(f_star) %>%
  setNames(panel.categories)

##insert the table labels
star.string <- " & \\multicolumn{4}{c}{United States} & \\multicolumn{4}{c}{Canada} & \\multicolumn{2}{c}{CA $-$ US Diff} \\\\
                \\cline{2-5} \\cline{6-9} \\cline{10-11} \\\\[-1.8ex]"

## ##the panel
## star.panel <- star_panel(
##   starlist = star.results.list, reg = FALSE,
##   panel.names = panel.names,
##   panel.label.fontface = "italic") %>%
##   ##add column numbers
##   star_add_column_numbers(insert.after = 13, multicol = "c") %>%
##   ##star_rhs_names(pattern = vars$pattern, line1 = vars$name) %>%
##   star_insert_row(string = star.string, insert.after = 10) %>%
##   ##replace asterisks
##   star_asterisks %>%
##   ##sideways table
##   star_sidewaystable %>%
##   ##SI col
##   sub("cccccccccc", star_si_col('2.2', rep.times = 10), x = .) %>%
##   star_notes_tex(note.type = "threeparttable",
##                  note = "Sample Consists of 264 metro areas in the United States and 82 in Canada observed in  1990 and 2011. All variables are measured for the 24-59 working age population. Standard deviations and regressions are weighted by 1990 population ages 24-59. Weekly Wages and Unemployment Insurance are in 2010 dollars in local currency, and weekly wages and manufacturing share are compiled using year $t - 1$ data. Canadian metro areas those with population greater than 15,000 in 1990; U.S. metro areas have population greater than 50,000 in 1999.")


## star_tex_write(star.panel, file = "TexFiles/05-convergence_full_sample.tex",
##                headers = headers.tex)


#########################
### -- Common City -- ###
#########################

DT.us.cross.common <- DT.us.cross[city_id %chin% c(common.city.ids)]
DT.ca.cross.common <- DT.ca.cross[city_id %chin% c(common.city.ids)]
DT.all.common <- DT.all[city_id %chin% c(common.city.ids)]

## -- Common Sample -- ##
DT.us.sd.common <- f_sd(DT.us.cross.common)
DT.ca.sd.common <- f_sd(DT.ca.cross.common)
DT.us.slopes.common <- f_slopes(DT.us.cross.common)
DT.ca.slopes.common <- f_slopes(DT.ca.cross.common)
DT.diff.slopes.common <- f_diff_slopes(DT.all.common)


DT.results.all.common <- merge(DT.us.sd.common, DT.us.slopes.common, by = "variable") %>%
  merge(DT.ca.sd.common, by = "variable") %>%
  merge(DT.ca.slopes.common, by = "variable") %>%
  merge(DT.diff.slopes.common, by = "variable")


DT.results.temp.common <- copy(DT.results.all.common) %>% .[, variable := as.character(variable)]
DT.results.wage.common <- DT.results.temp.common %>%
  .[variable %chin% c(vars.wages[["var"]])]
DT.results.emp.common <- DT.results.temp.common %>%
  .[variable %chin% c(vars.emp[["var"]])]

##check
stopifnot(
  nrow(rbind(DT.results.emp.common, DT.results.wage.common)) ==
    nrow(DT.results.temp.common)
)

star.results.list.common <- list(DT.results.wage.common, DT.results.emp.common) %>%
  lapply(as.matrix) %>%
  lapply(f_star) %>%
  setNames(c("wage", "emp"))

##insert the table labels
star.string <- " & \\multicolumn{4}{c}{United States} & \\multicolumn{4}{c}{Canada} & \\multicolumn{2}{c}{CA $-$ US Diff} \\\\
                \\cline{2-5} \\cline{6-9} \\cline{10-11} \\\\[-1.8ex]"

## ##the panel
## star.panel <- star_panel(
##   starlist = star.results.list.common, reg = FALSE,
##   panel.names = c("Wages", "Population and Employment"),
##   panel.label.fontface = "italic") %>%
##   ##add column numbers
##   star_add_column_numbers(insert.after = 13, multicol = "c") %>%
##   ##star_rhs_names(pattern = vars$pattern, line1 = vars$name) %>%
##   star_insert_row(string = star.string, insert.after = 10) %>%
##   sub("-- Ages 24 to 59", "-- Ages 24 to 59; Common City Sizes", x = .) %>%
##   ##replace asterisks
##   star_asterisks %>%
##   ##side ways table
##   star_sidewaystable %>%
##   ##SI col
##   sub("cccccccccc", star_si_col('2.2', rep.times = 10), x = .) %>%
##   star_notes_tex(note.type = "threeparttable",
##                    note = "All variables are measured for the 24-59 working age population. Standard deviations and regressions are weighted by 1990 population ages 24-59. Weekly Wages and Unemployment Insurance are in 2010 dollars in local currency, and weekly wages and manufacturing share are compiled using year $t - 1$ data. Cities are chosen so that 1990 24-59 city population size has a common support. Specifically, the minimum city size within each country is equal to the minimum city size in the US and the maximum city size within each country is equal to the maximum city size in Canada based on start of sample population.")


## star_tex_write(star.panel, file = "TexFiles/05-convergence_common_cities.tex",
##                headers = headers.tex)


##see https://stackoverflow.com/a/25961969
##see https://stackoverflow.com/a/33882030
##interwave the lists
##star.list.all <- c(rbind(star.results.list, star.results.list.common))
panel.names <- c("Wage Measures", "Population Characteristics and Non-Wage Measures")
##interweave the list
##panel.names <- c(rbind(panel.names, c(paste0(panel.names, "; Common City Sizes"))))
star.list.all <- c(star.results.list, star.results.list.common)
panel.names <- c(panel.names, paste0(panel.names, "; Common City Sizes"))


star.panel.all <- star_panel(starlist = star.list.all, reg = FALSE,
                             panel.names = panel.names,
                             panel.label.fontface = "italic") %>%
  star_add_column_numbers(insert.after = 13, multicol = "c") %>%
  ##Update the panel names
  sub("Panel A", "Panel A1", x = .) %>%
  sub("Panel B", "Panel A2", x = .) %>%
  sub("Panel C", "Panel B1", x = .) %>%
  sub("Panel D", "Panel B2", x = .) %>%
  star_insert_row(string = star.string, insert.after = 10) %>%
  ##replace asterisks
  star_asterisks %>%
  ##side ways table
  star_sidewaystable %>%
  ##SI col
  sub("cccccccccc", star_si_col('2.2', rep.times = 10), x = .) %>%
  star_notes_tex(note.type = "threeparttable",
                   note = "All variables are measured for the 24-59 working age population. Standard deviations and regressions are weighted by 1990 population ages 24-59. Weekly Wages and Unemployment Insurance are in 2010 dollars in local currency, and weekly wages and manufacturing share are compiled using year $t - 1$ data. Common city size metros are chosen so that 1990 24-59 metro population across countries has a common support. Specifically, the minimum metro size within each country is equal to the minimum metro size in the U.S. and the maximum metro size within each country is equal to the maximum metro size in Canada based on start of sample population. *, **, *** indicates statistical significance at the 10, 5, and 1 percent levels, respectively.")

star_tex_write(star.panel.all, file = "TexFiles/05-convergence_full_common.tex", headers = headers.tex)




#### -- Combined Plots -- ####

y.layer <- scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0))


p.wage.initial.log.pop.us.full <- readRDS("05-plot_wage_initial-rds/05-full_sample_US_start_log_pop2459.rds") +
  ggtitle("1A: United States") +
  scale_x_continuous(limits = c(6.39, 7.07),
                     breaks = seq(6.0, 7.5, by = 0.1)) +
  scale_y_continuous(limits = c(-0.23, 1),
                     breaks = seq(-1, 1, by = 0.25))

p.wage.initial.log.pop.ca.full <-
  readRDS("05-plot_wage_initial-rds/05-full_sample_Canada_start_log_pop2459.rds") +
  ggtitle("1B: Canada") +
  scale_x_continuous(breaks = seq(6.0, 7.5, by = 0.1)) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.25))



## p.wage.initial.log.pop.us.common <-
##   readRDS("05-plot_wage_initial-rds/05-common_support_US_start_log_pop2459.rds") +
##   ggtitle("2A: United States Common City Size") +
##   layers.us + layers.us2
## p.wage.initial.log.pop.ca.common <-
##   readRDS("05-plot_wage_initial-rds/05-common_support_Canada_start_log_pop2459.rds") +
##   ggtitle("2A: Canada Common City Size")

plot.list <- list(
  p.wage.initial.log.pop.us.full, p.wage.initial.log.pop.ca.full## ,
  ## p.wage.initial.log.pop.us.common, p.wage.initial.log.pop.ca.common
) %>% lapply(function(p) p + theme(legend.position = "none"))



p.all.wage.initial.pop <- plot_grid(plotlist = plot.list,
                                    nrow = 1, align = "hv")

ggsave("PlotsFinal/05-convergence_wage_initial_log_pop.pdf", plot = p.all.wage.initial.pop,
       width = 14.8, height = 5.5, dpi = 600)
ggsave("../10-Plots/PlotsFinal/05-convergence_wage_initial_log_pop.pdf", plot = p.all.wage.initial.pop,
       width = 14, height = 6, dpi = 900)

