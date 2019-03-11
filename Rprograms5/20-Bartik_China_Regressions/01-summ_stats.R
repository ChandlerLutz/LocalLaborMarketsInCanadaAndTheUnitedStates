##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-30

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())

suppressMessages({library(CLmisc); library(Hmisc);
  library(starpolishr); library(stargazer)})

headers.tex <- FALSE

wtd.sd <- function(...) sqrt(wtd.var(...))

reg.city.ids <- readRDS("../../Data5/_DataFinal_/10-reg_city_ids.rds")

##Canadian data
DT.ca.cross <- readRDS("../../Data5/_DataFinal_/10-ca_data_cross_section.rds") %>%
  .[, start_fb_pop2459 := foreign.born / start_pop2459 * 100] %>%
  .[, start_census_college2459_pop2459 := exp(start_log_census_college2459_pop2459) *
        100]
DT.ca.union <- readRDS("../../Data5/_DataFinal_/11-ca_union_coverage.rds")
DT.ca.cross <- merge(DT.ca.cross, DT.ca.union, by = c("city_id", "start.year"),
                     all.x = TRUE) %>%
  .[city_id %chin% c(reg.city.ids)] %>%
  .[start.year %in% c(1991, 2001, 2007, 2011)] %>%
  .[start.year == 1991, start.year := 1990] %>%
  .[start.year == 2001, start.year := 2000]

DT.us.cross <- readRDS("../../Data5/_DataFinal_/10-us_data_cross_section.rds") %>%
  .[, start_fb_pop2459 := foreign.born / start_pop2459 * 100] %>%
  .[, start_census_college2459_pop2459 := exp(start_log_census_college2459_pop2459) *
        100]
DT.us.union <- readRDS("../../Data5/_DataFinal_/11-us_union_coverage.rds")
DT.us.cross <- merge(DT.us.cross, DT.us.union, by = c("city_id", "start.year")) %>%
  .[city_id %chin% c(reg.city.ids)]

DT.us.cross <- DT.us.cross[start.year %in% c(1990, 2000, 2007, 2011)] %>%
  .[city_id %chin% c(reg.city.ids)]

temp <- DT.us.cross[start.year!=1980,.(city_id,start.year,native.born, foreign.born,start_pop2459,fb_nb=native.born+foreign.born)][,fb_nb_start_pop2459 := fb_nb/start_pop2459]
temp[fb_nb_start_pop2459<0.95|fb_nb_start_pop2459>1.05]
print(temp)


##Update to get emp-pop and unemp rate in percentage points
DT.us.cross <- DT.us.cross %>%
  .[, `:=`(start_census_emp_pop = start_census_emp_pop * 100,
           start_census_unemp_rate = start_census_unemp_rate * 100,
           ##get the unemployment insurance in per population terms
           start_UnempInsurance = start_UnempInsurance / start_pop2459 * 1000,
           ##Divide start of period population by 1000
           start_pop2459 = start_pop2459 / 1000
           )
    ]

DT.ca.cross <- DT.ca.cross %>%
  .[, `:=`(start_census_emp_pop = start_census_emp_pop * 100,
           start_census_unemp_rate = start_census_unemp_rate * 100,
           start_UnempInsurance = start_UnempInsurance / start_pop2459 * 1000,
           ##Divide start of period population by 1000
           start_pop2459 = start_pop2459 / 1000
           )
    ]



f_summ_stats <- function(DT, var, sprintf.format) {

  DT <- copy(DT)
  ## if (var == "start_pop2459") {
  ##   print("Note: Equal Weights in summary stats for start_pop2459")
  ##   DT <- DT[, w.weight := 1]
  ## } else {
  ##   DT <- DT[, w.weight := start_pop2459]
  ## }

  sprintf.format.mean <- sprintf.format
  sprintf.format.sd <- paste0("(", sprintf.format, ")")

  DT <- DT[, w.weight := start_pop2459]

  DT.mean <- DT[, lapply(.SD, weighted.mean, w = w.weight, na.rm = TRUE), .SDcols = var,
                by = .(year = as.character(start.year))] %>%
    setNames(c("year", "mean")) %>%
    .[, mean := sprintf(sprintf.format.mean, mean)]

  DT.sd <- DT[, lapply(.SD, wtd.sd, weights = w.weight, na.rm = TRUE), .SDcols = var,
              by = .(year = as.character(start.year))] %>%
    setNames(c("year", "sd")) %>%
    .[, sd := sprintf(sprintf.format.sd, sd)]

  out <- merge(DT.mean, DT.sd) %>%
    t(.)

  ##update the row and column names
  rownames(out) <- c("", "Mean", "Std Dev")
  temp.colnames <- out[1, ] %>% paste0("yeartemp", .)
  out <- out[-1, ]
  colnames(out) <- temp.colnames



  return(out)

}


##Variable list
vars <- list(
  c("start_pop2459", "Working Age Population (thousands)", "%0.f"),
  c("start_census_emp_pop", "Employment-Population Ratio (\\%)", "%0.1f"),
  c("start_wkwage_2010usd", "Weekly Wage (USD 2010)", "%0.f"),
  c("start_census_unemp_rate", "Unemployment Rate (\\%)", "%0.2f"),
  ##c("start_census_manufac_share", "Manufacturing Share (\\%)", "%0.1f"),
  ## c("start_cd_hs_labor", "University / High School Labor Ratio (Katz-Murphy measure explained in notes)"),
  c("start_census_college2459_pop2459", "University/Population Ratio (\\%) (Katz-Murphy measure explained in notes)", "%0.1f"),
  ## c("start_fb_nb", "Foreign/Native-Born Ratio"),
  c("start_fb_pop2459", "Foreign/Population Ratio (\\%)", "%0.1f"),
  c("Union_Coverage", "Union Coverage (\\%)", "%0.1f"),
    c("start_UnempInsurance", "Unemployment Insurance (USD per capita)", "%0.f")
)

vars <- do.call("rbind", vars) %>%
  as.data.frame(., stringsAsFactors = FALSE)
vars[] <- lapply(vars, function(x) unlist(x))
vars <- setNames(vars, c("var", "var.name", "sprintf.format"))
vars %>% setDT


f_stargazer <- function(temp.var) {

  sprintf.format <- vars[var == c(temp.var), sprintf.format]

  us.stats <- f_summ_stats(DT.us.cross, temp.var, sprintf.format)
  ca.stats <- f_summ_stats(DT.ca.cross, temp.var, sprintf.format)

  all.stats <- cbind(us.stats, ca.stats)

  star.out <-
    capture.output(stargazer(all.stats,
              title = "Local Labor Market Outcomes for the U.S. and Canada for Prime-Age Population (24 to 59), 1990 to 2011",
              label = "tab:summ_stats")) %>%
    ##clean up see https://stackoverflow.com/a/33066209/1317443
    gsub("([0-9]{4})(.1)", "\\1", x = .) %>%
    gsub("yeartemp", "", x = .)

}


star.list <- lapply(vars$var, f_stargazer)



star.out <- star_panel(starlist = star.list, panel.names = vars$var.name,
                       reg = FALSE,
                       panel.label.fontface = "italic")

##insert some formatting

star.string.ca.us <- c(
  " & \\multicolumn{4}{c}{United States} & \\multicolumn{4}{c}{Canada} \\\\
   \\cline{2-5} \\cline{6-9} \\\\[-2ex]"
)

star.cline <- "\\cline{2-5} \\cline{6-9} \\\\[-2ex]"

##get where the year lines are
year.lines <- grepl("1990 \\& 2000", star.out) %>% which
num.year.lines <- length(year.lines)

star.string <- c(rep(star.string.ca.us, num.year.lines))
##star.string <- c(star.string, rep(star.cline, num.year.lines))
star.string.line.numbers <- c(year.lines - 1)
##star.string.line.numbers <- c(star.string.line.numbers, year.lines)
star.out <- star.out %>%
  star_insert_row(string = star.string, insert.after = star.string.line.numbers) %>%
  star_add_column_numbers(insert.after = 11)


##Some other cleaning
star.out <- star.out %>%
  ##update column justification
  sub("ccccccccc", "lcccccccc", x = .) %>%
  star_notes_tex(note.type = "caption", note = "Sample consists of 264 metro areas in the United States and 82 in Canada observed in 1990, 2000, 2007, and 2011. All variables (excluding unemployment insurance) are measured for the 24-59 working age population. Weekly Wages and Unemployment Insurance are in 2010 U.S. dollars, and weekly wages and manufacturing share are compiled using year $t - 1$ data. The unemployment insurance is annual dollars per person and unemployment insurance is not available for all metros in all time periods. Canadian metro areas those with population greater than 15,000 in 1990; U.S. metro areas have population greater than 50,000 in 1999. Averages and standard deviations are weighted by start of period population. Data definitions are in section \\ref{sec:data}.")

star_tex_write(star.out, file = "TexFiles/01-summ_stats.tex", headers = headers.tex)
