##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-02

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())



##Load Packages
library(data.table); library(dtplyr); library(dplyr);
library(magrittr); library(lubridate); library(haven);
library(stringr); library(crossplotr); library(ggrepel)

##Th regression cities
reg.city.ids <- readRDS("../../Data5/_DataFinal_/10-reg_city_ids.rds")

ggrepel.segment.color <- "black"

DT.bartik <- readRDS("../../Data5/_DataFinal_/10-bartik_panel_us_ca.rds") %>%
  .[, .(city_id, bartik_log_diff_asm)] %>%
  .[, lapply(.SD, sum), by = city_id] %>%
  .[, start.end := "1990_2011"]

##A cross section of the data
us.cross <- readRDS("../../Data5/_DataFinal_/10-us_data_cross_section.rds") %>%
  .[, c("census.division", "lon", "lat") := NULL] %>%
  setnames("start.year", "year") %>%
  ##Update the year for plotting
  .[, year := paste0("year.", year)] %>%
  ##Add in the weekly wage (in levels)
  .[, start_wkwage := start_wkwage] %>%
  ##Multiply the unemployment rate by 100
  .[, start_census_unemp_rate := start_census_unemp_rate * 100] %>%
  ##The college versus high school labor supply
  .[, college_high_school_labor := start_college_labor_force / start_hs_labor_force * 100] %>%
  ##The college/high school log wage ratio
  .[, college_high_school_wage := start_log_college_wkwage - start_log_hs_wkwage] %>%
  ##The 90/10 log wage ratio
  .[, log_wage_90_10 := start_log_wkwage90 - start_log_wkwage10] %>%
  ##Multiply the employment-population ratio by 100
  .[, start_census_emp_pop := start_census_emp_pop * 100] %>%
  ##level college to high school labor
  .[, start_cd_hs_labor := exp(start_log_cd_hs_labor)] %>%
  .[, start_log_fb_pop2459_times_100 := start_log_fb_pop2459 + log(100)] %>%
  .[, start_log_college_pop_share_times_100 := log(start_college_pop_share)] %>%
  ##melt
  melt(id.vars = c("country", "state.abb", "city_id", "msa",
                   "shortname", "metro", "NonEnglish", "year", "pop1990"),
       variable.name = "variable", value.var = "value") %>%
  ##Now cast based on the years
  dcast(city_id + msa + shortname + state.abb + metro + NonEnglish + pop1990 + variable ~ year,
        value.var = "value") %>%
  .[, variable := as.character(variable)] %>%
  ##get if it's a reg city
  .[, reg.city := as.integer(city_id %chin% c(reg.city.ids))] %>%
  .[, id.for.mean.by := ifelse(reg.city == 1, city_id, state.abb)] %>%
  .[, shortname := ifelse(reg.city == 1, shortname, state.abb)]


##get the average for the year variables and keep population in 1990
year.vars <- names(us.cross) %>% .[grepl("year.[0-9]{4}", x = .)]

us.cross <- us.cross[, c(list(pop1990 = sum(pop1990)),
                         lapply(.SD, function(x) weighted.mean(x, w = pop1990, na.rm = TRUE))),
                     by = .(variable, id.for.mean.by, shortname), .SDcols = year.vars]


##Further updates
us.cross <- us.cross %>%
  ##non-metro if the ID for by has two letters
  .[, metro := ifelse(grepl("^[A-Z]{2}$", id.for.mean.by), "Non-Metro", "Metro")] %>%
  ##update the shapes variable based on metro, non-metro, non-english
  .[, shapes.var := ifelse(metro == "Non-Metro", "Non-Metro", "Metro")] %>%
  .[, shapes.var := factor(shapes.var, levels = c("Metro", "Non-Metro"))]

##The list of available cross sectional variables
us.cross$variable %>% unique

##Canadian data
ca.cross <- readRDS("../../Data5/_DataFinal_/10-ca_data_cross_section.rds") %>%
  .[pop1990 >= 15000] %>%
  .[shortname != "Wood Buffalo"] %>%
  .[is.na(state.abb), state.abb := shortname] %>%
  .[, c("census.division", "lon", "lat") := NULL] %>%
  ##keep only years that we want
  .[start.year %in% c(1981, 1991, 2000, 2006, 2007, 2011)] %>%
  setnames("start.year", "year") %>%
  ##Update the year for plotting
  .[, year := paste0("year.", year)] %>%
  ##Multiply the unemployment rate by 100
  .[, start_census_unemp_rate := start_census_unemp_rate * 100] %>%
  ##The college versus high school labor supply
  .[, college_high_school_labor := start_college_labor_force / start_hs_labor_force * 100] %>%
  ##The college/high school log wage ratio
  .[, college_high_school_wage := start_log_college_wkwage - start_log_hs_wkwage] %>%
  ##The 90/10 log wage ratio
  .[, log_wage_90_10 := start_log_wkwage90 - start_log_wkwage10] %>%
  ##Multiply the employment-population ratio by 100
  .[, start_census_emp_pop := start_census_emp_pop * 100] %>%
  .[, start_cd_hs_labor := exp(start_log_cd_hs_labor)] %>%
  ##foreign born
  .[, start_log_fb_pop2459_times_100 := start_log_fb_pop2459 + log(100)] %>%
  .[, start_log_college_pop_share_times_100 := log(start_college_pop_share)]

##ensure that the variables are numeric
vars <- names(ca.cross) %>% .[!(. %in% c("country", "state.abb", "msa", "city_id",
                                         "shortname", "metro", "NonEnglish",
                                         "year"))]
ca.cross <- ca.cross %>%
  .[, (vars) := lapply(.SD, as.numeric), .SDcols = vars] %>%
  ##melt
  melt.data.table(id.vars = c("country", "state.abb", "city_id", "msa", "shortname", "metro",
                              "NonEnglish", "year", "pop1990"),
                  variable.name = "variable", value.var = "value") %>%
  ##Now cast based on the years
  dcast.data.table(country + state.abb + city_id + msa + shortname +
                     metro + NonEnglish + pop1990 + variable ~ year,
                   value.var = "value") %>%
  .[, variable := as.character(variable)] %>%
  ##get if it's a reg city
  .[, reg.city := as.integer(city_id %chin% c(reg.city.ids))] %>%
  .[, id.for.mean.by := ifelse(reg.city == 1, city_id, state.abb)] %>%
  .[, shortname := ifelse(reg.city == 1, shortname, state.abb)]

##get the average for the year variables and keep population in 1990
year.vars <- names(ca.cross) %>% .[grepl("year.[0-9]{4}", x = .)]

ca.cross <- ca.cross[, c(list(pop1990 = sum(pop1990)),
                         lapply(.SD, function(x) weighted.mean(x, w = pop1990, na.rm = TRUE))),
                     by = .(variable, id.for.mean.by, shortname), .SDcols = year.vars]


##Further updates
ca.cross <- ca.cross %>%
  ##non-metro if the ID for by has two letters
  .[, metro := ifelse(grepl("^[A-Z]{2}$", id.for.mean.by), "Non-Metro", "Metro")] %>%
  ##update the shapes variable based on metro, non-metro, non-english
  .[, shapes.var := ifelse(metro == "Non-Metro", "Non-Metro", "Metro")] %>%
  .[, shapes.var := factor(shapes.var, levels = c("Metro", "Non-Metro"))]



us.diff <- readRDS("../../Data5/_DataFinal_/10-us_all_data_diff.rds") %>%
    .[, diff_cbp_emp_manufac_pop := diff_cbp_emp_manufac_pop * 100] %>%
    ##update the shapes variable based on metro, non-metro, non-english
    .[, shapes.var := ifelse(metro == "Non-Metro", "Non-Metro", "Metro")] %>%
    .[, shapes.var := factor(shapes.var, levels = c("Metro", "Non-Metro"))] %>%
  merge(DT.bartik, by = c("city_id", "start.end"), all.x = TRUE)
us.diff.1990.2011 <- us.diff[start.end == "1990_2011"]
us.diff.1980.2011 <- us.diff[start.end == "1980_2011"]
names(us.diff) %>% unique

ca.diff <- readRDS("../../Data5/_DataFinal_/10-ca_all_data_diff.rds") %>%
    .[, diff_cbp_emp_manufac_pop := diff_cbp_emp_manufac_pop * 100] %>%
    ##update the shapes variable based on metro, non-metro, non-english
    .[, shapes.var := ifelse(metro == "Non-Metro", "Non-Metro", "Metro")] %>%
    .[, shapes.var := factor(shapes.var, levels = c("Metro", "Non-Metro"))]

DT.bartik <- DT.bartik[, start.end := "1991_2011"]
ca.diff <- ca.diff %>%
  merge(DT.bartik, by = c("city_id", "start.end"), all.x = TRUE)
ca.diff.1990.2011 <- ca.diff[start.end == "1991_2011"]
ca.diff.1980.2011 <- ca.diff[start.end == "1981_2011"]

##Function for the crossplot data for a given variable
f_cross_us <- function(temp.var) us.cross[variable == temp.var]
f_cross_ca <- function(temp.var) ca.cross[variable == temp.var]
##Test
f_cross_us("start_wkwage")
f_cross_ca("start_wkwage")

##A function to get the data for the plot text
##(a subset of the data based on outliers and big cities)
f_cross_text_us <- function(temp.var, year.vars) {

    df.out <- crossplot_outliers(f_cross_us(temp.var), year.vars, num.outliers = 6)
    df.big <- f_cross_us(temp.var)[shortname %in% c("Los Angeles", "New York", "Detroit", "Chicago",
                                                    "Washington", "Houston", "Denver", "Seattle",
                                                    "Orlando", "Pittsburgh")]
    df.out <- bind_rows(df.out, df.big) %>%
        filter(!duplicated(shortname))
    return(df.out)

}

f_cross_text_ca <- function(temp.var, year.vars) {

    df.out <- f_cross_ca(temp.var) %>%
        .[, mget(c("shortname", year.vars))]
    df.out <- crossplot_outliers(df.out, year.vars, num.outliers = 6)
    df.big <- f_cross_ca(temp.var)[shortname %in% c("Toronto", "Montreal", "Vancouver", "Windsor",
                                                    "Ottawa", "Hamilton", "Salaberry", "Kitchener",
                                                    "Calgary")]
    df.out <- bind_rows(df.out, df.big) %>%
        filter(!duplicated(shortname))
    return(df.out)

}
##Test
## f_cross_text_us("start_wkwage", c("year.1980", "year.2011"))
## f_cross_text_ca("start_wkwage", c("year.1980", "year.2011"))

##A function to combine the plots
f_plot_combine <- function(..., plot.rows = 3) {
    plot.list <- list(...) %>%
        ##Get the legend on the bottom
        lapply(function(p) p + theme(legend.position = "bottom",
                                     legend.justification = "center"))

    ##the legend
    plot.legend <- get_legend(plot.list[[1]])

    ##The relative heights for the legend
    ##for various plot rows
    rel.height.legend <- ifelse(plot.rows == 1, 0.09, 0.05)

    ##Get the output
    plot.out <- plot.list %>%
        lapply(function(p) p + theme(plot.margin = unit(c(6,6,6,6), "pt"),
                                     legend.position = "none")) %>%
        plot_grid(plotlist = ., align = "hv", nrow = plot.rows) %>%
        plot_grid(., plot.legend, nrow = 2, rel_heights = c(1, rel.height.legend))

}


## -- Figure 1 -- ##

##Employment population
p.emp.pop.us <- crossplot(f_cross_us("start_census_emp_pop"),
                        x.var = "year.1990", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "2A: U.S. -- Employment/Population Ratio",
                        xlabel = "1990 Employment/Population Ratio (%)",
                        ylabel = "2011 Employment/Population Ratio (%)",
                        points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(65, 80),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
  geom_text_repel(
    data = f_cross_text_us("start_census_emp_pop", c("year.1990", "year.2011")),
    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
    max.iter = 2e4) +
  scale_x_continuous(breaks = c(60, 70, 80))

p.emp.pop.ca <- crossplot(f_cross_ca("start_census_emp_pop"),
                        x.var = "year.1991", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "2B: Canada -- Employment/Population Ratio",
                        xlabel = "1990 Employment/Population Ratio (%)",
                        ylabel = "2011 Employment/Population Ratio (%)",
                        points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(55, 80),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
  geom_text_repel(
    data = f_cross_text_ca("start_census_emp_pop", c("year.1991", "year.2011")),
    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4) +
    scale_x_continuous(breaks = c(60, 70, 80)) +
    scale_y_continuous(breaks = c(60, 70, 80))




##weekly wage
p.wage.us <- crossplot(f_cross_us("start_wkwage_2010usd"),
                        x.var = "year.1990", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "1A: U.S. -- Weekly Wage (2010 US$)",
                       xlabel = "1990 Weekly Wage",
                       ylabel = "2011 Weekly Wage", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(520, 1000),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
  geom_text_repel(data = f_cross_text_us("start_wkwage_2010usd",
                                         c("year.1990", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4) +
    scale_x_continuous(breaks = c(600, 800, 1000)) +
    scale_y_continuous(breaks = c(600, 800, 1000, 1200))

p.wage.ca <- crossplot(f_cross_ca("start_wkwage_2010usd"),
                       x.var = "year.1991", y.var = "year.2011", size.var = "pop1990",
                       shapes.var = "shapes.var", label.var = "shortname",
                       title = "1B: Canada -- Weekly Wage (2010 US$)",
                       xlabel = "1990 Weekly Wage", ylabel = "2011 Weekly Wage",
                       points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(590, 1200),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
  geom_text_repel(data = f_cross_text_ca("start_wkwage_2010usd",
                                         c("year.1991", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4) +
  scale_x_continuous(breaks = c(600, 800, 1000, 1200))

##manufac share
p.manufac.us <- crossplot(f_cross_us("start_census_manufac_share"),
                        x.var = "year.1990", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "3A: U.S. -- Manufacturing Share",
                        xlabel = "1990 Manufacturing Share (%)",
                        ylabel = "2011 Manufacturing Share (%)", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(10, 30),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_us("start_census_manufac_share", c("year.1990", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
p.manufac.ca <- crossplot(f_cross_ca("start_census_manufac_share"),
                        x.var = "year.1991", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "3B: Canada -- Manufacturing Share",
                        xlabel = "1990 Manufacturing Share (%)",
                        ylabel = "2011 Manufacturing Share (%)", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(2, 20),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
  geom_text_repel(data = f_cross_text_ca("start_census_manufac_share",
                                         c("year.1991", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)



fig1 <- f_plot_combine(p.wage.us, p.wage.ca,
                       p.emp.pop.us, p.emp.pop.ca,
                       p.manufac.us, p.manufac.ca,
                       plot.rows = 3)
save_plot("PlotsFinal/10-figure_1.pdf", plot = fig1,
          base_width = 14, base_height = 16, dpi = 900)
























## -- Bartik -- ##

us.diff.bartik <- us.diff.1990.2011[city_id %chin% (reg.city.ids)]
ca.diff.bartik <- ca.diff.1990.2011[city_id %chin% (reg.city.ids)]

f_text <- function(DT, vars) {

  DT.outliers <- crossplot_outliers(DT, vars = c("census_bartik", "diff_log_census_emp"),
                                    num.outliers = 6) %>% setDT
  big.cities <- c("Los Angeles", "New York", "Detroit", "Chicago",
                  "Washington", "Houston", "Denver", "Seattle",
                  "Orlando", "Pittsburgh",
                  ##canada
                  "Toronto", "Montreal", "Vancouver", "Windsor",
                  "Ottawa", "Hamilton", "Salaberry", "Kitchener",
                  "Calgary"
                  )

  DT.big <- DT %>% setDT %>% .[shortname %chin% c(big.cities)]

  DT.out <- rbind(DT.outliers, DT.big, fill = TRUE) %>% .[!duplicated(shortname)]

  return(DT.out)


}


##bartik employment change
p.census.bartik.emp.us <- crossplot(us.diff.bartik,
                             x.var = "census_bartik", y.var = "diff_log_census_emp",
                             size.var = "pop1990",
                             shapes.var = "shapes.var", label.var = "shortname",
                             title = "1A: U.S. -- Employment Change",
                             xlabel = "Projected Log Employment Change (Bartik): 1990 to 2011",
                             ylabel = "Log Employment Change: 1990 to 2011",
                             shapes = c(21),
                             colors = c("blue"),
                             points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(0.0, 0.75),
                          stats = c("slope", "r.squared"),
                          weighted = TRUE, sprintf.format = "%.3f") +
    geom_text_repel(data = f_text(us.diff.bartik, c("census_bartik", "diff_log_census_emp")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)

##bartik employment change
p.census.bartik.emp.ca <- crossplot(ca.diff.bartik,
                             x.var = "census_bartik", y.var = "diff_log_census_emp",
                             size.var = "pop1990",
                             shapes.var = "shapes.var", label.var = "shortname",
                             title = "1B: Canada -- Employment Change",
                             xlabel = "Projected Log Employment Change (Bartik): 1990 to 2011",
                             ylabel = "Log Employment Change: 1990 to 2011",
                             shapes = c(21),
                             colors = c("blue"),
                             points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(0.3, -0.4),
                          stats = c("slope", "r.squared"),
                          weighted = TRUE, sprintf.format = "%.3f") +
    geom_text_repel(data = f_text(ca.diff.bartik,
                                  c("census_bartik", "diff_log_census_emp")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)


##bartik real wage change
p.census.bartik.wage.us <- crossplot(us.diff.bartik,
                             x.var = "census_bartik", y.var = "diff_log_w",
                             size.var = "pop1990",
                             shapes.var = "shapes.var", label.var = "shortname",
                             title = "2A: U.S. -- Location Wage Change",
                             xlabel = "Census Bartik 1990-2011",
                             ylabel = "Change in Wage Location Index, 1990 to 2011",
                             shapes = c(21),
                             colors = c("blue"),
                             points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(0.0, 0.15),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE, sprintf.format = "%.3f") +
    geom_text_repel(data = f_text(us.diff.bartik,
                                  c("census_bartik", "diff_log_w")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
##print(p.census.bartik.wage.us)

p.census.bartik.wage.ca <- crossplot(ca.diff.bartik,
                             x.var = "census_bartik", y.var = "diff_log_w",
                             size.var = "pop1990",
                             shapes.var = "shapes.var", label.var = "shortname",
                             title = "2B: CA. -- Location Wage Change",
                             xlabel = "Census Bartik 1990-2011",
                             ylabel = "Change in Wage Location Index, 1990 to 2011",
                             shapes = c(21),
                             colors = c("blue"),
                             points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(0.1, 0.5),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE, sprintf.format = "%.3f") +
    geom_text_repel(data = f_text(ca.diff.bartik,
                                  c("census_bartik", "diff_log_w")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
##print(p.census.bartik.wage.us)


##bartik diff_log_census_emp_pop
p.census.bartik.emp.pop.us <- crossplot(us.diff.bartik,
                             x.var = "census_bartik", y.var = "diff_log_census_emp_pop",
                             size.var = "pop1990",
                             shapes.var = "shapes.var", label.var = "shortname",
                             title = "2A: U.S. -- Log Employment-Population Ratio",
                             xlabel = "Census Bartik 1990-2011",
                             ylabel = "1990-2011 Diff Log Emp Change",
                             shapes = c(21),
                             colors = c("blue"),
                             points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(-0.2, 0.2),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE, sprintf.format = "%.3f") +
    geom_text_repel(data = crossplot_outliers(us.diff.bartik,
                                              c("census_bartik", "diff_log_census_emp_pop"),
                                              num.outliers = 6),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
##print(p.census.bartik.wage.us)

p.census.bartik.emp.pop.ca <- crossplot(ca.diff.bartik,
                             x.var = "census_bartik", y.var = "diff_log_census_emp_pop",
                             size.var = "pop1990",
                             shapes.var = "shapes.var", label.var = "shortname",
                             title = "2A: CA. -- Log Employment-Population Ratio",
                             xlabel = "Census Bartik 1990-2011",
                             ylabel = "1990-2011 Emp Change",
                             shapes = c(21),
                             colors = c("blue"),
                             points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(-0.2, 0.2),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE, sprintf.format = "%.3f") +
    geom_text_repel(data = f_text(ca.diff.bartik,
                                  c("census_bartik", "diff_log_census_emp_pop")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
##print(p.census.bartik.wage.us)


bartik.legend <- get_legend(p.census.bartik.emp.us + theme(legend.position = "bottom",
                                                    legend.justification = "center"))

p.census.bartik.out <- list(p.census.bartik.emp.us, p.census.bartik.emp.ca,
                            p.census.bartik.wage.us, p.census.bartik.wage.ca) %>%
                            ##p.census.bartik.emp.pop.us, p.census.bartik.emp.pop.ca) %>%
    lapply(function(p) p + theme(legend.position = "none")) %>%
    plot_grid(plotlist = ., align = "hv", nrow = 2) ## %>%
    ## plot_grid(., bartik.legend, nrow = 2, rel_heights = c(1, 0.04))

save_plot("PlotsFinal/10-census_bartik_reduced_form.pdf", plot = p.census.bartik.out,
          base_width = 14, base_height = 10, dpi = 900)





## -- Appendix figures -- ##

##Local Wage index
p.local.wage.us <- crossplot(f_cross_us("start_w"),
                        x.var = "year.1990", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "1A: U.S. -- Local Wage Index ",
                        xlabel = "1990 Local Wage Index",
                        ylabel = "2011 Local Wage Index", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(-0.4, 0.1),
                          stats = c("slope"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_us("start_w", c("year.1990", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
##print(p.local.wage.us)

p.local.wage.ca <- crossplot(f_cross_ca("start_w"),
                        x.var = "year.1991", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "1B: Canada -- Local Wage Index",
                        xlabel = "1990 Local Wage Index",
                        ylabel = "2011 Local Wage Index", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(-0.3, 0.2),
                          stats = c("slope"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_ca("start_w", c("year.1991", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
##print(p.local.wage.ca)


##Local skill index
p.local.skill.us <- crossplot(f_cross_us("start_w_xb"),
                        x.var = "year.1990", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "2A: U.S. -- Local Skill Index",
                        xlabel = "1990 Local Skill Index",
                        ylabel = "2011 Local Skill Index", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(-0.2, 0),
                          stats = c("slope"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_us("start_w_xb", c("year.1990", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
##print(p.local.skill.us)

p.local.skill.ca <- crossplot(f_cross_ca("start_w_xb"),
                        x.var = "year.1991", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "2B: Canada -- Local Skill Index ",
                        xlabel = "1990 Local Skill Index",
                        ylabel = "2011 Local Skill Index", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(-0.13, 0),
                          stats = c("slope"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_ca("start_w_xb", c("year.1991", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
##print(p.local.skill.ca)




##college.hs labor us
p.college.hs.labor.us <- crossplot(f_cross_us("start_cd_hs_labor"),
                                   x.var = "year.1990", y.var = "year.2011",
                                   size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "3A: U.S. -- Relative Supply of Univ/HS Labor",
                        xlabel = "1990 Univ/HS Labor",
                        ylabel = "2011 Univ/HS Labor", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(0.22, 1.3),
                          stats = c("slope"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_us("start_cd_hs_labor", c("year.1990", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
##print(p.college.hs.labor.us)

##College high school labor ca
p.college.hs.labor.ca <- crossplot(f_cross_ca("start_cd_hs_labor"),
                        x.var = "year.1991", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "3B: Canada -- Relative Supply of Univ/HS Labor",
                        xlabel = "1990 Univ/HS Labor",
                        ylabel = "2011 Univ/HS Labor", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(0.22, 0.9),
                          stats = c("slope"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_ca("start_cd_hs_labor", c("year.1991", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
##print(p.college.hs.labor.ca)

fig2 <- f_plot_combine(p.local.wage.us, p.local.wage.ca,
                       p.local.skill.us, p.local.skill.ca,
                       p.college.hs.labor.us, p.college.hs.labor.ca)

save_plot("PlotsFinal/10-figure_2.pdf", plot = fig2,
          base_width = 14, base_height = 16, dpi = 900)


## -- Appendix Figure -- ##
## -- Figure 3 -- ##

##College/High school Relative labor supply
p.college.hs.wage.us <- crossplot(f_cross_us("start_log_cd_hs_wkwage"),
                        x.var = "year.1990", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "1A: U.S. -- Log Univ/HS Wage",
                        xlabel = "1990 Log Univ/HS Wage",
                        ylabel = "2011 Log Univ/HS Wage", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(0.58, 0.25),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_us("start_log_cd_hs_wkwage", c("year.1990", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
##print(p.college.hs.wage.us)

p.college.hs.wage.ca <- crossplot(f_cross_ca("start_log_cd_hs_wkwage"),
                        x.var = "year.1991", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "1B: Canada -- Log Univ/HS Wage",
                        xlabel = "1990 Log Univ/HS Wage",
                        ylabel = "2011 Log Univ/HS Wage", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(0.1, 0.4),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_ca("start_log_cd_hs_wkwage", c("year.1991", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
##print(p.college.hs.wage.ca)


##90/10 log wage ratio us
p.90.10.wage.us <- crossplot(f_cross_us("log_wage_90_10"),
                        x.var = "year.1990", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "2A: U.S. -- Log 90/10 Wage Ratio",
                        xlabel = "1990 Log 90/10 Wage Ratio",
                        ylabel = "2011 Log 90/10 Wage Ratio", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(1.3, 2),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_us("log_wage_90_10", c("year.1990", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
##print(p.90.10.wage.us)

##90/10 log wage ratio CA
p.90.10.wage.ca <- crossplot(f_cross_ca("log_wage_90_10"),
                        x.var = "year.1991", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "2B: Canada -- 90/10 Log Wage Ratio",
                        xlabel = "1990 90/10 Log Wage Ratio",
                        ylabel = "2011 90/10 Log Wage Ratio", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(2.0, 1.6),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_ca("log_wage_90_10", c("year.1991", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)
##print(p.90.10.wage.ca)


## Log (University / population * 100 ) US
p.log.cd.pop.us <- crossplot(f_cross_us("start_log_college_pop_share_times_100"),
                        x.var = "year.1990", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "3A: U.S. -- Log University-Population Ratio",
                        xlabel = "1990 Log (University / Population x 100)",
                        ylabel = "2011 Log (University / Population x 100)", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(2.9, 3.9),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_us("start_log_college_pop_share_times_100", c("year.1990", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)

## Log (University / population * 100 ) ca
p.log.cd.pop.ca <- crossplot(f_cross_ca("start_log_college_pop_share_times_100"),
                        x.var = "year.1991", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "3B: Canada -- Log University-Population Ratio",
                        xlabel = "1990 Log (Univsersity / Population x 100)",
                        ylabel = "2011 Log (University / Population x 100)", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(2.8, 3.8),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_ca("start_log_college_pop_share_times_100", c("year.1991", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)


## Log (foreign born / population * 100 ) US
p.log.fb.pop.us <- crossplot(f_cross_us("start_log_fb_pop2459_times_100"),
                        x.var = "year.1990", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "4A: U.S. -- Log Foreign Born-Population Ratio",
                        xlabel = "1990 Log (Foreign Born / Population x 100)",
                        ylabel = "2011 Log (Foreign Born / Population x 100)", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(0, 3.5),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_us("start_log_fb_pop2459_times_100", c("year.1990", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)

## Log (foreign born / population * 100 ) ca
p.log.fb.pop.ca <- crossplot(f_cross_ca("start_log_fb_pop2459_times_100"),
                        x.var = "year.1991", y.var = "year.2011", size.var = "pop1990",
                        shapes.var = "shapes.var", label.var = "shortname",
                        title = "4B: Canada -- Log Foreign Born-Population Ratio",
                        xlabel = "1990 Log (Foreign Born / Population x 100)",
                        ylabel = "2011 Log (Foreign Born / Population x 100)", points.alpha = 0.75) %>%
    crossplot_print_stats(text.pos = c(-0.5, 3),
                          stats = c("slope", "r.squared"),
                          xlabel = "1990", ylabel = "2011",
                          weighted = TRUE) +
    geom_text_repel(data = f_cross_text_ca("start_log_fb_pop2459_times_100", c("year.1991", "year.2011")),
                    segment.colour = ggrepel.segment.color, fontface = "bold", size = 3.5, force = 1,
                    max.iter = 2e4)




##Combine
fig3 <- f_plot_combine(p.college.hs.wage.us, p.college.hs.wage.ca,
                       p.90.10.wage.us, p.90.10.wage.ca,
                       p.log.cd.pop.us, p.log.cd.pop.ca,
                       p.log.fb.pop.us, p.log.fb.pop.ca,
                       plot.rows = 4)

save_plot("PlotsFinal/10-figure_3.pdf", plot = fig3,
          base_width = 14, base_height = 22, dpi = 900)
