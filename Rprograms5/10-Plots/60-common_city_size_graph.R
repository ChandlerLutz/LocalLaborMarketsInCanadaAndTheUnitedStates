##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-02-01

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(crossplotr); library(ggrepel)})

DT.ca.cross <- readRDS("../../Data5/_DataFinal_/10-ca_data_cross_section.rds")

DT.us.cross <- readRDS("../../Data5/_DataFinal_/10-us_data_cross_section.rds")

reg.city.ids <- readRDS("../../Data5/_DataFinal_/10-reg_city_ids.rds")
common.city.ids <-
  readRDS("../20-Bartik_China_Regressions/RdsFiles/21-common_pop_support_city_ids.rds")


DT.cross <- rbind(DT.us.cross, DT.ca.cross, fill = TRUE) %>%
  .[city_id %chin% c(reg.city.ids)] %>%
  .[start.year %in% c(1990, 1991)] %>%
  ##Get population in 1000s
  .[, start_pop2459 := start_pop2459 / 1000] %>%
  .[, start_log_pop2459 := log(start_pop2459)] %>%
  .[city_id %chin% reg.city.ids] %>%
  .[country == "US", min.common.pop := min(start_log_pop2459)] %>%
  .[, min.common.pop := mean(min.common.pop, na.rm = TRUE)] %>%
  .[country == "CA", max.common.pop := max(start_log_pop2459)] %>%
  .[, max.common.pop := mean(max.common.pop, na.rm = TRUE)] %>%
  ## .[, cutoff := as.integer(start_log_pop2459 < min.common.pop |
  ##                          start_log_pop2459 > max.common.pop)] %>%
  .[, cutoff := as.integer(!city_id %chin% c(common.city.ids))] %>%
  .[country == "US", country := "United States"] %>%
  .[country == "CA", country := "Canada"]


min.common.pop <- DT.cross[["min.common.pop"]] %>% unique
max.common.pop <- DT.cross[["max.common.pop"]] %>% unique

p <- crossplot(
  DT.cross, x.var = "start_log_pop2459",
  y.var = "start_w",
  size.var = "start_pop2459",
  shapes.var = "country",
  label.var = "shortname",
  xlabel = "Population, Ages 24 to 59, in thousands, Logarithmic Scale",
  ylabel = "Location Wage Index, Log Differential relative to National Average",
  shapes = c(1, 0)) +
  stat_smooth(method = lm, se = FALSE, show.legend = FALSE,
              aes(weight = start_pop2459, color = country)) +
  ##add the vertical lines for pop cutoffs
  geom_vline(xintercept = c(min.common.pop, max.common.pop),
             linetype = "dashed") +
  theme(##Move the legend
    legend.position = c(.95, .15),
    legend.justification = c("right", "top")
  )

## p.outliers <- crossplot_outliers(p, vars = c("start_log_pop2459", "start_w"),
##                                  num.outliers = 7)
##Add in the cutoffs
DT.cross.cutoff <- DT.cross[cutoff == 1]

p <- p + geom_text_repel(data = DT.cross.cutoff,
                         ##segment.colour = NA,
                         fontface = "bold", size = 3.5, force = 1,
                         max.iter = 2e4)

x.scale <- c(50, 400, 3000, 15000)
p <- p +
  ##Update the x scale
  scale_x_continuous(breaks = log(x.scale), labels = x.scale,
                     limits = c(2, 9.62))

ggsave(filename = "PlotsFinal/60-common_city_size_plot.pdf",
       plot = p, width = 14.3, height = 7.8)




## p <- ggplot(DT.cross, aes(x = log(start_pop2459), y = log(start_wkwage_2010usd),
##                           group = country, label = shortname)) +
##   geom_point(aes(shape = country, color = country, size = start_pop2459))



