##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-24

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(cowplot)})

##ggplot2 colors
plot.colors <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
##ggplot2 shapes
plot.shapes <- c(16, 17, 15, 3)
##ggplot2 linetypes
plot.linetypes <- c("solid", "22", "42", "44")


DT.klems <- readRDS("../../Data5/_DataFinal_/10-klems_labor_data.rds") %>%
  .[year %between% c(1980, 2010)]

DT.klems.total <- DT.klems[, .(year, Sector = "Total (All Industries)",
                               Hours_Ratio_CA_US = Total_Hours_Ratio_CA_US,
                               Wage_Ratio_CA_US = AllSector_Wage_Ratio_CA_US)]

DT.klems <- rbind(DT.klems, DT.klems.total, fill = TRUE) %>%
  .[, Sector := factor(Sector,
                       levels = c("Resource/Utilities", "Construction", "Manufacturing",
                                  "Services", "Total (All Industries)"))]

p.hrs.na <- ggplot(DT.klems, aes(x = year, y = Hours_Share_NA)) +
  geom_line(aes(linetype = Sector, color = Sector)) +
  geom_point(aes(shape = Sector, color = Sector), size = 2) +
  scale_y_continuous(trans = "log", breaks = c(5, 10, 20, 40, 80)) +
  labs(y = "Hours Worked - Sector/All Industries (%)",
       title = "1A: Aggregate Hours Worked in Canada and the U.S. by Sector")

p.hrs.rel <- ggplot(DT.klems, aes(x = year, y = Hours_Ratio_CA_US)) +
  geom_line(aes(linetype = Sector, color = Sector, size = Sector)) +
  geom_point(aes(shape = Sector, color = Sector), size = 2) +
  scale_y_continuous(trans = "log", breaks = c(0.1, 0.125, 0.150, 0.175)) +
  labs(y = "Hours Worked - CA/US",
       title = "2A: Hours Worked by Sector - Canada/US")

p.wage.na <- ggplot(DT.klems, aes(x = year, y = Relative_Wage_NA)) +
  geom_line(aes(linetype = Sector, color = Sector)) +
  geom_point(aes(shape = Sector, color = Sector), size = 2) +
  scale_y_continuous(trans = "log", breaks = c(0.8, 0.9, 1.0, 1.1, 1.2)) +
  labs(y = "Labor Compensation - Sector/All Industries",
       title = "1B: Mean Hourly Labor Compensation in Canada and the U.S. by Sector")

p.wage.rel <- ggplot(DT.klems, aes(x = year, y = Wage_Ratio_CA_US)) +
  geom_line(aes(linetype = Sector, color = Sector, size = Sector)) +
  geom_point(aes(shape = Sector, color = Sector), size = 2) +
  scale_y_continuous(trans = "log", breaks = c(0.6, 0.7, 0.8, 0.9, 1.0)) +
  labs(y = "Hourly Labor Compensation - CA/US",
       title = "2B: Hourly Labor Compensation by Sector - Canada/US")

##the plot list nad multiple updates
plot.list <- list(p.hrs.na, p.wage.na,
                  p.hrs.rel,
                  p.wage.rel) %>%
  ##Update the plots
  lapply(function(p)
    p +
      scale_color_manual(values = c(plot.colors, "#000000")) +
      scale_linetype_manual(values = c(plot.linetypes, "solid")) +
      scale_size_manual(values = c(0.5, 0.5, 0.5, 0.5, 1.1)) +
      scale_shape_manual(values = c(plot.shapes, NA)) +
      background_grid(minor = "none") +
      theme(axis.title.x = element_blank())
    )


##get the legend
legend <- get_legend(plot.list[[3]] +
                     theme(legend.position = "bottom", legend.justification = "center",
                           legend.text = element_text(size = 16),
                           legend.title = element_blank(),
                           legend.background = element_rect(color = "black", size = 0.5,
                                                            linetype = "solid")))
##remove the legend and make a grid
plots.out <- plot.list %>%
  ##remove the legend
  lapply(function(p) p + theme(legend.position = "none")) %>%
  ##create a multi-plot
  plot_grid(plotlist = ., align = "hv", nrow = 2) %>%
  ##Add in the legend
  plot_grid(., legend, nrow = 2, rel_heights = c(1, 0.06))

save_plot("PlotsFinal/32-sector_emp_wages.pdf", plot = plots.out,
          base_width = 16.67, base_height = 9, dpi = 900)
