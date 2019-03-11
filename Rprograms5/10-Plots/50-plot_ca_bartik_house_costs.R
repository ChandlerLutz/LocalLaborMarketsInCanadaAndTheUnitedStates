##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-02

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(ggrepel); library(crossplotr)})

DT <- readRDS("../../Data5/_DataFinal_/10-bartik_panel_us_ca.rds") %>%
  .[country == "CA"] %>%
  .[, shortname := gsub("CA|[0-9]+", "", city_id)]


f_plot <- function(temp.start_end, title, text.pos) {

  ##The data for this iteration
  DT.temp <- DT[start_end == c(temp.start_end)]

  p <- crossplot(DT.temp,
                 x.var = "bartik_log_diff_asm", y.var = "diff_log_p",
                 size.var = "start_pop2459", label.var = "shortname",
                 xlabel = "CBP/ASM Bartik", ylabel = "Diff in Log Housing Costs",
                 points.alpha = 0.75) +
    ggtitle(title)
  p <- crossplot_print_stats(p, text.pos = text.pos, weighted = TRUE)
  DT.outliers <- crossplot_outliers(
    p, vars = c("bartik_log_diff_asm", "diff_log_p"),
    num.outliers = 4) %>% setDT
  ca.big.cities <- DT.temp[city_id %chin% c("CA462Montreal", "CA535Toronto",
                                            "CA933Vancouver")]
  DT.outliers <- rbind(DT.outliers, ca.big.cities) %>%
    .[!duplicated(city_id)]

  p <- p + geom_text_repel(data = DT.outliers,
                           segment.colour = NA, fontface = "bold", size = 4, force = 1,
                           max.iter = 2e4)
  return(p)

}

vars <- list(
  ## start_end, title, text.pos
  list("1990_2000", "1990 - 2000", c(-0.4, 0.3)),
  list("2000_2007", "2000 - 2007", c(-0.05, 0.2)),
  list("2007_2011", "2007 - 2011", c(-0.15, 0.2))
)

vars <- do.call("rbind", vars) %>%
  as.data.frame(., stringsAsFactors = FALSE)
vars <- setNames(vars, c("start_end", "title", "text.pos"))



p.all <- Map(f_plot,
             temp.start_end = vars$start_end,
             title = vars$title,
             text.pos = vars$text.pos) %>%
  plot_grid(plotlist = ., nrow = 2)


save_plot("PlotsFinal/50-plot_ca-bartik_housing_cost_all.pdf", plot = p.all,
          base_width = 16, base_height = 8.5,
          dpi = 900)
