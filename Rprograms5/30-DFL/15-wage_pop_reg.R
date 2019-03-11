##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-01-22

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(lfe); library(stargazer); library(starpolishr)})


##Table for regressions of wages on population -- pooled

##Set to TRUE if you want latex headers in the tex output
headers.tex <- FALSE

##the stargazer dfl panel labels
dfl.labels <- readRDS("RdsFiles/10-dfl_panel_labels.rds")

##the city ids used in the regressions
reg.city.ids <- readRDS("../../Data5/_DataFinal_/10-reg_city_ids.rds")

common.city.ids <- readRDS("../20-Bartik_China_Regressions/RdsFiles/21-common_pop_support_city_ids.rds")

us.data <- readRDS("../../Data5/_DataFinal_/10-us_data_cross_section.rds") %>%
  .[metro == "Metro"] %>%
  .[city_id %chin% c(reg.city.ids)] %>%
  .[start.year %in% c(1990, 2000, 2007, 2011)] %>%
  .[!is.na(start_pop2459)] %>%
  .[, log_pop2459 := log(start_pop2459)] %>%
  .[, NonEnglish := as.character(NonEnglish)] %>%
  .[, NonEnglish := ifelse(NonEnglish == "1", "Hispanophone Metro", "Metro")]

ca.data <- readRDS("../../Data5/_DataFinal_/10-ca_data_cross_section.rds") %>%
  .[metro == "Metro"] %>%
  .[city_id %chin% c(reg.city.ids)] %>%
  .[start.year %in% c(1991, 2001, 2007, 2011)] %>%
  .[!is.na(start_pop2459)] %>%
  .[, start.year := ifelse(start.year == 1981, 1980,
                           ifelse(start.year == 1991, 1990,
                                  ifelse(start.year == 2001, 2000, start.year)))] %>%
  .[, NonEnglish := as.character(NonEnglish)] %>%
  .[, NonEnglish := ifelse(NonEnglish == "1", "Quebec", "Metro")]


all.data <- rbind(us.data, ca.data, fill = TRUE) %>%
    ##keep just what we need
      .[, .(country = factor(country, levels = c("US", "CA")),
            state.abb, msa, city_id, shortname,
            NonEnglish, start.year = as.factor(start.year),
            decade = dplyr::dense_rank(start.year),
            start_pop2459, log_pop = log(start_pop2459),
            log_wkwage = start_log_wkwage - log(1.221),
            w = start_w,
            w_raw = start_w_raw,
            w_xb = start_w_xb,
            log_cd_hs_labor = log(start_college_labor_force / start_hs_labor_force),
            log_cd_pop = start_log_census_college2459_pop2459,
            log_cd_hs_wage = start_log_college_wkwage - start_log_hs_wkwage,
            log_90_10_wage = start_log_wkwage90 - start_log_wkwage10,
            log_fb_nb = start_log_fb_nb,
            log_fb_pop2459 = start_log_fb_pop2459,
            p = start_p,
            start_cbp_emp_manufac_share_msa,
            start_cbp_emp_forest_share_msa,
            start_cbp_emp_oil_share_msa,
            start_cbp_emp_all_mining_share_msa,
            start_census_manufac_share,
            start_census_old_Manufacturing_share, start_census_old_Mining_share,
            start_census_old_Petroleum_share)] %>%
  .[, NonEnglish := factor(NonEnglish, levels = c("Metro", "Quebec", "Hispanophone Metro"))] %>%
  .[, NonEnglish_char := as.character(NonEnglish)] %>%
  ##b/c stata doesn't encode factors correctly
  CLmisc::dummy_cols(select_columns = "NonEnglish", by_reference = TRUE) %>%
  setnames("NonEnglish_Hispanophone Metro", "NonEnglish_HispanophoneMetro")


##Import the odds ratio and merge and create the weight
DT.log.odds <- readRDS("RdsFiles/10-dfl_step1_odds_ratio.rds")
numeric.cols <- names(DT.log.odds) %>% .[!grepl("city_id", .)]


##update the city cizes for common city sizes
##get the max city size in Canada
all.data.common <- all.data %>%
  .[city_id %chin% c(common.city.ids)]

all.data.common <- merge(all.data.common, DT.log.odds, by = "city_id", all.x = TRUE)

all.data[, uniqueN(msa), by = country]
all.data.common[, uniqueN(msa), by = country]


lhs.vars <- list(
  c("w_raw", "Log", "Weekly", "Wage"),
  c("w", "Wage", "Location", "Index"),
  c("w_xb", "Wage", "Skill", "Index"),
  c("log_cd_pop", "Log", "Univ/", "Pop"),
  c("log_fb_pop2459", "Log", "Foreign/", "Pop"),
  c("log_cd_hs_wage", "Log", "Univ/HS", "Wage"),
  c("log_90_10_wage", "Log", "90/10", "Wage"),
  c(" p", "Local", "Housing", "Costs")
)

lhs.vars <- do.call("rbind", lhs.vars ) %>%
  as.data.frame(., stringsAsFactors = FALSE)
lhs.vars [] <- lapply(lhs.vars , function(x) unlist(x))
lhs.vars <- setNames(lhs.vars , c("lhs.var", "line1", "line2", "line3"))
lhs.vars <- lhs.vars %>% setDT %>%
  .[, regex.pattern := gsub("_", "\\\\\\\\_", x = lhs.var)]



f_panel_reg <- function(DT, dfl = FALSE) {

  felm.rhs <- "~ log_pop * country + NonEnglish + country * start.year | 0 | 0 | state.abb"

  f_felm <- function(lhs.var) {

    f <- as.formula(paste0(lhs.var, felm.rhs))

    mod.out <- do.call("felm", list(f, data = DT, weights = DT$w.weight))

  }

  if (!dfl) {
    rhs.order <- c("log\\\\_pop ")
    keep.rhs <- c("log_pop")
  } else {
    rhs.order <- c("log\\\\_pop ", "log\\\\_pop:countryCA")
    keep.rhs <- c("log_pop ", "log_pop:countryCA")
  }


  mod.list <- lapply(lhs.vars$lhs.var, f_felm)

  star.out <- capture.output(
    stargazer(mod.list,
              type = "latex",
              label = "tab:gradients",
              title = "Urban Population Gradients for Local Labor Market Outcomes in the U.S. and Canada: 1990-2011 Pooled",
              keep.stat = "n",
              keep = keep.rhs,
              omit = c("Constant", "^countryCA$"))) %>%
    ##star_rhs_order(rhs.order) %>%
    star_rhs_names(pattern = c("log\\\\_pop ", "log\\\\_pop:countryCA"),
                   line1 = c("Log ", "Log Pop $\\\\times$"),
                   line2 = c("Population", "Canada")) %>%
    star_lhs_names(pattern = lhs.vars$regex.pattern,
                   line1 = lhs.vars$line1,
                   line2 = lhs.vars$line2,
                   line3 = lhs.vars$line3)


  return(star.out)

}

star.full <- all.data %>% copy %>% .[, w.weight := start_pop2459] %>%
  f_panel_reg
star.common <- all.data.common %>% copy %>% .[, w.weight := start_pop2459] %>%
  f_panel_reg
star.dfl1 <- all.data.common %>% copy %>% .[!is.na(odds.ratio1)] %>%
  .[, w.weight := start_pop2459 * odds.ratio1] %>%
  f_panel_reg(dfl = TRUE)
star.dfl2 <- all.data.common %>% copy %>% .[!is.na(odds.ratio2)] %>%
  .[, w.weight := start_pop2459 * odds.ratio2] %>%
  f_panel_reg(dfl = TRUE)


star.panel <- star_panel(
  star.full, star.common, star.dfl1, star.dfl2,
  panel.names = c("Full Sample",
                  dfl.labels[1],
                  dfl.labels[2],
                  dfl.labels[3]),
  same.summary.stats = FALSE, panel.label.fontface = "italic") %>%
  star_notes_tex(note.type = "caption",
                 note = "Panel A corresponds to the full sample data, observed in 1990, 2000, 2007, and 2011 with 264 metros in the U.S. and 82 in Canada. In Panel A, Canadian metro areas are those with population greater than 15,000 in 1990; U.S. metro areas have population greater than 50,000 in 1999. Panel B uses common city size support where cities are chosen so that 1990 24-59 metro population size across countries has a common support. Specifically, the minimum city size within each country is equal to the minimum city size in the U.S. and the maximum city size within each country is equal to the maximum city size in Canada based on start of sample population. This yields 255 metros in the U.S. and 54 in Canada. See table \\ref{tab:dfl_step1} for the reweigthing model specifications. Regressions are weighted by start of period population 24-59. Heteroskedasticity robust standard errors clustered at the state/province level and are in parentheses. *, **, *** indicates statistical significance at the 10, 5, and 1 percent levels, respectively.")



star_tex_write(star.panel, file = "TexFiles/15-wage_pop_reg.tex", headers = headers.tex)
