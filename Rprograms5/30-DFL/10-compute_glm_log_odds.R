##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-12-31

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())


suppressMessages({library(CLmisc); library(survey);
  library(stargazer); library(starpolishr)})

headers.tex <- FALSE

##get the common city sizes
common.support.city.ids <-
  readRDS("../20-Bartik_China_Regressions/RdsFiles/21-common_pop_support_city_ids.rds")

f_winsorize <- function(x, n) {

  x.rank <- dplyr::min_rank(x)

  ##Max rank
  max.rank <- max(x.rank)
  x.max.rank.closest.to.n <- (max.rank - n + 1) %>%
    abs %>% .[1]
  x.max.cutoff <- x[x.rank == x.max.rank.closest.to.n]
  x.min.rank.closest.to.n <- (x.rank - n) %>%
    abs %>% which.min(.) %>% .[1] %>%
    x.rank[.]
  x.min.cutoff <- x[x.rank == x.min.rank.closest.to.n]
  x[x <= x.min.cutoff] <- x.min.cutoff
  x[x >= x.max.cutoff] <- x.max.cutoff

  return(x)

}



DT.us.union <- readRDS("../../Data5/_DataFinal_/11-us_union_coverage.rds")
DT.cross.us <- readRDS("../../Data5/_DataFinal_/10-us_data_cross_section.rds") %>%
  setDT %>%
  .[start.year == 1990] %>%
  .[city_id %chin% c(common.support.city.ids)] %>%
  merge(DT.us.union, by = c("city_id", "start.year"))



DT.ca.union <- readRDS("../../Data5/_DataFinal_/11-ca_union_coverage.rds")
DT.cross.ca <- readRDS("../../Data5/_DataFinal_/10-ca_data_cross_section.rds") %>%
  setDT %>%
  .[start.year == 1991] %>%
  .[city_id %chin% c(common.support.city.ids)] %>%
  merge(DT.ca.union, by = c("city_id", "start.year"))


DT.cross.all <- rbind(DT.cross.us, DT.cross.ca, fill = TRUE) %>%
  ##add in the US dummy
  .[, US.dummy := as.integer(country == "US")] %>%
  .[city_id %chin% c(common.support.city.ids)] %>%
  .[, CA.dummy := as.integer(country == "CA")] %>%
  .[, w.weight := pop1990 / sum(pop1990) * .N] %>%
  .[, start_fb_pop2459 := foreign.born / start_pop2459] %>%
  .[, start_cd_pop2459 := start_college_labor_force / start_pop2459]


mod1 <- glm(US.dummy ~ start_fb_pop2459 + start_cd_pop2459,
            family = binomial(link = "probit"), data = DT.cross.all,
            weights = w.weight,
            control = list(maxit = 250))

##See https://stats.stackexchange.com/a/315644
## design <- svydesign(ids=~1, weights=~w.weight, data = DT.cross.all)
## mod1 <- svyglm(US.dummy ~ start_log_cd_hs_labor + start_log_fb_nb +
##               start_census_manufac_share + start_cbp_emp_oil_share_msa,
##             family = binomial(link = "probit"), design = design,
##             weights = w.weight)

mod2 <- glm(US.dummy ~ start_fb_pop2459 + start_cd_pop2459 +
              start_census_manufac_share + start_cbp_emp_oil_share_msa +
              start_minwage,
              ## Union_Coverage + unemp.insurance_unemployed,
            family = binomial(link = "probit"), data = DT.cross.all,
            weights = w.weight,
            control = list(maxit = 250))

## design <- svydesign(ids=~1, weights=~w.weight, data = DT.cross.all)
## mod2 <- svyglm(US.dummy ~ start_log_cd_hs_labor + start_log_fb_nb +
##               start_census_manufac_share + start_cbp_emp_oil_share_msa +
##               start_minwage + Union_Coverage,
##             family = binomial(link = "probit"), design = design,
##             weights = w.weight)


mod2.odds <- DT.cross.all[!is.na(start_cbp_emp_oil_share_msa), .(city_id)] %>%
  ##.[, log.odds.ratio2 := mod2$fitted.values] %>%
  .[, p2 := mod2$fitted.values] %>%
  .[, odds.ratio2 := (p2) / (1 - p2)]


## mod.probit <- glm(US.dummy ~ start_log_pop2459 + start_census_emp_pop +
##                     start_log_wkwage + start_college_pop_share +
##                     start_log_90_10_wkwage + start_w + start_w_xb +
##                     start_p,
##                   family = binomial(link = "probit"), data = data.all,
##                   control = list(maxit = 100))

## mod.logit <- glm(US.dummy ~ start_log_pop2459 + start_census_emp_pop +
##                     start_college_pop_share +
##                     start_log_90_10_wkwage + start_w +
##                     start_p + start_census_manufac_share,
##                   family = binomial(link = "logit"), data = data.all,
##                   control = list(maxit = 100))

## star.out <- stargazer(mod.logit, type = "text", out = "10-logit_model_dfl_step1.txt")

##log odds
##see https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/

DT.odds <- data.table(city_id = DT.cross.all[["city_id"]],
                      p1 = mod1$fitted.values) %>%
  .[, odds.ratio1 := (p1) / (1 - p1)] %>%
  merge(mod2.odds, by = "city_id") %>%
  .[, country := ifelse(grepl("^CA", city_id), 1L, 0L)] %>%
  ## Winzorize the odds ratios
  .[, `:=`(odds.ratio1 = f_winsorize(odds.ratio1, 5),
           odds.ratio2 = f_winsorize(odds.ratio2, 5)), by = country] %>%
  .[, country := NULL]

if (grepl("^CA.dummy", as.character(mod2$call[2]))) {

  ##Make the US more like canada, set the odds ratio to 1 for CA
  DT.odds <- DT.odds[grepl("^CA", city_id),
                     `:=`(odds.ratio1 = 1, odds.ratio2 = 1)]
} else {

  ##Make the CA more like the US, set the odds ratio to 1 for the US
  DT.odds <- DT.odds[grepl("^US", city_id),
                     `:=`(odds.ratio1 = 1, odds.ratio2 = 1)]
}



saveRDS(DT.odds, "RdsFiles/10-dfl_step1_odds_ratio.rds")



DT.odds.ca <- DT.odds[grepl("^CA", city_id)]

## stargazer

star.rhs.vars <- list(
  c("start_cd_hs_labor", "University/HS Labor", ""),
  c("start_fb_nb", "Foreign/Native", ""),
  c("start_census_manufac_share", "Manufacturing", "Share"),
  c("start_cbp_emp_oil_share_msa", "Oil Share", ""),
  c("start_minwage", "Minimum Wage", ""),
  c("Union_Coverage", "Union Coverage", ""),
  c("NonEnglish", "Non-English", ""),
  c("start_fb_pop2459", "Foreign Born/Pop", ""),
  c("start_cd_pop2459", "University/Pop", "")
)

star.rhs.vars <- do.call("rbind", star.rhs.vars) %>%
  as.data.frame(., stringsAsFactors = FALSE)
star.rhs.vars[] <- lapply(star.rhs.vars, function(x) unlist(x))
star.rhs.vars <- setNames(star.rhs.vars, c("var", "line1", "line2"))
star.rhs.vars$pattern <- gsub("\\_", "\\\\\\\\_", star.rhs.vars$var)


star.out <- stargazer(mod1, mod2,
                      title = "Probit Models Predicting a U.S. Indicator -- US and Canada",
                      label = "tab:dfl_step1") %>%
  star_rhs_names(pattern = star.rhs.vars$pattern,
                 line1 = star.rhs.vars$line1,
                 line2 = star.rhs.vars$line2) %>%
  gsub("US.dummy", "U.S. Dummy", x = .) %>%
  star_notes_tex(note.type = "caption", note = "Probit estimates where the LHS variable is a U.S. Dummy.")


star_tex_write(star.out, file = "TexFiles/10-dfl_probits_step1.tex",
               headers = headers.tex)


##Get the shortnames
DT.ca.names <- readRDS("../../Data5/_DataFinal_/10-ca_data_cross_section.rds") %>%
  .[!duplicated(city_id), .(city_id, shortname)]


DT.odds.ca <- merge(DT.odds.ca, DT.ca.names, by = "city_id", all.x = TRUE) %>%
  .[, p1 := sprintf("%.02f", p1)] %>%
  .[, p2 := sprintf("%.02f", p2)]

DT.odds1 <- DT.odds.ca[order(p1), .(Metro = shortname,  Prob = p1)]
DT.odds2 <- DT.odds.ca[order(p2), .(Metro = shortname,  Prob = p2)]

f_star_dfl_prob <- function(DT.odds.temp) {

  DT.odds.top10 <- DT.odds.temp[1:10]
  DT.odds.bottom10 <- DT.odds.temp[(.N - 9):.N]
  DT.odds1.tex <- cbind(DT.odds.top10, DT.odds.bottom10) %>%
    as.matrix %>%
    stargazer(label = "tab:dfl_prob",
              title = "Pridicted Probabilities for Select Metros") %>%
    gsub("Metro.1", "Metro", x = .) %>%
    gsub("Prob.1", "Prob", x = .) %>%
    gsub("Metro ", "\\\\multicolumn{1}{c}{Metro} ", x = .) %>%
    gsub("cccc", "lclc", x = .)

  return(DT.odds1.tex)

}

star.prob1 <- f_star_dfl_prob(DT.odds1)
star.prob2 <- f_star_dfl_prob(DT.odds2)

star.prob.panel <- star_panel(
  star.prob1, star.prob2,
  panel.names = c("Using population demographics",
                  "Using population, industry, and institutions"),
  panel.label.fontface = "italic",
  reg = FALSE
) %>%
  star_add_column_numbers(insert.after = 10, skip.col.1 = FALSE, multicol = "c")

star.string <- "\\multicolumn{2}{c}{Bottom 10} & \\multicolumn{2}{c}{Top 10} \\\\
\\cline{1-2} \\cline{3-4} \\\\[-1.8ex]"
star.prob.panel <- star_insert_row(star.prob.panel, string = star.string, insert.after = 9)
star.prob.panel <- star_notes_tex(star.prob.panel, note.type = "caption",
                                  note = "Predicted probabilities of a U.S. Dummy from probit specifications. See table \\ref{tab:dfl_step1} for model specifications.")

star_tex_write(star.prob.panel, file = "TexFiles/10-metro_probs.tex",
               headers = headers.tex)

##The panel labels for future tables
dfl.label.0 <- "Common City Sizes (Prime Age from 24 thousand to 2 million)"
dfl.label.1 <- "Common City Sizes, Reweighted using demographic characteristics"
dfl.label.2 <- "Common City Sizes, Reweighted using demographics, industries, and institutions"

saveRDS(c(dfl.label.0, dfl.label.1, dfl.label.2),
        "RdsFiles/10-dfl_panel_labels.rds")

