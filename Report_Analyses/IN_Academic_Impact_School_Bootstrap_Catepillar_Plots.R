################################################################################
###                                                                          ###
###           Investigation of Pandemic Academic Impact by School:           ###
###   Calculate Bootstrap Gamma Effect Sizes and produce Catepillar Plots    ###
###                                                                          ###
################################################################################

###   Locate the "Universal_Content" directory (assume woring directory is ./Documentation)
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

###   Load required packages and custom functions
require(cfaTools)
require(data.table)
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Functions", "bootArray.R"))

###   General Setup (assumes running from ./Documentation directory)

if (!exists("Report_Analyses")) {
  stop("Script assumes you have loaded or created a 'Report_Analyses' object.")
}

###   Load formated Report_Data
if (!exists("Report_Data")) stop("Script assumes you have loaded a 'Report_Data' object.")

###   Declare an assessment flavor --  need to extend to WIDA
if (!exists("assessment")) assessment <- "State_Assessment"

###   Load existing params or create by sourcing appropriate scripts
if (!exists("params")) {
  ###   Load required packages and custom functions
  require(SGP)
  require(Literasee)
  if (exists("assessment")) {tmp.assessment <- assessment; chg.assess <- TRUE} else chg.assess <- FALSE
  setwd("..")
  source("./Documentation/4_Make_Configs.R")
  setwd("./Documentation")
  params <- report.config$params
  source(knitr::purl(file.path(universal.content.path, "Learning_Loss_Analysis", "Child_RMD", "params.Rmd"), quiet=TRUE))
  file.remove("params.R")
  if (chg.assess) tmp.assessment -> assessment
}

current.year <- tail(params[["years"]][[assessment]], 1)
prior.year   <- tail(params[["years"]][[assessment]], 2)[-2]

content.areas <- params[["GL_subjects"]][[assessment]]

subgroup.min.size <- params[["min.size.school"]] # 15
test.abv <- params[["test.abv"]][[assessment]]


ges_boot <- function(data, indices, var, yr1, yr2, key_by) {
  tmp_ges_tbl <- data[indices,
    as.list(gammaEffectSizeLong(.SD, var, yr1, yr2, quantiles=0.5, digits = 5)),
    keyby=key_by]
  tmp_n_tbl <- data[indices, .(N=sum(!is.na(get(var)))), keyby=c("YEAR", key_by)]
  tmp_n_tbl <- dcast(tmp_n_tbl[YEAR %in% c(yr1, yr2)],
                     as.formula(paste(paste(key_by, collapse = " + "), "YEAR", sep = " ~ ")), value.var="N")
  setnames(tmp_n_tbl, c(yr1, yr2), paste0("COUNT_", c(yr1, yr2)))
  setkeyv(tmp_n_tbl, key_by)
  tmp_n_tbl[tmp_ges_tbl]
}

long_data <- Report_Data[[assessment]][YEAR %in% c(prior.year, current.year),
                .(YEAR, CONTENT_AREA, SCHOOL_NUMBER, SCALE_SCORE_STANDARDIZED, SGP_BASELINE)]
long_data[, TMP_STRATA := factor(paste(SCHOOL_NUMBER, CONTENT_AREA, YEAR, sep="--"))]

boot_ges_sss <- bootArray(long_data, ges_boot, var="SCALE_SCORE_STANDARDIZED", yr1=prior.year, yr2=current.year, key_by = c("CONTENT_AREA", "SCHOOL_NUMBER"),
                          R=200, strata = long_data$TMP_STRATA, parallel = "multicore", ncpus=15)

boot_ges_sgp <- bootArray(long_data, ges_boot, var="SGP_BASELINE", yr1=prior.year, yr2=current.year, key_by = c("CONTENT_AREA", "SCHOOL_NUMBER"),
                          R=200, strata = long_data$TMP_STRATA, parallel = "multicore", ncpus=15)

###   Create catepillar plot data
require(abind)

##    Baseline SGPs
t_star <- list()
for (r in seq_len(boot_ges_sgp$R)) t_star[[r]] <- boot_ges_sgp$t[[r]][, c(grep("Q_", names(boot_ges_sgp$t0))), with=FALSE]
t_star <- do.call(abind, c(t_star, along = 3))

boot_est <- apply(t_star, c(1, 2), median, na.rm = TRUE)
# boot_hdi <- t(apply(t_star, c(1, 2), HDInterval::hdi, credMass = 0.95)[, ,])
boot_hdi <- t(apply(t_star, c(1, 2), HDInterval::hdi, credMass = 0.67)[, ,])

# boot_lwr <- apply(t_star, c(1, 2), quantile, probs = 0.165, names=FALSE, digits = 5, na.rm = TRUE)
# boot_upr <- apply(t_star, c(1, 2), quantile, probs = 0.835, names=FALSE, digits = 5, na.rm = TRUE)

ges_sgp_plotdata <- data.table(boot_ges_sgp$t0[, .(CONTENT_AREA, SCHOOL_NUMBER, COUNT_2019, COUNT_2021, Q_50)],
                              Boot_Estimate = boot_est[, "Q_50"],  boot_hdi)
                              # CI_L = boot_lwr[, "Q_50"], CI_U = boot_upr[, "Q_50"])

#     Exclude Schools (Corporations) with fewer than 50 students with valid scores/SGPs
ges_sgp_plotdata <- ges_sgp_plotdata[COUNT_2019 >= 20 & COUNT_2021 >= 20]

# cor(ges_sgp_plotdata[, Q_50, Boot_Estimate], method="spearman", use="complete.obs")
# cor(ges_sgp_plotdata[, Q_50, Boot_Estimate], method="kendall", use="complete.obs")
# summary(ges_sgp_plotdata[, Q_50 - Boot_Estimate])

##    Scale Scores
t_star <- list()
for (r in seq_len(boot_ges_sss$R)) t_star[[r]] <- boot_ges_sss$t[[r]][, c(grep("Q_", names(boot_ges_sss$t0))), with=FALSE]
t_star <- do.call(abind, c(t_star, along = 3))

boot_est <- apply(t_star, c(1, 2), median, na.rm = TRUE)
# boot_hdi <- t(apply(t_star, c(1, 2), HDInterval::hdi, credMass = 0.95)[, ,])
boot_hdi <- t(apply(t_star, c(1, 2), HDInterval::hdi, credMass = 0.67)[, ,])

# boot_lwr <- apply(t_star, c(1, 2), quantile, probs = 0.025, names=FALSE, digits = 5, na.rm = TRUE)
# boot_upr <- apply(t_star, c(1, 2), quantile, probs = 0.975, names=FALSE, digits = 5, na.rm = TRUE)

ges_sss_plotdata <- data.table(boot_ges_sss$t0[, .(CONTENT_AREA, SCHOOL_NUMBER, COUNT_2019, COUNT_2021, Q_50)],
                              Boot_Estimate = boot_est[, "Q_50"],  boot_hdi)
                              # CI_L = boot_lwr[, "Q_50"], CI_U = boot_upr[, "Q_50"])

##    Exclude Schools (Corporations) with fewer than 50 students with valid scores/SGPs
ges_sss_plotdata <- ges_sss_plotdata[COUNT_2019 >= 20 & COUNT_2021 >= 20]

# cor(ges_sss_plotdata[, Q_50, Boot_Estimate], method="spearman", use="complete.obs")
# cor(ges_sss_plotdata[, Q_50, Boot_Estimate], method="kendall", use="complete.obs")
# summary(ges_sss_plotdata[, Q_50 - Boot_Estimate])


#####
###   School Catepillar Plots
#####

require(ggplot2)

plot.dir <- file.path("assets", "Rplots", "Catepillar", assessment)
if (!dir.exists(file.path(plot.dir, "School_Level"))) dir.create(file.path(plot.dir, "School_Level"), recursive=TRUE)

for (ges_type in c("Status", "Growth")) {
  for (CA in content.areas) {
    tmp.subj <- SGP::capwords(CA)
    title.main <- paste0(test.abv, " ", gsub("ematics", "", tmp.subj), ":") # ILEARN Math:

    sch.cat.dat <- switch(ges_type,
      Status = ges_sss_plotdata[!is.na(Q_50) & CONTENT_AREA == CA, list(SCHOOL_NUMBER, Q_50, Boot_Estimate, lower, upper, COUNT_2021)],
      Growth = ges_sgp_plotdata[!is.na(Q_50) & CONTENT_AREA == CA, list(SCHOOL_NUMBER, Q_50, Boot_Estimate, lower, upper, COUNT_2021)])

    ##    Create COVID Impact Levels for G.E.S. for 2021 - 2019 Median SGP differences
    sch.cat.dat[, COVID_ACADEMIC_IMPACT := fcase(
                  Q_50 >= 0.2, "Improvement",
                  Q_50 <  0.2 & Q_50 >= -0.2, "Modest to None",
                  Q_50 < -0.2 & Q_50 >= -0.5, "Moderate",
                  Q_50 < -0.5 & Q_50 >= -0.8, "Large",
                  Q_50 < -0.8, "Severe")]

    sch.cat.dat[, COVID_ACADEMIC_IMPACT := factor(COVID_ACADEMIC_IMPACT,
                    levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

    setkey(sch.cat.dat, Boot_Estimate)
    sch.cat.dat[, SCHOOL_RANK := seq(nrow(sch.cat.dat))]

    cp <- ggplot(sch.cat.dat, aes(x=SCHOOL_RANK, y=Q_50, fill=COVID_ACADEMIC_IMPACT)) +
          # geom_point(aes(col=COVID_ACADEMIC_IMPACT), size=1.5) +
          ylab("Gamma Effect Size (Median)") + xlab("Estimated School Impact Rank") + # Flipped!
          geom_errorbar(aes(col=COVID_ACADEMIC_IMPACT, ymin=lower, ymax=upper), width=0, size=0.35) +
          geom_point(size=0.25, show.legend=FALSE) +
          geom_line(sch.cat.dat, mapping = aes(x=SCHOOL_RANK, y=Boot_Estimate, fill=NULL), col="green", size=0.35, show.legend=FALSE) +
          geom_hline(yintercept=0.2, col="white") + geom_hline(yintercept=-0.2, col="white") +
          geom_hline(yintercept=-0.5, col="white") + geom_hline(yintercept=-0.8, col="white") + geom_hline(yintercept= 0, col="red") +
          coord_flip() + scale_colour_viridis_d(option = "C") +
          theme(panel.background = element_rect(colour="#515151", fill="#515151"),
                panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.key = element_rect(colour="#515151", fill="grey"),
                legend.title = element_text(size = 10, colour = "black"),
                legend.text = element_text(size = 9, colour = "black"),
                legend.position = "bottom",
                legend.direction = "horizontal",
                plot.title = element_text(size = 14),
                axis.title.x = element_text(size = 12),
                axis.text = element_text(size = 10, colour = "black"),
                axis.title.y = element_text(size = 12),
                strip.text = element_text(size = 10, colour = "black")) +
          labs(title = paste(title.main, "School Level COVID", ges_type, "Impact."), color="COVID Impact: ")

    ggsave(filename = file.path(plot.dir, "School_Level", paste0(gsub("ematics", "", tmp.subj), "_Impact_Catepillar_GES_", ges_type, ".png")),
           plot=cp, width=7, height = 5, dpi=150, units="in", bg="transparent")
  }
}
