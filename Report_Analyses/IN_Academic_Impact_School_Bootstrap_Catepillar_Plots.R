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
require(abind)
require(ggplot2)

source(file.path(universal.content.path, "Learning_Loss_Analysis", "Functions", "bootArray.R"))

###   General Setup (assumes running from ./Documentation directory)

if (!exists("Report_Analyses")) {
  stop("Script assumes you have loaded or created a 'Report_Analyses' object.")
}
if (!exists("school.aggregates")) {
  school.aggregates <- Report_Analyses[["Summary_Tables"]][[assessment]][["SCHOOL_NUMBER"]][["Academic_Impact"]]
  if (is.null(school.aggregates)) stop("A 'school.aggregates' object must be loaded or available in 'Report_Analyses$Summary_Tables[[assessment]]$SCHOOL_NUMBER$Academic_Impact'")
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

ss.var <- "SCALE_SCORE_STANDARDIZED"
sgp.var <- "SGP_BASELINE"
bootsrap.n <- 500
ci.cover <- 0.5 # 0.68 # 0.95  ##  hdi.cred.mass

subgroup.min.size <- 20 # params[["min.size.school"]]
test.abv <- params[["test.abv"]][[assessment]]


ges_boot <- function(data, indices, var, yr1, yr2, key_by) {
  tmp_ges_tbl <- data[indices,
    as.list(gammaEffectSizeLong(.SD, var, yr1, yr2, quantiles=0.5, digits = 5)),
    keyby=key_by]
  tmp_n_tbl <- data[indices, .(N=sum(!is.na(get(var)))), keyby=c("YEAR", key_by)]
  tmp_n_tbl <- dcast(data = tmp_n_tbl[YEAR %in% c(yr1, yr2)],
                     formula = as.formula(paste(paste(key_by, collapse = " + "), "YEAR", sep = " ~ ")),
                     value.var = "N")[, YEAR := yr2]
  setnames(tmp_n_tbl, c(yr1, yr2), paste0("COUNT_", c("PRIOR", "CURRENT")))
  setkeyv(tmp_n_tbl, key_by)
  tmp_n_tbl[tmp_ges_tbl]
}

vars.to.get <- c("YEAR", "CONTENT_AREA", "SCHOOL_NUMBER", ss.var, sgp.var)
long_data <- Report_Data[[assessment]][YEAR %in% c(prior.year, current.year), ..vars.to.get]
long_data[, TMP_STRATA := factor(paste(SCHOOL_NUMBER, CONTENT_AREA, YEAR, sep="--"))]

###   Calculate Bootstrap Gamma Effect Size Estimates
boot_ges_sgp <- bootArray(data = long_data, statistic = ges_boot, var = sgp.var,
                          yr1 = prior.year, yr2 = current.year, key_by = c("CONTENT_AREA", "SCHOOL_NUMBER"),
                          R = bootsrap.n, strata = long_data$TMP_STRATA, parallel = "multicore", ncpus = 15)

boot_ges_sss <- bootArray(data = long_data, statistic = ges_boot, var = ss.var,
                          yr1 = prior.year, yr2 = current.year, key_by = c("CONTENT_AREA", "SCHOOL_NUMBER"),
                          R = bootsrap.n, strata = long_data$TMP_STRATA, parallel = "multicore", ncpus = 15)


###   Create catepillar plot data

##    Baseline SGPs
t_star <- list()
# for (r in seq_len(boot_ges_sgp$R)) t_star[[r]] <- boot_ges_sgp$t[[r]][, c(grep("Q_", names(boot_ges_sgp$t0))), with=FALSE]
for (r in seq_len(boot_ges_sgp$R)) {
  t_star[[r]] <- copy(boot_ges_sgp$t[[r]])#[, c(grep("Q_", names(boot_ges_sgp$t0))), with=FALSE]
  setnames(t_star[[r]], "Q_50", "GES_MEDIAN_SGP")
  setkey(t_star[[r]], SCHOOL_NUMBER, CONTENT_AREA, YEAR)
  setkey(school.aggregates, SCHOOL_NUMBER, CONTENT_AREA, YEAR)
  t_star[[r]] <- school.aggregates[, .(SCHOOL_NUMBER, CONTENT_AREA, YEAR, PRIOR_MSGP_CENTERED_2YEAR)][t_star[[r]]]
  t_star[[r]][is.na(PRIOR_MSGP_CENTERED_2YEAR), PRIOR_MSGP_CENTERED_2YEAR := 0] # not an issue for IN SGPs, but is for prior ss...
  t_star[[r]][, GES_MEDIAN_SGP_ADJUSTED := as.numeric(NA)]
  for (CA in content.areas) {
    tmp.coef <- Report_Analyses[["RTM_Models"]][[assessment]][["SCHOOL_NUMBER"]][["GES_MSGP"]][[CA]]
    t_star[[r]][CONTENT_AREA == CA, GES_MEDIAN_SGP_ADJUSTED := GES_MEDIAN_SGP - (PRIOR_MSGP_CENTERED_2YEAR*tmp.coef[["PRIOR_MSGP_CENTERED_2YEAR"]])]
  }
  t_star[[r]] <- t_star[[r]][, c(grep("GES_", names(t_star[[r]]))), with=FALSE]
  setnames(t_star[[r]], gsub("GES_MEDIAN_SGP", "BOOT_ESTIMATE", names(t_star[[r]])))
}
t_star <- do.call(abind, c(t_star, along = 3))

##    Explore the distribution of random samples of bootstrap estimates
##    Show several options for CI. Favorite flavor? BCa... Don't doubt Efron
##    Run this diagnostic multiple times to see variation in distributions
par(mfrow = c(4, 4))
i <- 2 # 1 for unadjusted, 2 for adjusted
randsamp <- sample(1:nrow(t_star), 16)
jsel <- c()
for (j in randsamp) {
  while(all(is.na(t_star[j, i, ]))) {j <- j+1}
  plot(density(t_star[j, i, ], na.rm=TRUE), main="")
  abline(v=median(t_star[j, i, ]), col="green")
  abline(v=BCa(t_star[j, i, ], conf.level = ci.cover), col="red")
  abline(v=HDInterval::hdi(t_star[j, i, ], credMass = ci.cover), col="blue")
  abline(v=quantile(t_star[j, i, ], probs = c((1-ci.cover)/2, 1-(1-ci.cover)/2), na.rm=TRUE), col="orange")
  jsel <- c(jsel, j)
}

for (j in jsel) {
  qqnorm(t_star[j, i, ], main="", xlab="", ylab="")
  qqline(t_star[j, i, ])
}


boot_est <- apply(t_star, c(1, 2), median, na.rm = TRUE)
boot_bca <- t(apply(t_star, c(1, 2), BCa, conf.level = ci.cover)[, ,1])
boot_bca_adj <- t(apply(t_star, c(1, 2), BCa, conf.level = ci.cover)[, ,2])
colnames(boot_bca) <- c("lower", "upper")
colnames(boot_bca_adj) <- c("lower_ADJUSTED", "upper_ADJUSTED")
# boot_hdi <- t(apply(t_star, c(1, 2), HDInterval::hdi, credMass = hdi.cred.mass)[, ,1])
# boot_hdi_adj <- t(apply(t_star, c(1, 2), HDInterval::hdi, credMass = hdi.cred.mass)[, ,2])
# colnames(boot_hdi_adj) <- paste0(colnames(boot_hdi_adj), "_ADJUSTED")
# boot_lwr <- apply(t_star, c(1, 2), quantile, probs = 0.165, names=FALSE, digits = 5, na.rm = TRUE) # probs = 0.025
# boot_upr <- apply(t_star, c(1, 2), quantile, probs = 0.835, names=FALSE, digits = 5, na.rm = TRUE) # probs = 0.975
setkey(boot_ges_sgp$t0, SCHOOL_NUMBER, CONTENT_AREA)

setkey(boot_ges_sgp$t0, SCHOOL_NUMBER, CONTENT_AREA, YEAR)
setkey(school.aggregates, SCHOOL_NUMBER, CONTENT_AREA, YEAR)
boot_ges_sgp$t0 <- school.aggregates[, .(SCHOOL_NUMBER, CONTENT_AREA, YEAR, PRIOR_MSGP_CENTERED_2YEAR)][boot_ges_sgp$t0]
boot_ges_sgp$t0[is.na(PRIOR_MSGP_CENTERED_2YEAR), PRIOR_MSGP_CENTERED_2YEAR := 0] # ~300 schools with G.E.S. that don't get adjustments
boot_ges_sgp$t0[, Q_50_ADJUSTED := as.numeric(NA)]
for (CA in content.areas) {
  tmp.coef <- Report_Analyses[["RTM_Models"]][[assessment]][["SCHOOL_NUMBER"]][["GES_MSGP"]][[CA]]
  boot_ges_sgp$t0[CONTENT_AREA == CA, Q_50_ADJUSTED := Q_50 - (PRIOR_MSGP_CENTERED_2YEAR*tmp.coef[["PRIOR_MSGP_CENTERED_2YEAR"]])]
}
setkey(boot_ges_sgp$t0, SCHOOL_NUMBER, CONTENT_AREA, YEAR)

ges_sgp_plotdata <- data.table(boot_ges_sgp$t0[, .(CONTENT_AREA, SCHOOL_NUMBER, COUNT_PRIOR, COUNT_CURRENT, Q_50, Q_50_ADJUSTED)],
                               boot_est, boot_bca, boot_bca_adj)
                              # lower = boot_lwr[, "Q_50"], upper = boot_upr[, "Q_50"])

#     Exclude Schools (Corporations) with fewer than 50 students with valid scores/SGPs
ges_sgp_plotdata <- ges_sgp_plotdata[COUNT_PRIOR >= subgroup.min.size & COUNT_CURRENT >= subgroup.min.size]

# cor(ges_sgp_plotdata[, Q_50, BOOT_ESTIMATE], method="spearman", use="complete.obs")
# cor(ges_sgp_plotdata[, Q_50, BOOT_ESTIMATE], method="kendall", use="complete.obs")
# summary(ges_sgp_plotdata[, Q_50 - BOOT_ESTIMATE])

##    Scale Scores
t_star <- list()
for (r in seq_len(boot_ges_sss$R)) {
  t_star[[r]] <- copy(boot_ges_sss$t[[r]])#[, c(grep("Q_", names(boot_ges_sss$t0))), with=FALSE]
  setnames(t_star[[r]], "Q_50", "GES_MEDIAN_SSS")
  setkey(t_star[[r]], SCHOOL_NUMBER, CONTENT_AREA, YEAR)
  setkey(school.aggregates, SCHOOL_NUMBER, CONTENT_AREA, YEAR)
  t_star[[r]] <- school.aggregates[, .(SCHOOL_NUMBER, CONTENT_AREA, YEAR, PRIOR_MSSS_CENTERED_2YEAR)][t_star[[r]]]
  t_star[[r]][is.na(PRIOR_MSSS_CENTERED_2YEAR), PRIOR_MSSS_CENTERED_2YEAR := 0] # ~300 schools with G.E.S. that don't get adjustments
  t_star[[r]][, GES_MEDIAN_SSS_ADJUSTED := as.numeric(NA)]
  for (CA in content.areas) {
    tmp.coef <- Report_Analyses[["RTM_Models"]][[assessment]][["SCHOOL_NUMBER"]][["GES_MSSS"]][[CA]]
    t_star[[r]][CONTENT_AREA == CA, GES_MEDIAN_SSS_ADJUSTED := GES_MEDIAN_SSS - (PRIOR_MSSS_CENTERED_2YEAR*tmp.coef[["PRIOR_MSSS_CENTERED_2YEAR"]])]
  }
  setkey(t_star[[r]], SCHOOL_NUMBER, CONTENT_AREA, YEAR)
  t_star[[r]] <- t_star[[r]][, c(grep("GES_", names(t_star[[r]]))), with=FALSE]
  setnames(t_star[[r]], gsub("GES_MEDIAN_SSS", "BOOT_ESTIMATE", names(t_star[[r]])))
}
t_star <- do.call(abind, c(t_star, along = 3))

##    Explore the distribution of random samples of bootstrap estimates
##    Show several options for CI. Favorite flavor? BCa... Don't doubt Efron
##    Run this diagnostic multiple times to see variation in distributions
par(mfrow = c(4, 4))
i <- 2 # 1 for unadjusted, 2 for adjusted
randsamp <- sample(1:nrow(t_star), 16)
jsel <- c()
for (j in randsamp) {
  while(all(is.na(t_star[j, i, ]))) {j <- j+1}
  while(any(abs(t_star[j, i, ])==Inf)) {j <- j+1}
  plot(density(t_star[j, i, ], na.rm=TRUE), main="")
  abline(v=mean(t_star[j, i, ]), col="darkgreen")
  abline(v=median(t_star[j, i, ]), col="green")
  abline(v=BCa(t_star[j, i, ], conf.level = ci.cover), col="red")
  abline(v=HDInterval::hdi(t_star[j, i, ], credMass = ci.cover), col="blue")
  abline(v=quantile(t_star[j, i, ], probs = c((1-ci.cover)/2, 1-(1-ci.cover)/2), na.rm=TRUE), col="orange")
  jsel <- c(jsel, j)
}

for (j in jsel) {
  qqnorm(t_star[j, i, ], main="", xlab="", ylab="")
  qqline(t_star[j, i, ])
}

boot_est <- apply(t_star, c(1, 2), median, na.rm = TRUE)
boot_bca <- t(apply(t_star, c(1, 2), BCa, conf.level = ci.cover)[, ,1])
boot_bca_adj <- t(apply(t_star, c(1, 2), BCa, conf.level = ci.cover)[, ,2])
colnames(boot_bca) <- c("lower", "upper")
colnames(boot_bca_adj) <- c("lower_ADJUSTED", "upper_ADJUSTED")
# boot_hdi <- t(apply(t_star, c(1, 2), HDInterval::hdi, credMass = hdi.cred.mass)[, ,1])
# boot_hdi_adj <- t(apply(t_star, c(1, 2), HDInterval::hdi, credMass = hdi.cred.mass)[, ,2])
# colnames(boot_hdi_adj) <- paste0(colnames(boot_hdi_adj), "_ADJUSTED")
# boot_lwr <- apply(t_star, c(1, 2), quantile, probs = 0.165, names=FALSE, digits = 5, na.rm = TRUE) # probs = 0.025
# boot_upr <- apply(t_star, c(1, 2), quantile, probs = 0.835, names=FALSE, digits = 5, na.rm = TRUE) # probs = 0.975

setkey(boot_ges_sss$t0, SCHOOL_NUMBER, CONTENT_AREA, YEAR)
setkey(school.aggregates, SCHOOL_NUMBER, CONTENT_AREA, YEAR)
boot_ges_sss$t0 <- school.aggregates[, .(SCHOOL_NUMBER, CONTENT_AREA, YEAR, PRIOR_MSSS_CENTERED_2YEAR)][boot_ges_sss$t0]
boot_ges_sss$t0[is.na(PRIOR_MSSS_CENTERED_2YEAR), PRIOR_MSSS_CENTERED_2YEAR := 0] # ~300 schools with G.E.S. that don't get adjustments
boot_ges_sss$t0[, Q_50_ADJUSTED := as.numeric(NA)]
for (CA in content.areas) {
  tmp.coef <- Report_Analyses[["RTM_Models"]][[assessment]][["SCHOOL_NUMBER"]][["GES_MSSS"]][[CA]]
  boot_ges_sss$t0[CONTENT_AREA == CA, Q_50_ADJUSTED := Q_50 - (PRIOR_MSSS_CENTERED_2YEAR*tmp.coef[["PRIOR_MSSS_CENTERED_2YEAR"]])]
}
setkey(boot_ges_sss$t0, SCHOOL_NUMBER, CONTENT_AREA, YEAR)

ges_sss_plotdata <- data.table(boot_ges_sss$t0[, .(CONTENT_AREA, SCHOOL_NUMBER, COUNT_PRIOR, COUNT_CURRENT, Q_50, Q_50_ADJUSTED)],
                               boot_est, boot_bca, boot_bca_adj)

##    Exclude Schools (Corporations) with fewer than 50 students with valid scores/SGPs
ges_sss_plotdata <- ges_sss_plotdata[COUNT_PRIOR >= subgroup.min.size & COUNT_CURRENT >= subgroup.min.size]

# cor(ges_sss_plotdata[, Q_50, BOOT_ESTIMATE], method="spearman", use="complete.obs")
# cor(ges_sss_plotdata[, Q_50, BOOT_ESTIMATE], method="kendall", use="complete.obs")
# summary(ges_sss_plotdata[, Q_50 - BOOT_ESTIMATE])


#####
###   School Catepillar Plots
#####

plot.dir <- file.path("assets", "Rplots", "Catepillar", assessment)
if (!dir.exists(file.path(plot.dir, "School_Level"))) dir.create(file.path(plot.dir, "School_Level"), recursive=TRUE)

for (ges_type in c("Status", "Adjusted_Status", "Growth", "Adjusted_Growth")) {
  for (CA in content.areas) {
    tmp.subj <- SGP::capwords(CA)
    title.main <- paste0(test.abv, " ", gsub("ematics", "", tmp.subj), ":") # ILEARN Math:

    sch.cat.dat <- switch(ges_type,
      Status = ges_sss_plotdata[!is.na(Q_50) & CONTENT_AREA == CA,
        .(SCHOOL_NUMBER, Q_50, BOOT_ESTIMATE, lower, upper, COUNT_CURRENT)],
      Adjusted_Status = ges_sss_plotdata[!is.na(Q_50_ADJUSTED) & CONTENT_AREA == CA,
        .(SCHOOL_NUMBER, Q_50_ADJUSTED, BOOT_ESTIMATE_ADJUSTED, lower_ADJUSTED, upper_ADJUSTED, COUNT_CURRENT)],
      Growth = ges_sgp_plotdata[!is.na(Q_50) & CONTENT_AREA == CA,
        .(SCHOOL_NUMBER, Q_50, BOOT_ESTIMATE, lower, upper, COUNT_CURRENT)],
      Adjusted_Growth = ges_sgp_plotdata[!is.na(Q_50_ADJUSTED) & CONTENT_AREA == CA,
        .(SCHOOL_NUMBER, Q_50_ADJUSTED, BOOT_ESTIMATE_ADJUSTED, lower_ADJUSTED, upper_ADJUSTED, COUNT_CURRENT)]
    )

    setnames(sch.cat.dat, gsub("_ADJUSTED", "", names(sch.cat.dat)))

    ##    Create COVID Impact Levels for G.E.S. for 2021 - 2019 Median SGP differences
    sch.cat.dat[, COVID_ACADEMIC_IMPACT := fcase(
                  Q_50 >= 0.2, "Improvement",
                  Q_50 <  0.2 & Q_50 >= -0.2, "Modest to None",
                  Q_50 < -0.2 & Q_50 >= -0.5, "Moderate",
                  Q_50 < -0.5 & Q_50 >= -0.8, "Large",
                  Q_50 < -0.8, "Severe")]

    sch.cat.dat[, COVID_ACADEMIC_IMPACT := factor(COVID_ACADEMIC_IMPACT,
                    levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

    setkey(sch.cat.dat, BOOT_ESTIMATE)
    sch.cat.dat[, SCHOOL_RANK := seq(nrow(sch.cat.dat))]

    cp <- ggplot(sch.cat.dat, aes(x=SCHOOL_RANK, y=Q_50, fill=COVID_ACADEMIC_IMPACT)) +
          # geom_point(aes(col=COVID_ACADEMIC_IMPACT), size=1.5) +
          ylab("Gamma Effect Size (Median)") + xlab("Estimated School Impact Rank") + # Flipped!
          geom_errorbar(aes(col=COVID_ACADEMIC_IMPACT, ymin=lower, ymax=upper), width=0, size=0.35) +
          geom_point(size=0.25, show.legend=FALSE) +
          geom_line(sch.cat.dat, mapping = aes(x=SCHOOL_RANK, y=BOOT_ESTIMATE, fill=NULL), col="green", size=0.35, show.legend=FALSE) +
          geom_hline(yintercept=0.2, col="white") + geom_hline(yintercept=-0.2, col="white") +
          geom_hline(yintercept=-0.5, col="white") + geom_hline(yintercept=-0.8, col="white") + geom_hline(yintercept= 0, col="red") +
          scale_y_continuous(limits = c(-1.25, 1.025), oob = scales::oob_keep) +
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

###   Save/Update `Report_Analyses` with bootstrap results (save full boot data seperately)
# Report_Analyses[["Summary_Tables"]][[assessment]][["SCHOOL_NUMBER"]][["Bootstrap_GES"]][["MSGP"]] <- ges_sgp_plotdata
# Report_Analyses[["Summary_Tables"]][[assessment]][["SCHOOL_NUMBER"]][["Bootstrap_GES"]][["MSSS"]] <- ges_sss_plotdata

save(boot_ges_sgp, file="../Data/boot_ges_sgp.rda")
save(boot_ges_sss, file="../Data/boot_ges_sss.rda")
