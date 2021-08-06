################################################################################
###                                                                          ###
###  School Level Aggregate Summary Table with Measures of Academic Impact   ###
###                                                                          ###
################################################################################

###   General Setup (assumes running from ./Documentation directory)

if (!exists("Report_Analyses")) {
  stop("Script assumes you have loaded or created a 'Report_Analyses' object.")
}

###   Load formated Report_Data
if (!exists("Report_Data")) stop("Script assumes you have loaded a 'Report_Data' object.")

###   Declare an assessment flavor --  need to extend to WIDA
if (!exists("assessment")) assessment <- "State_Assessment"

###   Locate the "Universal_Content" directory (assume woring directory is ./Documentation)
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

###   Load required packages and custom functions
require(SGP)
require(data.table)
require(cfaTools)
require(MASS)

### Utility functions
hdmedian <- function(x, ...) as.numeric(Hmisc::hdquantile(x, probs=0.5, names=FALSE, ...))

##    Use ACHIEVEMENT_ProfandAbove and PRIOR_ACHIEVEMENT_ProfandAbove for all states
percent_proficient <- function(achievement_level) {
  tmp.table <- table(achievement_level)
  100*sum(tmp.table["Proficient"])/sum(tmp.table)
}

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

all.grades <- params[["grades"]][[assessment]]
content.areas <- params[["GL_subjects"]][[assessment]]

### Create School Level Summary Table

school.aggregates <- Report_Data[[assessment]][
                        VALID_CASE=="VALID_CASE" & GRADE %in% all.grades,
                          .(MEAN_SGP_BASELINE=mean(SGP_BASELINE, na.rm=TRUE),
                            MEDIAN_SGP_BASELINE=hdmedian(as.numeric(SGP_BASELINE), na.rm=TRUE),
                            MEAN_SGP=mean(SGP, na.rm=TRUE),
                            MEDIAN_SGP=hdmedian(as.numeric(SGP), na.rm=TRUE),
                            MEAN_SCALE_SCORE_STANDARDIZED = mean(SCALE_SCORE_STANDARDIZED, na.rm=TRUE),
                            MEAN_SCALE_SCORE_PRIOR_STANDARDIZED=mean(SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR, na.rm=TRUE),
                            PERCENT_PROFICIENT=percent_proficient(ACHIEVEMENT_ProfandAbove),
                            PERCENT_PROFICIENT_PRIOR=percent_proficient(PRIOR_ACHIEVEMENT_ProfandAbove),
                            COUNT_SGP=sum(!is.na(SGP_BASELINE))),
                          keyby=c("YEAR", "SCHOOL_NUMBER", "CONTENT_AREA")]

shift.key <- c("SCHOOL_NUMBER", "CONTENT_AREA", "YEAR")
setkeyv(school.aggregates, shift.key)

school.aggregates[, c("MEDIAN_SGP_PRIOR_2YEAR", "MEDIAN_SGP_PRIOR_3YEAR") := shift(MEDIAN_SGP, 2:3), by = list(SCHOOL_NUMBER, CONTENT_AREA)]
school.aggregates[, MEDIAN_SGP_BASELINE_PRIOR := shift(MEDIAN_SGP_BASELINE, 1), by = list(SCHOOL_NUMBER, CONTENT_AREA)] # Only getting this for 2021 (1 year shift = 2 years)

# table(school.aggregates[, YEAR, is.na(MEDIAN_SGP_PRIOR_2YEAR)])
# table(school.aggregates[, YEAR, is.na(MEDIAN_SGP_PRIOR_3YEAR)])
# table(school.aggregates[, YEAR, is.na(MEDIAN_SGP_BASELINE_PRIOR)])

school.aggregates[YEAR == current.year, MEDIAN_SGP_PRIOR_3YEAR := MEDIAN_SGP_PRIOR_2YEAR]
school.aggregates[YEAR == current.year, MEDIAN_SGP_PRIOR_2YEAR := MEDIAN_SGP_BASELINE_PRIOR]

school.aggregates[, PRIOR_MSGP_CENTERED_2YEAR := MEDIAN_SGP_PRIOR_2YEAR - mean(MEDIAN_SGP_PRIOR_2YEAR, na.rm=TRUE), by = list(YEAR, CONTENT_AREA)]
school.aggregates[, PRIOR_MSGP_CENTERED_3YEAR := MEDIAN_SGP_PRIOR_3YEAR - mean(MEDIAN_SGP_PRIOR_3YEAR, na.rm=TRUE), by = list(YEAR, CONTENT_AREA)]
school.aggregates <- school.aggregates[YEAR %in% c(prior.year, current.year)]
school.aggregates[, as.list(summary(PRIOR_MSGP_CENTERED_2YEAR)), keyby = list(YEAR, CONTENT_AREA)]
table(school.aggregates[, is.na(PRIOR_MSGP_CENTERED_2YEAR), is.na(PRIOR_MSGP_CENTERED_3YEAR)], exclude=NULL)

###   Create uncorrected Baseline difference (2021 - 2019)
school.aggregates[, MSGP_BASELINE_DIFFERENCE_UNCORRECTED := MEDIAN_SGP_BASELINE - MEDIAN_SGP_PRIOR_2YEAR]

###   RTM Adjusted MSGP_BASELINE_DIFFERENCE
msgp_rtm_models <- list()
for (CA in content.areas) {
  msgp_rtm_models[[CA]] <- rlm(MSGP_BASELINE_DIFFERENCE_UNCORRECTED ~ 0 + PRIOR_MSGP_CENTERED_2YEAR, data=school.aggregates[YEAR == prior.year & CONTENT_AREA == CA])
}

##    Model diagnostics
# boot.msgp.rtm <- function(data, indices, maxit=20){
#   data <- data[indices, ] # select obs. in bootstrap sample
#   # mod <- lm(MSGP_BASELINE_DIFFERENCE_UNCORRECTED ~ 0 + PRIOR_MSGP_CENTERED_2YEAR, data=data)
#   mod <- rlm(MSGP_BASELINE_DIFFERENCE_UNCORRECTED ~ 0 + PRIOR_MSGP_CENTERED_2YEAR, data=data, maxit=maxit)
#   coef(mod) # return coefficient vector
# }
#
# set.seed(12345) # for reproducibility
# boot_robust_rtm <- boot(data=school.aggregates[YEAR == prior.year & CONTENT_AREA == CA], statistic=boot.msgp.rtm,
#                         R=2000, maxit=200, parallel = "multicore", ncpus = 10)
#
# par(mfrow = c(2, 2))
# for (CA in content.areas) {
# hist(msgp_rtm_models[[CA]]$residuals, breaks=50)
# qqnorm(msgp_rtm_models[[CA]]$residuals);qqline(msgp_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior.year & CONTENT_AREA == CA & !is.na(PRIOR_MSGP_CENTERED_2YEAR), MSGP_BASELINE_DIFFERENCE_UNCORRECTED]), msgp_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior.year & CONTENT_AREA == CA & !is.na(PRIOR_MSGP_CENTERED_2YEAR), MEDIAN_SGP_BASELINE]), msgp_rtm_models[[CA]]$fitted.values)
# }

##    Create Adjusted MSGP_BASELINE_DIFFERENCE by subtracting coefficient for PRIOR_MSGP_CENTERED_2YEAR
school.aggregates[, MSGP_BASELINE_DIFFERENCE_ADJUSTED := as.numeric(NA)]
for (CA in content.areas) {
  school.aggregates[CONTENT_AREA == CA, MSGP_BASELINE_DIFFERENCE_ADJUSTED := MSGP_BASELINE_DIFFERENCE_UNCORRECTED - (PRIOR_MSGP_CENTERED_2YEAR*msgp_rtm_models[[CA]]$coef[["PRIOR_MSGP_CENTERED_2YEAR"]])]
}

##    correlation checks
# cor(school.aggregates[, MSGP_BASELINE_DIFFERENCE_UNCORRECTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == prior.year, MSGP_BASELINE_DIFFERENCE_UNCORRECTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current.year, MSGP_BASELINE_DIFFERENCE_UNCORRECTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[, MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == prior.year, MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current.year, MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current.year & CONTENT_AREA == "ELA", MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current.year & CONTENT_AREA == "MATHEMATICS", MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')

##    Create COVID Impact Levels for MSGP Baseline Differences
school.aggregates[, COVID_ACADEMIC_IMPACT_SGP_DIFF := fcase(
                    MSGP_BASELINE_DIFFERENCE_UNCORRECTED >= 5, "Improvement",
                    MSGP_BASELINE_DIFFERENCE_UNCORRECTED < 5 & MSGP_BASELINE_DIFFERENCE_UNCORRECTED >= -5, "Modest to None",
                    MSGP_BASELINE_DIFFERENCE_UNCORRECTED < -5 & MSGP_BASELINE_DIFFERENCE_UNCORRECTED >= -15, "Moderate",
                    MSGP_BASELINE_DIFFERENCE_UNCORRECTED < -15 & MSGP_BASELINE_DIFFERENCE_UNCORRECTED >= -25, "Large",
                    MSGP_BASELINE_DIFFERENCE_UNCORRECTED < -25, "Severe")]

school.aggregates[, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ := fcase(
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED >= 5, "Improvement",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < 5 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -5, "Modest to None",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < -5 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -15, "Moderate",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < -15 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -25, "Large",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < -25, "Severe")]

# table(school.aggregates[YEAR==prior.year, COVID_ACADEMIC_IMPACT_SGP_DIFF, CONTENT_AREA], exclude=NULL)
# table(school.aggregates[YEAR=='2021', COVID_ACADEMIC_IMPACT_SGP_DIFF, CONTENT_AREA], exclude=NULL)
# table(school.aggregates[YEAR==prior.year, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ, CONTENT_AREA], exclude=NULL)
# table(school.aggregates[YEAR=='2021', COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ, CONTENT_AREA], exclude=NULL)

#####
###   Gamma Effect Size (within School MSGP 2021 - MSGP 2019)
#####

###   Create uncorrected G.E.S.
ges_sgp <- rbindlist(list(
    Report_Data[[assessment]][,
        as.list(gammaEffectSizeLong(.SD, "SGP", SGP:::yearIncrement(prior.year, -2), prior.year, quantiles=c(0.5), digits=2)),
      keyby=c("CONTENT_AREA", "SCHOOL_NUMBER")][, YEAR := prior.year],
    Report_Data[[assessment]][,
        as.list(gammaEffectSizeLong(.SD, "SGP_BASELINE", prior.year, current.year, quantiles=c(0.5), digits=2)),
      keyby=c("CONTENT_AREA", "SCHOOL_NUMBER")][, YEAR := current.year]))

setnames(ges_sgp, "Q_50", "GES_MEDIAN_SGP")
setkey(ges_sgp, SCHOOL_NUMBER, YEAR, CONTENT_AREA)
setkey(school.aggregates, SCHOOL_NUMBER, YEAR, CONTENT_AREA)

##    Merge in GES with other summary statistics
school.aggregates <- ges_sgp[school.aggregates]

###   RTM Adjusted G.E.S.
gessgp_rtm_models <- list()
for (CA in content.areas) {
  gessgp_rtm_models[[CA]] <- rlm(GES_MEDIAN_SGP ~ 0 + PRIOR_MSGP_CENTERED_2YEAR, data=school.aggregates[YEAR == prior.year & CONTENT_AREA == CA])
}

##    Model diagnostics
# par(mfrow = c(2, 2))
# for (CA in content.areas) {
# hist(gessgp_rtm_models[[CA]]$residuals, breaks=50)
# qqnorm(gessgp_rtm_models[[CA]]$residuals);qqline(gessgp_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior.year & !is.na(PRIOR_MSGP_CENTERED_2YEAR) & CONTENT_AREA == CA, GES_MEDIAN_SGP]), gessgp_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior.year & !is.na(PRIOR_MSGP_CENTERED_2YEAR) & CONTENT_AREA == CA, GES_MEDIAN_SGP]), gessgp_rtm_models[[CA]]$fitted.values)
# }

##    Create Adjusted GES_MEDIAN_SGP by subtracting coefficient for PRIOR_MSGP_CENTERED_2YEAR
school.aggregates[, GES_MEDIAN_SGP_ADJUSTED := as.numeric(NA)]
for (CA in content.areas) {
  school.aggregates[CONTENT_AREA == CA, GES_MEDIAN_SGP_ADJUSTED := GES_MEDIAN_SGP - (PRIOR_MSGP_CENTERED_2YEAR*gessgp_rtm_models[[CA]]$coef[["PRIOR_MSGP_CENTERED_2YEAR"]])]
}

##    Visualization, summary and correlation checks
# na.omit(school.aggregates[, as.list(round(summary(GES_MEDIAN_SGP_ADJUSTED),3)), keyby=c("YEAR", "CONTENT_AREA")])
# plot(school.aggregates[YEAR == prior.year, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR])
# plot(school.aggregates[YEAR == prior.year, GES_MEDIAN_SGP, PRIOR_MSGP_CENTERED_2YEAR])
# plot(school.aggregates[YEAR == prior.year, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR])
# plot(school.aggregates[YEAR == prior.year, GES_MEDIAN_SGP_ADJUSTED, PRIOR_MSGP_CENTERED_2YEAR])
# cor(school.aggregates[, GES_MEDIAN_SGP_ADJUSTED, GES_MEDIAN_SGP], use='complete.obs')
# cor(school.aggregates[YEAR == prior.year, GES_MEDIAN_SGP_ADJUSTED, GES_MEDIAN_SGP], use='complete.obs')
# cor(school.aggregates[YEAR == current.year, GES_MEDIAN_SGP_ADJUSTED, GES_MEDIAN_SGP], use='complete.obs')
#
# cor(school.aggregates[, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == prior.year, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current.year, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == prior.year, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current.year, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current.year & CONTENT_AREA == "ELA", GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
# cor(school.aggregates[YEAR == current.year & CONTENT_AREA == "MATHEMATICS", GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
#
# ges_2019_ela_adj <- lm(GES_MEDIAN_SGP_ADJUSTED ~ MEDIAN_SGP_PRIOR_2YEAR, data=school.aggregates[YEAR == prior.year & CONTENT_AREA == "ELA"]); summary(ges_2019_ela_adj)
# ges_2019_math_adj<- lm(GES_MEDIAN_SGP_ADJUSTED ~ MEDIAN_SGP_PRIOR_2YEAR, data=school.aggregates[YEAR == prior.year & CONTENT_AREA == "MATHEMATICS"]); summary(ges_2019_math_adj)

##    Create COVID Impact Levels for G.E.S. for 2021 - 2019 Median SGP differences
school.aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP := fcase(
                    GES_MEDIAN_SGP >= 0.2, "Improvement",
                    GES_MEDIAN_SGP <  0.2 & GES_MEDIAN_SGP >= -0.2, "Modest to None",
                    GES_MEDIAN_SGP < -0.2 & GES_MEDIAN_SGP >= -0.5, "Moderate",
                    GES_MEDIAN_SGP < -0.5 & GES_MEDIAN_SGP >= -0.8, "Large",
                    GES_MEDIAN_SGP < -0.8, "Severe")]

school.aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ := fcase(
                    GES_MEDIAN_SGP_ADJUSTED >= 0.2, "Improvement",
                    GES_MEDIAN_SGP_ADJUSTED <  0.2 & GES_MEDIAN_SGP_ADJUSTED >= -0.2, "Modest to None",
                    GES_MEDIAN_SGP_ADJUSTED < -0.2 & GES_MEDIAN_SGP_ADJUSTED >= -0.5, "Moderate",
                    GES_MEDIAN_SGP_ADJUSTED < -0.5 & GES_MEDIAN_SGP_ADJUSTED >= -0.8, "Large",
                    GES_MEDIAN_SGP_ADJUSTED < -0.8, "Severe")]

# table(school.aggregates[YEAR==prior.year, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP, CONTENT_AREA], exclude=NULL)
# table(school.aggregates[YEAR=='2021', COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP, CONTENT_AREA], exclude=NULL)
# table(school.aggregates[YEAR==prior.year, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ, CONTENT_AREA], exclude=NULL)
# table(school.aggregates[YEAR=='2021', COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ, CONTENT_AREA], exclude=NULL)


#####
###   MEAN_SCALE_SCORE_STANDARDIZED
#####

school.aggregates[, PRIOR_MSSS_CENTERED_2YEAR := MEAN_SCALE_SCORE_PRIOR_STANDARDIZED - mean(MEAN_SCALE_SCORE_PRIOR_STANDARDIZED, na.rm=TRUE), by = list(YEAR, CONTENT_AREA)]
# school.aggregates[, as.list(summary(PRIOR_MSSS_CENTERED_2YEAR)), keyby = list(YEAR, CONTENT_AREA)]

###   Create uncorrected mean scale score difference (2021 - 2019)
school.aggregates[, MSSS_DIFFERENCE_UNCORRECTED := MEAN_SCALE_SCORE_STANDARDIZED - MEAN_SCALE_SCORE_PRIOR_STANDARDIZED]

###   RTM Adjusted MSSS_DIFFERENCE
msss_rtm_models <- list()
for (CA in content.areas) {
  msss_rtm_models[[CA]] <- rlm(MSSS_DIFFERENCE_UNCORRECTED ~ 0 + PRIOR_MSSS_CENTERED_2YEAR, data=school.aggregates[YEAR == prior.year & CONTENT_AREA == CA])
}

##    Model diagnostics
# par(mfrow = c(2, 2))
# for (CA in content.areas) {
# hist(msss_rtm_models[[CA]]$residuals, breaks=50)
# qqnorm(msss_rtm_models[[CA]]$residuals);qqline(msss_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior.year & CONTENT_AREA == CA & !is.na(PRIOR_MSSS_CENTERED_2YEAR), MSSS_DIFFERENCE_UNCORRECTED]), msss_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior.year & CONTENT_AREA == CA & !is.na(PRIOR_MSSS_CENTERED_2YEAR), MEAN_SCALE_SCORE_STANDARDIZED]), msss_rtm_models[[CA]]$fitted.values)
# }

##    Create Adjusted MSSS_DIFFERENCE_ by subtracting coefficient for PRIOR_MSSS_CENTERED_2YEAR
school.aggregates[, MSSS_DIFFERENCE_ADJUSTED := as.numeric(NA)]
for (CA in content.areas) {
  school.aggregates[CONTENT_AREA == CA, MSSS_DIFFERENCE_ADJUSTED := MSSS_DIFFERENCE_UNCORRECTED - (PRIOR_MSSS_CENTERED_2YEAR*msss_rtm_models[[CA]]$coef[["PRIOR_MSSS_CENTERED_2YEAR"]])]
}

##    correlation checks
# cor(school.aggregates[, MSSS_DIFFERENCE_UNCORRECTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == prior.year, MSSS_DIFFERENCE_UNCORRECTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current.year, MSSS_DIFFERENCE_UNCORRECTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[, MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == prior.year, MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current.year, MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current.year & CONTENT_AREA == "ELA", MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current.year & CONTENT_AREA == "MATHEMATICS", MSSS_DIFFERENCE_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')


#####
###   Gamma Effect Size (within School MSSS 2021 - MSSS 2019)
#####

###   Create uncorrected G.E.S.
ges_sss <- rbindlist(list(
    Report_Data[[assessment]][,
        as.list(gammaEffectSizeLong(.SD, "SCALE_SCORE_STANDARDIZED", SGP:::yearIncrement(prior.year, -2), prior.year, quantiles=c(0.5), digits=2)),
      keyby=c("CONTENT_AREA", "SCHOOL_NUMBER")][, YEAR := prior.year],
    Report_Data[[assessment]][,
        as.list(gammaEffectSizeLong(.SD, "SCALE_SCORE_STANDARDIZED", prior.year, current.year, quantiles=c(0.5), digits=2)),
      keyby=c("CONTENT_AREA", "SCHOOL_NUMBER")][, YEAR := current.year]))

setnames(ges_sss, "Q_50", "GES_MEDIAN_SSS")
setkey(ges_sss, SCHOOL_NUMBER, YEAR, CONTENT_AREA)
setkey(school.aggregates, SCHOOL_NUMBER, YEAR, CONTENT_AREA)

##    Merge in GES with other summary statistics
school.aggregates <- ges_sss[school.aggregates]

###   RTM Adjusted G.E.S.
gesss_rtm_models <- list()
for (CA in content.areas) {
  gesss_rtm_models[[CA]] <- rlm(GES_MEDIAN_SSS ~ 0 + PRIOR_MSSS_CENTERED_2YEAR, data=school.aggregates[YEAR == prior.year & CONTENT_AREA == CA])
}

##    Model diagnostics
# par(mfrow = c(2, 2))
# for (CA in content.areas) {
# hist(gesss_rtm_models[[CA]]$residuals, breaks=50)
# qqnorm(gesss_rtm_models[[CA]]$residuals);qqline(gesss_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior.year & !is.na(PRIOR_MSSS_CENTERED_2YEAR) & CONTENT_AREA == CA, GES_MEDIAN_SSS]), gesss_rtm_models[[CA]]$residuals)
# plot(na.omit(school.aggregates[YEAR == prior.year & !is.na(PRIOR_MSSS_CENTERED_2YEAR) & CONTENT_AREA == CA, GES_MEDIAN_SSS]), gesss_rtm_models[[CA]]$fitted.values)
# }

##    Create Adjusted GES_MEDIAN_SSS by subtracting coefficient for PRIOR_MSSS_CENTERED_2YEAR
school.aggregates[, GES_MEDIAN_SSS_ADJUSTED := as.numeric(NA)]
for (CA in content.areas) {
  school.aggregates[CONTENT_AREA == CA, GES_MEDIAN_SSS_ADJUSTED := GES_MEDIAN_SSS - (PRIOR_MSSS_CENTERED_2YEAR*gesss_rtm_models[[CA]]$coef[["PRIOR_MSSS_CENTERED_2YEAR"]])]
}

##    Visualization, summary and correlation checks
# na.omit(school.aggregates[, as.list(round(summary(GES_MEDIAN_SSS_ADJUSTED),3)), keyby=c("YEAR", "CONTENT_AREA")])
# plot(school.aggregates[YEAR == prior.year, GES_MEDIAN_SSS, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED])
# plot(school.aggregates[YEAR == prior.year, GES_MEDIAN_SSS, PRIOR_MSSS_CENTERED_2YEAR])
# plot(school.aggregates[YEAR == prior.year, GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED])
# plot(school.aggregates[YEAR == prior.year, GES_MEDIAN_SSS_ADJUSTED, PRIOR_MSSS_CENTERED_2YEAR])
# cor(school.aggregates[, GES_MEDIAN_SSS_ADJUSTED, GES_MEDIAN_SSS], use='complete.obs')
# cor(school.aggregates[YEAR == prior.year, GES_MEDIAN_SSS_ADJUSTED, GES_MEDIAN_SSS], use='complete.obs')
# cor(school.aggregates[YEAR == current.year, GES_MEDIAN_SSS_ADJUSTED, GES_MEDIAN_SSS], use='complete.obs')

# cor(school.aggregates[, GES_MEDIAN_SSS, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == prior.year, GES_MEDIAN_SSS, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current.year, GES_MEDIAN_SSS, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[, GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == prior.year, GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current.year, GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current.year & CONTENT_AREA == "ELA", GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')
# cor(school.aggregates[YEAR == current.year & CONTENT_AREA == "MATHEMATICS", GES_MEDIAN_SSS_ADJUSTED, MEAN_SCALE_SCORE_PRIOR_STANDARDIZED], use='complete.obs')

# ges_ss_19_ela_adj <- lm(GES_MEDIAN_SSS_ADJUSTED ~ MEAN_SCALE_SCORE_PRIOR_STANDARDIZED, data=school.aggregates[YEAR == prior.year & CONTENT_AREA == "ELA"]); summary(ges_ss_19_ela_adj)
# ges_ss_19_math_adj<- lm(GES_MEDIAN_SSS_ADJUSTED ~ MEAN_SCALE_SCORE_PRIOR_STANDARDIZED, data=school.aggregates[YEAR == prior.year & CONTENT_AREA == "MATHEMATICS"]); summary(ges_ss_19_math_adj)


##    Create COVID Impact Levels for MSGP Baseline Differences
school.aggregates[, COVID_ACADEMIC_IMPACT_SSS_DIFF := fcase(
                    MSSS_DIFFERENCE_UNCORRECTED >= 5, "Improvement",
                    MSSS_DIFFERENCE_UNCORRECTED < 5 & MSSS_DIFFERENCE_UNCORRECTED >= -5, "Modest to None",
                    MSSS_DIFFERENCE_UNCORRECTED < -5 & MSSS_DIFFERENCE_UNCORRECTED >= -15, "Moderate",
                    MSSS_DIFFERENCE_UNCORRECTED < -15 & MSSS_DIFFERENCE_UNCORRECTED >= -25, "Large",
                    MSSS_DIFFERENCE_UNCORRECTED < -25, "Severe")]

school.aggregates[, COVID_ACADEMIC_IMPACT_SSS_DIFF_ADJ := fcase(
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED >= 5, "Improvement",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < 5 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -5, "Modest to None",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < -5 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -15, "Moderate",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < -15 & MSGP_BASELINE_DIFFERENCE_ADJUSTED >= -25, "Large",
                    MSGP_BASELINE_DIFFERENCE_ADJUSTED < -25, "Severe")]

##    Create COVID Impact Levels for G.E.S. for 2021 - 2019 Median SGP differences
school.aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS := fcase(
                    GES_MEDIAN_SSS >= 0.2, "Improvement",
                    GES_MEDIAN_SSS <  0.2 & GES_MEDIAN_SSS >= -0.2, "Modest to None",
                    GES_MEDIAN_SSS < -0.2 & GES_MEDIAN_SSS >= -0.5, "Moderate",
                    GES_MEDIAN_SSS < -0.5 & GES_MEDIAN_SSS >= -0.8, "Large",
                    GES_MEDIAN_SSS < -0.8, "Severe")]

school.aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ := fcase(
                    GES_MEDIAN_SSS_ADJUSTED >= 0.2, "Improvement",
                    GES_MEDIAN_SSS_ADJUSTED <  0.2 & GES_MEDIAN_SSS_ADJUSTED >= -0.2, "Modest to None",
                    GES_MEDIAN_SSS_ADJUSTED < -0.2 & GES_MEDIAN_SSS_ADJUSTED >= -0.5, "Moderate",
                    GES_MEDIAN_SSS_ADJUSTED < -0.5 & GES_MEDIAN_SSS_ADJUSTED >= -0.8, "Large",
                    GES_MEDIAN_SSS_ADJUSTED < -0.8, "Severe")]


###   Create Regression to the Mean diagnostic plots if desired
# source("Report_Analyses/IN_RTM_School_bubblePlot.R")

###   Save/Update `school.aggregates` and `RTM_Models` to `Report_Analyses`
Report_Analyses[["Summary_Tables"]][[assessment]][["SCHOOL_NUMBER"]][["Academic_Impact"]] <- school.aggregates

for (CA in content.areas) {
  Report_Analyses[["RTM_Models"]][[assessment]][["SCHOOL_NUMBER"]][["MSGP"]][[CA]] <- msgp_rtm_models[[CA]]$coefficients
  Report_Analyses[["RTM_Models"]][[assessment]][["SCHOOL_NUMBER"]][["GES_MSGP"]][[CA]] <- gessgp_rtm_models[[CA]]$coefficients
  Report_Analyses[["RTM_Models"]][[assessment]][["SCHOOL_NUMBER"]][["MSSS"]][[CA]] <- msss_rtm_models[[CA]]$coefficients
  Report_Analyses[["RTM_Models"]][[assessment]][["SCHOOL_NUMBER"]][["GES_MSSS"]][[CA]] <- gesss_rtm_models[[CA]]$coefficients
}
