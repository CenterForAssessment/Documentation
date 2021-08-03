################################################################################
###                                                                          ###
###      Indiana -- Investigation of Pandemic Academic Impact by School      ###
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

### Utility functions
hdmedian <- function(x, ...) as.numeric(Hmisc::hdquantile(x, probs=0.5, names=FALSE, ...))

percent_proficient <- function(achievement_level) {
        tmp.table <- table(achievement_level)
#        100*sum(tmp.table[c("At Proficiency", "Above Proficiency")])/sum(tmp.table)
#        100*sum(tmp.table[c("Level 3", "Level 4")])/sum(tmp.table)
        100*sum(tmp.table[c(, "Level 4")])/sum(tmp.table)
}

percent_proficient_prior <- function(achievement_level, year) {
    if (year!=current.year) {
        tmp.table <- table(achievement_level)
        100*sum(tmp.table[c("Pass", "Pass +")])/sum(tmp.table)
    } else {
        tmp.table <- table(achievement_level)
        100*sum(tmp.table[c("At Proficiency", "Above Proficiency")])/sum(tmp.table)
    }
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
sgp.grades <- params[["sgp.grades"]][[assessment]]
stat.grades<- setdiff(all.grades, sgp.grades)

content.areas <- params[["GL_subjects"]][[assessment]]

subgroup.min.size <- params[["min.size.school"]] # 15
test.abv <- params[["test.abv"]][[assessment]]

### Create School Level Summary Table

school.aggregates <- Report_Data[[assessment]][
                        VALID_CASE=="VALID_CASE" & GRADE %in% all.grades,
                          .(MEAN_SGP_BASELINE=mean(SGP_BASELINE, na.rm=TRUE),
                            MEDIAN_SGP_BASELINE=hdmedian(as.numeric(SGP_BASELINE), na.rm=TRUE),
                            MEAN_SGP=mean(SGP, na.rm=TRUE),
                            MEDIAN_SGP=hdmedian(as.numeric(SGP), na.rm=TRUE),
                            MEAN_SCALE_SCORE_PRIOR_STANDARDIZED=mean(SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR, na.rm=TRUE),
                            PERCENT_PROFICIENT=percent_proficient(ACHIEVEMENT_LEVEL),
                            PERCENT_PROFICIENT_PRIOR=percent_proficient_prior(ACHIEVEMENT_LEVEL_ORIGINAL_PRIOR, YEAR),
                            COUNT_SGP=sum(!is.na(SGP_BASELINE))),
                          keyby=c("YEAR", "SCHOOL_NUMBER", "CONTENT_AREA")]

shift.key <- c("SCHOOL_NUMBER", "CONTENT_AREA", "YEAR")
setkeyv(school.aggregates, shift.key)

school.aggregates[, c("MEDIAN_SGP_PRIOR_2YEAR", "MEDIAN_SGP_PRIOR_3YEAR") := shift(MEDIAN_SGP, 2:3), by = list(SCHOOL_NUMBER, CONTENT_AREA)]
school.aggregates[, MEDIAN_SGP_BASELINE_PRIOR := shift(MEDIAN_SGP_BASELINE, 1), by = list(SCHOOL_NUMBER, CONTENT_AREA)] # Only getting this for 2021 (1 year shift = 2 years)

table(school.aggregates[, YEAR, is.na(MEDIAN_SGP_PRIOR_2YEAR)])
table(school.aggregates[, YEAR, is.na(MEDIAN_SGP_PRIOR_3YEAR)])
table(school.aggregates[, YEAR, is.na(MEDIAN_SGP_BASELINE_PRIOR)])

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
mod_2019_ela <- lm(MSGP_BASELINE_DIFFERENCE_UNCORRECTED ~ 0 + PRIOR_MSGP_CENTERED_2YEAR, data=school.aggregates[YEAR == prior.year & CONTENT_AREA == "ELA"])
mod_2019_math<- lm(MSGP_BASELINE_DIFFERENCE_UNCORRECTED ~ 0 + PRIOR_MSGP_CENTERED_2YEAR, data=school.aggregates[YEAR == prior.year & CONTENT_AREA == "MATHEMATICS"])

##    Model diagnostics
par(mfrow = c(2, 2))
hist(mod_2019_ela$residuals, breaks=50)
qqnorm(mod_2019_ela$residuals);qqline(mod_2019_ela$residuals)
plot(na.omit(school.aggregates[YEAR == prior.year & CONTENT_AREA == "ELA" & !is.na(PRIOR_MSGP_CENTERED_2YEAR), MSGP_BASELINE_DIFFERENCE_UNCORRECTED]), mod_2019_ela$residuals)
plot(na.omit(school.aggregates[YEAR == prior.year & CONTENT_AREA == "ELA" & !is.na(PRIOR_MSGP_CENTERED_2YEAR), MEDIAN_SGP_BASELINE]), mod_2019_ela$fitted.values)

hist(mod_2019_math$residuals, breaks=50)
qqnorm(mod_2019_math$residuals);qqline(mod_2019_math$residuals)
plot(na.omit(school.aggregates[YEAR == prior.year & CONTENT_AREA == "MATHEMATICS" & !is.na(PRIOR_MSGP_CENTERED_2YEAR), MSGP_BASELINE_DIFFERENCE_UNCORRECTED]), mod_2019_math$residuals)
plot(na.omit(school.aggregates[YEAR == prior.year & CONTENT_AREA == "MATHEMATICS" & !is.na(PRIOR_MSGP_CENTERED_2YEAR), MEDIAN_SGP_BASELINE]), mod_2019_math$fitted.values)

##    Create Adjusted MSGP_BASELINE_DIFFERENCE by subtracting coefficient for PRIOR_MSGP_CENTERED_2YEAR
school.aggregates[, MSGP_BASELINE_DIFFERENCE_ADJUSTED := as.numeric(NA)]
school.aggregates[CONTENT_AREA == "ELA", MSGP_BASELINE_DIFFERENCE_ADJUSTED := MSGP_BASELINE_DIFFERENCE_UNCORRECTED - (PRIOR_MSGP_CENTERED_2YEAR*mod_2019_ela$coef[["PRIOR_MSGP_CENTERED_2YEAR"]])]
school.aggregates[CONTENT_AREA == "MATHEMATICS", MSGP_BASELINE_DIFFERENCE_ADJUSTED := MSGP_BASELINE_DIFFERENCE_UNCORRECTED - (PRIOR_MSGP_CENTERED_2YEAR*mod_2019_math$coef[["PRIOR_MSGP_CENTERED_2YEAR"]])]

##    correlation checks
cor(school.aggregates[, MSGP_BASELINE_DIFFERENCE_UNCORRECTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
cor(school.aggregates[YEAR == prior.year, MSGP_BASELINE_DIFFERENCE_UNCORRECTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
cor(school.aggregates[YEAR == current.year, MSGP_BASELINE_DIFFERENCE_UNCORRECTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
cor(school.aggregates[, MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
cor(school.aggregates[YEAR == prior.year, MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
cor(school.aggregates[YEAR == current.year, MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
cor(school.aggregates[YEAR == current.year & CONTENT_AREA == "ELA", MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
cor(school.aggregates[YEAR == current.year & CONTENT_AREA == "MATHEMATICS", MSGP_BASELINE_DIFFERENCE_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')

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

table(school.aggregates[YEAR=='2019', COVID_ACADEMIC_IMPACT_SGP_DIFF, CONTENT_AREA], exclude=NULL)
table(school.aggregates[YEAR=='2021', COVID_ACADEMIC_IMPACT_SGP_DIFF, CONTENT_AREA], exclude=NULL)
table(school.aggregates[YEAR=='2019', COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ, CONTENT_AREA], exclude=NULL)
table(school.aggregates[YEAR=='2021', COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ, CONTENT_AREA], exclude=NULL)

#####
###   Gamma Effect Size (within School MSGP 2021 - MSGP 2019)
#####

###   Create uncorrected G.E.S.
ges_sgp <- rbindlist(list(
    Report_Data[[assessment]][,
        as.list(gammaEffectSizeLong(.SD, "SGP", '2017', '2019', quantiles=c(0.5), digits=2)),
      keyby=c("CONTENT_AREA", "SCHOOL_NUMBER")][, YEAR := "2019"],
    Report_Data[[assessment]][,
        as.list(gammaEffectSizeLong(.SD, "SGP_BASELINE", prior.year, current.year, quantiles=c(0.5), digits=2)),
      keyby=c("CONTENT_AREA", "SCHOOL_NUMBER")][, YEAR := "2021"]))

setnames(ges_sgp, "Q_50", "GES_MEDIAN_SGP")
setkey(ges_sgp, SCHOOL_NUMBER, YEAR, CONTENT_AREA)
setkey(school.aggregates, SCHOOL_NUMBER, YEAR, CONTENT_AREA)

##    Merge in GES with other summary statistics
school.aggregates <- ges_sgp[school.aggregates]

###   RTM Adjusted G.E.S.  --  Keep it simple, stupid
ges_2019_ela <- lm(GES_MEDIAN_SGP ~ 0 + PRIOR_MSGP_CENTERED_2YEAR, data=school.aggregates[YEAR == prior.year & CONTENT_AREA == "ELA"])
ges_2019_math<- lm(GES_MEDIAN_SGP ~ 0 + PRIOR_MSGP_CENTERED_2YEAR, data=school.aggregates[YEAR == prior.year & CONTENT_AREA == "MATHEMATICS"])

##    Model diagnostics
par(mfrow = c(2, 2))
hist(ges_2019_ela$residuals, breaks=50)
qqnorm(ges_2019_ela$residuals);qqline(ges_2019_ela$residuals)
plot(na.omit(school.aggregates[YEAR == prior.year & !is.na(PRIOR_MSGP_CENTERED_2YEAR) & CONTENT_AREA == "ELA", GES_MEDIAN_SGP]), ges_2019_ela$residuals)
plot(na.omit(school.aggregates[YEAR == prior.year & !is.na(PRIOR_MSGP_CENTERED_2YEAR) & CONTENT_AREA == "ELA", GES_MEDIAN_SGP]), ges_2019_ela$fitted.values)

par(mfrow = c(2, 2))
hist(ges_2019_math$residuals, breaks=50)
qqnorm(ges_2019_math$residuals);qqline(ges_2019_math$residuals)
plot(na.omit(school.aggregates[YEAR == prior.year & !is.na(PRIOR_MSGP_CENTERED_2YEAR) & CONTENT_AREA == "MATHEMATICS", GES_MEDIAN_SGP]), ges_2019_math$residuals)
plot(na.omit(school.aggregates[YEAR == prior.year & !is.na(PRIOR_MSGP_CENTERED_2YEAR) & CONTENT_AREA == "MATHEMATICS", GES_MEDIAN_SGP]), ges_2019_math$fitted.values)

##    Create Adjusted GES_MEDIAN_SGP by subtracting coefficient for PRIOR_MSGP_CENTERED_2YEAR
school.aggregates[, GES_MEDIAN_SGP_ADJUSTED := as.numeric(NA)]
school.aggregates[CONTENT_AREA == "ELA", GES_MEDIAN_SGP_ADJUSTED := GES_MEDIAN_SGP - (PRIOR_MSGP_CENTERED_2YEAR*ges_2019_ela$coef[["PRIOR_MSGP_CENTERED_2YEAR"]])]
school.aggregates[CONTENT_AREA == "MATHEMATICS", GES_MEDIAN_SGP_ADJUSTED := GES_MEDIAN_SGP - (PRIOR_MSGP_CENTERED_2YEAR*ges_2019_math$coef[["PRIOR_MSGP_CENTERED_2YEAR"]])]

##    Visualization, summary and correlation checks
na.omit(school.aggregates[, as.list(round(summary(GES_MEDIAN_SGP_ADJUSTED),3)), keyby=c("YEAR", "CONTENT_AREA")])
plot(school.aggregates[YEAR == prior.year, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR])
plot(school.aggregates[YEAR == prior.year, GES_MEDIAN_SGP, PRIOR_MSGP_CENTERED_2YEAR])
plot(school.aggregates[YEAR == prior.year, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR])
plot(school.aggregates[YEAR == prior.year, GES_MEDIAN_SGP_ADJUSTED, PRIOR_MSGP_CENTERED_2YEAR])
cor(school.aggregates[, GES_MEDIAN_SGP_ADJUSTED, GES_MEDIAN_SGP], use='complete.obs')
cor(school.aggregates[YEAR == prior.year, GES_MEDIAN_SGP_ADJUSTED, GES_MEDIAN_SGP], use='complete.obs')
cor(school.aggregates[YEAR == current.year, GES_MEDIAN_SGP_ADJUSTED, GES_MEDIAN_SGP], use='complete.obs')

cor(school.aggregates[, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
cor(school.aggregates[YEAR == prior.year, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
cor(school.aggregates[YEAR == current.year, GES_MEDIAN_SGP, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
cor(school.aggregates[, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
cor(school.aggregates[YEAR == prior.year, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
cor(school.aggregates[YEAR == current.year, GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
cor(school.aggregates[YEAR == current.year & CONTENT_AREA == "ELA", GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')
cor(school.aggregates[YEAR == current.year & CONTENT_AREA == "MATHEMATICS", GES_MEDIAN_SGP_ADJUSTED, MEDIAN_SGP_PRIOR_2YEAR], use='complete.obs')

ges_2019_ela_adj <- lm(GES_MEDIAN_SGP_ADJUSTED ~ MEDIAN_SGP_PRIOR_2YEAR, data=school.aggregates[YEAR == prior.year & CONTENT_AREA == "ELA"]); summary(ges_2019_ela_adj)
ges_2019_math_adj<- lm(GES_MEDIAN_SGP_ADJUSTED ~ MEDIAN_SGP_PRIOR_2YEAR, data=school.aggregates[YEAR == prior.year & CONTENT_AREA == "MATHEMATICS"]); summary(ges_2019_math_adj)

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

table(school.aggregates[YEAR=='2019', COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP, CONTENT_AREA], exclude=NULL)
table(school.aggregates[YEAR=='2021', COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP, CONTENT_AREA], exclude=NULL)
table(school.aggregates[YEAR=='2019', COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ, CONTENT_AREA], exclude=NULL)
table(school.aggregates[YEAR=='2021', COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ, CONTENT_AREA], exclude=NULL)


###   Create wide plot_data

plot_data_wide <- dcast(school.aggregates, SCHOOL_NUMBER + CONTENT_AREA ~ YEAR,
                        value.var = c("COUNT_SGP", "MEDIAN_SGP_BASELINE", "MEDIAN_SGP_PRIOR_2YEAR",
                                      "GES_MEDIAN_SGP", "GES_MEDIAN_SGP_ADJUSTED", "PERCENT_PROFICIENT",
                                      "MSGP_BASELINE_DIFFERENCE_UNCORRECTED", "MSGP_BASELINE_DIFFERENCE_ADJUSTED",
                                      "COVID_ACADEMIC_IMPACT_SGP_DIFF", "COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ",
                                      "COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP", "COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ"))

plot_data_wide[, COVID_ACADEMIC_IMPACT_SGP_DIFF_2019 :=
                    factor(COVID_ACADEMIC_IMPACT_SGP_DIFF_2019,
                           levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

plot_data_wide[, COVID_ACADEMIC_IMPACT_SGP_DIFF_2021 :=
                    factor(COVID_ACADEMIC_IMPACT_SGP_DIFF_2021,
                           levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

plot_data_wide[, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2019 :=
                    factor(COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2019,
                           levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

plot_data_wide[, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021 :=
                    factor(COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021,
                           levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

plot_data_wide[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2019 :=
                    factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2019,
                           levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

plot_data_wide[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2021 :=
                    factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2021,
                           levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

plot_data_wide[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2019 :=
                    factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2019,
                           levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

plot_data_wide[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021 :=
                    factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021,
                           levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]



###   Get a general sense of the alignment of COVID_ACADEMIC_IMPACT_SGP_DIFF with the GES indicators
N_COUNT <- plot_data_wide[!is.na(GES_MEDIAN_SGP_2021), .N, keyby = c("CONTENT_AREA", "COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021")]
na.omit(plot_data_wide[!is.na(GES_MEDIAN_SGP_2021), as.list(round(summary(GES_MEDIAN_SGP_2021), 2)), keyby = c("CONTENT_AREA", "COVID_ACADEMIC_IMPACT_SGP_DIFF_2021")][N_COUNT])
na.omit(plot_data_wide[!is.na(GES_MEDIAN_SGP_2021), as.list(round(summary(GES_MEDIAN_SGP_ADJUSTED_2021), 2)), keyby = c("CONTENT_AREA", "COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021")][N_COUNT])

###   Check agreement on impact measures
table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_DIFF_2021, CONTENT_AREA])
table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021, CONTENT_AREA])
table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2021, CONTENT_AREA])
table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021, CONTENT_AREA])

table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021, COVID_ACADEMIC_IMPACT_SGP_DIFF_2021])
table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2021])
table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021]) # most agreement between the 2 Adjusted measures
table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2021])

###   Create Regression to the Mean diagnostic plots if desired
# source("Report_Analyses/IN_RTM_School_bubblePlot.R")

### Create bubble plots for state of Indiana

impact.measures <- c("COVID_ACADEMIC_IMPACT_SGP_DIFF_2021", "COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021",
                    "COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2021", "COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021")
tmp.cross.tabs.impact <- list()

for (impact.measure.iter in impact.measures) {
impact.measure.scale <- ifelse(grepl("SGP_DIFF", impact.measure.iter), 1, 2)
impact.measure.scale <- list(c("Improvement (> 5)", "Modest to None (-5 to 5)", "Moderate (-15 to -5)", "Large (-25 to -15)", "Severe (< -25)"),
                             c("Improvement (> 0.2)", "Modest to None (-0.2 to 0.2)", "Moderate (-0.2 to -0.5)", "Large (-0.5 to -0.8)", "Severe (< -0.8)"))[[impact.measure.scale]]

  for (content_area.iter in c("ELA", "MATHEMATICS")) {
    tmp.school.data.wide[CONTENT_AREA==content_area.iter]$COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021))
    tmp.school.data.wide <- plot_data_wide[CONTENT_AREA==content_area.iter & COUNT_SGP_2021 >= 20 & !is.na(get(impact.measure.iter))]
    tmp.cross.tabs.impact[[content_area.iter]] <- addmargins(table(
        tmp.school.data.wide[CONTENT_AREA==content_area.iter]$COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021,
        tmp.school.data.wide[CONTENT_AREA==content_area.iter]$COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021))

    for (covid.iter in covid.levels <- c("Improvement", "Modest to None", "Moderate", "Large", "Severe")) {

      ### message
      tmp.color <- rev(colorspace::rainbow_hcl(length(covid.levels)))[which(covid.iter==covid.levels)]
      tmp.percentage <- round(100*sum(tmp.school.data.wide[[impact.measure.iter]]==covid.iter)/length(tmp.school.data.wide[[impact.measure.iter]]==covid.iter), digits=1)
      tmp.lgla.percentage <- round(100*sum(tmp.school.data.wide[eval(parse(text=paste0("MEDIAN_SGP_BASELINE_", prior.year))) < 50 & eval(parse(text=paste0("PERCENT_PROFICIENT_", prior.year))) < 50][[impact.measure.iter]]==covid.iter)/
                                    length(tmp.school.data.wide[eval(parse(text=paste0("MEDIAN_SGP_BASELINE_", prior.year))) < 50 & eval(parse(text=paste0("PERCENT_PROFICIENT_", prior.year))) < 50][[impact.measure.iter]]==covid.iter), digits=1)
      tmp.hgla.percentage <- round(100*sum(tmp.school.data.wide[eval(parse(text=paste0("MEDIAN_SGP_BASELINE_", prior.year))) >= 50 & eval(parse(text=paste0("PERCENT_PROFICIENT_", prior.year))) < 50][[impact.measure.iter]]==covid.iter)/
                                    length(tmp.school.data.wide[eval(parse(text=paste0("MEDIAN_SGP_BASELINE_", prior.year))) >= 50 & eval(parse(text=paste0("PERCENT_PROFICIENT_", prior.year))) < 50][[impact.measure.iter]]==covid.iter), digits=1)
      tmp.hgha.percentage <- round(100*sum(tmp.school.data.wide[eval(parse(text=paste0("MEDIAN_SGP_BASELINE_", prior.year))) >= 50 & eval(parse(text=paste0("PERCENT_PROFICIENT_", prior.year))) >= 50][[impact.measure.iter]]==covid.iter)/
                                    length(tmp.school.data.wide[eval(parse(text=paste0("MEDIAN_SGP_BASELINE_", prior.year))) >= 50 & eval(parse(text=paste0("PERCENT_PROFICIENT_", prior.year))) >= 50][[impact.measure.iter]]==covid.iter), digits=1)
      tmp.lgha.percentage  <- round(100*sum(tmp.school.data.wide[eval(parse(text=paste0("MEDIAN_SGP_BASELINE_", prior.year))) < 50 & eval(parse(text=paste0("PERCENT_PROFICIENT_", prior.year))) >= 50][[impact.measure.iter]]==covid.iter)/
                                    length(tmp.school.data.wide[eval(parse(text=paste0("MEDIAN_SGP_BASELINE_", prior.year))) < 50 & eval(parse(text=paste0("PERCENT_PROFICIENT_", prior.year))) >= 50][[impact.measure.iter]]==covid.iter), digits=1)
      bPlot.message <- c("grid.lines(x=unit(50, 'native'), y=c(0.03,0.97), gp=gpar(col='grey40', lwd=1.25, lty=2, alpha=0.5))",
                         paste0("grid.text('", paste0(tmp.percentage, '%'), "', x=unit(50, 'native'), y=unit(90, 'native'), gp=gpar(cex=4.0, col='", tmp.color, "'), just='center')"),
                         paste0("grid.text('", paste0(tmp.lgla.percentage, '%'), "', x=unit(10, 'native'), y=unit(20, 'native'), gp=gpar(cex=2.0, col='", tmp.color, "'), just='left')"),
                         paste0("grid.text('", paste0(tmp.hgla.percentage, '%'), "', x=unit(90, 'native'), y=unit(20, 'native'), gp=gpar(cex=2.0, col='", tmp.color, "'), just='right')"),
                         paste0("grid.text('", paste0(tmp.hgha.percentage, '%'), "', x=unit(90, 'native'), y=unit(80, 'native'), gp=gpar(cex=2.0, col='", tmp.color, "'), just='right')"),
                         paste0("grid.text('", paste0(tmp.lgha.percentage, '%'), "', x=unit(10, 'native'), y=unit(80, 'native'), gp=gpar(cex=2.0, col='", tmp.color, "'), just='left')")
                     )

      bubblePlot(
        bubble_plot_data.X=tmp.school.data.wide[[paste0("MEDIAN_SGP_BASELINE_", prior.year)]],
        bubble_plot_data.Y=tmp.school.data.wide[[paste0("PERCENT_PROFICIENT_", prior.year)]],
        bubble_plot_data.SUBSET=which(tmp.school.data.wide[[impact.measure.iter]]==covid.iter),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.school.data.wide[[paste0("COUNT_SGP_", prior.year)]],
        bubble_plot_data.LEVELS=tmp.school.data.wide[[impact.measure.iter]],
        bubble_plot_labels.X=c("Growth", paste(prior.year, "Median Student Growth Percentile")),
        bubble_plot_labels.Y=c("Achievement", paste(prior.year, "Percent Proficient")),
        bubble_plot_labels.SIZE=c(50, 100, 250, 500),
        bubble_plot_labels.LEVELS=impact.measure.scale,
        bubble_plot_titles.MAIN=paste0(test.abv, ": ", capwords(content_area.iter)),
        bubble_plot_titles.SUB1="School Level COVID Academic Impact",
        bubble_plot_titles.SUB2=paste(prior.year, "Growth by Achievement"),
        bubble_plot_titles.LEGEND1="School Size",
        bubble_plot_titles.LEGEND2_P1="COVID Impact",
        bubble_plot_titles.LEGEND2_P2=paste(current.year, "SGP -", prior.year, "SGP"),
        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 0.8, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_Y_TICKS_SIZE=rep(0.6, 11),
        bubble_plot_configs.BUBBLE_PLOT_BACKGROUND_LABELS=c("Growth", "Achievement"),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="print",#"presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_SUMMARY_STATISTICS=FALSE,
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(test.abv, capwords(content_area.iter), "Academic_Impact_by_School", covid.iter, sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("assets", "Rplots", "bubblePlots", test.abv, impact.measure.iter),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
    }
  }
}
