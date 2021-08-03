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

### Utility function
percent_proficient <- function(achievement_level) {
        tmp.table <- table(achievement_level)
        100*sum(tmp.table[c("At Proficiency", "Above Proficiency")])/sum(tmp.table)
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

### Create School Level File
school.aggregates <- Report_Data[[assessment]][
                        VALID_CASE=="VALID_CASE" & GRADE %in% all.grades,
                          .(MEAN_SGP_BASELINE=mean(SGP_BASELINE, na.rm=TRUE),
                            MEDIAN_SGP_BASELINE=median(as.numeric(SGP_BASELINE), na.rm=TRUE),
                            MEAN_SGP=mean(SGP, na.rm=TRUE),
                            MEDIAN_SGP=median(as.numeric(SGP), na.rm=TRUE),
                            MEAN_SCALE_SCORE_PRIOR_STANDARDIZED=mean(SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR, na.rm=TRUE),
                            PERCENT_PROFICIENT=percent_proficient(ACHIEVEMENT_LEVEL),
                            PERCENT_PROFICIENT_PRIOR=percent_proficient_prior(ACHIEVEMENT_LEVEL_ORIGINAL_PRIOR, YEAR),
                            COUNT_SGP=sum(!is.na(SGP_BASELINE))),
                          keyby=c("YEAR", "SCHOOL_NUMBER", "CONTENT_AREA")]

tmp.school.prior <- school.aggregates[YEAR==prior.year][,YEAR:=NULL]
tmp.school.current <- school.aggregates[YEAR==current.year][,YEAR:=NULL]

agg.names <- grep("MEAN|MEDIAN|PERCENT|COUNT", names(school.aggregates), value=TRUE)
setnames(tmp.school.prior, agg.names, paste0(agg.names, "_", prior.year))
setnames(tmp.school.current, agg.names, paste0(agg.names, "_", current.year))
setkey(tmp.school.prior, SCHOOL_NUMBER, CONTENT_AREA)
setkey(tmp.school.current, SCHOOL_NUMBER, CONTENT_AREA)

tmp.school.all <- merge(tmp.school.prior, tmp.school.current, all=TRUE)
tmp.school.all[,MEAN_SGP_BASELINE_PRE_POST_DIFF := eval(parse(text=paste(paste0("MEAN_SGP_BASELINE_", c(current.year, prior.year)), collapse=" - ")))][,
                MEDIAN_SGP_BASELINE_PRE_POST_DIFF := eval(parse(text=paste(paste0("MEDIAN_SGP_BASELINE_", c(current.year, prior.year)), collapse=" - ")))][,
                PERCENT_COUNT_PRE_POST_RATIO := 100*eval(parse(text=paste(paste0("COUNT_SGP_", c(current.year, prior.year)), collapse="/")))]

tmp.school.all[, COVID_ACADEMIC_IMPACT_SGP_DIFF := fcase(
                    MEDIAN_SGP_BASELINE_PRE_POST_DIFF >= 5, "Improvement",
                    MEDIAN_SGP_BASELINE_PRE_POST_DIFF < 5 & MEDIAN_SGP_BASELINE_PRE_POST_DIFF >= -5, "Modest to None",
                    MEDIAN_SGP_BASELINE_PRE_POST_DIFF < -5 & MEDIAN_SGP_BASELINE_PRE_POST_DIFF >= -15, "Moderate",
                    MEDIAN_SGP_BASELINE_PRE_POST_DIFF < -15 & MEDIAN_SGP_BASELINE_PRE_POST_DIFF >= -25, "Large",
                    MEDIAN_SGP_BASELINE_PRE_POST_DIFF < -25, "Severe")]

tmp.school.all[, COVID_ACADEMIC_IMPACT_SGP_DIFF := factor(COVID_ACADEMIC_IMPACT_SGP_DIFF,
                                                            levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

###   Calculate/merge Akinshin's Gamma Effect Size estimates into summaries

##    SGP_BASELINE - School Growth Quantile Shift (by prior achievement deciles)
ges.sgp.within.deciles <- Report_Data[[assessment]][VALID_CASE=="VALID_CASE" & GRADE %in% all.grades,
                              as.list(gammaEffectSizeLong(.SD, "SGP_BASELINE", prior.year, current.year, quantiles=c(0.25, 0.5, 0.75), digits=2)),
                            keyby=c("CONTENT_AREA", "PRIOR_DECILE_2YEAR", "SCHOOL_NUMBER")]

ges.sgp.within.deciles[, IQRM := rowSums(.SD, na.rm=TRUE)/3, .SDcols = c("Q_25", "Q_50", "Q_75")]
ges.sgp.within.deciles.summary <- ges.sgp.within.deciles[, .(
                                      GES_MEDIAN_SGP_COMPOSITE= mean(Q_50, na.rm=TRUE),
                                      GES_IQRM_SGP_COMPOSITE  = mean(IQRM, na.rm=TRUE)),
                                    keyby=c("CONTENT_AREA", "SCHOOL_NUMBER")][!is.na(GES_MEDIAN_SGP_COMPOSITE)]

setkey(ges.sgp.within.deciles.summary, CONTENT_AREA, SCHOOL_NUMBER)
setkey(tmp.school.all, CONTENT_AREA, SCHOOL_NUMBER)
tmp.school.all <- ges.sgp.within.deciles.summary[tmp.school.all]


##    SCALE_SCORE - School Status Quantile Shift (by prior achievement deciles)
ges.ss.within.deciles <- Report_Data[[assessment]][VALID_CASE=="VALID_CASE" & GRADE %in% all.grades,
                              as.list(gammaEffectSizeLong(.SD, "SCALE_SCORE", prior.year, current.year, quantiles=c(0.25, 0.5, 0.75), digits=2)),
                            keyby=c("CONTENT_AREA", "PRIOR_DECILE_2YEAR", "SCHOOL_NUMBER")]

ges.ss.within.deciles[, IQRM := rowSums(.SD, na.rm=TRUE)/3, .SDcols = c("Q_25", "Q_50", "Q_75")]
ges.ss.within.deciles.summary <- ges.ss.within.deciles[, .(
                                      GES_MEDIAN_SS_COMPOSITE= mean(Q_50, na.rm=TRUE),
                                      GES_IQRM_SS_COMPOSITE  = mean(IQRM, na.rm=TRUE)),
                                    keyby=c("CONTENT_AREA", "SCHOOL_NUMBER")][!is.na(GES_MEDIAN_SS_COMPOSITE)]

setkey(ges.ss.within.deciles.summary, CONTENT_AREA, SCHOOL_NUMBER)
setkey(tmp.school.all, CONTENT_AREA, SCHOOL_NUMBER)
tmp.school.all <- ges.ss.within.deciles.summary[tmp.school.all]

##    SGP_BASELINE - School Growth Quantile Shift (across all prior achievement)
ges.sgp.within.overall <- na.omit(Report_Data[[assessment]][VALID_CASE=="VALID_CASE" & GRADE %in% all.grades,
                                      as.list(gammaEffectSizeLong(.SD, "SGP_BASELINE", prior.year, current.year, digits=2)),
                                    keyby=c("CONTENT_AREA", "SCHOOL_NUMBER")])

##    Or retrieve from Report_Analyses[["Quantile_Shift"]]...
# ges.sgp.within.overall <- data.table()
# for (ca in content.areas)
#   ges.sgp.within.overall <- rbindlist(list(
#         ges.sgp.within.overall,
#         Report_Analyses[["Quantile_Shift"]][[assessment]][["Hierarchical_Shift"]][[
#             paste0(current.year, "_", prior.year)]][["SCHOOL"]][["GAMMA_ES"]][[
#             "SGP_BASELINE"]][[ca]][["WITHIN"]][["CONTENT_AREA"]][, CONTENT_AREA := ..ca]))


GES_IMPACT_SGP <- ges.sgp.within.overall[, GES_SUMSCORE_SGP := rowSums(.SD)/9, .SDcols = (paste0("Q_", 1:9*10))]
GES_IMPACT_SGP <- GES_IMPACT_SGP[, .(CONTENT_AREA, SCHOOL_NUMBER, Q_50, GES_SUMSCORE_SGP)]
setnames(GES_IMPACT_SGP, "Q_50", "GES_MEDIAN_SGP")
setkey(GES_IMPACT_SGP, CONTENT_AREA, SCHOOL_NUMBER)
setkey(tmp.school.all, CONTENT_AREA, SCHOOL_NUMBER)

tmp.school.all <- GES_IMPACT_SGP[tmp.school.all]

table(tmp.school.all[, is.na(GES_MEDIAN_SGP), is.na(COVID_ACADEMIC_IMPACT_SGP_DIFF)])
table(tmp.school.all[is.na(GES_MEDIAN_SGP) & !is.na(COVID_ACADEMIC_IMPACT_SGP_DIFF), COUNT_SGP_2019, COUNT_SGP_2021]) # Just small schools (1 year or the other or both)

##    SCALE_SCORE - School Status Quantile Shift (across all prior achievement)
ges.ss.within.overall <- na.omit(Report_Data[[assessment]][VALID_CASE=="VALID_CASE" & GRADE %in% all.grades,
                                    as.list(gammaEffectSizeLong(.SD, "SCALE_SCORE", prior.year, current.year, digits=2)),
                                  keyby=c("CONTENT_AREA", "SCHOOL_NUMBER")])

GES_IMPACT_SS <- ges.ss.within.overall[, GES_SUMSCORE_SS := rowSums(.SD)/9, .SDcols = (paste0("Q_", 1:9*10))]
GES_IMPACT_SS <- GES_IMPACT_SS[, .(CONTENT_AREA, SCHOOL_NUMBER, Q_50, GES_SUMSCORE_SS)]
setnames(GES_IMPACT_SS, "Q_50", "GES_MEDIAN_SS")
setkey(GES_IMPACT_SS, CONTENT_AREA, SCHOOL_NUMBER)
setkey(tmp.school.all, CONTENT_AREA, SCHOOL_NUMBER)

tmp.school.all <- GES_IMPACT_SS[tmp.school.all]

table(tmp.school.all[, is.na(GES_MEDIAN_SS), is.na(COVID_ACADEMIC_IMPACT_SGP_DIFF)])
table(tmp.school.all[!is.na(GES_MEDIAN_SS) & is.na(COVID_ACADEMIC_IMPACT_SGP_DIFF) & COUNT_SGP_2021 != 0, COUNT_SGP_2019, COUNT_SGP_2021]) # Just small schools (1 year or the other)

###   Get a general sense of the alignment of COVID_ACADEMIC_IMPACT_SGP_DIFF with the GES indicators
N_COUNT <- tmp.school.all[!is.na(GES_MEDIAN_SGP), .N, keyby = c("CONTENT_AREA", "COVID_ACADEMIC_IMPACT_SGP_DIFF")]
na.omit(tmp.school.all[!is.na(GES_MEDIAN_SGP), as.list(round(summary(GES_MEDIAN_SGP), 2)), keyby = c("CONTENT_AREA", "COVID_ACADEMIC_IMPACT_SGP_DIFF")][N_COUNT])
na.omit(tmp.school.all[!is.na(GES_MEDIAN_SGP), as.list(round(summary(GES_SUMSCORE_SGP), 2)), keyby = c("CONTENT_AREA", "COVID_ACADEMIC_IMPACT_SGP_DIFF")][N_COUNT])

tmp.school.all[, COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN := fcase(
                    GES_MEDIAN_SGP >= 0.2, "Improvement",
                    GES_MEDIAN_SGP <  0.2 & GES_MEDIAN_SGP >= -0.2, "Modest to None",
                    GES_MEDIAN_SGP < -0.2 & GES_MEDIAN_SGP >= -0.5, "Moderate",
                    GES_MEDIAN_SGP < -0.5 & GES_MEDIAN_SGP >= -0.8, "Large",
                    GES_MEDIAN_SGP < -0.8, "Severe")]

tmp.school.all[, COVID_ACADEMIC_IMPACT_SGP_GES_SUMSCORE := fcase(
                    GES_SUMSCORE_SGP >= 0.2, "Improvement",
                    GES_SUMSCORE_SGP <  0.2 & GES_SUMSCORE_SGP >= -0.2, "Modest to None",
                    GES_SUMSCORE_SGP < -0.2 & GES_SUMSCORE_SGP >= -0.5, "Moderate",
                    GES_SUMSCORE_SGP < -0.5 & GES_SUMSCORE_SGP >= -0.8, "Large",
                    GES_SUMSCORE_SGP < -0.8, "Severe")]

tmp.school.all[, COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN_COMPOSITE := fcase(
                    GES_MEDIAN_SGP_COMPOSITE >= 0.2, "Improvement",
                    GES_MEDIAN_SGP_COMPOSITE <  0.2 & GES_MEDIAN_SGP_COMPOSITE >= -0.2, "Modest to None",
                    GES_MEDIAN_SGP_COMPOSITE < -0.2 & GES_MEDIAN_SGP_COMPOSITE >= -0.5, "Moderate",
                    GES_MEDIAN_SGP_COMPOSITE < -0.5 & GES_MEDIAN_SGP_COMPOSITE >= -0.8, "Large",
                    GES_MEDIAN_SGP_COMPOSITE < -0.8, "Severe")]

tmp.school.all[, COVID_ACADEMIC_IMPACT_GES_SGP_IQRM_COMPOSITE := fcase(
                    GES_IQRM_SGP_COMPOSITE >= 0.2, "Improvement",
                    GES_IQRM_SGP_COMPOSITE <  0.2 & GES_IQRM_SGP_COMPOSITE >= -0.2, "Modest to None",
                    GES_IQRM_SGP_COMPOSITE < -0.2 & GES_IQRM_SGP_COMPOSITE >= -0.5, "Moderate",
                    GES_IQRM_SGP_COMPOSITE < -0.5 & GES_IQRM_SGP_COMPOSITE >= -0.8, "Large",
                    GES_IQRM_SGP_COMPOSITE < -0.8, "Severe")]

tmp.school.all[, COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN :=
                    factor(COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN,
                           levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

tmp.school.all[, COVID_ACADEMIC_IMPACT_SGP_GES_SUMSCORE :=
                    factor(COVID_ACADEMIC_IMPACT_SGP_GES_SUMSCORE,
                           levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

tmp.school.all[, COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN_COMPOSITE :=
                    factor(COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN_COMPOSITE,
                           levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]

tmp.school.all[, COVID_ACADEMIC_IMPACT_GES_SGP_IQRM_COMPOSITE :=
                    factor(COVID_ACADEMIC_IMPACT_GES_SGP_IQRM_COMPOSITE,
                           levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]


###   Check agreement on impact measures
table(tmp.school.all[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_DIFF, CONTENT_AREA])
table(tmp.school.all[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN, CONTENT_AREA])
table(tmp.school.all[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_GES_SUMSCORE, CONTENT_AREA])
table(tmp.school.all[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN_COMPOSITE, CONTENT_AREA])
table(tmp.school.all[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_GES_SGP_IQRM_COMPOSITE, CONTENT_AREA])

table(tmp.school.all[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_DIFF, COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN])
table(tmp.school.all[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_DIFF, COVID_ACADEMIC_IMPACT_SGP_GES_SUMSCORE])
table(tmp.school.all[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_GES_SUMSCORE, COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN])
table(tmp.school.all[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_GES_SUMSCORE, COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN])
table(tmp.school.all[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN_COMPOSITE, COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN])

### Create bubble plots for state of Indiana

impact.measures <- c("COVID_ACADEMIC_IMPACT_SGP_DIFF", "COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN", "COVID_ACADEMIC_IMPACT_SGP_GES_SUMSCORE",
                     "COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN_COMPOSITE", "COVID_ACADEMIC_IMPACT_GES_SGP_IQRM_COMPOSITE")
# impact.measure.iter <- "COVID_ACADEMIC_IMPACT_GES_SGP_MEDIAN_COMPOSITE"

for (impact.measure.iter in impact.measures) {
impact.measure.scale <- ifelse(impact.measure.iter=="COVID_ACADEMIC_IMPACT_SGP_DIFF", 1, 2)
impact.measure.scale <- list(c("Improvement (> 5)", "Modest to None (-5 to 5)", "Moderate (-15 to -5)", "Large (-25 to -15)", "Severe (< -25)"),
                             c("Improvement (> 0.2)", "Modest to None (-0.2 to 0.2)", "Moderate (-0.2 to -0.5)", "Large (-0.5 to -0.8)", "Severe (< -0.8)"))[[impact.measure.scale]]

  for (content_area.iter in c("ELA", "MATHEMATICS")) {
    tmp.school.data.wide <- tmp.school.all[CONTENT_AREA==content_area.iter & COUNT_SGP_2021 >= 20 & !is.na(get(impact.measure.iter))]
    # tmp.lm <- lm(tmp.school.data.wide$MEDIAN_SGP_BASELINE_PRE_POST_DIFF ~ tmp.school.data.wide$MEDIAN_SGP_BASELINE_2019)
    # tmp.school.data.wide[,MEDIAN_SGP_BASELINE_PRE_POST_DIFF_RESIDUALIZED:=tmp.lm$residuals]

    for (covid.iter in c("Improvement", "Modest to None", "Moderate", "Large", "Severe")) {

        ### message
        bPlot.message <- c("grid.lines(x=unit(50, 'native'), y=c(0.03,0.97), gp=gpar(col='grey40', lwd=1.25, lty=2, alpha=0.5))"#,
                )

    bubblePlot(
      bubble_plot_data.X=tmp.school.data.wide[[paste0("MEDIAN_SGP_BASELINE_", prior.year)]],
      bubble_plot_data.Y=tmp.school.data.wide[[paste0("PERCENT_PROFICIENT_", prior.year)]],
      bubble_plot_data.SUBSET=which(tmp.school.data.wide[[impact.measure.iter]]==covid.iter),
      bubble_plot_data.INDICATE=NULL,
      bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
      bubble_plot_data.SIZE=tmp.school.data.wide[[paste0("COUNT_SGP_", prior.year)]],
      bubble_plot_data.LEVELS=tmp.school.data.wide[[impact.measure.iter]],
      # bubble_plot_data.BUBBLE_TIPS_LINES=list(
      #   paste(tmp.school.data.wide[["MEDIAN_SGP_2014"]], " (", tmp.school.data.wide[["MEDIAN_SGP_COUNT_2014"]], ")", sep=""),
      #   paste(tmp.school.data.wide[["MEDIAN_SGP_2015"]], " (", tmp.school.data.wide[["MEDIAN_SGP_COUNT_2015"]], ")", sep="")),
      bubble_plot_labels.X=c("Growth", paste(prior.year, "Median Student Growth Percentile")),
      bubble_plot_labels.Y=c("Achievement", paste(prior.year, "Percent Proficient")),
      bubble_plot_labels.SIZE=c(50, 100, 250, 500),
      bubble_plot_labels.LEVELS=impact.measure.scale,
      # bubble_plot_labels.BUBBLE_TIPS_LINES=list(
      #   "2014 Median SGP (Count)",
      #   "2015 Median SGP (Count)" ),
      # bubble_plot_labels.BUBBLE_TITLES=tmp.school.data.wide[["DISTRICT_NAME"]],
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
      # bubble_plot_configs.BUBBLE_TIPS="TRUE",
      bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
      bubble_plot_configs.BUBBLE_PLOT_FORMAT="print",#"presentation",
      bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
      bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
      bubble_plot_configs.BUBBLE_PLOT_SUMMARY_STATISTICS=FALSE,
      bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
      bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(test.abv, capwords(content_area.iter), current.year, "Academic_Impact_by_School_COVID", covid.iter, sep="_"), ".pdf", sep=""),
      bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("assets", "Rplots", "bubblePlots", test.abv, impact.measure.iter),
      bubble_plot_pdftk.CREATE_CATALOG=FALSE)
    }
  }
}
