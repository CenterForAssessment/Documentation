##############################################################################
###
### Investigation of Pandemic Academic Impact by School in Indiana
###
##############################################################################

### Load SGP package
require(SGP)
require(data.table)


### Load data
#load("/Users/conet/Github/CenterForAssessment/Indiana/master/Data/Indiana_SGP_LONG_Data.Rdata")

### Utility function
percent_proficient <- function(achievement_level) {
        tmp.table <- table(achievement_level)
        100*sum(tmp.table[c("At Proficiency", "Above Proficiency")])/sum(tmp.table)
}

percent_proficient_prior <- function(achievement_level, year) {
    if (year!="2021") {
        tmp.table <- table(achievement_level)
        100*sum(tmp.table[c("Pass", "Pass +")])/sum(tmp.table)
    } else {
        tmp.table <- table(achievement_level)
        100*sum(tmp.table[c("At Proficiency", "Above Proficiency")])/sum(tmp.table)
    }
}

### Create School Level File
school.aggregates <- Indiana_SGP_LONG_Data[VALID_CASE=="VALID_CASE" & GRADE_ID %in% as.character(3:8),
                                                                    list(MEAN_SGP_BASELINE=mean(SGP_BASELINE, na.rm=TRUE),
                                                                          MEDIAN_SGP_BASELINE=median(as.numeric(SGP_BASELINE), na.rm=TRUE),
                                                                          MEAN_SCALE_SCORE_PRIOR_STANDARDIZED=mean(SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE, na.rm=TRUE),
                                                                          PERCENT_PROFICIENT=percent_proficient(ACHIEVEMENT_LEVEL),
                                                                          PERCENT_PROFICIENT_PRIOR=percent_proficient_prior(ACHIEVEMENT_LEVEL_PRIOR, SCHOOL_YEAR), COUNT_SGP=sum(!is.na(SGP_BASELINE))),
                                                    keyby=c("SCHOOL_YEAR", "IDOE_SCHOOL_ID", "CONTENT_AREA")]
tmp.school.2019 <- school.aggregates[SCHOOL_YEAR=="2019"][,SCHOOL_YEAR:=NULL]
tmp.school.2021 <- school.aggregates[SCHOOL_YEAR=="2021"][,SCHOOL_YEAR:=NULL]

setnames(tmp.school.2019, c("MEAN_SGP_BASELINE", "MEDIAN_SGP_BASELINE", "MEAN_SCALE_SCORE_PRIOR_STANDARDIZED", "PERCENT_PROFICIENT", "PERCENT_PROFICIENT_PRIOR", "COUNT_SGP"), c("MEAN_SGP_BASELINE_2019", "MEDIAN_SGP_BASELINE_2019", "MEAN_SCALE_SCORE_PRIOR_STANDARDIZED_2019", "PERCENT_PROFICIENT_2019", "PERCENT_PROFICIENT_PRIOR_2019", "COUNT_SGP_2019"))
setnames(tmp.school.2021, c("MEAN_SGP_BASELINE", "MEDIAN_SGP_BASELINE", "MEAN_SCALE_SCORE_PRIOR_STANDARDIZED", "PERCENT_PROFICIENT", "PERCENT_PROFICIENT_PRIOR", "COUNT_SGP"), c("MEAN_SGP_BASELINE_2021", "MEDIAN_SGP_BASELINE_2021", "MEAN_SCALE_SCORE_PRIOR_STANDARDIZED_2021", "PERCENT_PROFICIENT_2021", "PERCENT_PROFICIENT_PRIOR_2021", "COUNT_SGP_2021"))
setkey(tmp.school.2019, IDOE_SCHOOL_ID, CONTENT_AREA)
setkey(tmp.school.2021, IDOE_SCHOOL_ID, CONTENT_AREA)

tmp.school.all <- merge(tmp.school.2019, tmp.school.2021, all=TRUE)
tmp.school.all[,MEAN_SGP_BASELINE_2019_2021_DIFF:=MEAN_SGP_BASELINE_2021-MEAN_SGP_BASELINE_2019][,MEDIAN_SGP_BASELINE_2019_2021_DIFF:=MEDIAN_SGP_BASELINE_2021-MEDIAN_SGP_BASELINE_2019][,PERCENT_COUNT_2021_to_COUNT_2019:=100*COUNT_SGP_2021/COUNT_SGP_2019]
tmp.school.all[MEDIAN_SGP_BASELINE_2019_2021_DIFF >= 5, COVID_ACADEMIC_IMPACT_2021:="Improvement"]
tmp.school.all[MEDIAN_SGP_BASELINE_2019_2021_DIFF < 5 & MEDIAN_SGP_BASELINE_2019_2021_DIFF >= -5, COVID_ACADEMIC_IMPACT_2021:="Modest to None"]
tmp.school.all[MEDIAN_SGP_BASELINE_2019_2021_DIFF < -5 & MEDIAN_SGP_BASELINE_2019_2021_DIFF >= -15, COVID_ACADEMIC_IMPACT_2021:="Moderate"]
tmp.school.all[MEDIAN_SGP_BASELINE_2019_2021_DIFF < -15 & MEDIAN_SGP_BASELINE_2019_2021_DIFF >= -25, COVID_ACADEMIC_IMPACT_2021:="Large"]
tmp.school.all[MEDIAN_SGP_BASELINE_2019_2021_DIFF < -25, COVID_ACADEMIC_IMPACT_2021:="Severe"]
tmp.school.all[,COVID_ACADEMIC_IMPACT_2021:=factor(COVID_ACADEMIC_IMPACT_2021, levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]




### Create bubble plots for state of Indiana

for (content_area.iter in c("ELA", "MATHEMATICS")) {
    tmp.school.data.wide <- tmp.school.all[CONTENT_AREA==content_area.iter & COUNT_SGP_2021 >= 20 & !is.na(MEDIAN_SGP_BASELINE_2019_2021_DIFF)]
    tmp.lm <- lm(tmp.school.data.wide$MEDIAN_SGP_BASELINE_2019_2021_DIFF ~ tmp.school.data.wide$MEDIAN_SGP_BASELINE_2019)
    tmp.school.data.wide[,MEDIAN_SGP_BASELINE_2019_2021_DIFF_RESIDUALIZED:=tmp.lm$residuals]

    for (covid.iter in c("Improvement", "Modest to None", "Moderate", "Large", "Severe")) {

        ### message
        bPlot.message <- c("grid.lines(x=unit(50, 'native'), y=c(0.03,0.97), gp=gpar(col='grey40', lwd=1.25, lty=2, alpha=0.5))"#,
                )

    bubblePlot(
        bubble_plot_data.X=tmp.school.data.wide[["MEDIAN_SGP_BASELINE_2019"]],
        bubble_plot_data.Y=tmp.school.data.wide[["PERCENT_PROFICIENT_2019"]],
        bubble_plot_data.SUBSET=which(tmp.school.data.wide[["COVID_ACADEMIC_IMPACT_2021"]]==covid.iter),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.school.data.wide[["COUNT_SGP_2019"]],
        bubble_plot_data.LEVELS=tmp.school.data.wide[["COVID_ACADEMIC_IMPACT_2021"]],
#        bubble_plot_data.BUBBLE_TIPS_LINES=list(
#                paste(tmp.school.data.wide[["MEDIAN_SGP_2014"]], " (", tmp.school.data.wide[["MEDIAN_SGP_COUNT_2014"]], ")", sep=""),
#                paste(tmp.school.data.wide[["MEDIAN_SGP_2015"]], " (", tmp.school.data.wide[["MEDIAN_SGP_COUNT_2015"]], ")", sep="")),
        bubble_plot_labels.X=c("Growth", "2019 Median Student Growth Percentile"),
        bubble_plot_labels.Y=c("Achievement", "2019 Percent Proficient"),
        bubble_plot_labels.SIZE=c(50, 100, 250, 500),
        bubble_plot_labels.LEVELS=c("Improvement (> 5)", "Modest to None (-5 to 5)", "Moderate (-15 to -5)", "Large (-25 to -15)", "Severe (< -25)"),
#        bubble_plot_labels.BUBBLE_TIPS_LINES=list(
#                "2014 Median SGP (Count)",
#                "2015 Median SGP (Count)" ),
#        bubble_plot_labels.BUBBLE_TITLES=tmp.school.data.wide[["DISTRICT_NAME"]],
        bubble_plot_titles.MAIN=paste("ILEARN:", capwords(content_area.iter)),
        bubble_plot_titles.SUB1="School Level COVID Academic Impact",
        bubble_plot_titles.SUB2="2019 Growth by Achievement",
        bubble_plot_titles.LEGEND1="School Size",
        bubble_plot_titles.LEGEND2_P1="COVID Impact",
        bubble_plot_titles.LEGEND2_P2="2021 SGP - 2019 SGP",
        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 0.8, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_Y_TICKS_SIZE=rep(0.6, 11),
        bubble_plot_configs.BUBBLE_PLOT_BACKGROUND_LABELS=c("Growth", "Achievement"),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
#        bubble_plot_configs.BUBBLE_TIPS="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="print",#"presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_SUMMARY_STATISTICS=FALSE,
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste("ILEARN", capwords(content_area.iter), "2021_Academic_Impact_by_School_COVID", covid.iter, sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("Visualizations", "bubblePlots", "ILEARN"),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)

    }
}
