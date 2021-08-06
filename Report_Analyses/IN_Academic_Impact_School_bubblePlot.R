################################################################################
###                                                                          ###
###    Investigation of Pandemic Academic Impact by School -- bubblePlots    ###
###                                                                          ###
################################################################################

###   General Setup (assumes running from ./Documentation directory)

###   Declare an assessment flavor --  need to extend to WIDA
if (!exists("assessment")) assessment <- "State_Assessment"

###   Load `school.aggregates` from `Report_Analyses` if not in environment.
if (!exists("school.aggregates")) {
  if (!exists("Report_Analyses")) {
    stop("Script assumes you have loaded or created a 'Report_Analyses' object.")
  } else {
    school.aggregates <- Report_Analyses[["Summary_Tables"]][[assessment]][["SCHOOL_NUMBER"]][["Academic_Impact"]]
  }
  if (is.null(school.aggregates)) stop("A 'school.aggregates' object must be loaded or available in 'Report_Analyses$Summary_Tables[[assessment]]$SCHOOL_NUMBER$Academic_Impact'")
}

###   Load required packages and custom functions
require(SGP)
require(data.table)

###   Get general variables from `params`
current.year <- tail(params[["years"]][[assessment]], 1)
prior.year   <- tail(params[["years"]][[assessment]], 2)[-2]

content.areas <- params[["GL_subjects"]][[assessment]]

subgroup.min.size <- 20 # params[["min.size.school"]]
test.abv <- params[["test.abv"]][[assessment]]


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
# N_COUNT <- plot_data_wide[!is.na(GES_MEDIAN_SGP_2021), .N, keyby = c("CONTENT_AREA", "COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021")]
# na.omit(plot_data_wide[!is.na(GES_MEDIAN_SGP_2021), as.list(round(summary(GES_MEDIAN_SGP_2021), 2)), keyby = c("CONTENT_AREA", "COVID_ACADEMIC_IMPACT_SGP_DIFF_2021")][N_COUNT])
# na.omit(plot_data_wide[!is.na(GES_MEDIAN_SGP_2021), as.list(round(summary(GES_MEDIAN_SGP_ADJUSTED_2021), 2)), keyby = c("CONTENT_AREA", "COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021")][N_COUNT])

###   Check agreement on impact measures
# table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_DIFF_2021, CONTENT_AREA])
# table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021, CONTENT_AREA])
# table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2021, CONTENT_AREA])
# table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021, CONTENT_AREA])
#
# table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021, COVID_ACADEMIC_IMPACT_SGP_DIFF_2021])
# table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2021])
# table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021]) # most agreement between the 2 Adjusted measures
# table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2021])

### Create bubble plots for state of Indiana

impact.measures <- c("COVID_ACADEMIC_IMPACT_SGP_DIFF_2021", "COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021",
                    "COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2021", "COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021")
tmp.cross.tabs.impact <- list()

for (impact.measure.iter in impact.measures) {
impact.measure.scale <- ifelse(grepl("SGP_DIFF", impact.measure.iter), 1, 2)
impact.measure.scale <- list(c("Improvement (> 5)", "Modest to None (-5 to 5)", "Moderate (-15 to -5)", "Large (-25 to -15)", "Severe (< -25)"),
                             c("Improvement (> 0.2)", "Modest to None (-0.2 to 0.2)", "Moderate (-0.2 to -0.5)", "Large (-0.5 to -0.8)", "Severe (< -0.8)"))[[impact.measure.scale]]

  for (content_area.iter in content.areas) {
    # tmp.school.data.wide[CONTENT_AREA==content_area.iter]$COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021))
    tmp.school.data.wide <- plot_data_wide[CONTENT_AREA==content_area.iter & COUNT_SGP_2021 >= subgroup.min.size & !is.na(get(impact.measure.iter))]
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
