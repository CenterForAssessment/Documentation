################################################################################
###                   --  Create RTM diagnostic plots  --                    ###
###      An accessory script to IN_Academic_Impact_School_bubblePlot.R       ###
################################################################################

range(c(plot_data_wide[COUNT_SGP_2021 > 19, MSGP_BASELINE_DIFFERENCE_UNCORRECTED_2021], plot_data_wide[COUNT_SGP_2021 > 19, MSGP_BASELINE_DIFFERENCE_ADJUSTED_2021]), na.rm=TRUE)
diff.range <- c(-70, 50)

impact.measure.scale <- c("Improvement", "Modest to None", "Moderate", "Large", "Severe") # "Static or Improved", "Insignificant"...
impact.measures <- c("COVID_ACADEMIC_IMPACT_SGP_DIFF_2021", "COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021",
                     "COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2021", "COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021")

for (impact.measure.iter in impact.measures) {
# for (year.iter in c(prior.year, current.year)) {
  year.iter <- "2021"
  # impact.measure.iter <- paste0("COVID_ACADEMIC_IMPACT_SGP_DIFF_", year.iter)
  for (content_area.iter in c("ELA", "MATHEMATICS")) {
  for (diff.measure in c("MSGP_BASELINE_DIFFERENCE_UNCORRECTED_", "MSGP_BASELINE_DIFFERENCE_ADJUSTED_")) {
    tmp.school.data.wide <- plot_data_wide[CONTENT_AREA==content_area.iter & get(paste0("COUNT_SGP_", year.iter)) >= 20 & !is.na(get(impact.measure.iter)) & !is.na(get(paste0(diff.measure, year.iter)))]

    for (covid.iter in c("Improvement", "Modest to None", "Moderate", "Large", "Severe")) {

      ### message
      bPlot.message <- c("grid.lines(x=unit(50, 'native'), y=c(0.03,0.97), gp=gpar(col='grey40', lwd=1.25, lty=2, alpha=0.5))")

      bubblePlot(
        bubble_plot_data.X=tmp.school.data.wide[[paste0("MEDIAN_SGP_PRIOR_2YEAR_", year.iter)]],
        bubble_plot_data.Y=tmp.school.data.wide[[paste0(diff.measure, year.iter)]],
        bubble_plot_data.SUBSET=which(tmp.school.data.wide[[impact.measure.iter]]==covid.iter),
        bubble_plot_data.INDICATE=NULL,
        bubble_plot_data.BUBBLE_CENTER_LABEL=NULL,
        bubble_plot_data.SIZE=tmp.school.data.wide[[paste0("COUNT_SGP_", year.iter)]],
        bubble_plot_data.LEVELS=tmp.school.data.wide[[impact.measure.iter]],
        bubble_plot_labels.X=c("Prior MSGP", paste(year.iter, "Median SGP 2 Years Prior")),
        bubble_plot_labels.Y=c("Difference", paste(capwords(diff.measure, special.words = "MSGP"), year.iter)),
        bubble_plot_labels.SIZE=c(50, 100, 250, 500),
        bubble_plot_labels.LEVELS=impact.measure.scale,
        bubble_plot_titles.MAIN=paste0(test.abv, ": ", capwords(content_area.iter)),
        bubble_plot_titles.SUB1="School Level COVID Academic Impact",
        bubble_plot_titles.SUB2=paste(year.iter, "Change in Growth by Initial MSGP"),
        bubble_plot_titles.LEGEND1="School Size",
        bubble_plot_titles.LEGEND2_P1="COVID Impact",
        bubble_plot_titles.LEGEND2_P2=paste(year.iter, "SGP -", SGP:::yearIncrement(year.iter, -2), "SGP"),
        bubble_plot_configs.BUBBLE_MIN_MAX=c(0.04, 0.11),
        bubble_plot_configs.BUBBLE_X_TICKS=seq(0,100,10),
        bubble_plot_configs.BUBBLE_X_TICKS_SIZE=c(rep(0.6, 5), 0.8, rep(0.6, 5)),
        bubble_plot_configs.BUBBLE_Y_TICKS=seq(diff.range[1], diff.range[2], 10),
        bubble_plot_configs.BUBBLE_Y_TICKS_SIZE=rep(0.6, 11),
        bubble_plot_configs.BUBBLE_PLOT_BACKGROUND_LABELS=c("Prior Growth", "Change in Growth"),
        bubble_plot_configs.BUBBLE_SUBSET_INCREASE=0.00,
        bubble_plot_configs.BUBBLE_COLOR=NULL,
        bubble_plot_configs.BUBBLE_SUBSET_ALPHA=list(Transparent=0.3, Opaque=0.9),
        bubble_plot_configs.BUBBLE_PLOT_DEVICE="PDF",
        bubble_plot_configs.BUBBLE_PLOT_FORMAT="print",#"presentation",
        bubble_plot_configs.BUBBLE_PLOT_LEGEND="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_TITLE="TRUE",
        bubble_plot_configs.BUBBLE_PLOT_SUMMARY_STATISTICS=FALSE,
        bubble_plot_configs.BUBBLE_PLOT_EXTRAS=bPlot.message,
        bubble_plot_configs.BUBBLE_PLOT_NAME=paste(paste(test.abv, capwords(content_area.iter), year.iter, "Academic_Impact_by_School", tail(strsplit(diff.measure, "_")[[1]], 1), covid.iter, sep="_"), ".pdf", sep=""),
        bubble_plot_configs.BUBBLE_PLOT_PATH=file.path("assets", "Rplots", "bubblePlots", test.abv, "RTM_Diagnostics", impact.measure.iter),
        bubble_plot_pdftk.CREATE_CATALOG=FALSE)
    }
  }}
}
# }
