################################################################################
###                                                                          ###
###    Investigation of Pandemic Academic Impact by School -- arrowPlots     ###
###                                                                          ###
################################################################################

if (!exists("school.aggregates")) {
  
}

###   Locate the "Universal_Content" directory (assume woring directory is ./Documentation)
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

###   Load required packages and custom functions
require(ggplot2)
require(gghighlight)
require(dplyr, warn.conflicts = FALSE)

source(file.path(universal.content.path, "Learning_Loss_Analysis", "Functions", "shift_legend.R"))


###   Create wide plot_data

plot_data_wide <- dcast(school.aggregates, SCHOOL_NUMBER + CONTENT_AREA ~ YEAR,
                        value.var = c("COUNT_SGP", "MEDIAN_SGP_BASELINE", "MEDIAN_SGP_PRIOR_2YEAR", "PERCENT_PROFICIENT",
                                      "MEAN_SCALE_SCORE_STANDARDIZED", "MEAN_SCALE_SCORE_PRIOR_STANDARDIZED",
                                      "GES_MEDIAN_SGP", "GES_MEDIAN_SGP_ADJUSTED", "GES_MEDIAN_SSS", "GES_MEDIAN_SSS_ADJUSTED",
                                      "MSGP_BASELINE_DIFFERENCE_UNCORRECTED", "MSGP_BASELINE_DIFFERENCE_ADJUSTED",
                                      "MSSS_DIFFERENCE_UNCORRECTED", "MSSS_DIFFERENCE_ADJUSTED",
                                      "COVID_ACADEMIC_IMPACT_SGP_DIFF", "COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ",
                                      "COVID_ACADEMIC_IMPACT_SSS_DIFF", "COVID_ACADEMIC_IMPACT_SSS_DIFF_ADJ",
                                      "COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP", "COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ",
                                      "COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS", "COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ"))

###   Re-label Academic Impact variables from the original
gen.levs <- c("Improvement", "Modest to None", "Moderate", "Large", "Severe")

##    Growth
grth.labs <- c("Improved Growth", "Modest Growth Increase or Decrease", "Moderate Growth Decrease", "Large Growth Decrease", "Severe Growth Decrease")

plot_data_wide[, COVID_ACADEMIC_IMPACT_SGP_DIFF_2019 :=
                  factor(COVID_ACADEMIC_IMPACT_SGP_DIFF_2019, levels = gen.levs, labels = grth.labs, ordered=TRUE)]
plot_data_wide[, COVID_ACADEMIC_IMPACT_SGP_DIFF_2021 :=
                  factor(COVID_ACADEMIC_IMPACT_SGP_DIFF_2021, levels = gen.levs, labels = grth.labs, ordered=TRUE)]

plot_data_wide[, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2019 :=
                  factor(COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2019, levels = gen.levs, labels = grth.labs, ordered=TRUE)]
plot_data_wide[, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021 :=
                  factor(COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ_2021, levels = gen.levs, labels = grth.labs, ordered=TRUE)]

plot_data_wide[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2019 :=
                  factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2019, levels = gen.levs, labels = grth.labs, ordered=TRUE)]
plot_data_wide[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2021 :=
                  factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_2021, levels = gen.levs, labels = grth.labs, ordered=TRUE)]

plot_data_wide[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2019 :=
                  factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2019, levels = gen.levs, labels = grth.labs, ordered=TRUE)]
plot_data_wide[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021 :=
                  factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021, levels = gen.levs, labels = grth.labs, ordered=TRUE)]

##    Status
stat.labs <- c("Improved Status", "Modest Status Increase or Decrease", "Moderate Status Decrease", "Large Status Decrease", "Severe Status Decrease")

plot_data_wide[, COVID_ACADEMIC_IMPACT_SSS_DIFF_2019 :=
                  factor(COVID_ACADEMIC_IMPACT_SSS_DIFF_2019, levels = gen.levs, labels = stat.labs, ordered=TRUE)]
plot_data_wide[, COVID_ACADEMIC_IMPACT_SSS_DIFF_2021 :=
                  factor(COVID_ACADEMIC_IMPACT_SSS_DIFF_2021, levels = gen.levs, labels = stat.labs, ordered=TRUE)]

plot_data_wide[, COVID_ACADEMIC_IMPACT_SSS_DIFF_ADJ_2019 :=
                  factor(COVID_ACADEMIC_IMPACT_SSS_DIFF_ADJ_2019, levels = gen.levs, labels = stat.labs, ordered=TRUE)]
plot_data_wide[, COVID_ACADEMIC_IMPACT_SSS_DIFF_ADJ_2021 :=
                  factor(COVID_ACADEMIC_IMPACT_SSS_DIFF_ADJ_2021, levels = gen.levs, labels = stat.labs, ordered=TRUE)]

plot_data_wide[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_2019 :=
                  factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_2019, levels = gen.levs, labels = stat.labs, ordered=TRUE)]
plot_data_wide[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_2021 :=
                  factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_2021, levels = gen.levs, labels = stat.labs, ordered=TRUE)]

plot_data_wide[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ_2019 :=
                  factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ_2019, levels = gen.levs, labels = stat.labs, ordered=TRUE)]
plot_data_wide[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ_2021 :=
                  factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ_2021, levels = gen.levs, labels = stat.labs, ordered=TRUE)]

# table(plot_data_wide[COUNT_SGP_2021 >= 20, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ_2021, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021]) # most agreement between the 2 Adjusted measures


plot.dir <- file.path("assets", "Rplots", "arrowPlots", assessment)
if (!dir.exists(file.path(plot.dir, "School_Level"))) dir.create(file.path(plot.dir, "School_Level"), recursive=TRUE)

for (CA in content.areas) {
  tmp.subj <- SGP::capwords(CA)
  title.main <- paste0(test.abv, " ", gsub("ematics", "", tmp.subj), ": ") # ILEARN Math:

  ###    Highlight Growth

  g <- ggplot(plot_data_wide[CONTENT_AREA == CA & COUNT_SGP_2021 > 19 & !is.na(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021)],
              aes(color=COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021)) +
       geom_segment(aes(x = MEDIAN_SGP_BASELINE_2019, y = MEAN_SCALE_SCORE_STANDARDIZED_2019,
                        xend = MEDIAN_SGP_BASELINE_2021, yend = MEAN_SCALE_SCORE_STANDARDIZED_2021),
                    arrow = arrow(type = "closed", ends = "last", length=unit(0.035, "inches"))) +
                    xlab("Median Baseline SGP") + ylab("Mean Standardized Scale Score") +
       scale_colour_viridis_d(option = "C") + # viridis_option
       theme(panel.background = element_rect(colour="#515151", fill="#515151"),
             panel.border = element_blank(),
             # legend.direction = "horizontal",
             legend.key = element_rect(colour="#515151", fill="grey"),
             legend.title = element_text(size = 12, colour = "black"),
             legend.text = element_text(size = 10, colour = "black"),
             plot.title = element_text(size = 16),
             axis.title.x = element_text(size = 14),
             axis.text = element_text(size = 11, colour = "black"),
             axis.title.y = element_text(size = 14),
             strip.text = element_text(size = 10, colour = "black"))

  gr.only <- g + gghighlight(use_direct_label = FALSE, unhighlighted_params = list(alpha = 0.2)) +
                 facet_wrap(~ COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021, nrow=3) +
                 labs(title = paste0(title.main, "School Level COVID Growth Impact."), color="COVID Growth Impact")

  st.by.gr<- g + gghighlight(use_direct_label = FALSE, unhighlighted_params = list(alpha = 0.2)) +
                 facet_wrap(~ COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ_2021, nrow=3) +
                 labs(title = paste0(title.main, "School Status Impact (with Growth)."), color="COVID Growth Impact")

  tmp.filename <- file.path(plot.dir, "School_Level", paste0(gsub("ematics", "", tmp.subj), "_Impact_Facet_Growth_Only", ".png"))
  ggsave(filename = tmp.filename, plot=shift_legend(gr.only), width=6, height = 7, dpi=150, units="in", bg="transparent")

  tmp.filename <- file.path(plot.dir, "School_Level", paste0(gsub("ematics", "", tmp.subj), "_Impact_Facet_Status_by_Growth", ".png"))
  ggsave(filename = tmp.filename, plot=shift_legend(st.by.gr), width=6, height = 7, dpi=150, units="in", bg="transparent")

  ###    Highlight Status

  s <- ggplot(plot_data_wide[CONTENT_AREA == CA & COUNT_SGP_2021 > 19 & !is.na(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021)],
              aes(color=COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ_2021)) +
       geom_segment(aes(x = MEDIAN_SGP_BASELINE_2019, y = MEAN_SCALE_SCORE_STANDARDIZED_2019,
                        xend = MEDIAN_SGP_BASELINE_2021, yend = MEAN_SCALE_SCORE_STANDARDIZED_2021),
                    arrow = arrow(type = "closed", ends = "last", length=unit(0.035, "inches"))) +
                    xlab("Median Baseline SGP") + ylab("Mean Standardized Scale Score") +
       scale_colour_viridis_d(option = "C") + # viridis_option
       theme(panel.background = element_rect(colour="#515151", fill="#515151"),
             panel.border = element_blank(),
             # legend.direction = "horizontal",
             legend.key = element_rect(colour="#515151", fill="grey"),
             legend.title = element_text(size = 12, colour = "black"),
             legend.text = element_text(size = 10, colour = "black"),
             plot.title = element_text(size = 16),
             axis.title.x = element_text(size = 14),
             axis.text = element_text(size = 11, colour = "black"),
             axis.title.y = element_text(size = 14),
             strip.text = element_text(size = 10, colour = "black"))

  st.only <- s + gghighlight(use_direct_label = FALSE, unhighlighted_params = list(alpha = 0.2)) +
                 facet_wrap(~ COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ_2021, nrow=3) +
                 labs(title = paste0(title.main, "School Level COVID Status Impact."), color="COVID Status Impact")

  gr.by.st<- s + gghighlight(use_direct_label = FALSE, unhighlighted_params = list(alpha = 0.2)) +
                 facet_wrap(~ COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021, nrow=3) +
                 labs(title = paste0(title.main, "School Growth Impact (with Status)."), color="COVID Status Impact")

  tmp.filename <- file.path(plot.dir, "School_Level", paste0(gsub("ematics", "", tmp.subj), "_Impact_Facet_Status_Only", ".png"))
  ggsave(filename = tmp.filename, plot=shift_legend(st.only), width=6, height = 7, dpi=150, units="in", bg="transparent")

  tmp.filename <- file.path(plot.dir, "School_Level", paste0(gsub("ematics", "", tmp.subj), "_Impact_Facet_Growth_by_Status", ".png"))
  ggsave(filename = tmp.filename, plot=shift_legend(gr.by.st), width=6, height = 7, dpi=150, units="in", bg="transparent")
}


#####
###   Random initial/trail code
#####

# ###   Pct Proficient
# p <- ggplot(plot_data_wide[CONTENT_AREA == "MATHEMATICS" & COUNT_SGP_2021 > 19 & !is.na(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021)],
#             aes(x = MEDIAN_SGP_BASELINE_2019, y = PERCENT_PROFICIENT_2019, xend = MEDIAN_SGP_BASELINE_2021, yend = PERCENT_PROFICIENT_2021)) +
#      geom_segment(aes(color=COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021),
#             arrow = arrow(type = "closed", ends = "last", length=unit(0.075, "inches"))) +
#             xlab("Median Baseline SGP") + ylab("Percent Proficient")
#
# ##    Add style
# p1 <- p + scale_colour_viridis_d(option = "C") + # viridis_option
#   theme(panel.background = element_rect(colour="#515151", fill="#515151"),
#         panel.border = element_blank(),
#         # legend.direction = "horizontal",
#         legend.key = element_rect(colour="#515151", fill="grey"),
#         legend.title = element_text(size = 20, colour = "black"),
#         legend.text = element_text(size = 16, colour = "black"),
#         plot.title = element_text(size = 22),
#         axis.title.x = element_text(size = 18),
#         axis.text = element_text(size = 16, colour = "black"),
#         axis.title.y = element_text(size = 18))
#
# p2 <- p1 + gghighlight(use_direct_label = FALSE, unhighlighted_params = list(alpha = 0.2)) + facet_wrap(~ COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021, nrow=3) +
#            labs(title = "ILEARN Math: School Level COVID Growth Impact.", color="COVID Growth Impact")
#
# p2b<- p1 + gghighlight(use_direct_label = FALSE, unhighlighted_params = list(alpha = 0.2)) + facet_wrap(~ COVID_ACADEMIC_IMPACT_GES_MEDIAN_SSS_ADJ_2021, nrow=3) +
#            labs(title = "ILEARN Math: School Level COVID Status Impact (Growth Highlighted).", color="COVID Growth Impact")
# shift_legend(p2)
# shift_legend(p2b)
#
#
# p3 <- p1 + gghighlight(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ_2021 == "Moderate Growth Decrease", use_direct_label = FALSE,
#               unhighlighted_params = list(colour = NULL, alpha = 0.4)) +
#            labs(title = "ILEARN Math: School Level COVID Growth Impact.", color="COVID Growth Impact")
# p3
#
#
# school.aggregates[, COVID_ACADEMIC_IMPACT_SGP_DIFF :=
#                     factor(COVID_ACADEMIC_IMPACT_SGP_DIFF,
#                            levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]
#
# school.aggregates[, COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ :=
#                     factor(COVID_ACADEMIC_IMPACT_SGP_DIFF_ADJ,
#                            levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]
#
# school.aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP :=
#                     factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP,
#                            levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]
#
# school.aggregates[, COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ :=
#                     factor(COVID_ACADEMIC_IMPACT_GES_MEDIAN_SGP_ADJ,
#                            levels=c("Improvement", "Modest to None", "Moderate", "Large", "Severe"), ordered=TRUE)]
