#####
###   From 01-Participation_IN.Rmd
#####

###   Load formated Report_Data
if (!exists("Report_Data"))  load("../Data/Report_Data.Rdata")

###   Locate the "Universal_Content" directory (assume woring directory is ./Documentation)
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

###   Declare an assessment flavor --  need to extend to WIDA
if (!exists("assessment")) assessment <- "State_Assessment"

# Next Steps
  # A. Set up a list object, participation, then write into the object all of the
  #    key outputs, including:
  #   A. Table of all relevant intersections of the data
  #     A1. Build out a number of "checks" that correspond to
  #   B. Schools that are not the "same", using a differences in differences approach
  #   C. Regressions & Predictions, possibly including:
  #       C1. methods from the VIM package
  #       C2. Characterization in terms of MCAR, MAR or MNAR
  #       C3. Create flag for interaction with CONTENT_AREA
  #   D. Executive Summary based on checks of A-C above
  #       D1. Create canned text that notes whether non-participation is associated with
  #           student-characteristics and/or schools/districts and school/districts characteristics
  #   E. Appendices with group level plots
  #   F. Add this analysis as a Step 3E into the current work flow


###   setup_libraries_functions chunk

#A. Libraries
# source(file.path(universal.content.path, "Learning_Loss_Analysis", "Meta_Data", "Report_Packages.R"))

#B. Functions
#B1. Generate GG Plot Colors
gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }


#E. Create Dummy Variables  ------------------------------------------------

###   Looks like this is ignored now...  Correct me if I'm wrong!

if (FALSE) {
  ell.dummies <- params$demos$values[["ENGLISH_LANGUAGE_LEARNER_STATUS"]][[2]]

  params$demos$dummies <- list("ETHNICITY" = params$demos$values[["ETHNICITY"]],
                               "SOCIO_ECONOMIC_STATUS" = params$demos$values[["SOCIO_ECONOMIC_STATUS"]][[2]],
                               "ENGLISH_LANGUAGE_LEARNER_STATUS" = ell.dummies,
                               "SPECIAL_EDUCATION_STATUS" = params$demos$values[["SPECIAL_EDUCATION_STATUS"]][[2]],
                               "GENDER" = params$demos$values[["GENDER"]][[2]])

  Report_Data[[assessment]][,(params$demos$dummies$ETHNICITY) := lapply(params$demos$dummies$ETHNICITY, function(x) ETHNICITY  == x)]
  Report_Data[[assessment]][,(params$demos$dummies$SOCIO_ECONOMIC_STATUS) := lapply(params$demos$dummies$SOCIO_ECONOMIC_STATUS, function(x) SOCIO_ECONOMIC_STATUS  == x)]
  Report_Data[[assessment]][,(params$demos$dummies$ENGLISH_LANGUAGE_LEARNER_STATUS) := lapply(params$demos$dummies$ENGLISH_LANGUAGE_LEARNER_STATUS, function(x) ENGLISH_LANGUAGE_LEARNER_STATUS  == x)]
  Report_Data[[assessment]][,(params$demos$dummies$IEP_STATUS) := lapply(params$demos$dummies$IEP_STATUS, function(x) IEP_STATUS  == x)]
  Report_Data[[assessment]][,(params$demos$dummies$GENDER) := lapply(params$demos$dummies$GENDER, function(x) GENDER  == x)]

  params$demos$dummies <- as.character(unlist(params$demos$dummies))
  Report_Data[[assessment]][ ,(params$demos$dummies) := lapply(.SD, as.numeric),
                .SDcols = params$demos$dummies]

  names(Report_Data[[assessment]]) <- gsub(" ", "_", names(Report_Data[[assessment]]))
  names(Report_Data[[assessment]]) <- gsub(":", "", names(Report_Data[[assessment]]))
  params$demos$dummies <- gsub(" ", "_",  params$demos$dummies)
  params$demos$dummies <- gsub(":", "",  params$demos$dummies)
}

###   setup_participation chunk

source(file.path(universal.content.path, "Learning_Loss_Analysis", "Functions", "func_fullTable.R")) # only uses tmprms$demos$names
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Functions", "func_logistic_regressions.R")) # tmprms$demos$names, tmprms$demos$values, tmprms$years

if (!exists("Report_Analyses")) Report_Analyses <- list()

# A. Participation Table
Report_Analyses[["participation"]][[assessment]][["full_table"]] <-
    fullParticipationTable(long_data = Report_Data[[assessment]], params = tmprms)

# B. Logistic Regression
Report_Analyses[["participation"]][[assessment]][["onelevel_logistic"]] <-
    logisticModels(long_data = Report_Data[[assessment]],
                   params = tmprms,
                   assessment_type = assessment,
                   extra_interactions=FALSE)

# C. Capture "params" used to produce tables/regressions
Report_Analyses[["participation"]][[assessment]][["params"]] <- tmprms

#
# p <- Report_Analyses[["participation"]][[assessment]][["full_table"]]
# p[,NUMBER_NOT_TESTED := NUMBER_STUDENTS - NUMBER_TESTED]
#
