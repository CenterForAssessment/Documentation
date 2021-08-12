#####
###   Run any external analyses for any/all elements of the `Report_Data` object
###   for the Learning Loss Analysis Report and house the results in a seperate
###   `Report_Analyses` object that is also passed to the rendered report.
###   As with `Report_Data`, this can include `State_Assessment`, `College_Entrance`,
###   `ELP_Assessment` and (potentially multiple) `Interim_Assessment` branches
###   with multiple analysis results types common to them all.
###   For example, all assessments may have a `participation` slot, but only one
###   may have a `multiple_imputation` analysis.
#####


###   Set up your R working directory
setwd("./Documentation")

###   Locate the "Universal_Content" directory
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

###   Load packages used in the Report Analyses (and install/update as necessary)
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Meta_Data", "Report_Packages.R"))

###   Load formated Report_Data from `2_Report_Data.R`
load("../Data/Report_Data.Rdata")

###   Either load an existing `Report_Analyses` object
# load("../Data/Report_Analyses.Rdata")
  ##  or create a new one and run all analyses ##
     Report_Analyses <- list()


#####
###   State_Assessment
#####

###   Declare an assessment flavor (or loop around source(...) files)
assessment <- "State_Assessment"

###   Missing data visualizations
"Report_Analyses/CO_Missing_Data_Viz_2021.R"


###   Quantile Shift Analyses

load("params_dry_run.rda")
beeswarm.plots <- TRUE
hsf.analysis <- TRUE
between.inst.ges <- TRUE

for (assessment in c("State_Assessment", "ELP_Assessment")) {
    source("Report_Analyses/IN_Quantile_Shift_Effects_2021.R")
}

#####
###   College_Entrance
#####



#####
###   ELP_Assessment
#####



#####
###   Interim_Assessment
#####



#####
###   Combine all data analyses into `Report_Analyses` and Save
#####

###  Do we need this monster?  Not seeing it used anywhere, so not keeping it...
# Report_Analyses[["participation"]][[assessment]][["onelevel_logistic"]] <- NULL

save(Report_Analyses, file = "../Data/Report_Analyses.Rdata")
setwd("..")
