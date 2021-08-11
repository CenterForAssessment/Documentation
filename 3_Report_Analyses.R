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
load("../Data/Report_Analyses.Rdata")
  ##  or create a new one and run all analyses ##
    #  Report_Analyses <- list()

###   Either load an existing `params` object or create one in source file(s)
load("params_dry_run.rda")


#####
###   State_Assessment
#####



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

save(Report_Analyses, file = "../Data/Report_Analyses.Rdata")
setwd("..")
