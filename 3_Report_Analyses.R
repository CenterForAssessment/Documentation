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
# load("params_dry_run.rda")
# if (!exists("params")) {
#   ###   Load required packages and custom functions
#   require(SGP)
#   require(Literasee)
#   if (exists("assessment")) {tmp.assessment <- assessment; chg.assess <- TRUE} else chg.assess <- FALSE
#   setwd("..")
#   source("./Documentation/4_Make_Configs.R")
#   setwd("./Documentation")
#   params <- report.config$params
#   source(knitr::purl(file.path(universal.content.path, "Learning_Loss_Analysis", "Child_RMD", "params.Rmd"), quiet=TRUE))
#   file.remove("params.R")
#   if (chg.assess) tmp.assessment -> assessment
# }


#####
###   State_Assessment
#####

###   Declare an assessment flavor (or loop around source(...) files)
assessment <- "State_Assessment"

###   (School Level) Summary Tables
require(cfaTools)
Report_Analyses[["Summary_Tables"]][[assessment]][["Academic_Impact"]][["SCHOOL_NUMBER"]] <-
      academicImpactSummary(
          sgp_data = Report_Data[[assessment]],
          state="PARCC",
          current_year="2020_2021.2", # tail(params[["years"]][[assessment]], 1),
          prior_year="2018_2019.2", # tail(params[["years"]][[assessment]], 2)[-2],
          content_areas=c("ELA", "MATHEMATICS", "ALGEBRA_I", "ALGEBRA_II", "GEOMETRY"), # params[["GL_subjects"]][[assessment]],
          all_grades=c(3:8, 10, 11, "EOCT"), # params[["grades"]][[assessment]],
          sgp_grades=c(5:8, 10, 11, "EOCT"), # params[["sgp.grades"]][[assessment]],
          aggregation_group="SCHOOL_NUMBER")[["SCHOOL_NUMBER"]]

Report_Analyses[["Summary_Tables"]][[assessment]][["Academic_Impact"]][["SCHOOL_NUMBER_by_GRADE"]] <-
      academicImpactSummary(
          sgp_data = Report_Data[[assessment]],
          state="PARCC",
          current_year="2020_2021.2", # tail(params[["years"]][[assessment]], 1),
          prior_year="2018_2019.2", # tail(params[["years"]][[assessment]], 2)[-2],
          content_areas=c("ELA", "MATHEMATICS", "ALGEBRA_I", "ALGEBRA_II", "GEOMETRY"), # params[["GL_subjects"]][[assessment]],
          all_grades=c(3:11, "EOCT"), # params[["grades"]][[assessment]],
          sgp_grades=c(5:11, "EOCT"), # params[["sgp.grades"]][[assessment]],
          aggregation_group=c("SCHOOL_NUMBER", "GRADE"))[["SCHOOL_NUMBER_by_GRADE"]]



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
