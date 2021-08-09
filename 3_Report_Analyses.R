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
