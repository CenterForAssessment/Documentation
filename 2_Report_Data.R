#####
###   Create the `Report_Data` object with data-(sub)sets for the Learning Loss Analysis Report.
###   House all data in one object that is passed to the report (and in some cases
###   used to create the report `params`). Create/format/alter/augment one or more
###   raw data sets including `State_Assessment`, `College_Entrance`, `ELP_Assessment`
###   and (potentially multiple) `Interim_Assessment` data objects.
#####

#####
###   State_Assessment
#####

###   Load/Format/Subset Report Data
load("../Data/Demonstration_COVID_SGP_2021_STEP_3c.Rdata")
Report_Data <- data.table::copy(Demonstration_COVID_SGP@Data)[VALID_CASE == "VALID_CASE" & YEAR %in% c("2019", "2021") & CONTENT_AREA %in% c("ELA", "MATHEMATICS")] # & SCHOOL_ENROLLMENT_STATUS == "Enrolled School: Yes"]



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
###   Save Report_Data
#####

save(Report_Data, file = "./Data/Report_Data.Rdata")
