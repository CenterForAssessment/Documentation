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
State_Assessment_LONG_Data <- data.table::copy(Demonstration_COVID_SGP@Data)[VALID_CASE == "VALID_CASE" & YEAR %in% c("2019", "2021") & CONTENT_AREA %in% c("ELA", "MATHEMATICS")] # & SCHOOL_ENROLLMENT_STATUS == "Enrolled School: Yes"]



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
###   Combine all data sources into `Report_Data` and Save
#####

Report_Data <- vector("list", 4);
names(Report_Data) <- c("State_Assessment", "College_Entrance", "ELP_Assessment", "Interim_Assessment")

Report_Data[["State_Assessment"]] <- copy(State_Assessment_LONG_Data); rm(State_Assessment_LONG_Data)

if (!dir.exists(file.path("..", "Data"))) dir.create(file.path("..", "Data"))
save(Report_Data, file = file.path("..", "Data", "Report_Data.Rdata"))

setwd("..")
