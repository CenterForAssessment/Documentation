#####
###   Create the `Report_Data` object with data-(sub)sets for the Learning Loss Analysis Report.
###   House all data in one object that is passed to the report (and in some cases
###   used to create the report `params`). Create/format/alter/augment one or more
###   raw data sets including `State_Assessment`, `College_Entrance`, `ELP_Assessment`
###   and (potentially multiple) `Interim_Assessment` data objects.
#####

###   Set R working directory to the Documentation folder
setwd("./Documentation")

###   Load required packages
require(SGP)
require(data.table)


#####
###   State_Assessment
#####

###   Load/Format/Subset Report Data
load(file.path("..", "..", "..", "..", "Indiana", "Data", "Indiana_SGP_LONG_Data.Rdata"))
setNamesSGP(Indiana_SGP_LONG_Data)

###   Subset only variables and GRADE levels relevant to 2021
vars.to.keep <- c(
  "VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "GRADE",
  "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED",
  "SCALE_SCORE_PRIOR_BASELINE", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE",
  "SCALE_SCORE_ORIGINAL", "ACHIEVEMENT_LEVEL_ORIGINAL", "ACHIEVEMENT_LEVEL_PRIOR",
  "SGP", "SGP_BASELINE", "SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE",
  "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME",
  "SPECIAL_EDUCATION_STATUS", "ENGLISH_LANGUAGE_LEARNER_STATUS", "SOCIO_ECONOMIC_STATUS", "GENDER", "ETHNICITY")

Indiana_SGP_LONG_Data <- Indiana_SGP_LONG_Data[GRADE %in% 3:8, ..vars.to.keep]


#####
###   College_Entrance
#####



#####
###   ELP_Assessment
#####

###   Load/Format/Subset Report Data
load(file.path("..", "..", "..", "..", "WIDA_IN", "Data", "WIDA_IN_SGP_LONG_Data.Rdata"))

###   Subset only variables relevant to Learning Loss Report
elp.vars.to.keep <- c(
  "VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "GRADE",
  "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL_ORIGINAL",
  "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED", "ACHIEVEMENT_LEVEL_PRIOR",
  "SCALE_SCORE_PRIOR_BASELINE", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE",
  "SGP", "SGP_BASELINE", "SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE",
  "DISTRICT_NUMBER", "SCHOOL_NUMBER")

WIDA_IN_SGP_LONG_Data <- WIDA_IN_SGP_LONG_Data[, ..elp.vars.to.keep]


#####
###   Interim_Assessment
#####

###   Load and format NWEA files -- in Learning_Loss_Analysis/Data/Base_Files/
tmp.fall <- fread("../Data/Base_Files/NWEA_FALL_2020_2021.txt")
tmp.winter <- fread("../Data/Base_Files/NWEA_WINTER_2020_2021.txt")
nwea.demogs <- fread("../Data/Base_Files/2021 nwea demographics.csv")
setnames(nwea.demogs, toupper(gsub("_CATEGORY", "", names(nwea.demogs))))

tmp.nwea.data <- rbindlist(list(tmp.fall, tmp.winter))
setkey(tmp.nwea.data, STUDENT_ID)
setkey(nwea.demogs, STUDENT_ID)

Indiana_NWEA_Data_2021 <- nwea.demogs[tmp.nwea.data]
# table(Indiana_NWEA_Data_2021[, ETHNICITY, i.ETHNICITY], exclude=NULL)

## Fill in ETHNICITY provided in external demographics file, but keep/preference it
Indiana_NWEA_Data_2021[is.na(ETHNICITY), ETHNICITY := i.ETHNICITY]
Indiana_NWEA_Data_2021[, i.ETHNICITY := NULL]

setNamesSGP(Indiana_NWEA_Data_2021)

Indiana_NWEA_Data_2021[, YEAR := plyr::mapvalues(TermName,
                            from=c("Fall 2020-2021", "Winter 2020-2021"),
                            to  =c("2021.1", "2021.2"))]

Indiana_NWEA_Data_2021[, CONTENT_AREA := plyr::mapvalues(Subject,
                            from=c("Language Arts", "Mathematics", "Science"),
                            to  =c("ELA", "MATHEMATICS", "SCIENCE"))]

Indiana_NWEA_Data_2021[, GRADE := as.character(GRADE)]
Indiana_NWEA_Data_2021[, VALID_CASE := "VALID_CASE"]

nwea.vars.to.keep <- c(
  "VALID_CASE", "CONTENT_AREA", "Course", "YEAR", "ID", "GRADE", "DISTRICT_NUMBER", "SCHOOL_NUMBER",
  "SPECIAL_EDUCATION_STATUS", "ENGLISH_LANGUAGE_LEARNER_STATUS", "SOCIO_ECONOMIC_STATUS", "GENDER", "ETHNICITY",
  "TestRITScore", "TestStandardError", "TestPercentile",
  grep("ObservedGrowth$", names(Indiana_NWEA_Data_2021), value=TRUE), ##  Is "*ObservedGrowth$" the change score?  Could get prior scale scores from that?
  grep("ConditionalGrowthPercentile", names(Indiana_NWEA_Data_2021), value=TRUE))

Indiana_NWEA_Data_2021 <- Indiana_NWEA_Data_2021[, ..nwea.vars.to.keep]

##    Rename NWEA Growth Percentiles and Change Scores
cgp.orig <- grep("ConditionalGrowthPercentile", names(Indiana_NWEA_Data_2021), value=TRUE)
cgp.names <- gsub("ConditionalGrowthPercentile", "_CGP", cgp.orig)
cgp.names <- gsub("To", "_", cgp.names)
cgp.names <- toupper(cgp.names)

ob.g.orig <- grep("ObservedGrowth$", names(Indiana_NWEA_Data_2021), value=TRUE)
ob.g.names <- gsub("ObservedGrowth", "_OBSERVED_GROWTH", ob.g.orig)
ob.g.names <- gsub("To", "_", ob.g.names)
ob.g.names <- toupper(ob.g.names)

setnames(Indiana_NWEA_Data_2021,
  c(cgp.orig, ob.g.orig, "TestRITScore", "TestStandardError"),
  c(cgp.names, ob.g.names, "SCALE_SCORE", "SCALE_SCORE_CSEM"))

##    Remove all NA columns
non.nas <- names(Indiana_NWEA_Data_2021)[!unlist(lapply(names(Indiana_NWEA_Data_2021), function(f) all(is.na(Indiana_NWEA_Data_2021[,get(f)]))))]
Indiana_NWEA_Data_2021 <- Indiana_NWEA_Data_2021[, ..non.nas]

##   Remove SCIENCE
Indiana_NWEA_Data_2021 <- Indiana_NWEA_Data_2021[!CONTENT_AREA %in% "SCIENCE"]


#####
###   Combine all data sources into `Report_Data` and Save
#####

Report_Data <- vector("list", 4);
names(Report_Data) <- c("State_Assessment", "College_Entrance", "ELP_Assessment", "Interim_Assessment")

Report_Data[["State_Assessment"]] <- copy(Indiana_SGP_LONG_Data); rm(Indiana_SGP_LONG_Data)
Report_Data[["ELP_Assessment"]] <- copy(WIDA_IN_SGP_LONG_Data); rm(WIDA_IN_SGP_LONG_Data)
Report_Data[["Interim_Assessment"]] <- copy(Indiana_NWEA_Data_2021); rm(Indiana_NWEA_Data_2021)

save(Report_Data, file = "../Data/Report_Data.Rdata")
