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
require(plyr)

Z <- function(data_table, var.to.standardize, reference.year = NULL, rm.na = TRUE) {
  YEAR <- NULL
  x <- data_table[, get(var.to.standardize)]
  if (!is.null(reference.year)){
    y <- data_table[YEAR==reference.year, get(var.to.standardize)]
  } else y <- x
  (x - mean(y, na.rm = rm.na)) / sd(y, na.rm = rm.na)
}


###   Locate the "Universal_Content" directory (assume woring directory is ./Documentation)
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

#####
###   State_Assessment
#####

###   Load/Format/Subset Report Data
load(file.path("..", "..", "..", "..", "Indiana", "Data", "Indiana_SGP_LONG_Data.Rdata"))
setNamesSGP(Indiana_SGP_LONG_Data)

###   Subset only variables and GRADE levels relevant to 2021
demographics <- c("SPECIAL_EDUCATION_STATUS", "ENGLISH_LANGUAGE_LEARNER_STATUS",
                  "SOCIO_ECONOMIC_STATUS", "GENDER", "ETHNICITY")
vars.to.keep <- c(
  "VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "GRADE",
  "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED",
  "SCALE_SCORE_PRIOR_BASELINE", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE",
  "SCALE_SCORE_ORIGINAL", "ACHIEVEMENT_LEVEL_ORIGINAL", "ACHIEVEMENT_LEVEL_PRIOR",
  "SGP", "SGP_BASELINE", "SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE",
  "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME",
  demographics, "MODE_OF_INSTRUCTION")

Indiana_SGP_LONG_Data <- Indiana_SGP_LONG_Data[GRADE %in% 3:8, ..vars.to.keep]

###   Re-factor ACHIEVEMENT_LEVEL_PRIOR to get values for kids with missing scores
Indiana_SGP_LONG_Data[is.na(SCALE_SCORE), VALID_CASE := "VALID_CASE"]

setkey(Indiana_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)
setkey(Indiana_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
Indiana_SGP_LONG_Data[which(duplicated(Indiana_SGP_LONG_Data, by=key(Indiana_SGP_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"]

setkey(Indiana_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE)
setkey(Indiana_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID)
Indiana_SGP_LONG_Data[which(duplicated(Indiana_SGP_LONG_Data, by=key(Indiana_SGP_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"]

Indiana_SGP_LONG_Data <- Indiana_SGP_LONG_Data[VALID_CASE == "VALID_CASE"]

###   Create Lagged Achievement (ORIGINAL and EQUATED) variables that include missing scores (and others potentially)
shift.key <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "VALID_CASE")
setkeyv(Indiana_SGP_LONG_Data, shift.key)

Indiana_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_ORIGINAL_PRIOR := shift(ACHIEVEMENT_LEVEL_ORIGINAL, 1), by = list(ID, CONTENT_AREA)]
Indiana_SGP_LONG_Data[is.na(ACHIEVEMENT_LEVEL_ORIGINAL_PRIOR), ACHIEVEMENT_LEVEL_ORIGINAL_PRIOR := ACHIEVEMENT_LEVEL_PRIOR] # Mainly fill in 2016
# table(Indiana_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR, ACHIEVEMENT_LEVEL_ORIGINAL_PRIOR], exclude=NULL)

# Indiana_SGP_LONG_Data[, ACH_LEV_P2 := as.character(NA)] # for testing - create a alternate version first and check equality for non-NA values
Indiana_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR := shift(ACHIEVEMENT_LEVEL, 1), by = list(ID, CONTENT_AREA)]
# table(Indiana_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR, ACH_LEV_P2], exclude=NULL) # for testing - create a alternate version first and check equality for non-NA values

###   Create Lagged Scale Score (EQUATED and STANDARDIZED) variables that include missing scores (and others potentially)

##    Standardize SCALE_SCORE by CONTENT_AREA and GRADE using 2019 norms
Indiana_SGP_LONG_Data[, SCALE_SCORE_STANDARDIZED := Z(.SD, "SCALE_SCORE", reference.year = "2019"), by = list(CONTENT_AREA, GRADE), .SDcols = c("YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE")]
# Indiana_SGP_LONG_Data[, as.list(round(summary(SCALE_SCORE_STANDARDIZED), 3)), keyby = c("YEAR", "CONTENT_AREA", "GRADE")]

setkeyv(Indiana_SGP_LONG_Data, shift.key)

Indiana_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR_1YEAR", "SCALE_SCORE_PRIOR_2YEAR") := shift(SCALE_SCORE, 1:2), by = list(ID, CONTENT_AREA)]
Indiana_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR", "SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR") := shift(SCALE_SCORE_STANDARDIZED, 1:2), by = list(ID, CONTENT_AREA)]
##    Fix 2021 Lags since no 2020 data:
Indiana_SGP_LONG_Data[YEAR == '2021', SCALE_SCORE_PRIOR_2YEAR := SCALE_SCORE_PRIOR_1YEAR]
Indiana_SGP_LONG_Data[YEAR == '2021', SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR := SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR]
Indiana_SGP_LONG_Data[YEAR == '2021', SCALE_SCORE_PRIOR_1YEAR := NA]
Indiana_SGP_LONG_Data[YEAR == '2021', SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR := NA]
# table(Indiana_SGP_LONG_Data[, YEAR, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# table(Indiana_SGP_LONG_Data[, GRADE, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL) # Remove 2YEAR priors for Grades 3 & 4 - repeaters
Indiana_SGP_LONG_Data[GRADE %in% c(3, 4), SCALE_SCORE_PRIOR_2YEAR := NA]
Indiana_SGP_LONG_Data[GRADE %in% c(3, 4), SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR := NA]
Indiana_SGP_LONG_Data[GRADE == 3, SCALE_SCORE_PRIOR_1YEAR := NA]
Indiana_SGP_LONG_Data[GRADE == 3, SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR := NA]
# table(Indiana_SGP_LONG_Data[YEAR=='2019', is.na(SCALE_SCORE_PRIOR_BASELINE), is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# cor(Indiana_SGP_LONG_Data[, SCALE_SCORE_PRIOR_BASELINE, SCALE_SCORE_PRIOR_2YEAR], use='complete.obs') # Not perfect.  Use BASELINE data when available:
Indiana_SGP_LONG_Data[!is.na(SCALE_SCORE_PRIOR_BASELINE), SCALE_SCORE_PRIOR_2YEAR := SCALE_SCORE_PRIOR_BASELINE]
# Indiana_SGP_LONG_Data[!is.na(SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE), SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR := SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE] # Keep on 2019 scale!

###   Create Prior Score Deciles
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Functions", "Quantile_Cut_Functions.R"))

##    Establish a decile lookup based on 2019 for each CONTENT_AREA/GRADE combination
Indiana_SGP_LONG_Data[, SCALE_SCORE_PRIOR_2YEAR := round(SCALE_SCORE_PRIOR_2YEAR, 0)] # 2016 - 2018 EQUATED scores are not rounded!  Makes for ugly individual_sf tables...
Decile_Lookup <- Indiana_SGP_LONG_Data[YEAR == "2019" & !is.na(SCALE_SCORE_PRIOR_2YEAR),
                            as.list(getQuantcut(SCALE_SCORE_PRIOR_2YEAR)), keyby = c("CONTENT_AREA", "GRADE")]
prior.decile.fcase <- getFCASE("SCALE_SCORE_PRIOR_2YEAR", Decile_Lookup)
Indiana_SGP_LONG_Data[, PRIOR_DECILE_2YEAR := eval(parse(text=prior.decile.fcase))]
Indiana_SGP_LONG_Data[, PRIOR_DECILE_2YEAR := factor(PRIOR_DECILE_2YEAR, levels = paste0("Q", 1:10), labels=paste0("Decile_", 1:10))]

##    Some sanity checks
# table(Indiana_SGP_LONG_Data[, PRIOR_DECILE_2YEAR, GRADE], exclude=NULL)
# table(Indiana_SGP_LONG_Data[, PRIOR_DECILE_2YEAR, YEAR], exclude=NULL)
# table(Indiana_SGP_LONG_Data[, !is.na(SCALE_SCORE_PRIOR_2YEAR), PRIOR_DECILE_2YEAR,], exclude=NULL) # Should be 0 in the TRUE/<NA> cell
# Indiana_SGP_LONG_Data[!is.na(PRIOR_DECILE_2YEAR) & CONTENT_AREA=='MATHEMATICS' & GRADE=='5',
#             as.list(summary(SCALE_SCORE_PRIOR_2YEAR)), keyby = "PRIOR_DECILE_2YEAR"]
# Decile_Lookup[CONTENT_AREA=='MATHEMATICS' & GRADE=='5']

###   From Nathan's
Indiana_SGP_LONG_Data[, FREE_REDUCED_LUNCH_STATUS := plyr::mapvalues(SOCIO_ECONOMIC_STATUS,
                           from=c("Paid meals", "Free meals", "Reduced price meals", "Unknown"),
                           to  =c("Free Reduced Lunch: No", "Free Reduced Lunch: Yes",
                                  "Free Reduced Lunch: Yes", "Unkown"))]

Indiana_SGP_LONG_Data[, ACHIEVEMENT_ProfandAbove := plyr::mapvalues(ACHIEVEMENT_LEVEL,
                           from=c("Above Proficiency", "At Proficiency",
                                  "Approaching Proficiency", "Below Proficiency"),
                           to  =c("Proficient", "Proficient",
                                  "Not Proficient", "Not Proficient"))]

Indiana_SGP_LONG_Data$ACHIEVEMENT_ProfandAbove[is.na(Indiana_SGP_LONG_Data$ACHIEVEMENT_LEVEL)] <- NA

Indiana_SGP_LONG_Data[, PRIOR_ACHIEVEMENT_ProfandAbove := plyr::mapvalues(ACHIEVEMENT_LEVEL_ORIGINAL_PRIOR,
                           from = c("Below/Approaching Proficiency", "Did Not Pass",
                                    "Above Proficiency", "At Proficiency", "Pass", "Pass +"),
                           to  =  c("Not Proficient", "Not Proficient",
                                    "Proficient", "Proficient", "Proficient", "Proficient"))]

###   Remove redundant/replaced variables
Indiana_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED", "SCALE_SCORE_PRIOR_BASELINE", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE") := NULL]

###   For later use with WIDA_IN
###   Only works ~decent~ for grades 3:8... duh!
# Indiana_Demographics <-
#     unique(Indiana_SGP_LONG_Data[VALID_CASE == "VALID_CASE" & YEAR %in% c("2019", "2021"),
#                                   c("ID", "YEAR", ..demographics, "FREE_REDUCED_LUNCH_STATUS")])


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

###   Merge in 2019 and 2020 school/district numbers
Indiana_WIDA_SCORP_NUMS <- fread(file=file.path("..", "..", "..", "..", "WIDA_IN", "Data", "Base_Files", "WIDA_ACCESS_IN_2019_and_2020_with_SCHOOL_and_CORP.csv"))
setNamesSGP(Indiana_WIDA_SCORP_NUMS)
setnames(Indiana_WIDA_SCORP_NUMS, c("Composite_Overall_Scale_Score", "SCHOOL_YEAR_ID"), c("SCALE_SCORE", "YEAR"))
Indiana_WIDA_SCORP_NUMS[, Composite_Overall_Proficiency_Level := NULL]
Indiana_WIDA_SCORP_NUMS[, DISTRICT_NUMBER := as.character(DISTRICT_NUMBER)]
Indiana_WIDA_SCORP_NUMS[, YEAR := as.character(YEAR)]
Indiana_WIDA_SCORP_NUMS[, ID := as.character(ID)]

setkey(WIDA_IN_SGP_LONG_Data, ID, YEAR, SCALE_SCORE)
setkey(Indiana_WIDA_SCORP_NUMS, ID, YEAR, SCALE_SCORE)

WIDA_IN_SGP_LONG_Data <- Indiana_WIDA_SCORP_NUMS[WIDA_IN_SGP_LONG_Data]
WIDA_IN_SGP_LONG_Data[is.na(SCHOOL_NUMBER), SCHOOL_NUMBER := i.SCHOOL_NUMBER]
WIDA_IN_SGP_LONG_Data[is.na(DISTRICT_NUMBER), DISTRICT_NUMBER := i.DISTRICT_NUMBER]
WIDA_IN_SGP_LONG_Data[, i.SCHOOL_NUMBER := NULL]
WIDA_IN_SGP_LONG_Data[, i.DISTRICT_NUMBER := NULL]

##    Remove leading IN* and leading/padding 0s from 2021 District Numbers
# sort(unique(grep("IN", WIDA_IN_SGP_LONG_Data[, DISTRICT_NUMBER], value = T)))
WIDA_IN_SGP_LONG_Data[, DISTRICT_NUMBER := gsub("IN00|INB00", "", DISTRICT_NUMBER)]
WIDA_IN_SGP_LONG_Data[, DISTRICT_NUMBER := gsub("IN0|INA0|INB0|INC0|IND0", "", DISTRICT_NUMBER)]
WIDA_IN_SGP_LONG_Data[, DISTRICT_NUMBER := gsub("INA|INB|INC|IND", "", DISTRICT_NUMBER)]
WIDA_IN_SGP_LONG_Data[, DISTRICT_NUMBER := gsub("IN", "", DISTRICT_NUMBER)]

###   Re-factor ACHIEVEMENT_LEVEL_PRIOR to get values for kids with missing scores
WIDA_IN_SGP_LONG_Data[is.na(SCALE_SCORE), VALID_CASE := "VALID_CASE"]

#   Resolve duplicates
setkey(WIDA_IN_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, SCALE_SCORE)
setkey(WIDA_IN_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE)
WIDA_IN_SGP_LONG_Data[which(duplicated(WIDA_IN_SGP_LONG_Data, by=key(WIDA_IN_SGP_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"]

WIDA_IN_SGP_LONG_Data <- WIDA_IN_SGP_LONG_Data[VALID_CASE=="VALID_CASE"]

###   Create Lagged Score (SCALE and STANDARDIZED) variables that include missing scores (and others potentially)
shift.key <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "VALID_CASE")

##    Standardize SCALE_SCORE by CONTENT_AREA and GRADE using 2019 norms
WIDA_IN_SGP_LONG_Data[, SCALE_SCORE_STANDARDIZED := Z(.SD, "SCALE_SCORE", reference.year = "2020"), by = list(CONTENT_AREA, GRADE), .SDcols = c("YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE")]
# WIDA_IN_SGP_LONG_Data[, as.list(round(summary(SCALE_SCORE_STANDARDIZED), 3)), keyby = c("YEAR", "CONTENT_AREA", "GRADE")]

setkeyv(WIDA_IN_SGP_LONG_Data, shift.key)

WIDA_IN_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR_1YEAR", "SCALE_SCORE_PRIOR_2YEAR") := shift(SCALE_SCORE, 1:2), by = list(ID, CONTENT_AREA)]
WIDA_IN_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR", "SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR") := shift(SCALE_SCORE_STANDARDIZED, 1:2), by = list(ID, CONTENT_AREA)]

# table(WIDA_IN_SGP_LONG_Data[, YEAR, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# table(WIDA_IN_SGP_LONG_Data[, GRADE, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL) # Remove 2YEAR priors for Grades 0 & 1 - repeaters
WIDA_IN_SGP_LONG_Data[GRADE %in% c(0, 1), SCALE_SCORE_PRIOR_2YEAR := NA]
WIDA_IN_SGP_LONG_Data[GRADE == 0, SCALE_SCORE_PRIOR_1YEAR := NA]
WIDA_IN_SGP_LONG_Data[GRADE %in% c(0, 1), SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR := NA]
WIDA_IN_SGP_LONG_Data[GRADE == 0, SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR := NA]
# table(WIDA_IN_SGP_LONG_Data[YEAR=="2021", is.na(SCALE_SCORE_PRIOR), is.na(SCALE_SCORE_PRIOR_1YEAR)], exclude=NULL) # No SCALE_SCORE_PRIOR_BASELINE in WIDA
# cor(WIDA_IN_SGP_LONG_Data[, SCALE_SCORE_PRIOR, SCALE_SCORE_PRIOR_1YEAR], use="complete.obs") # Perfect.

###   Create Lagged Achievement variables that include missing scores (and others potentially)
setkeyv(WIDA_IN_SGP_LONG_Data, shift.key)

# WIDA_IN_SGP_LONG_Data[, ACH_LEV_P2 := as.character(NA)] # for testing - create a alternate version first and check equality for non-NA values
WIDA_IN_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR := shift(ACHIEVEMENT_LEVEL, 1), by = list(ID, CONTENT_AREA)]
# table(WIDA_IN_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR, ACH_LEV_P2], exclude=NULL)


###   Create Prior Score Deciles
##    Establish a decile lookup based on 2019 & 2020 for each GRADE
Decile_Lookup_1yr <- WIDA_IN_SGP_LONG_Data[YEAR == "2020" & !is.na(SCALE_SCORE_PRIOR_1YEAR),
                            as.list(getQuantcut(SCALE_SCORE_PRIOR_1YEAR)), keyby = c("CONTENT_AREA", "GRADE")]
prior1yr.decile.fcase <- getFCASE("SCALE_SCORE_PRIOR_1YEAR", Decile_Lookup_1yr)
WIDA_IN_SGP_LONG_Data[, PRIOR_DECILE_1YEAR := eval(parse(text=prior1yr.decile.fcase))]
WIDA_IN_SGP_LONG_Data[, PRIOR_DECILE_1YEAR := factor(PRIOR_DECILE_1YEAR, levels = paste0("Q", 1:10), labels=paste0("Decile_", 1:10))]

Decile_Lookup_2yr <- WIDA_IN_SGP_LONG_Data[YEAR == "2019" & !is.na(SCALE_SCORE_PRIOR_2YEAR),
                            as.list(getQuantcut(SCALE_SCORE_PRIOR_2YEAR)), keyby = c("CONTENT_AREA", "GRADE")]
prior2yr.decile.fcase <- getFCASE("SCALE_SCORE_PRIOR_2YEAR", Decile_Lookup_2yr)
WIDA_IN_SGP_LONG_Data[, PRIOR_DECILE_2YEAR := eval(parse(text=prior2yr.decile.fcase))]
WIDA_IN_SGP_LONG_Data[, PRIOR_DECILE_2YEAR := factor(PRIOR_DECILE_2YEAR, levels = paste0("Q", 1:10), labels=paste0("Decile_", 1:10))]

##    Some sanity checks
# table(WIDA_IN_SGP_LONG_Data[, PRIOR_DECILE_1YEAR, GRADE], exclude=NULL)
# table(WIDA_IN_SGP_LONG_Data[, PRIOR_DECILE_2YEAR, GRADE], exclude=NULL)
# table(WIDA_IN_SGP_LONG_Data[, PRIOR_DECILE_1YEAR, YEAR], exclude=NULL)
# table(WIDA_IN_SGP_LONG_Data[, PRIOR_DECILE_2YEAR, YEAR], exclude=NULL)
# table(WIDA_IN_SGP_LONG_Data[, !is.na(SCALE_SCORE_PRIOR_1YEAR), PRIOR_DECILE_1YEAR,], exclude=NULL) # Should be 0 in the TRUE/<NA> cell
# WIDA_IN_SGP_LONG_Data[!is.na(PRIOR_DECILE_1YEAR) & GRADE=='5',
#             as.list(summary(SCALE_SCORE_PRIOR_1YEAR)), keyby = "PRIOR_DECILE_1YEAR"]
# Decile_Lookup_1yr[GRADE=='5']


###   Merge in demographics as available from ILEARN
###   Only works ~decent~ for grades 3:8... duh!
# setkey(Indiana_Demographics, ID, YEAR)
# setkey(WIDA_IN_SGP_LONG_Data, ID, YEAR)
# WIDA_IN_SGP_LONG_Data <- Indiana_Demographics[WIDA_IN_SGP_LONG_Data]
#
# setkey(WIDA_IN_SGP_LONG_Data, ID, YEAR)
# WIDA_IN_SGP_LONG_Data <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(WIDA_IN_SGP_LONG_Data, ID),
#                           tidyselect::all_of(demographics), .direction="downup")))


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

Indiana_NWEA_Data_2021[, YEAR := mapvalues(TermName,
                            from=c("Fall 2020-2021", "Winter 2020-2021"),
                            to  =c("2021.1", "2021.2"))]

Indiana_NWEA_Data_2021[, CONTENT_AREA := mapvalues(Subject,
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

if (!dir.exists(file.path("..", "Data"))) dir.create(file.path("..", "Data"))
save(Report_Data, file = file.path("..", "Data", "Report_Data.Rdata"))

setwd("..")
