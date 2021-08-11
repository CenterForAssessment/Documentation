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
load(file.path("..", "..", "..", "..", "Rhode_Island", "Data", "Rhode_Island_SGP_LONG_Data.Rdata"))

prior.year <- "2018_2019"
current.year <- "2020_2021"

###   Subset only variables and GRADE levels relevant to 2021
demographics <- c("IEP_STATUS", "ELL_STATUS", "FREE_REDUCED_LUNCH_STATUS", "GENDER", "ETHNICITY")
vars.to.keep <- c(
  "VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "GRADE",
  "SCALE_SCORE", "SCALE_SCORE_CSEM", "ACHIEVEMENT_LEVEL", "SCALE_SCORE_ACTUAL",
  "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED", "ACHIEVEMENT_LEVEL_PRIOR",
  "SCALE_SCORE_PRIOR_BASELINE", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE",
  "SGP", "SGP_BASELINE", "SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE",
  "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME",
  "DISTRICT_ENROLLMENT_STATUS", "SCHOOL_ENROLLMENT_STATUS",
  demographics, "TEST_FORMAT")

RICAS_SGP_LONG_Data <- Rhode_Island_SGP_LONG_Data[GRADE %in% 3:8, ..vars.to.keep]

###   Re-factor ACHIEVEMENT_LEVEL_PRIOR to get values for kids with missing scores
RICAS_SGP_LONG_Data[is.na(SCALE_SCORE), VALID_CASE := "VALID_CASE"]

setkey(RICAS_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)
setkey(RICAS_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
RICAS_SGP_LONG_Data[which(duplicated(RICAS_SGP_LONG_Data, by=key(RICAS_SGP_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"]

setkey(RICAS_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE)
setkey(RICAS_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID)
RICAS_SGP_LONG_Data[which(duplicated(RICAS_SGP_LONG_Data, by=key(RICAS_SGP_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"]

RICAS_SGP_LONG_Data <- RICAS_SGP_LONG_Data[VALID_CASE == "VALID_CASE"]

###   Create Lagged Achievement variable that include missing scores (and others potentially)
# RICAS_SGP_LONG_Data[, NUMYEAR := tstrsplit(YEAR, "_", type.convert=TRUE, keep=2)]
shift.key <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "VALID_CASE") # "NUMYEAR" - not necessary
setkeyv(RICAS_SGP_LONG_Data, shift.key)

# RICAS_SGP_LONG_Data[, ACH_LEV_P2 := as.character(NA)] # for testing - create a alternate version first and check equality for non-NA values
RICAS_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR := shift(ACHIEVEMENT_LEVEL, 1), by = list(ID, CONTENT_AREA)]
# table(RICAS_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR, ACH_LEV_P2], exclude=NULL) # for testing - create a alternate version first and check equality for non-NA values
RICAS_SGP_LONG_Data[YEAR %in% c("2018_2019", "2020_2021") & ACHIEVEMENT_LEVEL_PRIOR %like% "Level ", ACHIEVEMENT_LEVEL_PRIOR := NA] # Remove priors (obviously) from more than the most recent prior year

###   Create Lagged Scale Score (Regular and STANDARDIZED) variables that include missing scores (and others potentially)

##    Standardize SCALE_SCORE by CONTENT_AREA and GRADE using 2019 norms
RICAS_SGP_LONG_Data[, SCALE_SCORE_STANDARDIZED := Z(.SD, "SCALE_SCORE", reference.year = prior.year), by = list(CONTENT_AREA, GRADE), .SDcols = c("YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE")]
# RICAS_SGP_LONG_Data[, as.list(round(summary(SCALE_SCORE), 3)), keyby = c("YEAR", "CONTENT_AREA", "GRADE")]

setkeyv(RICAS_SGP_LONG_Data, shift.key)

RICAS_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR_1YEAR", "SCALE_SCORE_PRIOR_2YEAR") := shift(SCALE_SCORE, 1:2), by = list(ID, CONTENT_AREA)]
RICAS_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR", "SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR") := shift(SCALE_SCORE_STANDARDIZED, 1:2), by = list(ID, CONTENT_AREA)]
##    Fix 2021 Lags since no 2020 data:
RICAS_SGP_LONG_Data[YEAR == current.year, SCALE_SCORE_PRIOR_2YEAR := SCALE_SCORE_PRIOR_1YEAR]
RICAS_SGP_LONG_Data[YEAR == current.year, SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR := SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR]
RICAS_SGP_LONG_Data[YEAR == current.year, SCALE_SCORE_PRIOR_1YEAR := NA]
RICAS_SGP_LONG_Data[YEAR == current.year, SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR := NA]
# table(RICAS_SGP_LONG_Data[, YEAR, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# table(RICAS_SGP_LONG_Data[, GRADE, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL) # Remove 2YEAR priors for Grades 3 & 4 - repeaters
RICAS_SGP_LONG_Data[GRADE %in% c(3, 4), SCALE_SCORE_PRIOR_2YEAR := NA]
RICAS_SGP_LONG_Data[GRADE %in% c(3, 4), SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR := NA]
RICAS_SGP_LONG_Data[GRADE == 3, SCALE_SCORE_PRIOR_1YEAR := NA]
RICAS_SGP_LONG_Data[GRADE == 3, SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR := NA]
# table(RICAS_SGP_LONG_Data[YEAR==prior.year, is.na(SCALE_SCORE_PRIOR_BASELINE), is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# cor(RICAS_SGP_LONG_Data[, SCALE_SCORE_PRIOR_BASELINE, SCALE_SCORE_PRIOR_2YEAR], use='complete.obs') # Not perfect.  Use BASELINE data when available:
RICAS_SGP_LONG_Data[!is.na(SCALE_SCORE_PRIOR_BASELINE), SCALE_SCORE_PRIOR_2YEAR := SCALE_SCORE_PRIOR_BASELINE]
# RICAS_SGP_LONG_Data[!is.na(SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE), SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR := SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE] # Keep on 2019 scale!

###   Create Prior Score Deciles
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Functions", "Quantile_Cut_Functions.R"))

##    Establish a decile lookup based on 2019 for each CONTENT_AREA/GRADE combination
# RICAS_SGP_LONG_Data[, SCALE_SCORE_PRIOR_2YEAR := round(SCALE_SCORE_PRIOR_2YEAR, 4)] # RICAS on Theta scale - round to max digits used in getQuantcut
Decile_Lookup <- RICAS_SGP_LONG_Data[YEAR == prior.year & !is.na(SCALE_SCORE_PRIOR_2YEAR),
                            as.list(getQuantcut(SCALE_SCORE_PRIOR_2YEAR)), keyby = c("CONTENT_AREA", "GRADE")]
prior.decile.fcase <- getFCASE("SCALE_SCORE_PRIOR_2YEAR", Decile_Lookup)
RICAS_SGP_LONG_Data[, PRIOR_DECILE_2YEAR := eval(parse(text=prior.decile.fcase))]
RICAS_SGP_LONG_Data[, PRIOR_DECILE_2YEAR := factor(PRIOR_DECILE_2YEAR, levels = paste0("Q", 1:10), labels=paste0("Decile_", 1:10))]

##    Some sanity checks
# table(RICAS_SGP_LONG_Data[, PRIOR_DECILE_2YEAR, GRADE], exclude=NULL)
# table(RICAS_SGP_LONG_Data[, PRIOR_DECILE_2YEAR, YEAR], exclude=NULL)
# table(RICAS_SGP_LONG_Data[, !is.na(SCALE_SCORE_PRIOR_2YEAR), PRIOR_DECILE_2YEAR,], exclude=NULL) # Should be 0 in the TRUE/<NA> cell
# RICAS_SGP_LONG_Data[!is.na(PRIOR_DECILE_2YEAR) & CONTENT_AREA=='MATHEMATICS' & GRADE=='5',
#             as.list(summary(SCALE_SCORE_PRIOR_2YEAR)), keyby = "PRIOR_DECILE_2YEAR"]
# Decile_Lookup[CONTENT_AREA=='MATHEMATICS' & GRADE=='5']

###   From Nathan's
# RICAS_SGP_LONG_Data[, FREE_REDUCED_LUNCH_STATUS := plyr::mapvalues(SOCIO_ECONOMIC_STATUS,
#                            from=c("Paid meals", "Free meals", "Reduced price meals", "Unknown"),
#                            to  =c("Free Reduced Lunch: No", "Free Reduced Lunch: Yes",
#                                   "Free Reduced Lunch: Yes", "Unkown"))]

RICAS_SGP_LONG_Data[, ACHIEVEMENT_ProfandAbove := plyr::mapvalues(ACHIEVEMENT_LEVEL,
                       from=c("Level 1", "Level 2", "Level 3", "Not Meeting Expectations", "Partially Meeting Expectations",
                              "Level 4", "Level 5", "Meeting Expectations", "Exceeding Expectations"),
                       to = c("Not Proficient", "Not Proficient", "Not Proficient", "Not Proficient", "Not Proficient",
                              "Proficient", "Proficient", "Proficient", "Proficient"))]

RICAS_SGP_LONG_Data$ACHIEVEMENT_ProfandAbove[is.na(RICAS_SGP_LONG_Data$ACHIEVEMENT_LEVEL)] <- NA

RICAS_SGP_LONG_Data[, PRIOR_ACHIEVEMENT_ProfandAbove := plyr::mapvalues(ACHIEVEMENT_LEVEL_PRIOR,
                       from=c("Level 1", "Level 2", "Level 3", "Not Meeting Expectations", "Partially Meeting Expectations",
                              "Level 4", "Level 5", "Meeting Expectations", "Exceeding Expectations"),
                       to = c("Not Proficient", "Not Proficient", "Not Proficient", "Not Proficient", "Not Proficient",
                              "Proficient", "Proficient", "Proficient", "Proficient"))]

###   Remove redundant/replaced variables
RICAS_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED", "SCALE_SCORE_PRIOR_BASELINE", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE") := NULL]



#####
###   College_Entrance
#####

##    Not available as of 8/9/2021

##    Might want to do as part of the above to get all variables
##    populated and formatted the same. Then split RICAS/P/SAT at end:
# RISAT_SGP_LONG_Data <- Rhode_Island_SGP_LONG_Data[GRADE == "EOCT", ..vars.to.keep]


#####
###   ELP_Assessment
#####

###   Load/Format/Subset Report Data
load(file.path("..", "..", "..", "..", "WIDA_RI", "Data", "WIDA_RI_SGP_LONG_Data.Rdata"))

###   Subset only variables relevant to Learning Loss Report

##    Demographics only available (currently) for 2021
demographics <- c("IEP_STATUS", "GENDER", "ETHNICITY") # "FREE_REDUCED_LUNCH_STATUS",

elp.vars.to.keep <- c(
  "VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "GRADE", "LENGTH_TIME_ELL_PROGRAM",
  "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL_ORIGINAL",
  "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED", "ACHIEVEMENT_LEVEL_PRIOR",
  "SCALE_SCORE_PRIOR_BASELINE", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE",
  "SGP", "SGP_BASELINE", "SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE",
  "DISTRICT_NUMBER", "SCHOOL_NUMBER", demographics)

WIDA_RI_SGP_LONG_Data <- WIDA_RI_SGP_LONG_Data[, ..elp.vars.to.keep]


###   Re-factor ACHIEVEMENT_LEVEL_PRIOR to get values for kids with missing scores
WIDA_RI_SGP_LONG_Data[is.na(SCALE_SCORE), VALID_CASE := "VALID_CASE"]

#   Resolve duplicates
# setkey(WIDA_RI_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, SCALE_SCORE)
# setkey(WIDA_RI_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE)
# WIDA_RI_SGP_LONG_Data[which(duplicated(WIDA_RI_SGP_LONG_Data, by=key(WIDA_RI_SGP_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"]
#
# setkey(WIDA_RI_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID, SCALE_SCORE)
# setkey(WIDA_RI_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID)
# WIDA_RI_SGP_LONG_Data[which(duplicated(WIDA_RI_SGP_LONG_Data, by=key(WIDA_RI_SGP_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"]

WIDA_RI_SGP_LONG_Data <- WIDA_RI_SGP_LONG_Data[VALID_CASE=="VALID_CASE"]

###   Create Lagged Score (SCALE and STANDARDIZED) variables that include missing scores (and others potentially)
shift.key <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "VALID_CASE")

##    Standardize SCALE_SCORE by CONTENT_AREA and GRADE using 2020 norms
WIDA_RI_SGP_LONG_Data[, SCALE_SCORE_STANDARDIZED := Z(.SD, "SCALE_SCORE", reference.year = "2020"), by = list(CONTENT_AREA, GRADE), .SDcols = c("YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE")]
# WIDA_RI_SGP_LONG_Data[, as.list(round(summary(SCALE_SCORE_STANDARDIZED), 3)), keyby = c("YEAR", "CONTENT_AREA", "GRADE")]

setkeyv(WIDA_RI_SGP_LONG_Data, shift.key)

WIDA_RI_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR_1YEAR", "SCALE_SCORE_PRIOR_2YEAR") := shift(SCALE_SCORE, 1:2), by = list(ID, CONTENT_AREA)]
WIDA_RI_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR", "SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR") := shift(SCALE_SCORE_STANDARDIZED, 1:2), by = list(ID, CONTENT_AREA)]

# table(WIDA_RI_SGP_LONG_Data[, YEAR, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# table(WIDA_RI_SGP_LONG_Data[, GRADE, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL) # Remove 2YEAR priors for Grades 0 & 1 - repeaters
WIDA_RI_SGP_LONG_Data[GRADE %in% c(0, 1), SCALE_SCORE_PRIOR_2YEAR := NA]
WIDA_RI_SGP_LONG_Data[GRADE == 0, SCALE_SCORE_PRIOR_1YEAR := NA]
WIDA_RI_SGP_LONG_Data[GRADE %in% c(0, 1), SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR := NA]
WIDA_RI_SGP_LONG_Data[GRADE == 0, SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR := NA]
# table(WIDA_RI_SGP_LONG_Data[YEAR=="2021", is.na(SCALE_SCORE_PRIOR), is.na(SCALE_SCORE_PRIOR_1YEAR)], exclude=NULL) # No SCALE_SCORE_PRIOR_BASELINE in WIDA
# cor(WIDA_RI_SGP_LONG_Data[, SCALE_SCORE_PRIOR, SCALE_SCORE_PRIOR_1YEAR], use="complete.obs") # Perfect.

###   Create Lagged Achievement variables that include missing scores (and others potentially)
setkeyv(WIDA_RI_SGP_LONG_Data, shift.key)

# WIDA_RI_SGP_LONG_Data[, ACH_LEV_P2 := as.character(NA)] # for testing - create a alternate version first and check equality for non-NA values
WIDA_RI_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR := shift(ACHIEVEMENT_LEVEL, 1), by = list(ID, CONTENT_AREA)]
# table(WIDA_RI_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR, ACH_LEV_P2], exclude=NULL)


###   Create Prior Score Deciles
##    Establish a decile lookup based on 2020 for each GRADE
Decile_Lookup_1yr <- WIDA_RI_SGP_LONG_Data[YEAR == "2020" & !is.na(SCALE_SCORE_PRIOR_1YEAR),
                            as.list(getQuantcut(SCALE_SCORE_PRIOR_1YEAR)), keyby = c("CONTENT_AREA", "GRADE")]
prior1yr.decile.fcase <- getFCASE("SCALE_SCORE_PRIOR_1YEAR", Decile_Lookup_1yr)
WIDA_RI_SGP_LONG_Data[, PRIOR_DECILE_1YEAR := eval(parse(text=prior1yr.decile.fcase))]
WIDA_RI_SGP_LONG_Data[, PRIOR_DECILE_1YEAR := factor(PRIOR_DECILE_1YEAR, levels = paste0("Q", 1:10), labels=paste0("Decile_", 1:10))]

#     Use 2020 again here for WIDA_RI - Historical data only goes to 2018
Decile_Lookup_2yr <- WIDA_RI_SGP_LONG_Data[YEAR == "2020" & !is.na(SCALE_SCORE_PRIOR_2YEAR),
                            as.list(getQuantcut(SCALE_SCORE_PRIOR_2YEAR)), keyby = c("CONTENT_AREA", "GRADE")]
prior2yr.decile.fcase <- getFCASE("SCALE_SCORE_PRIOR_2YEAR", Decile_Lookup_2yr)
WIDA_RI_SGP_LONG_Data[, PRIOR_DECILE_2YEAR := eval(parse(text=prior2yr.decile.fcase))]
WIDA_RI_SGP_LONG_Data[, PRIOR_DECILE_2YEAR := factor(PRIOR_DECILE_2YEAR, levels = paste0("Q", 1:10), labels=paste0("Decile_", 1:10))]

##    Some sanity checks
# table(WIDA_RI_SGP_LONG_Data[, PRIOR_DECILE_1YEAR, GRADE], exclude=NULL)
# table(WIDA_RI_SGP_LONG_Data[, PRIOR_DECILE_2YEAR, GRADE], exclude=NULL)
# table(WIDA_RI_SGP_LONG_Data[, PRIOR_DECILE_1YEAR, YEAR], exclude=NULL)
# table(WIDA_RI_SGP_LONG_Data[, PRIOR_DECILE_2YEAR, YEAR], exclude=NULL)
# table(WIDA_RI_SGP_LONG_Data[, !is.na(SCALE_SCORE_PRIOR_1YEAR), PRIOR_DECILE_1YEAR,], exclude=NULL) # Should be 0 in the TRUE/<NA> cell
# WIDA_RI_SGP_LONG_Data[!is.na(PRIOR_DECILE_1YEAR) & GRADE=='5',
#             as.list(summary(SCALE_SCORE_PRIOR_1YEAR)), keyby = "PRIOR_DECILE_1YEAR"]
# Decile_Lookup_1yr[GRADE=='5']


WIDA_RI_SGP_LONG_Data[, ACHIEVEMENT_ProfandAbove := plyr::mapvalues(ACHIEVEMENT_LEVEL,
                          from=c("WIDA Level 1", "WIDA Level 2", "WIDA Level 3", "WIDA Level 4",
                                 "WIDA Level 5", "WIDA Level 6"),
                          to = c("Not Proficient", "Not Proficient", "Not Proficient", "Not Proficient",
                                 "Proficient", "Proficient"))]

WIDA_RI_SGP_LONG_Data[, PRIOR_ACHIEVEMENT_ProfandAbove := plyr::mapvalues(ACHIEVEMENT_LEVEL_PRIOR,
                          from=c("WIDA Level 1", "WIDA Level 2", "WIDA Level 3", "WIDA Level 4",
                                 "WIDA Level 5", "WIDA Level 6"),
                          to = c("Not Proficient", "Not Proficient", "Not Proficient", "Not Proficient",
                                 "Proficient", "Proficient"))]

###   Remove redundant/replaced variables
WIDA_RI_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED", "SCALE_SCORE_PRIOR_BASELINE", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE") := NULL]



#####
###   Interim_Assessment
#####



#####
###   Combine all data sources into `Report_Data` and Save
#####

Report_Data <- vector("list", 4);
names(Report_Data) <- c("State_Assessment", "College_Entrance", "ELP_Assessment", "Interim_Assessment")

Report_Data[["State_Assessment"]] <- copy(RICAS_SGP_LONG_Data); rm(RICAS_SGP_LONG_Data)
Report_Data[["ELP_Assessment"]] <- copy(WIDA_RI_SGP_LONG_Data); rm(WIDA_RI_SGP_LONG_Data)

if (!dir.exists(file.path("..", "Data"))) dir.create(file.path("..", "Data"))
save(Report_Data, file = file.path("..", "Data", "Report_Data.Rdata"))

setwd("..")
