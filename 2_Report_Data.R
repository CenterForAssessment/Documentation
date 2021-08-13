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
load(file.path("..", "..", "..", "..", "Arizona", "Content_Area", "Data", "Arizona_SGP_LONG_Data.Rdata"))

prior.year <- "2019"
current.year <- "2021"

###   Subset only variables and GRADE levels relevant to 2021
demographics <- c("SPED_STATUS", "ELL_STATUS", "FREE_REDUCED_LUNCH_STATUS", "GENDER", "ETHNICITY")
vars.to.keep <- c(
  "VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "GRADE",
  "SCALE_SCORE", "ACHIEVEMENT_LEVEL", # "SCALE_SCORE_CSEM",
  "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED", "ACHIEVEMENT_LEVEL_PRIOR",
  "SCALE_SCORE_PRIOR_BASELINE", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE",
  "SGP", "SGP_BASELINE", "SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE",
  "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME",
  "DISTRICT_ENROLLMENT_STATUS", "SCHOOL_ENROLLMENT_STATUS",
  demographics, "TEST_MODE")

AZMAT_SGP_LONG_Data <- Arizona_SGP_LONG_Data[GRADE %in% c(3:8, 10), ..vars.to.keep]

###   Re-factor ACHIEVEMENT_LEVEL_PRIOR to get values for kids with missing scores

##    Currently NO NA scale scores

# AZMAT_SGP_LONG_Data[is.na(SCALE_SCORE), VALID_CASE := "VALID_CASE"]
#
# setkey(AZMAT_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)
# setkey(AZMAT_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
# AZMAT_SGP_LONG_Data[which(duplicated(AZMAT_SGP_LONG_Data, by=key(AZMAT_SGP_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"]
#
# setkey(AZMAT_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE)
# setkey(AZMAT_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID)
# AZMAT_SGP_LONG_Data[which(duplicated(AZMAT_SGP_LONG_Data, by=key(AZMAT_SGP_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"]

AZMAT_SGP_LONG_Data <- AZMAT_SGP_LONG_Data[VALID_CASE == "VALID_CASE"]

###   Create Lagged Achievement variable that include missing scores (and others potentially)
# AZMAT_SGP_LONG_Data[, NUMYEAR := tstrsplit(YEAR, "_", type.convert=TRUE, keep=2)]
shift.key <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "VALID_CASE") # "NUMYEAR" - not necessary
setkeyv(AZMAT_SGP_LONG_Data, shift.key)

##    No difference in AZ with ACHIEVEMENT_LEVEL_PRIOR
# AZMAT_SGP_LONG_Data[, ACH_LEV_P2 := as.character(NA)] # for testing - create a alternate version first and check equality for non-NA values
# AZMAT_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR := shift(ACHIEVEMENT_LEVEL, 1), by = list(ID, CONTENT_AREA)]
# table(AZMAT_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR, ACH_LEV_P2], exclude=NULL) # for testing - create a alternate version first and check equality for non-NA values

###   Create Lagged Scale Score (Regular and STANDARDIZED) variables that include missing scores (and others potentially)

##    Standardize SCALE_SCORE by CONTENT_AREA and GRADE using 2019 norms
AZMAT_SGP_LONG_Data[, SCALE_SCORE_STANDARDIZED := Z(.SD, "SCALE_SCORE", reference.year = prior.year), by = list(CONTENT_AREA, GRADE), .SDcols = c("YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE")]
# AZMAT_SGP_LONG_Data[, as.list(round(summary(SCALE_SCORE), 3)), keyby = c("YEAR", "CONTENT_AREA", "GRADE")]

setkeyv(AZMAT_SGP_LONG_Data, shift.key)

AZMAT_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR_1YEAR", "SCALE_SCORE_PRIOR_2YEAR") := shift(SCALE_SCORE, 1:2), by = list(ID, CONTENT_AREA)]
AZMAT_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR", "SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR") := shift(SCALE_SCORE_STANDARDIZED, 1:2), by = list(ID, CONTENT_AREA)]
##    Fix 2021 Lags since no 2020 data:
AZMAT_SGP_LONG_Data[YEAR == current.year, SCALE_SCORE_PRIOR_2YEAR := SCALE_SCORE_PRIOR_1YEAR]
AZMAT_SGP_LONG_Data[YEAR == current.year, SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR := SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR]
AZMAT_SGP_LONG_Data[YEAR == current.year, SCALE_SCORE_PRIOR_1YEAR := NA]
AZMAT_SGP_LONG_Data[YEAR == current.year, SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR := NA]
# table(AZMAT_SGP_LONG_Data[, YEAR, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# table(AZMAT_SGP_LONG_Data[, GRADE, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL) # Remove 2YEAR priors for Grades 3 & 4 - repeaters
AZMAT_SGP_LONG_Data[GRADE %in% c(3, 4), SCALE_SCORE_PRIOR_2YEAR := NA]
AZMAT_SGP_LONG_Data[GRADE %in% c(3, 4), SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR := NA]
AZMAT_SGP_LONG_Data[GRADE == 3, SCALE_SCORE_PRIOR_1YEAR := NA]
AZMAT_SGP_LONG_Data[GRADE == 3, SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR := NA]
# table(AZMAT_SGP_LONG_Data[YEAR==prior.year, is.na(SCALE_SCORE_PRIOR_BASELINE), is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# cor(AZMAT_SGP_LONG_Data[, SCALE_SCORE_PRIOR_BASELINE, SCALE_SCORE_PRIOR_2YEAR], use='complete.obs') # Not perfect.  Use BASELINE data when available:
AZMAT_SGP_LONG_Data[!is.na(SCALE_SCORE_PRIOR_BASELINE), SCALE_SCORE_PRIOR_2YEAR := SCALE_SCORE_PRIOR_BASELINE]
# AZMAT_SGP_LONG_Data[!is.na(SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE), SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR := SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE] # Keep on 2019 scale!

###   Create Prior Score Deciles
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Functions", "Quantile_Cut_Functions.R"))

##    Establish a decile lookup based on 2019 for each CONTENT_AREA/GRADE combination
# AZMAT_SGP_LONG_Data[, SCALE_SCORE_PRIOR_2YEAR := round(SCALE_SCORE_PRIOR_2YEAR, 4)] # RICAS on Theta scale - round to max digits used in getQuantcut
Decile_Lookup <- AZMAT_SGP_LONG_Data[YEAR == prior.year & !is.na(SCALE_SCORE_PRIOR_2YEAR),
                            as.list(getQuantcut(SCALE_SCORE_PRIOR_2YEAR)), keyby = c("CONTENT_AREA", "GRADE")]
prior.decile.fcase <- getFCASE("SCALE_SCORE_PRIOR_2YEAR", Decile_Lookup)
AZMAT_SGP_LONG_Data[, PRIOR_DECILE_2YEAR := eval(parse(text=prior.decile.fcase))]
AZMAT_SGP_LONG_Data[, PRIOR_DECILE_2YEAR := factor(PRIOR_DECILE_2YEAR, levels = paste0("Q", 1:10), labels=paste0("Decile_", 1:10))]

##    Some sanity checks
# table(AZMAT_SGP_LONG_Data[, PRIOR_DECILE_2YEAR, GRADE], exclude=NULL)
# table(AZMAT_SGP_LONG_Data[, PRIOR_DECILE_2YEAR, YEAR], exclude=NULL)
# table(AZMAT_SGP_LONG_Data[, !is.na(SCALE_SCORE_PRIOR_2YEAR), PRIOR_DECILE_2YEAR,], exclude=NULL) # Should be 0 in the TRUE/<NA> cell
# table(AZMAT_SGP_LONG_Data[!is.na(SCALE_SCORE_PRIOR_2YEAR) & is.na(PRIOR_DECILE_2YEAR), GRADE, CONTENT_AREA], exclude=NULL) # No Decile Info for Math grade 10
# AZMAT_SGP_LONG_Data[!is.na(PRIOR_DECILE_2YEAR) & CONTENT_AREA=='MATHEMATICS' & GRADE=='5',
#             as.list(summary(SCALE_SCORE_PRIOR_2YEAR)), keyby = "PRIOR_DECILE_2YEAR"]
# Decile_Lookup[CONTENT_AREA=='MATHEMATICS' & GRADE=='10']

###   From Nathan's
# AZMAT_SGP_LONG_Data[, FREE_REDUCED_LUNCH_STATUS := plyr::mapvalues(SOCIO_ECONOMIC_STATUS,
#                            from=c("Paid meals", "Free meals", "Reduced price meals", "Unknown"),
#                            to  =c("Free Reduced Lunch: No", "Free Reduced Lunch: Yes",
#                                   "Free Reduced Lunch: Yes", "Unkown"))]

AZMAT_SGP_LONG_Data[, ACHIEVEMENT_ProfandAbove := plyr::mapvalues(ACHIEVEMENT_LEVEL,
                        from=c("Minimally Proficient", "Partially Proficient",
                              "Proficient", "Highly Proficient"),
                        to = c("Not Proficient", "Not Proficient",
                              "Proficient", "Proficient"))]

AZMAT_SGP_LONG_Data$ACHIEVEMENT_ProfandAbove[is.na(AZMAT_SGP_LONG_Data$ACHIEVEMENT_LEVEL)] <- NA

AZMAT_SGP_LONG_Data[, PRIOR_ACHIEVEMENT_ProfandAbove := plyr::mapvalues(ACHIEVEMENT_LEVEL_PRIOR,
                        from=c("Minimally Proficient", "Partially Proficient",
                               "Proficient", "Highly Proficient"),
                        to = c("Not Proficient", "Not Proficient",
                               "Proficient", "Proficient"))]

###   Remove redundant/replaced variables
AZMAT_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED", "SCALE_SCORE_PRIOR_BASELINE", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE") := NULL]




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

Report_Data[["State_Assessment"]] <- copy(AZMAT_SGP_LONG_Data); rm(AZMAT_SGP_LONG_Data)

if (!dir.exists(file.path("..", "Data"))) dir.create(file.path("..", "Data"))
save(Report_Data, file = file.path("..", "Data", "Report_Data.Rdata"))

setwd("..")
