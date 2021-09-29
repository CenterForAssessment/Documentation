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

fixThetaRange <- function(x) {
  shrunk.range <- extendrange(x[abs(x)!=15])
  x[x==-15] <- shrunk.range[1]
  x[x==15] <- shrunk.range[2]
  x
}


###   Locate the "Universal_Content" directory (assume woring directory is ./Documentation)
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

#####
###   State_Assessment
#####


###   Load/Format/Subset Report Data
load(file.path("..", "..", "..", "..", "PARCC", "PARCC", "Data", "Archive", "2020_2021.2", "PARCC_SGP_LONG_Data.Rdata"))
load(file.path("..", "..", "..", "..", "PARCC", "PARCC", "Data", "Archive", "2016_2017.2", "PARCC_SGP_LONG_Data_2016_2017.2.Rdata"))
load(file.path("..", "..", "..", "..", "PARCC", "PARCC", "Data", "Archive", "2015_2016.2", "PARCC_SGP_LONG_Data_2015_2016.2.Rdata"))

content.areas <- c("ELA", "MATHEMATICS", "ALGEBRA_I", "ALGEBRA_II", "GEOMETRY")
PARCC_SGP_LONG_Data <- PARCC_SGP_LONG_Data[YEAR != "2019_2020.2" & CONTENT_AREA %in% content.areas]
PARCC_SGP_LONG_Data_2016_2017.2 <- PARCC_SGP_LONG_Data_2016_2017.2[StateAbbreviation %in% c("BI", "DD", "IL") & CONTENT_AREA %in% content.areas]
PARCC_SGP_LONG_Data_2015_2016.2 <- PARCC_SGP_LONG_Data_2015_2016.2[StateAbbreviation %in% c("BI", "DD", "IL") & CONTENT_AREA %in% content.areas]

PARCC_LONG_Data <- rbindlist(list(PARCC_SGP_LONG_Data_2015_2016.2, PARCC_SGP_LONG_Data_2016_2017.2, PARCC_SGP_LONG_Data), fill=TRUE, use.names=TRUE)

PARCC_LONG_Data[, YEAR := factor(YEAR)]
setattr(PARCC_LONG_Data$YEAR, "levels", c("2016", "2017", "2018", "2019", "2021"))
PARCC_LONG_Data[, YEAR := as.character(YEAR)]

prior.year <- "2019" # "2018_2019.2"
current.year <- "2021" # "2020_2021.2"

###   Subset only variables and GRADE levels relevant to 2021
demographics <- c("EconomicDisadvantageStatus",  "EnglishLearnerEL", "StudentWithDisabilities", "Sex",
                  "White", "BlackOrAfricanAmerican", "HispanicOrLatinoEthnicity", "Asian", "FederalRaceEthnicity")
setnames(PARCC_LONG_Data, c("AccountableDistrictCode", "AccountableSchoolCode"), c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))

vars.to.keep <- c(
  "VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "GRADE",
  "SCALE_SCORE", "SCALE_SCORE_CSEM", "ACHIEVEMENT_LEVEL", "SCALE_SCORE_ACTUAL",
  "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED", "ACHIEVEMENT_LEVEL_PRIOR",
  "SCALE_SCORE_PRIOR_BASELINE", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE",
  "SGP", "SGP_BASELINE", "SGP_SIMEX", "SGP_SIMEX_BASELINE", "SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE",
  "DISTRICT_NUMBER", "SCHOOL_NUMBER", demographics, "TestFormat", "StateAbbreviation")

PARCC_LONG_Data <- PARCC_LONG_Data[GRADE %in% c(3:11, "EOCT"), ..vars.to.keep]

###   Include all missing scores as VALID_CASEs
###   Not an issue for PARCC -- no missing scores provided in any years
# PARCC_LONG_Data[is.na(SCALE_SCORE), VALID_CASE := "VALID_CASE"]

##    Filter out duplicate cases (2018 only - keep highest grade)
table(PARCC_LONG_Data[, YEAR, VALID_CASE])
# setkey(PARCC_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)
# setkey(PARCC_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
# PARCC_LONG_Data[which(duplicated(PARCC_LONG_Data, by=key(PARCC_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"] # No change in VALID_CASEs - only different GRADEs

setkey(PARCC_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE)
setkey(PARCC_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID)
PARCC_LONG_Data[which(duplicated(PARCC_LONG_Data, by=key(PARCC_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"]

PARCC_LONG_Data <- PARCC_LONG_Data[VALID_CASE == "VALID_CASE"]

###   Create Lagged Achievement variable that include missing scores (and others potentially)
# PARCC_LONG_Data[, NUMYEAR := tstrsplit(YEAR, "_", type.convert=TRUE, keep=2)]
shift.key <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "VALID_CASE") # "NUMYEAR" - not necessary
setkeyv(PARCC_LONG_Data, shift.key)

# PARCC_LONG_Data[, ACH_LEV_P2 := as.character(NA)] # for testing - create a alternate version first and check equality for non-NA values
PARCC_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR := shift(ACHIEVEMENT_LEVEL, 1), by = list(ID, CONTENT_AREA)]
# table(PARCC_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR, ACH_LEV_P2], exclude=NULL) # for testing - create a alternate version first and check equality for non-NA values
# table(PARCC_LONG_Data[ACHIEVEMENT_LEVEL_PRIOR != ACH_LEV_P2, GRADE, YEAR], exclude=NULL) # Originally left in IL 2020 cases.  All equal after that.


###   Create Lagged Scale Score (Regular and STANDARDIZED) variables that include missing scores (and others potentially)

###   Fix stupid range of PARCC Theta
PARCC_LONG_Data[, SCALE_SCORE := fixThetaRange(SCALE_SCORE), keyby = c("CONTENT_AREA", "GRADE")]
# PARCC_LONG_Data[, as.list(round(summary(SCALE_SCORE), 3)), keyby = c("YEAR", "CONTENT_AREA", "GRADE")]

##    Standardize SCALE_SCORE by CONTENT_AREA and GRADE using 2019 norms
PARCC_LONG_Data[, SCALE_SCORE_STANDARDIZED := Z(.SD, "SCALE_SCORE", reference.year = prior.year), by = list(CONTENT_AREA, GRADE), .SDcols = c("YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE")]

setkeyv(PARCC_LONG_Data, shift.key)

PARCC_LONG_Data[, c("SCALE_SCORE_PRIOR_1YEAR", "SCALE_SCORE_PRIOR_2YEAR") := shift(SCALE_SCORE, 1:2), by = list(ID, CONTENT_AREA)]
PARCC_LONG_Data[, c("SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR", "SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR") := shift(SCALE_SCORE_STANDARDIZED, 1:2), by = list(ID, CONTENT_AREA)]
##    Fix 2021 Lags since no 2020 data:
PARCC_LONG_Data[YEAR == current.year, SCALE_SCORE_PRIOR_2YEAR := SCALE_SCORE_PRIOR_1YEAR]
PARCC_LONG_Data[YEAR == current.year, SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR := SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR]
PARCC_LONG_Data[YEAR == current.year, SCALE_SCORE_PRIOR_1YEAR := NA]
PARCC_LONG_Data[YEAR == current.year, SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR := NA]
# table(PARCC_LONG_Data[, YEAR, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# table(PARCC_LONG_Data[, GRADE, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL) # Remove 2YEAR priors for Grades 3 & 4 - repeaters
PARCC_LONG_Data[GRADE %in% c(3, 4), SCALE_SCORE_PRIOR_2YEAR := NA]
PARCC_LONG_Data[GRADE %in% c(3, 4), SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR := NA]
PARCC_LONG_Data[GRADE == 3, SCALE_SCORE_PRIOR_1YEAR := NA]
PARCC_LONG_Data[GRADE == 3, SCALE_SCORE_PRIOR_STANDARDIZED_1YEAR := NA]
# table(PARCC_LONG_Data[YEAR==prior.year, is.na(SCALE_SCORE_PRIOR_BASELINE), is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# cor(PARCC_LONG_Data[, SCALE_SCORE_PRIOR_BASELINE, SCALE_SCORE_PRIOR_2YEAR], use='complete.obs') # Not perfect.  Use BASELINE data when available:
PARCC_LONG_Data[!is.na(SCALE_SCORE_PRIOR_BASELINE), SCALE_SCORE_PRIOR_2YEAR := SCALE_SCORE_PRIOR_BASELINE]
# PARCC_LONG_Data[!is.na(SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE), SCALE_SCORE_PRIOR_STANDARDIZED_2YEAR := SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE] # Keep on 2019 scale!

###   Create Prior Score Deciles
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Functions", "Quantile_Cut_Functions.R"))

##    Establish a decile lookup based on 2019 for each CONTENT_AREA/GRADE combination
# PARCC_LONG_Data[, SCALE_SCORE_PRIOR_2YEAR := round(SCALE_SCORE_PRIOR_2YEAR, 4)] # PARCC on Theta scale - round to max digits used in getQuantcut
Decile_Lookup <- PARCC_LONG_Data[YEAR == prior.year & !is.na(SCALE_SCORE_PRIOR_2YEAR),
                            as.list(getQuantcut(SCALE_SCORE_PRIOR_2YEAR)), keyby = c("CONTENT_AREA", "GRADE")]
prior.decile.fcase <- getFCASE("SCALE_SCORE_PRIOR_2YEAR", Decile_Lookup)
PARCC_LONG_Data[, PRIOR_DECILE_2YEAR := eval(parse(text=prior.decile.fcase))]
PARCC_LONG_Data[, PRIOR_DECILE_2YEAR := factor(PRIOR_DECILE_2YEAR, levels = paste0("Q", 1:10), labels=paste0("Decile_", 1:10))]

##    Some sanity checks
# table(PARCC_LONG_Data[, PRIOR_DECILE_2YEAR, GRADE], exclude=NULL)
# table(PARCC_LONG_Data[, PRIOR_DECILE_2YEAR, YEAR], exclude=NULL)
# table(PARCC_LONG_Data[, !is.na(SCALE_SCORE_PRIOR_2YEAR), PRIOR_DECILE_2YEAR,], exclude=NULL) # Should be 0 in the TRUE/<NA> cell
# PARCC_LONG_Data[!is.na(PRIOR_DECILE_2YEAR) & CONTENT_AREA=='MATHEMATICS' & GRADE=='5',
#             as.list(summary(SCALE_SCORE_PRIOR_2YEAR)), keyby = "PRIOR_DECILE_2YEAR"]
# Decile_Lookup[CONTENT_AREA=='MATHEMATICS' & GRADE=='5']
# ## Verify no crazy numbers in HIGH of LOW
# summary(as.numeric(Decile_Lookup$HIGH))

###   From Nathan's
table(PARCC_LONG_Data[, ACHIEVEMENT_LEVEL], exclude=NULL)
PARCC_LONG_Data[, ACHIEVEMENT_ProfandAbove := plyr::mapvalues(ACHIEVEMENT_LEVEL,
                       from=c("Level 1", "Level 2", "Level 3",
                              "Level 4", "Level 5"),
                       to = c("Not Proficient", "Not Proficient", "Not Proficient",
                              "Proficient", "Proficient"))]

PARCC_LONG_Data[, PRIOR_ACHIEVEMENT_ProfandAbove := plyr::mapvalues(ACHIEVEMENT_LEVEL_PRIOR,
                       from=c("Level 1", "Level 2", "Level 3",
                              "Level 4", "Level 5"),
                       to = c("Not Proficient", "Not Proficient", "Not Proficient",
                              "Proficient", "Proficient"))]

###   Clean up and fill in demographics

##    Clean up blank vs NA
for (dmg in demographics) {
  PARCC_LONG_Data[get(dmg)=="", (dmg) := NA]
}

###   Fill in demographics
setkey(PARCC_LONG_Data, ID, YEAR)
PARCC_LONG_Data <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(PARCC_LONG_Data, ID),
                        tidyselect::all_of(demographics), .direction="downup")))

###   Remove redundant/replaced variables
PARCC_LONG_Data[, c("SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED", "SCALE_SCORE_PRIOR_BASELINE", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE") := NULL]



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

Report_Data[["State_Assessment"]] <- copy(PARCC_LONG_Data)
rm(list = c("PARCC_LONG_Data", "PARCC_SGP_LONG_Data_2015_2016.2", "PARCC_SGP_LONG_Data_2016_2017.2", "PARCC_SGP_LONG_Data")); gc()

if (!dir.exists(file.path("..", "Data"))) dir.create(file.path("..", "Data"))
save(Report_Data, file = file.path("..", "Data", "Report_Data.Rdata"))

setwd("..")
