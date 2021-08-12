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

###   Locate the "Universal_Content" directory (assume woring directory is ./Documentation)
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

#####
###   State_Assessment
#####

###   Load/Format/Subset Report Data
load(file.path("..", "..", "..", "..", "Colorado", "Data", "Colorado_SGP_LONG_Data.Rdata"))
setNamesSGP(Colorado_SGP_LONG_Data)

###   Subset only variables and GRADE levels relevant to 2021
demographics <- c("ETHNICITY", "GENDER", "FREE_REDUCED_LUNCH_STATUS",
                  "ELL_STATUS", "IEP_STATUS", "GIFTED_TALENTED_PROGRAM_STATUS")

enrollment.status <- c("SCHOOL_ENROLLMENT_STATUS", "DISTRICT_ENROLLMENT_STATUS", "STATE_ENROLLMENT_STATUS")

vars.to.keep <- c(
  "VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "GRADE",
  "SCALE_SCORE", "ACHIEVEMENT_LEVEL", "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED",
  "SCALE_SCORE_PRIOR_BASELINE", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE", "ACHIEVEMENT_LEVEL_PRIOR",
  "SGP", "SGP_BASELINE", "SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE",
  "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME",
  demographics, enrollment.status)

Colorado_SGP_LONG_Data <- Colorado_SGP_LONG_Data[GRADE %in% 3:8 & CONTENT_AREA %in% c("ELA", "MATHEMATICS"), ..vars.to.keep]

###   Re-factor ACHIEVEMENT_LEVEL_PRIOR to get values for kids with missing scores
Colorado_SGP_LONG_Data[is.na(SCALE_SCORE), VALID_CASE := "VALID_CASE"]

setkey(Colorado_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID, SCALE_SCORE)
setkey(Colorado_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, GRADE, ID)
# dups <- data.table(Colorado_SGP_LONG_Data[unique(c(which(duplicated(Colorado_SGP_LONG_Data, by=key(Colorado_SGP_LONG_Data)))-1,
#                                                    which(duplicated(Colorado_SGP_LONG_Data, by=key(Colorado_SGP_LONG_Data))))), ],
#                                             key=key(Colorado_SGP_LONG_Data))
# table(dups$VALID_CASE) # 1675 2021 duplicates are NA SCALE_SCOREs with another score present
# table(Colorado_SGP_LONG_Data[intersect(which(duplicated(Colorado_SGP_LONG_Data, by=key(Colorado_SGP_LONG_Data)))-1, which(VALID_CASE=="VALID_CASE")), is.na(SCALE_SCORE)])
Colorado_SGP_LONG_Data[which(duplicated(Colorado_SGP_LONG_Data, by=key(Colorado_SGP_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"]

setkey(Colorado_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE)
setkey(Colorado_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID)
# table(dups$VALID_CASE) # 10 2021 duplicates with different GRADE with NA scores and another valid score
Colorado_SGP_LONG_Data[which(duplicated(Colorado_SGP_LONG_Data, by=key(Colorado_SGP_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"]

Colorado_SGP_LONG_Data <- Colorado_SGP_LONG_Data[VALID_CASE == "VALID_CASE"]

###   Create Lagged Achievement (ORIGINAL and EQUATED) variables that include missing scores (and others potentially)
shift.key <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "VALID_CASE")
setkeyv(Colorado_SGP_LONG_Data, shift.key)

# Colorado_SGP_LONG_Data[, ACH_LEV_P2 := shift(ACHIEVEMENT_LEVEL, 1), by = list(ID, CONTENT_AREA)] # for testing - create a alternate version first and check equality for non-NA values
Colorado_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR := shift(ACHIEVEMENT_LEVEL, 1), by = list(ID, CONTENT_AREA)]
# table(Colorado_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR, ACH_LEV_P2], exclude=NULL) # for testing - create a alternate version first and check equality for non-NA values
# table(Colorado_SGP_LONG_Data[ACHIEVEMENT_LEVEL_PRIOR != ACH_LEV_P2, YEAR, GRADE], exclude=NULL)
# table(Colorado_SGP_LONG_Data[ACHIEVEMENT_LEVEL_PRIOR != ACH_LEV_P2 & YEAR=='2017', CONTENT_AREA, GRADE], exclude=NULL) Something off in 2017 (2016 prior) official results.  Prob don't need it - keep new


###   Create Lagged Scale Score (EQUATED) variables that include missing scores (and others potentially)
Colorado_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR_1YEAR", "SCALE_SCORE_PRIOR_2YEAR") := shift(SCALE_SCORE, 1:2), by = list(ID, CONTENT_AREA)]
##    Fix 2021 Lags since no 2020 data:
Colorado_SGP_LONG_Data[YEAR == '2021', SCALE_SCORE_PRIOR_2YEAR := SCALE_SCORE_PRIOR_1YEAR]
Colorado_SGP_LONG_Data[YEAR == '2021', SCALE_SCORE_PRIOR_1YEAR := NA]
# table(Colorado_SGP_LONG_Data[, YEAR, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# table(Colorado_SGP_LONG_Data[, GRADE, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL) # Remove 2YEAR priors for Grades 3 & 4 - repeaters
Colorado_SGP_LONG_Data[GRADE %in% c(3, 4), SCALE_SCORE_PRIOR_2YEAR := NA]
Colorado_SGP_LONG_Data[GRADE == 3, SCALE_SCORE_PRIOR_1YEAR := NA]
# table(Colorado_SGP_LONG_Data[YEAR=='2019', is.na(SCALE_SCORE_PRIOR_BASELINE), is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# cor(Colorado_SGP_LONG_Data[, SCALE_SCORE_PRIOR_BASELINE, SCALE_SCORE_PRIOR_2YEAR], use='complete.obs') # Not perfect.  Use BASELINE data when available:
Colorado_SGP_LONG_Data[!is.na(SCALE_SCORE_PRIOR_BASELINE), SCALE_SCORE_PRIOR_2YEAR := SCALE_SCORE_PRIOR_BASELINE]

###   Create Prior Score Deciles
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Functions", "Quantile_Cut_Functions.R"))

##    Establish a decile lookup based on 2019 for each CONTENT_AREA/GRADE combination
Colorado_SGP_LONG_Data[, SCALE_SCORE_PRIOR_2YEAR := round(SCALE_SCORE_PRIOR_2YEAR, 0)] # 2016 - 2018 EQUATED scores are not rounded!  Makes for ugly individual_sf tables...
Decile_Lookup <- Colorado_SGP_LONG_Data[YEAR == "2019" & !is.na(SCALE_SCORE_PRIOR_2YEAR),
                            as.list(getQuantcut(SCALE_SCORE_PRIOR_2YEAR)), keyby = c("CONTENT_AREA", "GRADE")]
prior.decile.fcase <- getFCASE("SCALE_SCORE_PRIOR_2YEAR", Decile_Lookup)
Colorado_SGP_LONG_Data[, PRIOR_DECILE_2YEAR := eval(parse(text=prior.decile.fcase))]
Colorado_SGP_LONG_Data[, PRIOR_DECILE_2YEAR := factor(PRIOR_DECILE_2YEAR, levels = paste0("Q", 1:10), labels=paste0("Decile_", 1:10))]

##    Some sanity checks
# table(Colorado_SGP_LONG_Data[, PRIOR_DECILE_2YEAR, GRADE], exclude=NULL)
# table(Colorado_SGP_LONG_Data[, PRIOR_DECILE_2YEAR, YEAR], exclude=NULL)
# table(Colorado_SGP_LONG_Data[, !is.na(SCALE_SCORE_PRIOR_2YEAR), PRIOR_DECILE_2YEAR,], exclude=NULL) # Should be 0 in the TRUE/<NA> cell
# Colorado_SGP_LONG_Data[!is.na(PRIOR_DECILE_2YEAR) & CONTENT_AREA=='MATHEMATICS' & GRADE=='5',
#             as.list(summary(SCALE_SCORE_PRIOR_2YEAR)), keyby = "PRIOR_DECILE_2YEAR"]
# Decile_Lookup[CONTENT_AREA=='MATHEMATICS' & GRADE=='5']
##  Verify no crazy numbers in HIGH of LOW
# summary(as.numeric(Decile_Lookup$HIGH))

###   From Nathan's
Colorado_SGP_LONG_Data[, ACHIEVEMENT_ProfandAbove := plyr::mapvalues(ACHIEVEMENT_LEVEL,
                           from = c("Level 1", "Level 2", "Level 3",
                                    "Level 4", "Level 5", "No Score"),
                           to   = c("Not Proficient", "Not Proficient", "Not Proficient",
                                    "Proficient", "Proficient", NA))]

Colorado_SGP_LONG_Data[, PRIOR_ACHIEVEMENT_ProfandAbove := plyr::mapvalues(ACHIEVEMENT_LEVEL_PRIOR,
                           from = c("Level 1", "Level 2", "Level 3",
                                    "Level 4", "Level 5", "No Score"),
                           to  =  c("Not Proficient", "Not Proficient", "Not Proficient",
                                    "Proficient", "Proficient", NA))]

#####
###   College_Entrance
#####



#####
###   ELP_Assessment
#####

###   Load/Format/Subset Report Data
load(file.path("..", "..", "..", "..", "ACCESS", "Data", "WIDA_CO_SGP_LONG_Data.Rdata"))

###   Subset only variables relevant to Learning Loss Report
# demographics <-  ...  #  Use from State_Assessment section above
# enrollment.status <- ...  #  Use from State_Assessment section above

elp.vars.to.keep <- c(
  "VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "GRADE",
  "SCALE_SCORE", "ACHIEVEMENT_LEVEL",
  "SCALE_SCORE_PRIOR", "SCALE_SCORE_PRIOR_STANDARDIZED", "ACHIEVEMENT_LEVEL_PRIOR",
  "SCALE_SCORE_PRIOR_BASELINE", "SCALE_SCORE_PRIOR_STANDARDIZED_BASELINE",
  "SGP", "SGP_BASELINE", "SGP_NORM_GROUP", "SGP_NORM_GROUP_BASELINE",
  "AYFEP", "AYFEP_PRIOR", "AGP", "OnTrack_AnyPathway",
  "DISTRICT_NUMBER", "DISTRICT_NAME", "SCHOOL_NUMBER", "SCHOOL_NAME",
  demographics, enrollment.status)

WIDA_CO_SGP_LONG_Data <- WIDA_CO_SGP_LONG_Data[, ..elp.vars.to.keep]

###   Merge in re-designation flag  --  CLARIFY YEAR WITH MARIE (2021? or 2020)

###   Re-factor ACHIEVEMENT_LEVEL_PRIOR to get values for kids with missing scores
WIDA_CO_SGP_LONG_Data[is.na(SCALE_SCORE), VALID_CASE := "VALID_CASE"]

#   Resolve duplicates
setkey(WIDA_CO_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, SCALE_SCORE)
setkey(WIDA_CO_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE)
# dups <- data.table(WIDA_CO_SGP_LONG_Data[unique(c(which(duplicated(WIDA_CO_SGP_LONG_Data, by=key(WIDA_CO_SGP_LONG_Data)))-1,
#                                                    which(duplicated(WIDA_CO_SGP_LONG_Data, by=key(WIDA_CO_SGP_LONG_Data))))), ],
#                                             key=key(WIDA_CO_SGP_LONG_Data))
# table(dups$VALID_CASE) # 40 2021 duplicates are NA SCALE_SCOREs with another score present
# table(WIDA_CO_SGP_LONG_Data[intersect(which(duplicated(WIDA_CO_SGP_LONG_Data, by=key(WIDA_CO_SGP_LONG_Data)))-1, which(VALID_CASE=="VALID_CASE")), is.na(SCALE_SCORE)])
WIDA_CO_SGP_LONG_Data[which(duplicated(WIDA_CO_SGP_LONG_Data, by=key(WIDA_CO_SGP_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"]


setkey(WIDA_CO_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE)
setkey(WIDA_CO_SGP_LONG_Data, VALID_CASE, CONTENT_AREA, YEAR, ID)
WIDA_CO_SGP_LONG_Data[which(duplicated(WIDA_CO_SGP_LONG_Data, by=key(WIDA_CO_SGP_LONG_Data)))-1, VALID_CASE:="INVALID_CASE"]

WIDA_CO_SGP_LONG_Data <- WIDA_CO_SGP_LONG_Data[VALID_CASE=="VALID_CASE"]

###   Create Lagged Achievement variables that include missing scores (and others potentially)
shift.key <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "VALID_CASE")
setkeyv(WIDA_CO_SGP_LONG_Data, shift.key)

# WIDA_CO_SGP_LONG_Data[, ACH_LEV_P2 := shift(ACHIEVEMENT_LEVEL, 1), by = list(ID, CONTENT_AREA)] # for testing - create a alternate version first and check equality for non-NA values
WIDA_CO_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR := shift(ACHIEVEMENT_LEVEL, 1), by = list(ID, CONTENT_AREA)]
# table(WIDA_CO_SGP_LONG_Data[, ACHIEVEMENT_LEVEL_PRIOR, ACH_LEV_P2], exclude=NULL)

###   Create Lagged Scale Score variables that include missing scores (and others potentially)
WIDA_CO_SGP_LONG_Data[, c("SCALE_SCORE_PRIOR_1YEAR", "SCALE_SCORE_PRIOR_2YEAR") := shift(SCALE_SCORE, 1:2), by = list(ID, CONTENT_AREA)]
# table(WIDA_CO_SGP_LONG_Data[, YEAR, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL)
# table(WIDA_CO_SGP_LONG_Data[, GRADE, is.na(SCALE_SCORE_PRIOR_2YEAR)], exclude=NULL) # Remove 2YEAR priors for Grades 0 & 1 - repeaters
WIDA_CO_SGP_LONG_Data[GRADE %in% c(0, 1), SCALE_SCORE_PRIOR_2YEAR := NA]
WIDA_CO_SGP_LONG_Data[GRADE == 0, SCALE_SCORE_PRIOR_1YEAR := NA]
# table(WIDA_CO_SGP_LONG_Data[YEAR=="2021", is.na(SCALE_SCORE_PRIOR), is.na(SCALE_SCORE_PRIOR_1YEAR)], exclude=NULL) # No SCALE_SCORE_PRIOR_BASELINE in WIDA
# cor(WIDA_CO_SGP_LONG_Data[, SCALE_SCORE_PRIOR, SCALE_SCORE_PRIOR_1YEAR], use="complete.obs") # Perfect.

###   Create Prior Score Deciles
##    Establish a decile lookup based on 2019 & 2020 for each GRADE
Decile_Lookup_1yr <- WIDA_CO_SGP_LONG_Data[YEAR == "2020" & !is.na(SCALE_SCORE_PRIOR_1YEAR),
                            as.list(getQuantcut(SCALE_SCORE_PRIOR_1YEAR)), keyby = c("CONTENT_AREA", "GRADE")]
prior1yr.decile.fcase <- getFCASE("SCALE_SCORE_PRIOR_1YEAR", Decile_Lookup_1yr)
WIDA_CO_SGP_LONG_Data[, PRIOR_DECILE_1YEAR := eval(parse(text=prior1yr.decile.fcase))]
WIDA_CO_SGP_LONG_Data[, PRIOR_DECILE_1YEAR := factor(PRIOR_DECILE_1YEAR, levels = paste0("Q", 1:10), labels=paste0("Decile_", 1:10))]

Decile_Lookup_2yr <- WIDA_CO_SGP_LONG_Data[YEAR == "2019" & !is.na(SCALE_SCORE_PRIOR_2YEAR),
                            as.list(getQuantcut(SCALE_SCORE_PRIOR_2YEAR)), keyby = c("CONTENT_AREA", "GRADE")]
prior2yr.decile.fcase <- getFCASE("SCALE_SCORE_PRIOR_2YEAR", Decile_Lookup_2yr)
WIDA_CO_SGP_LONG_Data[, PRIOR_DECILE_2YEAR := eval(parse(text=prior2yr.decile.fcase))]
WIDA_CO_SGP_LONG_Data[, PRIOR_DECILE_2YEAR := factor(PRIOR_DECILE_2YEAR, levels = paste0("Q", 1:10), labels=paste0("Decile_", 1:10))]

##    Some sanity checks
# table(WIDA_CO_SGP_LONG_Data[, PRIOR_DECILE_1YEAR, GRADE], exclude=NULL)
# table(WIDA_CO_SGP_LONG_Data[, PRIOR_DECILE_2YEAR, GRADE], exclude=NULL)
# table(WIDA_CO_SGP_LONG_Data[, PRIOR_DECILE_1YEAR, YEAR], exclude=NULL)
# table(WIDA_CO_SGP_LONG_Data[, PRIOR_DECILE_2YEAR, YEAR], exclude=NULL)
# table(WIDA_CO_SGP_LONG_Data[, !is.na(SCALE_SCORE_PRIOR_1YEAR), PRIOR_DECILE_1YEAR,], exclude=NULL) # Should be 0 in the TRUE/<NA> cell
# WIDA_CO_SGP_LONG_Data[!is.na(PRIOR_DECILE_1YEAR) & GRADE=='5',
#             as.list(summary(SCALE_SCORE_PRIOR_1YEAR)), keyby = "PRIOR_DECILE_1YEAR"]
# Decile_Lookup_1yr[GRADE=='5']
##  Verify no crazy numbers in HIGH of LOW
# summary(as.numeric(Decile_Lookup_1yr$HIGH))


#####
###   Interim_Assessment
#####


#####
###   Combine all data sources into `Report_Data` and Save
#####

Report_Data <- vector("list", 4);
names(Report_Data) <- c("State_Assessment", "College_Entrance", "ELP_Assessment", "Interim_Assessment")

Report_Data[["State_Assessment"]] <- copy(Colorado_SGP_LONG_Data); rm(Colorado_SGP_LONG_Data)
Report_Data[["ELP_Assessment"]] <- copy(WIDA_CO_SGP_LONG_Data); rm(WIDA_CO_SGP_LONG_Data)

if (!dir.exists(file.path("..", "Data"))) dir.create(file.path("..", "Data"))
save(Report_Data, file = file.path("..", "Data", "Report_Data.Rdata"))
setwd("..")
