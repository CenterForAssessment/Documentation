################################################################################
###                                                                          ###
###     Multiple imputation of 2021 scores and SGP analyses for Arizona      ###
###                                                                          ###
################################################################################

###   Load formated Report_Data
if (!exists("Report_Data"))  load("../Data/Report_Data.Rdata")

###   Locate the "Universal_Content" directory (assume woring directory is ./Documentation)
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

###   Declare an assessment flavor  --  does not extend directly to WIDA
assessment <- "State_Assessment"

###   Year to use for Knots_Boundaries (in none, use NULL)
kb.yr <- ".2015"

###   Load required packages
require(SGP)
require(SGPmatrices)
require(data.table)
require(cfaTools)

###   Make copy of the data to change/clean
long_data <- copy(Report_Data[[assessment]])

###   Clean up and dummy vars used in imputation models
long_data[, GENDER := ifelse(GENDER == "Female", 1, 0)]
# table(long_data[, ELL_STATUS, YEAR]) # Not sure what to make of ELL_STATUS...  Don't use for now
# long_data[, FREE_REDUCED_LUNCH_STATUS := ifelse(FREE_REDUCED_LUNCH_STATUS == "Not Economically Disadvantaged", 0, 1)] # already 1/0
# long_data[, SPED_STATUS := ifelse(SPED_STATUS == "Students without Disabilities (Non-IEP)", 0, 1)] # already 1/0
long_data[, ETHN_HISP := ifelse(ETHNICITY %like% "Hispanic", 1, 0)]
long_data[, ETHN_AFAM := ifelse(ETHNICITY %like% "African", 1, 0)]
long_data[, ETHN_WHITE := ifelse(ETHNICITY %like% "White", 1, 0)]
long_data[, ETHN_ASIAN := ifelse(ETHNICITY %like% "Asian", 1, 0)]


#####
###   Extend LONG data with "attrited" students and do a partial fill
#####

default.vars = c("CONTENT_AREA", "GRADE", "ACHIEVEMENT_LEVEL", "SCALE_SCORE", "SGP", "SGP_BASELINE")
demographics = c("FREE_REDUCED_LUNCH_STATUS", "SPED_STATUS",
                 "GENDER", "ETHN_AFAM", "ETHN_HISP", "ETHN_WHITE", "ETHN_ASIAN")
institutions = c("SCHOOL_NUMBER", "SCHOOL_ENROLLMENT_STATUS", "DISTRICT_NUMBER", "DISTRICT_ENROLLMENT_STATUS")

table(long_data[, GRADE, YEAR])
long_data[, GRADE := as.integer(GRADE)]

long.to.wide.vars <- c(default.vars, institutions, demographics)
tmp.wide <- dcast(long_data, ID + CONTENT_AREA ~ YEAR, sep=".", drop=FALSE, value.var=long.to.wide.vars)

##    Trim things down - remove cases with NAs in 2 most recent years
tmp.wide <- tmp.wide[!(is.na(SCALE_SCORE.2019) & is.na(SCALE_SCORE.2021))]

##  Fill in GRADE according to our expectations of normal progression
summary(tmp.wide[, grep("GRADE", names(tmp.wide), value=T), with=FALSE])
for (i in 1:4) { # Do this a couple times
  tmp.wide[is.na(GRADE.2016), GRADE.2016 := GRADE.2017-1L]
  tmp.wide[is.na(GRADE.2017), GRADE.2017 := GRADE.2018-1L]
  tmp.wide[is.na(GRADE.2017), GRADE.2017 := GRADE.2019-2L]
  tmp.wide[is.na(GRADE.2018), GRADE.2018 := GRADE.2019-1L]
  tmp.wide[is.na(GRADE.2018), GRADE.2018 := GRADE.2021-3L]
  tmp.wide[is.na(GRADE.2019), GRADE.2019 := GRADE.2021-2L]

  tmp.wide[is.na(GRADE.2021), GRADE.2021 := GRADE.2019+2L]
  tmp.wide[is.na(GRADE.2021), GRADE.2021 := GRADE.2018+3L]
  tmp.wide[is.na(GRADE.2019), GRADE.2019 := GRADE.2018+1L]
  tmp.wide[is.na(GRADE.2019), GRADE.2019 := GRADE.2017+2L]
  tmp.wide[is.na(GRADE.2019), GRADE.2019 := GRADE.2016+3L]
  tmp.wide[is.na(GRADE.2018), GRADE.2018 := GRADE.2017+1L]
  tmp.wide[is.na(GRADE.2018), GRADE.2018 := GRADE.2016+2L]
  tmp.wide[is.na(GRADE.2017), GRADE.2017 := GRADE.2016+1L]
}
summary(tmp.wide[, grep("GRADE", names(tmp.wide), value=T), with=FALSE])

meas.list <- vector(mode = "list", length = length(long.to.wide.vars))
meas.list <- lapply(long.to.wide.vars, function(f) meas.list[[f]] <- grep(paste0(f, "[.]"), names(tmp.wide)))
names(meas.list) <- long.to.wide.vars

###   First stretch out to get missings in log data
long_data <- melt(tmp.wide, id = c("ID", "CONTENT_AREA"), variable.name = "YEAR", measure=meas.list)
long_data[, CONTENT_AREA.1 := NULL] # table(long_data[, CONTENT_AREA.1, CONTENT_AREA], exclude=NULL)

##  Exclude non-existent GRADE levels
long_data <- long_data[GRADE %in% c(3:8, 10)]
table(long_data[, GRADE, YEAR])

#####
###   END -- Extend LONG data ...
#####

###   Fill in demographics
setkey(long_data, ID, YEAR)
long_data <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(long_data, ID),
                        tidyselect::all_of(demographics), .direction="downup")))

###   Fill in school and district numbers
long_data[is.na(SCHOOL_NUMBER), SCHOOL_NUMBER := "-99999"]
long_data[is.na(DISTRICT_NUMBER), DISTRICT_NUMBER := "-9999999"]

setattr(long_data$YEAR, "levels", c("2016", "2017", "2018", "2019", "2021"))
long_data[, YEAR := as.character(YEAR)]
long_data[, GRADE := as.character(GRADE)]
long_data[, VALID_CASE := "VALID_CASE"]

long_data[, SCHNMB_INT := as.integer(factor(SCHOOL_NUMBER))]
long_data[, DSTNMB_INT := as.integer(factor(DISTRICT_NUMBER))]

rm(tmp.wide);gc()


#####
###   Scale Score Imputation
#####

##    Read in GROWTH and STATUS imputation configs
source("./Report_Analyses/SGP_CONFIG/2021/MULTIPLE_IMPUTATION/ELA.R")
source("./Report_Analyses/SGP_CONFIG/2021/MULTIPLE_IMPUTATION/MATHEMATICS.R")

##    NO Missing for GRADE 3 and GRADE 4 when "extended".  DO NOT run STATUS for AZ!
# table(long_data[YEAR == "2021", is.na(SCALE_SCORE), GRADE])

growth_config_2021 <- c(ela_growth_config_2021, math_growth_config_2021)
status_config_2021 <- NULL # c(ela_status_config_2021, math_status_config_2021)

##    Change working directory
setwd("../Data")
if (!dir.exists("Imputation/diagnostics/")) dir.create("Imputation/diagnostics/", recursive=TRUE)

##    Set up a couple imputeScaleScore arguents used in message log
my.impute.method <- "2l.pan"
my.impute.long <- FALSE

tmp.messages <- paste("\n\t#####  BEGIN Scale Score Imputation", date(), "  #####\n")
started.impute <- proc.time()

##    Impute missing scale scores
Arizona_2021_Imputed <- imputeScaleScore(
	impute.data = long_data,
	additional.data = NULL,
	include.additional.missing = TRUE, # FALSE to just include kids in data (October Count)
	return.current.year.only = TRUE,
	compact.results = TRUE,
	diagnostics.dir = "Imputation",
	growth.config = growth_config_2021,
	status.config = status_config_2021,
	default.vars = c("CONTENT_AREA", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL"),
  demographics = c("FREE_REDUCED_LUNCH_STATUS", "SPED_STATUS", "GENDER",
                   "ETHN_AFAM", "ETHN_HISP", "ETHN_WHITE", "ETHN_ASIAN"),
	institutions = c("SCHNMB_INT", "SCHOOL_NUMBER", "SCHOOL_ENROLLMENT_STATUS",
                   "DSTNMB_INT", "DISTRICT_NUMBER", "DISTRICT_ENROLLMENT_STATUS"),
	impute.factors=c("SCHNMB_INT", "SCALE_SCORE",
	                 "FREE_REDUCED_LUNCH_STATUS", "SPED_STATUS", "GENDER",
                   "ETHN_AFAM", "ETHN_HISP", "ETHN_WHITE", "ETHN_ASIAN"),
	impute.long = my.impute.long,
	impute.method = my.impute.method,
	cluster.institution = TRUE,
	partial.fill = TRUE,
	parallel.config = list(packages = "mice", cores = 30),
	seed = 2072L,
	M = 30,
	maxit = 10,
	verbose=TRUE,
	allow.na=TRUE)

tmp.messages <- c(tmp.messages, paste("\n\t\tRaw Scale Score", my.impute.method, "LONG"[my.impute.long], "Imputation completed in", SGP:::convertTime(SGP:::timetakenSGP(started.impute))))

Arizona_2021_Imputed[, SCHNMB_INT := NULL]
Arizona_2021_Imputed[, DSTNMB_INT := NULL]

#####
###   SGP Analyses with Imputed Scale Scores
#####

started.sgp <- proc.time()

##    Create temporary pre-COVID (2018-2020) SGP Object from LONG data from above
Arizona_SGP_PreCovid <- prepareSGP(long_data[YEAR < 2021 & YEAR > 2017], state="AZ", create.additional.variables = FALSE)

###   Add single-cohort baseline matrices to SGPstateData
SGPstateData <- addBaselineMatrices("AZ", "2021")
# SGPstateData[["AZ"]][["Assessment_Program_Information"]][["CSEM"]] <- NULL

###   Read in BASELINE percentiles configuration scripts and combine
source("../Documentation/Report_Analyses/SGP_CONFIG/2021/PART_A/ELA.R")
source("../Documentation/Report_Analyses/SGP_CONFIG/2021/PART_A/MATHEMATICS.R")

AZ_2021_CONFIG_PART_A <- c(
	ELA_2021.config,
	MATHEMATICS_2021.config
)

##   Parallel processing for SGP analyses
sgp.cores <- 15
parallel.config <- list(
  BACKEND="PARALLEL",
  WORKERS=list(PERCENTILES = sgp.cores, BASELINE_PERCENTILES = sgp.cores))

imputation.n <- length(grep("SCORE_IMP_", names(Arizona_2021_Imputed)))

variables.to.get <-
      c("VALID_CASE", "ID", "YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL",
        "SCHOOL_NUMBER", "DISTRICT_NUMBER", "SCHOOL_ENROLLMENT_STATUS", "DISTRICT_ENROLLMENT_STATUS")

variables.to.keep <-
      c("VALID_CASE", "ID", "YEAR", "CONTENT_AREA", "GRADE", "SCALE_SCORE", "SGP", "SGP_BASELINE",
        "SCHOOL_NUMBER", "DISTRICT_NUMBER", "SCHOOL_ENROLLMENT_STATUS", "DISTRICT_ENROLLMENT_STATUS")

##    Run SGP analyses on each imputation (using `updateSGP`)
Imputed_SGP_Data <- data.table()
setwd("Imputation")

for (IMP in seq(imputation.n)) {
  Arizona_Data_LONG_2021 <- copy(Arizona_2021_Imputed[, c(variables.to.get, paste0("SCORE_IMP_", IMP)), with=FALSE])
  setnames(Arizona_Data_LONG_2021, c("SCALE_SCORE", paste0("SCORE_IMP_", IMP)), c("SCALE_SCORE_OBSERVED", "SCALE_SCORE"))

  ##  Force scores outside LOSS/HOSS back into range
  for (CA in c("ELA", "MATHEMATICS")) {
    for (G in c(3:8, 10)) {
      tmp.loss <- SGPstateData[["AZ"]][["Achievement"]][["Knots_Boundaries"]][[paste0(CA, kb.yr)]][[paste0("loss.hoss_", G)]][1]
      tmp.hoss <- SGPstateData[["AZ"]][["Achievement"]][["Knots_Boundaries"]][[paste0(CA, kb.yr)]][[paste0("loss.hoss_", G)]][2]
      Arizona_Data_LONG_2021[CONTENT_AREA == CA & GRADE == G & SCALE_SCORE < tmp.loss, SCALE_SCORE := tmp.loss]
      Arizona_Data_LONG_2021[CONTENT_AREA == CA & GRADE == G & SCALE_SCORE > tmp.hoss, SCALE_SCORE := tmp.hoss]
    }
  }

  TEMP_SGP <- updateSGP(
          what_sgp_object = Arizona_SGP_PreCovid,
          with_sgp_data_LONG = Arizona_Data_LONG_2021,
          steps = c("prepareSGP", "analyzeSGP", "combineSGP"),
          sgp.config = AZ_2021_CONFIG_PART_A,
          sgp.percentiles = TRUE,
          sgp.projections = FALSE,
          sgp.projections.lagged = FALSE,
          sgp.percentiles.baseline = TRUE,
          sgp.projections.baseline = FALSE,
          sgp.projections.lagged.baseline = FALSE,
          simulate.sgps=FALSE,
          goodness.of.fit.print = FALSE,
          save.intermediate.results = FALSE,
          parallel.config=parallel.config
  )

  Imputed_SGP_Data <- rbindlist(list(Imputed_SGP_Data, TEMP_SGP@Data[YEAR == "2021", variables.to.keep, with = FALSE][, IMP_N := IMP]))
  message(paste("\n\tSGP", my.impute.method, "LONG"[my.impute.long], "Imputation analysis -- IMP:", IMP, "-- completed", date()))
}  #  END IMP

##    Format and clean the Imputed_SGP_Data
Arizona_SGP_Data_Imputed <- dcast(Imputed_SGP_Data, VALID_CASE + ID + YEAR + CONTENT_AREA + GRADE ~ IMP_N,
  sep = "_IMPUTED_", drop = FALSE, value.var = c("SCHOOL_NUMBER", "DISTRICT_NUMBER", "SCALE_SCORE", "SGP", "SGP_BASELINE"))
Arizona_SGP_Data_Imputed <- Arizona_SGP_Data_Imputed[!is.na(SCALE_SCORE_IMPUTED_1)]
setnames(Arizona_SGP_Data_Imputed, c("SCHOOL_NUMBER_IMPUTED_1", "DISTRICT_NUMBER_IMPUTED_1"), c("SCHOOL_NUMBER", "DISTRICT_NUMBER"))
Arizona_SGP_Data_Imputed[, grep("SCHOOL_NUMBER_IMPUTED_|DISTRICT_NUMBER_IMPUTED_", names(Arizona_SGP_Data_Imputed)) := NULL]

##    Merge (2021 only):
setkeyv(Arizona_SGP_Data_Imputed, SGP:::getKey(Arizona_SGP_Data_Imputed))
Arizona_SGP_Data_Imputed <- long_data[Arizona_SGP_Data_Imputed]
Arizona_SGP_Data_Imputed[, c("i.SCHOOL_NUMBER", "i.DISTRICT_NUMBER") := NULL]
setnames(Arizona_SGP_Data_Imputed, c("SCALE_SCORE", "SGP", "SGP_BASELINE"), c("SCALE_SCORE_OBSERVED", "SGP_OBSERVED", "SGP_BASELINE_OBSERVED"))

tmp.messages <- c(tmp.messages, paste("\n\t\tSGP Analysis with", IMP, "imputations", "completed in", SGP:::convertTime(SGP:::timetakenSGP(started.sgp))))

##    Save Merged Imputed/Observed Data
setwd("..")
save(Arizona_SGP_Data_Imputed, file="Imputation/Arizona_SGP_Data_Imputed.rda")


#####
###   Summaries/Imputation Statistics
#####

started.smry <- proc.time()

setDTthreads(threads = min(15, parallel::detectCores(logical = FALSE)), throttle = 1024)
if (!dir.exists("Imputation/Summary_Tables")) dir.create("Imputation/Summary_Tables", recursive = TRUE)

Tmp_Summaries <- list()
Tmp_Summaries[["STATE"]][["GRADE"]] <- imputationSummary(copy(Arizona_SGP_Data_Imputed), summary.level = "GRADE")
Tmp_Summaries[["STATE"]][["CONTENT"]] <- imputationSummary(copy(Arizona_SGP_Data_Imputed), summary.level = "CONTENT_AREA")
Tmp_Summaries[["STATE"]][["GRADE_CONTENT"]] <- imputationSummary(copy(Arizona_SGP_Data_Imputed), summary.level = c("GRADE", "CONTENT_AREA"))
Tmp_Summaries[["DISTRICT"]][["GLOBAL"]] <- imputationSummary(copy(Arizona_SGP_Data_Imputed), summary.level = NULL, institution.level = "DISTRICT_NUMBER")
Tmp_Summaries[["DISTRICT"]][["GRADE"]] <- imputationSummary(copy(Arizona_SGP_Data_Imputed), summary.level = "GRADE", institution.level = "DISTRICT_NUMBER")
Tmp_Summaries[["DISTRICT"]][["CONTENT"]] <- imputationSummary(copy(Arizona_SGP_Data_Imputed), summary.level = "CONTENT_AREA", institution.level = "DISTRICT_NUMBER")
Tmp_Summaries[["DISTRICT"]][["GRADE_CONTENT"]] <- imputationSummary(copy(Arizona_SGP_Data_Imputed), summary.level = c("GRADE", "CONTENT_AREA"), institution.level = "DISTRICT_NUMBER")
Tmp_Summaries[["SCHOOL"]][["GLOBAL"]] <- imputationSummary(copy(Arizona_SGP_Data_Imputed), summary.level = NULL, institution.level = "SCHOOL_NUMBER")
Tmp_Summaries[["SCHOOL"]][["GRADE"]] <- imputationSummary(copy(Arizona_SGP_Data_Imputed), summary.level = "GRADE", institution.level = "SCHOOL_NUMBER")
Tmp_Summaries[["SCHOOL"]][["CONTENT"]] <- imputationSummary(copy(Arizona_SGP_Data_Imputed), summary.level = "CONTENT_AREA", institution.level = "SCHOOL_NUMBER")
Tmp_Summaries[["SCHOOL"]][["GRADE_CONTENT"]] <- imputationSummary(copy(Arizona_SGP_Data_Imputed), summary.level = c("GRADE", "CONTENT_AREA"), institution.level = "SCHOOL_NUMBER")
tmp.messages <- c(tmp.messages, paste("\n\t\tSGP Imputation summaries with 30 imputations completed in ", SGP:::convertTime(SGP:::timetakenSGP(started.smry))))

assign("Arizona_Imputation_Summaries", Tmp_Summaries)
save(Arizona_Imputation_Summaries, file="Imputation/Summary_Tables/Arizona_Imputation_Summaries.rda")

tmp.messages <- c(tmp.messages, paste("\n\n\t#####  END Arizona Multiple Imputation Analyses", date(), "  #####\n\n"))
messageLog(log.message = tmp.messages, logfile = "Arizona_Imputation_Analyses.txt", log.directory = "Imputation/Logs")

###   Investigate summaries

nrow(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 0,]) # 1901/1901 # 14.6% with 0 missing
summary(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 0, N])
hist(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 0, N], breaks = 50)
nrow(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 0 & N > 9,]) # 1826/1901 # 9.4% > 9, 5.15% > 49
length(unique(Tmp_Summaries[["SCHOOL"]][["GRADE"]][["Summary"]][Percent_Missing == 0, SCHOOL_NUMBER]))

nrow(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 100,])
summary(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 100, N])
miss.100 <- Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 100, SCHOOL_NUMBER] # Just the NA and-99999
# data.table(unique(long_data[YEAR == "2021" & SCHOOL_NUMBER %in% miss.100,
					# list(DISTRICT_NUMBER, SCHOOL_NUMBER, DISTRICT_NAME, SCHOOL_NAME)]), key=c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))
#
# summary(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing != 0 & Percent_Missing != 100, N])
# big.schools <- Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][N > 600, SCHOOL_NUMBER]
# big.schools <- data.table(unique(long_data[YEAR == "2021" & SCHOOL_NUMBER %in% big.schools,
#                             list(DISTRICT_NUMBER, SCHOOL_NUMBER)]), key="DISTRICT_NUMBER")
# big.schools
#
# nrow(Tmp_Summaries[["SCHOOL"]][["GRADE"]][["Summary"]][Percent_Missing == 0,]) # 82/233
# summary(Tmp_Summaries[["SCHOOL"]][["GRADE"]][["Summary"]][Percent_Missing == 0, N])
# nrow(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 0,])
# summary(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 0, N])

###
##    Schools Enrollement Changes
###

sch_size <- long_data[,
								list(SCORE_COUNT = sum(!is.na(SCALE_SCORE)),
                     TOTAL = .N),
									keyby = c("YEAR", "SCHOOL_NUMBER")]
setkey(sch_size, YEAR, SCORE_COUNT)

sch_size[, as.list(summary(TOTAL)), keyby = c("YEAR")]
sch_size[, as.list(summary(SCORE_COUNT)), keyby = c("YEAR")]

sch_size[, as.list(quantile(TOTAL, probs = seq(0, 1, 0.05))), keyby = c("YEAR")]
sch_size[, as.list(quantile(SCORE_COUNT, probs = seq(0, 1, 0.05))), keyby = c("YEAR")]

sch_size_wide <- dcast(sch_size[YEAR > "2017"], SCHOOL_NUMBER ~ YEAR, value.var = c("TOTAL", "SCORE_COUNT"))
summary(sch_size_wide[, TOTAL_2021])
summary(sch_size_wide[, TOTAL_2019 - TOTAL_2021])
big.19 <- sch_size_wide[TOTAL_2019 > 500, SCHOOL_NUMBER]
big.21 <- sch_size_wide[TOTAL_2021 > 500, SCHOOL_NUMBER]
big.21[!big.21 %in% big.19]
sch_size[SCHOOL_NUMBER=="5424"] # Look at!
sch_size[SCHOOL_NUMBER=="89799"] # Look at!
sch_size[SCHOOL_NUMBER=="6191"] # 6015 # Only 2021

sch_size_wide[, PCT_CHANGE_1_YEAR := TOTAL_2021/TOTAL_2019]
sch_size_wide[, PCT_CHANGE_2_YEAR := TOTAL_2021/rowMeans(.SD, na.rm=TRUE), .SDcols = c("TOTAL_2018", "TOTAL_2019")]
summary(sch_size_wide[, PCT_CHANGE_1_YEAR])
summary(sch_size_wide[, PCT_CHANGE_2_YEAR])

# big.increase <- sch_size_wide[TOTAL_2021 > 49 & PCT_CHANGE_2_YEAR > 1.25, SCHOOL_NUMBER] # moderately big increase, decent size
big.increase <- sch_size_wide[TOTAL_2021 > 49 & PCT_CHANGE_2_YEAR > 2, SCHOOL_NUMBER]
data.table(sch_size_wide[SCHOOL_NUMBER %in% big.increase], key="SCHOOL_NUMBER")
data.table(unique(long_data[YEAR == "2021" & SCHOOL_NUMBER %in% big.increase,
					list(DISTRICT_NUMBER, SCHOOL_NUMBER)]), key=c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))

sch_size_wide[SCHOOL_NUMBER %in% c(5039, 5040, 6007, 79823, 90124)] # District 4239
