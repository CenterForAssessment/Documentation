################################################################################
###                                                                          ###
###   Multiple imputation of 2021 scores and SGP analyses for Rhode Island   ###
###                                                                          ###
################################################################################

###   Load formated Report_Data
if (!exists("Report_Data"))  load("../Data/Report_Data.Rdata")

###   Locate the "Universal_Content" directory (assume woring directory is ./Documentation)
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

###   Declare an assessment flavor  --  does not extend directly to WIDA
assessment <- "State_Assessment"

###   Load required packages
require(SGP)
require(SGPmatrices)
require(data.table)
require(cfaTools)

###   Make copy of the data to change/clean
long_data <- copy(Report_Data[[assessment]])

long_data[, GENDER := ifelse(GENDER %in% c("F", "Female"), 1, 0)]
long_data[, ELL_STATUS := ifelse(ELL_STATUS == "Non-English Language Learners (ELL)", 0, 1)]
long_data[, FREE_REDUCED_LUNCH_STATUS := ifelse(FREE_REDUCED_LUNCH_STATUS == "Not Economically Disadvantaged", 0, 1)]
long_data[, IEP_STATUS := ifelse(IEP_STATUS == "Students without Disabilities (Non-IEP)", 0, 1)]
long_data[, ETHN_HISP := ifelse(ETHNICITY %like% "Hispanic", 1, 0)]
long_data[, ETHN_AFAM := ifelse(ETHNICITY %like% "African", 1, 0)]
long_data[, ETHN_WHITE := ifelse(ETHNICITY %like% "White", 1, 0)]
long_data[, ETHN_ASIAN := ifelse(ETHNICITY %like% "Asian", 1, 0)]


#####
###   Extend LONG data with "attrited" students and do a partial fill
#####

long_data[, YEAR := tstrsplit(YEAR, "_", type.convert=TRUE, keep=2)]

default.vars = c("CONTENT_AREA", "GRADE", "ACHIEVEMENT_LEVEL", "SCALE_SCORE", "SGP", "SGP_BASELINE")
demographics = c("FREE_REDUCED_LUNCH_STATUS", "IEP_STATUS", "ELL_STATUS",
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
long_data[, CONTENT_AREA.1 := NULL]

##  Exclude non-existent GRADE levels
long_data <- long_data[GRADE %in% 3:8]
table(long_data[, GRADE, YEAR])

###   Fill in demographics
setkey(long_data, ID, YEAR)
long_data <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(long_data, ID),
                        tidyselect::all_of(demographics), .direction="downup")))

###   Fill in school and district numbers
long_data[is.na(SCHOOL_NUMBER), SCHOOL_NUMBER := "-99999"]
long_data[is.na(DISTRICT_NUMBER), DISTRICT_NUMBER := "-9999999"]

###   Fill in school numbers (CLUDGE) - try to figure out how to do with random forrest or something better...
# tmp.long.elem <- long_data[GRADE %in% c(3:5)]
# tmp.long.elem <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(tmp.long.elem, ID),
#                               SCHOOL_NUMBER, .direction="updown")))
#
# tmp.long.mid <- long_data[GRADE %in% c(6:8)]
# tmp.long.mid <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(tmp.long.mid, ID),
#                             SCHOOL_NUMBER, .direction="updown")))

# long_data <- rbindlist(list(tmp.long.elem, tmp.long.mid))
# setkey(long_data, ID, YEAR)
#
# long_data <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(long_data, ID),
#                                           DISTRICT_NUMBER, .direction="updown")))

setattr(long_data$YEAR, "levels", c("2015_2016", "2016_2017", "2017_2018", "2018_2019", "2020_2021"))
long_data[, YEAR := as.character(YEAR)]
long_data[, GRADE := as.character(GRADE)]
long_data[, VALID_CASE := "VALID_CASE"]

long_data[, SCHNMB_INT := as.integer(factor(SCHOOL_NUMBER))]
long_data[, DSTNMB_INT := as.integer(factor(DISTRICT_NUMBER))]

# rm(list=c("long.final", "tmp.wide", "tmp.long.elem", "tmp.long.mid"));gc()
rm(tmp.wide);gc()

#####
###   Scale Score Imputation
#####

##    Read in GROWTH and STATUS imputation configs
source("./Report_Analyses/SGP_CONFIG/2020_2021/MULTIPLE_IMPUTATION/ELA.R")
source("./Report_Analyses/SGP_CONFIG/2020_2021/MULTIPLE_IMPUTATION/MATHEMATICS.R")

##    NO Missing for GRADE 3 and only 5 for GRADE 4.  DO NOT run STATUS for RI!
# table(long_data[YEAR == "2020_2021", is.na(SCALE_SCORE), GRADE])

growth_config_2021 <- c(ela_growth_config_2021, math_growth_config_2021)
status_config_2021 <- NULL  #  c(ela_status_config_2021, math_status_config_2021)

##    Change working directory
setwd("../Data")
if (!dir.exists("Imputation/diagnostics/")) dir.create("Imputation/diagnostics/", recursive=TRUE)

##    Set up a couple imputeScaleScore arguents used in message log
my.impute.method <- "2l.pan"
my.impute.long <- FALSE

tmp.messages <- paste("\n\t#####  BEGIN Scale Score Imputation", date(), "  #####\n")
started.impute <- proc.time()

##    Impute missing scale scores
Rhode_Island_2021_Imputed <- imputeScaleScore(
	impute.data = long_data,
	additional.data = NULL,
	include.additional.missing = FALSE, # Just include kids in data (usually October Count, but NA schools/districts filled in with -99... above)
	return.current.year.only = TRUE,
	compact.results = TRUE,
	diagnostics.dir = "Imputation",
	growth.config = growth_config_2021,
	status.config = status_config_2021,
	default.vars = c("CONTENT_AREA", "GRADE", "SCALE_SCORE", "ACHIEVEMENT_LEVEL"),
  demographics = c("FREE_REDUCED_LUNCH_STATUS", "IEP_STATUS", "ELL_STATUS",
                   "GENDER", "ETHN_AFAM", "ETHN_HISP", "ETHN_WHITE", "ETHN_ASIAN"),
	institutions = c("SCHNMB_INT", "SCHOOL_NUMBER", "SCHOOL_ENROLLMENT_STATUS", "DSTNMB_INT", "DISTRICT_NUMBER", "DISTRICT_ENROLLMENT_STATUS"),
	impute.factors = c("SCHNMB_INT", "SCALE_SCORE",
	                   "FREE_REDUCED_LUNCH_STATUS", "IEP_STATUS", "ELL_STATUS",
                     "GENDER", "ETHN_AFAM", "ETHN_HISP", "ETHN_WHITE", "ETHN_ASIAN"),
	impute.long = my.impute.long,
	impute.method = my.impute.method,
	cluster.institution = TRUE,
	partial.fill = TRUE,
	parallel.config = list(packages = "mice", cores = 15),
	seed = 2072L,
	M = 30,
	maxit = 10,
	verbose=TRUE,
	allow.na=TRUE)

tmp.messages <- c(tmp.messages, paste("\n\t\tRaw Scale Score", my.impute.method, "LONG"[my.impute.long], "Imputation completed in", SGP:::convertTime(SGP:::timetakenSGP(started.impute))))

# save(Rhode_Island_2021_Imputed, file="Imputation/Rhode_Island_2021_Imputed.rda")
# Rhode_Island_2021_Imputed[, SN2 := factor(SCHOOL_NUMBER, labels=sch.levels)]
# Rhode_Island_2021_Imputed[, DN2 := factor(DISTRICT_NUMBER, labels=dist.levels)]

Rhode_Island_2021_Imputed[, SCHNMB_INT := NULL]
Rhode_Island_2021_Imputed[, DSTNMB_INT := NULL]

#####
###   SGP Analyses with Imputed Scale Scores
#####

started.sgp <- proc.time()

##    Create temporary pre-COVID (2018-2020) SGP Object from LONG data from above
# load("Data/Archive/2020_2021/long_data.Rdata")
Rhode_Island_SGP_PreCovid <- prepareSGP(long_data[YEAR < "2020_2021" & YEAR > "2016_2017"], state="RI", create.additional.variables = FALSE)

###   Add single-cohort baseline matrices to SGPstateData
SGPstateData <- addBaselineMatrices("RI", "2020_2021")
SGPstateData[["RI"]][["Assessment_Program_Information"]][["CSEM"]] <- NULL

###   Read in BASELINE percentiles configuration scripts and combine
source("../Documentation/Report_Analyses/SGP_CONFIG/2020_2021/PART_A/ELA.R")
source("../Documentation/Report_Analyses/SGP_CONFIG/2020_2021/PART_A/MATHEMATICS.R")

RI_2021_CONFIG_PART_A <- c(
	ELA_2020_2021.config,
	MATHEMATICS_2020_2021.config
)

##   Parallel processing for SGP analyses
sgp.cores <- 13
parallel.config <- list(
  BACKEND="PARALLEL",
  WORKERS=list(PERCENTILES = sgp.cores, BASELINE_PERCENTILES = sgp.cores))

imputation.n <- length(grep("SCORE_IMP_", names(Rhode_Island_2021_Imputed)))

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
  Rhode_Island_Data_LONG_2021 <- copy(Rhode_Island_2021_Imputed[, c(variables.to.get, paste0("SCORE_IMP_", IMP)), with=FALSE])
  setnames(Rhode_Island_Data_LONG_2021, c("SCALE_SCORE", paste0("SCORE_IMP_", IMP)), c("SCALE_SCORE_OBSERVED", "SCALE_SCORE"))

  ##  Force scores outside LOSS/HOSS back into range
  for (CA in c("ELA", "MATHEMATICS")) {
    for (G in 3:8) {
      tmp.loss <- SGPstateData[["RI"]][["Achievement"]][["Knots_Boundaries"]][[CA]][[paste0("loss.hoss_", G)]][1]
      tmp.hoss <- SGPstateData[["RI"]][["Achievement"]][["Knots_Boundaries"]][[CA]][[paste0("loss.hoss_", G)]][2]
      Rhode_Island_Data_LONG_2021[CONTENT_AREA == CA & GRADE == G & SCALE_SCORE < tmp.loss, SCALE_SCORE := tmp.loss]
      Rhode_Island_Data_LONG_2021[CONTENT_AREA == CA & GRADE == G & SCALE_SCORE > tmp.hoss, SCALE_SCORE := tmp.hoss]
    }
  }

  TEMP_SGP <- updateSGP(
          what_sgp_object = Rhode_Island_SGP_PreCovid,
          with_sgp_data_LONG = Rhode_Island_Data_LONG_2021,
          steps = c("prepareSGP", "analyzeSGP", "combineSGP"),
          sgp.config = RI_2021_CONFIG_PART_A,
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

  Imputed_SGP_Data <- rbindlist(list(Imputed_SGP_Data, TEMP_SGP@Data[YEAR == "2020_2021", variables.to.keep, with = FALSE][, IMP_N := IMP]))
  message(paste("\n\tSGP", my.impute.method, "LONG"[my.impute.long], "Imputation analysis -- IMP:", IMP, "-- completed", date()))
}  #  END IMP

# setkey(Imputed_SGP_Data, IMP_N, VALID_CASE, CONTENT_AREA, YEAR, ID, GRADE, SCALE_SCORE)
# setkey(Imputed_SGP_Data, IMP_N, VALID_CASE, CONTENT_AREA, YEAR, ID)
# dups <- Imputed_SGP_Data[VALID_CASE=="VALID_CASE"][c(which(duplicated(Imputed_SGP_Data[VALID_CASE=="VALID_CASE"], by=key(Imputed_SGP_Data)))-1, which(duplicated(Imputed_SGP_Data[VALID_CASE=="VALID_CASE"], by=key(Imputed_SGP_Data)))),]
# setkeyv(dups, key(Imputed_SGP_Data))  #  0 duplicate cases RICAS data 8/8/19
# Imputed_SGP_Data[which(duplicated(Imputed_SGP_Data, by=key(Imputed_SGP_Data)))-1, VALID_CASE:="INVALID_CASE"]

Rhode_Island_SGP_Data_Imputed <- dcast(Imputed_SGP_Data, VALID_CASE + ID + YEAR + CONTENT_AREA + GRADE ~ IMP_N,
  sep = "_IMPUTED_", drop = FALSE, value.var = c("SCHOOL_NUMBER", "DISTRICT_NUMBER", "SCALE_SCORE", "SGP", "SGP_BASELINE"))
Rhode_Island_SGP_Data_Imputed <- Rhode_Island_SGP_Data_Imputed[!is.na(SCALE_SCORE_IMPUTED_1)]
setnames(Rhode_Island_SGP_Data_Imputed, c("SCHOOL_NUMBER_IMPUTED_1", "DISTRICT_NUMBER_IMPUTED_1"), c("SCHOOL_NUMBER", "DISTRICT_NUMBER"))
Rhode_Island_SGP_Data_Imputed[, grep("SCHOOL_NUMBER_IMPUTED_|DISTRICT_NUMBER_IMPUTED_", names(Rhode_Island_SGP_Data_Imputed)) := NULL]
tmp.messages <- c(tmp.messages, paste("\n\t\tSGP Analysis with", IMP, "imputations", "completed in", SGP:::convertTime(SGP:::timetakenSGP(started.sgp))))

setwd("..")
save(Rhode_Island_SGP_Data_Imputed, file="Imputation/Rhode_Island_SGP_Data_Imputed.rda")


#####
###   Summaries/Imputation Statistics
#####

started.smry <- proc.time()

setkeyv(Rhode_Island_SGP_Data_Imputed, SGP:::getKey(Rhode_Island_SGP_Data_Imputed))

##   Merge (2021 only):
Summary_Data <- long_data[Rhode_Island_SGP_Data_Imputed]
Summary_Data[, c("i.SCHOOL_NUMBER", "i.DISTRICT_NUMBER") := NULL]
# Summary_Data[is.na(SCHOOL_NUMBER), SCHOOL_NUMBER := "MISSING"]
# Summary_Data[is.na(DISTRICT_NUMBER), DISTRICT_NUMBER := "MISSING"]

setnames(Summary_Data, c("SCALE_SCORE", "SGP", "SGP_BASELINE"), c("SCALE_SCORE_OBSERVED", "SGP_OBSERVED", "SGP_BASELINE_OBSERVED"))

setDTthreads(threads = min(15, parallel::detectCores(logical = FALSE)), throttle = 1024)
if (!dir.exists("Imputation/Summary_Tables")) dir.create("Imputation/Summary_Tables", recursive = TRUE)

Tmp_Summaries <- list()
Tmp_Summaries[["STATE"]][["GRADE"]] <- imputationSummary(Summary_Data, summary.level = "GRADE")
Tmp_Summaries[["STATE"]][["CONTENT"]] <- imputationSummary(Summary_Data, summary.level = "CONTENT_AREA")
Tmp_Summaries[["STATE"]][["GRADE_CONTENT"]] <- imputationSummary(Summary_Data, summary.level = c("GRADE", "CONTENT_AREA"))
Tmp_Summaries[["DISTRICT"]][["GLOBAL"]] <- imputationSummary(Summary_Data, summary.level = NULL, institution.level = "DISTRICT_NUMBER")
Tmp_Summaries[["DISTRICT"]][["GRADE"]] <- imputationSummary(Summary_Data, summary.level = "GRADE", institution.level = "DISTRICT_NUMBER")
Tmp_Summaries[["DISTRICT"]][["CONTENT"]] <- imputationSummary(Summary_Data, summary.level = "CONTENT_AREA", institution.level = "DISTRICT_NUMBER")
Tmp_Summaries[["DISTRICT"]][["GRADE_CONTENT"]] <- imputationSummary(Summary_Data, summary.level = c("GRADE", "CONTENT_AREA"), institution.level = "DISTRICT_NUMBER")
Tmp_Summaries[["SCHOOL"]][["GLOBAL"]] <- imputationSummary(Summary_Data, summary.level = NULL, institution.level = "SCHOOL_NUMBER")
Tmp_Summaries[["SCHOOL"]][["GRADE"]] <- imputationSummary(Summary_Data, summary.level = "GRADE", institution.level = "SCHOOL_NUMBER")
Tmp_Summaries[["SCHOOL"]][["CONTENT"]] <- imputationSummary(Summary_Data, summary.level = "CONTENT_AREA", institution.level = "SCHOOL_NUMBER")
Tmp_Summaries[["SCHOOL"]][["GRADE_CONTENT"]] <- imputationSummary(Summary_Data, summary.level = c("GRADE", "CONTENT_AREA"), institution.level = "SCHOOL_NUMBER")
tmp.messages <- c(tmp.messages, paste("\n\t\tSGP Imputation summaries with 30 imputations completed in ", SGP:::convertTime(SGP:::timetakenSGP(started.smry))))

assign("Rhode_Island_Imputation_Summaries", Tmp_Summaries)
save(Rhode_Island_Imputation_Summaries, file="Imputation/Summary_Tables/Rhode_Island_Imputation_Summaries.rda")

tmp.messages <- c(tmp.messages, paste("\n\n\t#####  END FULL ANALYSES", date(), "  #####\n\n"))
messageLog(log.message = tmp.messages, logfile = "Rhode_Island_Imputation_Analyses.txt", log.directory = "Imputation/Logs")

###   Investigate summaries

nrow(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 0,]) # 34/233 # 14.6% with 0 missing
summary(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 0, N])
hist(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 0, N], breaks = 50)
nrow(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 0 & N > 9,]) # 22/233 # 9.4% > 9, 5.15% > 49
length(unique(Tmp_Summaries[["SCHOOL"]][["GRADE"]][["Summary"]][Percent_Missing == 0, SCHOOL_NUMBER]))

nrow(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 100,])
summary(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 100, N])
miss.100 <- Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 100, SCHOOL_NUMBER] # Just the "MISSING"/-99999 and a 2 student school
# data.table(unique(long_data[YEAR == "2020_2021" & SCHOOL_NUMBER %in% miss.100,
					# list(DISTRICT_NUMBER, SCHOOL_NUMBER, DISTRICT_NAME, SCHOOL_NAME)]), key=c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))

summary(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing != 0 & Percent_Missing != 100, N])
big.schools <- Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][N > 600, SCHOOL_NUMBER]
big.schools <- data.table(unique(long_data[YEAR == "2020_2021" & SCHOOL_NUMBER %in% big.schools,
                            list(DISTRICT_NUMBER, SCHOOL_NUMBER)]), key="DISTRICT_NUMBER")
big.schools # 4024, 5685

nrow(Tmp_Summaries[["SCHOOL"]][["GRADE"]][["Summary"]][Percent_Missing == 0,]) # 82/233
summary(Tmp_Summaries[["SCHOOL"]][["GRADE"]][["Summary"]][Percent_Missing == 0, N])
nrow(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 0,])
summary(Tmp_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 0, N])

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

sch_size_wide <- dcast(sch_size[YEAR > "2016_2017"], SCHOOL_NUMBER ~ YEAR, value.var = c("TOTAL", "SCORE_COUNT"))
summary(sch_size_wide[, TOTAL_2020_2021])
summary(sch_size_wide[, TOTAL_2018_2019 - TOTAL_2020_2021])
big.19 <- sch_size_wide[TOTAL_2018_2019 > 500, SCHOOL_NUMBER]
big.21 <- sch_size_wide[TOTAL_2020_2021 > 500, SCHOOL_NUMBER]
big.21[!big.21 %in% big.19]
sch_size[SCHOOL_NUMBER=="8606"] # Look at!
sch_size[SCHOOL_NUMBER=="28615"] # Look at!
sch_size[SCHOOL_NUMBER=="39133"] # Only 2021

sch_size_wide[, PCT_CHANGE_1_YEAR := TOTAL_2020_2021/TOTAL_2018_2019]
sch_size_wide[, PCT_CHANGE_2_YEAR := TOTAL_2020_2021/rowMeans(.SD, na.rm=TRUE), .SDcols = c("TOTAL_2017_2018", "TOTAL_2018_2019")]
summary(sch_size_wide[, PCT_CHANGE_1_YEAR])
summary(sch_size_wide[, PCT_CHANGE_2_YEAR])

# big.increase <- sch_size_wide[TOTAL_2020 > 29 & PCT_CHANGE_2_YEAR > 1.25, SCHOOL_NUMBER] # moderately big increase, decent size
big.increase <- sch_size_wide[PCT_CHANGE_2_YEAR > 2, SCHOOL_NUMBER]
data.table(sch_size_wide[SCHOOL_NUMBER %in% big.increase], key="SCHOOL_NUMBER")
data.table(unique(long_data[YEAR == "2020_2021" & SCHOOL_NUMBER %in% big.increase,
					list(DISTRICT_NUMBER, SCHOOL_NUMBER)]), key=c("DISTRICT_NUMBER", "SCHOOL_NUMBER"))

sch_size_wide[SCHOOL_NUMBER %in% c(28613, 28611, 28615)]

#####
###   Catepillar Plots
#####

plot.dir <- file.path("Imputation", "School_Level", "Catepillar")
if (!dir.exists(plot.dir)) dir.create(plot.dir, recursive=TRUE)
#
# ss.wide <- Summary_Data[, grep("SCHOOL_NUMBER|SCALE_SCORE_IMPUTED_", names(Summary_Data)),with=FALSE]
# setnames(ss.wide, gsub("SCALE_SCORE_IMPUTED_", "SSI_", names(ss.wide)))
# round(cor(ss.wide[,-1]), 3) # .95 for entire group,
# round(cor(ss.wide[SCHOOL_NUMBER!="-99999", -1], use='na.or.complete'), 3) # .97 for restricted group
#
# sgp.wide <- Summary_Data[, grep("SCHOOL_NUMBER|SGP_IMPUTED_", names(Summary_Data)),with=FALSE]
# setnames(sgp.wide, gsub("SGP_IMPUTED_", "SGPI_", names(sgp.wide)))
# round(cor(sgp.wide[,-1], use='na.or.complete'), 3) # .81 for entire group
# round(cor(sgp.wide[SCHOOL_NUMBER!="-99999", -1], use='na.or.complete'), 3) # .88 for restricted group
#
# nrow(Rhode_Island_Imputation_Summaries[["SCHOOL"]][["GLOBAL"]][["Summary"]][Percent_Missing == 0,])

ff <- copy(Rhode_Island_Imputation_Summaries[["SCHOOL"]][["CONTENT"]][["Summary"]])[SCHOOL_NUMBER != "-99999"]

summary(ff[, Mean_SS_Observed - Mean_SS_Imputed])
summary(ff[N > 9, Mean_SS_Observed - Mean_SS_Imputed])

##  ##  ##

for (CA in c("ELA", "MATHEMATICS")) {
  sch.cat.dat <- ff[CONTENT_AREA == CA & Percent_Missing != 0 & Percent_Missing != 100 & N > 14,
      list(SCHOOL_NUMBER, Mean_SS_Observed, Mean_SS_Imputed,
           SS_CI_low_simp, SS_CI_high_simp, SS_F_p_simp, Percent_Missing, N)]
  sch.cat.dat[, Sig_F := ifelse(SS_F_p_simp < 0.09, "Sig", "NS")]
  setkey(sch.cat.dat, Mean_SS_Imputed)
  sch.cat.dat[, SCHOOL_RANK := seq(nrow(sch.cat.dat))]

  ##  Status
  mean.imputed <- mean(sch.cat.dat$Mean_SS_Imputed)

  mss.cat <-
      ggplot(sch.cat.dat, aes(x=SCHOOL_RANK, y=Mean_SS_Observed, fill=Sig_F)) +
      # geom_point(aes(col=Sig_F), size=1.5) + # , position = position_dodge(width = 0.5)
      geom_errorbar(aes(col=Sig_F, ymin=SS_CI_low_simp, ymax=SS_CI_high_simp), width=0, size=1) +
      geom_point(size=1.5) + # , position = position_dodge(width = 0.5)
      geom_line(sch.cat.dat, mapping = aes(x=SCHOOL_RANK, y=Mean_SS_Imputed, fill=NULL), col="green", size=1, show.legend=FALSE) +
      # geom_smooth(sch.cat.dat, method = "gam", mapping = aes(x=SCHOOL_RANK, y=Mean_SS_Imputed, fill=NULL), se=FALSE, show.legend=FALSE) +
      geom_hline(yintercept=mean.imputed, col="black") + scale_color_manual(values=c("#B0B0B0", "#ff0000")) + theme_bw() + coord_flip()

  ggsave(filename = file.path(plot.dir, paste0(gsub("ematics", "", SGP::capwords(CA)), "_Sch_MSS_Catepillar.png")),
         plot=mss.cat, width=7, height = 5, dpi=150, units="in", bg="transparent")

  ##  Growth
  sch.cat.dat <- na.omit(ff[Percent_Missing != 0 & Percent_Missing != 100 & N > 14,
      list(SCHOOL_NUMBER, Mean_SGPB_Observed, Mean_SGPB_Imputed,
           SGPB_CI_low_simp, SGPB_CI_high_simp, SGP_F_p_simp, Percent_Missing, N)])
  sch.cat.dat[, Sig_F := ifelse(SGP_F_p_simp < 0.09, "Sig", "NS")]

  setkey(sch.cat.dat, Mean_SGPB_Imputed)
  sch.cat.dat[, SCHOOL_RANK := seq(nrow(sch.cat.dat))]

  msgp.cat <-
      ggplot(sch.cat.dat, aes(x=SCHOOL_RANK, y=Mean_SGPB_Observed, fill=Sig_F)) +
      # geom_point(aes(col=Sig_F), size=1.5) + # , position = position_dodge(width = 0.5)
      geom_errorbar(aes(col=Sig_F, ymin=SGPB_CI_low_simp, ymax=SGPB_CI_high_simp), width=0, size=1) +
      geom_point(size=1.5) + # , position = position_dodge(width = 0.5)
      geom_line(sch.cat.dat, mapping = aes(x=SCHOOL_RANK, y=Mean_SGPB_Imputed, fill=NULL), col="green", size=1, show.legend=FALSE) +
      # geom_smooth(sch.cat.dat, method = "gam", mapping = aes(x=SCHOOL_RANK, y=Mean_SGPB_Imputed, fill=NULL), se=FALSE, show.legend=FALSE) +
      geom_hline(yintercept=50, col="black") + scale_color_manual(values=c("#B0B0B0", "#ff0000")) + theme_bw() + coord_flip()

  ggsave(filename = file.path(plot.dir, paste0(gsub("ematics", "", SGP::capwords(CA)), "_Sch_MSGP_Catepillar.png")), # paste0(gsub("ematics", "", tmp.subj), "_Impact_Catepillar_GES_", ges_type, ".png")),
         plot=msgp.cat, width=7, height = 5, dpi=150, units="in", bg="transparent")
}

###  School by Grade
tmp_smry <- copy(Rhode_Island_Imputation_Summaries[["SCHOOL"]][["GRADE_CONTENT"]][["Summary"]])[SCHOOL_NUMBER != "MISSING"]

ss.cat.dat = tmp_smry[Percent_Missing != 0 & Percent_Missing != 100 & N > 14,
    list(SCHOOL_NUMBER, GRADE, Mean_SS_Observed, Mean_SS_Imputed,
         SS_CI_low_simp, SS_CI_high_simp, SS_F_p_simp, Percent_Missing, N)]
ss.cat.dat[, Sig_F := ifelse(SS_F_p_simp < 0.09, "Sig", "NS")]
setkey(ss.cat.dat, GRADE, Mean_SS_Imputed)
ss.cat.dat[, SCHOOL_RANK := seq(.N), by="GRADE"]

mean.imputed <- mean(ss.cat.dat$Mean_SS_Imputed)

# faceted
mss.cat.fac <-
    ggplot(ss.cat.dat[GRADE %in% 5:8], aes(x=SCHOOL_RANK, y=Mean_SS_Observed, fill=Sig_F)) +
    geom_errorbar(aes(col=Sig_F, ymin=SS_CI_low_simp, ymax=SS_CI_high_simp), width=0, size=1) +
    geom_point(size=1.5) + # , position = position_dodge(width = 0.5)
    geom_line(ss.cat.dat[GRADE %in% 5:8], mapping = aes(x=SCHOOL_RANK, y=Mean_SS_Imputed, fill=NULL), col="green", size=1, show.legend=FALSE) +
    scale_color_manual(values=c("#B0B0B0", "#ff0000")) + facet_wrap(~GRADE) + theme_bw() + coord_flip()

ggsave(filename = file.path(plot.dir, "Test_Sch_MSS_Grade_Catepillar.png"), # paste0(gsub("ematics", "", tmp.subj), "_Impact_Catepillar_GES_", ges_type, ".png")),
       plot=mss.cat.fac, width=7, height = 5, dpi=150, units="in", bg="transparent")

# individual
G <- "5"
tmp.prof.cut <- SGPstateData[["RI"]][["Achievement"]][["Cutscores"]][["READING.2017"]][[paste0("GRADE_", G)]][3]
ela.cut.1yp <- SGPstateData[["RI"]][["Achievement"]][["Cutscores"]][["ELA.2017_2018"]][[paste0("GRADE_", G)]][prof.cut]
ela.cut.2yp <- SGPstateData[["RI"]][["Achievement"]][["Cutscores"]][["ELA.2017_2018"]][["GRADE_3"]][prof.cut]
math.cut.1yp<- SGPstateData[["RI"]][["Achievement"]][["Cutscores"]][["MATHEMATICS.2017_2018"]][["GRADE_5"]][prof.cut]
math.cut.2yp<- SGPstateData[["RI"]][["Achievement"]][["Cutscores"]][["MATHEMATICS.2017_2018"]][["GRADE_4"]][prof.cut]

ggplot(ss.cat.dat[GRADE == 5], aes(x=SCHOOL_RANK, y=Mean_SS_Observed, fill=Sig_F)) +
geom_errorbar(aes(col=Sig_F, ymin=SS_CI_low_simp, ymax=SS_CI_high_simp), width=0, size=1) +
geom_point(size=1.5) + # , position = position_dodge(width = 0.5)
geom_line(ss.cat.dat[GRADE == 5], mapping = aes(x=SCHOOL_RANK, y=Mean_SS_Imputed, fill=NULL), col="green", size=1, show.legend=FALSE) +
geom_hline(yintercept=tmp.prof.cut, col="black") + scale_color_manual(values=c("#B0B0B0", "#ff0000"))+ theme_bw() + coord_flip()



##

sch.cat.dat <- ff[CONTENT_AREA == CA & Percent_Missing != 0 & Percent_Missing != 100 & N > 14,
    list(SCHOOL_NUMBER, Mean_SS_Observed, Mean_SS_Imputed,
         SS_CI_low_simp, SS_CI_high_simp, SS_F_p_simp, Percent_Missing, N)]
sch.cat.dat[, Sig_F := ifelse(SS_F_p_simp < 0.09, "Sig", "NS")]
setkey(sch.cat.dat, Mean_SS_Imputed)
sch.cat.dat[, SCHOOL_RANK := seq(nrow(sch.cat.dat))]

mss.pctm <-
    ggplot(sch.cat.dat, aes(x=Percent_Missing, y=Mean_SS_Observed, fill=Sig_F)) +
    # geom_point(aes(col=Sig_F), size=1.5) + # , position = position_dodge(width = 0.5)
    geom_point(aes(size=N, alpha=0.5)) + # , position = position_dodge(width = 0.5)
    geom_errorbar(aes(col=Sig_F, ymin=SS_CI_low_simp, ymax=SS_CI_high_simp), width=0, size=1) +
    # geom_line(sch.cat.dat, mapping = aes(x=SCHOOL_RANK, y=Mean_SS_Imputed, fill=NULL), col="green", size=1, show.legend=FALSE) +
    geom_smooth(sch.cat.dat, method = "gam", mapping = aes(x=Percent_Missing, y=Mean_SS_Imputed, fill=NULL), se=FALSE, show.legend=FALSE) +
    geom_hline(yintercept=mean.imputed, col="black") + scale_color_manual(values=c("#B0B0B0", "#ff0000")) + theme_bw() # + coord_flip()

ggsave(filename = file.path(plot.dir, paste0(gsub("ematics", "", SGP::capwords(CA)), "_Sch_MSS_Pct_Missing.png")),
       plot=mss.pctm, width=7, height = 5, dpi=150, units="in", bg="transparent")
