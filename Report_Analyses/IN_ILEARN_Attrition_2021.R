################################################################################
###                                                                          ###
###        Indiana  --  2021 Missing Data (enrollment and attrition)         ###
###                                                                          ###
################################################################################

###   General Setup (assumes running from ./Documentation directory)

if (!exists("Report_Analyses"))
    stop("Script assumes you have either loaded an existing 'Report_Analyses' object.")

###   Load formated Report_Data
if (!exists("Report_Data"))  load("../Data/Report_Data.Rdata")

###   Locate the "Universal_Content" directory (assume woring directory is ./Documentation)
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

###   Declare an assessment flavor  --  does not extend directly to WIDA
assessment <- "State_Assessment"

###  Load required packages
require(SGP)
require(VIM)
require(cfaTools)
require(data.table)

###   Make copy of the data to change/clean
long_data <- copy(Report_Data[[assessment]])

long_data[, GRADE := as.integer(GRADE)]
long_data[, ACHIEVEMENT_LEVEL := gsub(" Proficiency", "", ACHIEVEMENT_LEVEL)]

###   clean up demographics for plots
long_data[, SOCIO_ECONOMIC_STATUS := gsub(" meals", "", SOCIO_ECONOMIC_STATUS)]
long_data[, SOCIO_ECONOMIC_STATUS := gsub(" price", "", SOCIO_ECONOMIC_STATUS)]
long_data[, SPECIAL_EDUCATION_STATUS := gsub(" Education", "", SPECIAL_EDUCATION_STATUS)]
long_data[, ENGLISH_LANGUAGE_LEARNER_STATUS := gsub(" English Proficient", "", ENGLISH_LANGUAGE_LEARNER_STATUS)]
long_data[ENGLISH_LANGUAGE_LEARNER_STATUS %in% c("Native English Speaking Immigrant", "Not an English Language Learner"),
                        ENGLISH_LANGUAGE_LEARNER_STATUS := "No"]

long_data[, ETHNICITY := as.character(ETHNICITY)]
# long_data[, ETHNICITY := gsub("Native Hawaiian or Other Pacific Islander", "NH/PI", ETHNICITY)]
long_data[!ETHNICITY %in% c("Black", "Hispanic", "White") & !is.na(ETHNICITY), ETHNICITY := "All Other"]

# setkey(long_data, VALID_CASE, CONTENT_AREA, YEAR, ID, SCALE_SCORE)
# setkey(long_data, VALID_CASE, CONTENT_AREA, YEAR, ID)
# dups <- data.table(long_data[unique(c(which(duplicated(long_data,
#            by=key(long_data)))-1, which(duplicated(long_data,
#               by=key(long_data))))), ], key=key(long_data))
# table(dups$VALID_CASE) # 2 duplicates (1 student) in 2021 with multiple grades
# long_data[which(duplicated(long_data, by=key(long_data)))-1, VALID_CASE:="INVALID_CASE"]

# long_data <- long_data[GRADE %in% 3:8 & VALID_CASE == "VALID_CASE"]

##    Create a WIDE dataset

default.vars = c("VALID_CASE", "GRADE", "SCALE_SCORE", "SCALE_SCORE_ORIGINAL", "ACHIEVEMENT_LEVEL")
demographics = c("SOCIO_ECONOMIC_STATUS", "ETHNICITY", "ENGLISH_LANGUAGE_LEARNER_STATUS", "SPECIAL_EDUCATION_STATUS", "GENDER")
institutions = c("DISTRICT_NUMBER", "SCHOOL_NUMBER")

long.to.wide.vars <- c(default.vars, institutions, demographics)

ILEARN_WIDE <- dcast(long_data,
                  ID + CONTENT_AREA ~ YEAR, sep=".", drop=FALSE, value.var=long.to.wide.vars)

###   Probably only needed for missing data (student attrition?) analysis:
# summary(ILEARN_WIDE[, grep("GRADE", names(ILEARN_WIDE), value=T), with=FALSE])
for (i in 1:4) { # Do this a couple times
  ILEARN_WIDE[is.na(GRADE.2016), GRADE.2016 := GRADE.2017-1L]
  ILEARN_WIDE[is.na(GRADE.2017), GRADE.2017 := GRADE.2018-1L]
  ILEARN_WIDE[is.na(GRADE.2017), GRADE.2017 := GRADE.2019-2L]
  ILEARN_WIDE[is.na(GRADE.2018), GRADE.2018 := GRADE.2019-1L]
  ILEARN_WIDE[is.na(GRADE.2018), GRADE.2018 := GRADE.2021-3L]
  ILEARN_WIDE[is.na(GRADE.2019), GRADE.2019 := GRADE.2021-2L]

  ILEARN_WIDE[is.na(GRADE.2021), GRADE.2021 := GRADE.2019+2L]
  ILEARN_WIDE[is.na(GRADE.2021), GRADE.2021 := GRADE.2018+3L]
  ILEARN_WIDE[is.na(GRADE.2019), GRADE.2019 := GRADE.2018+1L]
  ILEARN_WIDE[is.na(GRADE.2019), GRADE.2019 := GRADE.2017+2L]
  ILEARN_WIDE[is.na(GRADE.2019), GRADE.2019 := GRADE.2016+3L]
  ILEARN_WIDE[is.na(GRADE.2018), GRADE.2018 := GRADE.2017+1L]
  ILEARN_WIDE[is.na(GRADE.2018), GRADE.2018 := GRADE.2016+2L]
  ILEARN_WIDE[is.na(GRADE.2017), GRADE.2017 := GRADE.2016+1L]
}

meas.list <- vector(mode = "list", length = length(long.to.wide.vars))
meas.list <- lapply(long.to.wide.vars, function(f) meas.list[[f]] <- grep(paste0(f, "[.]"), names(ILEARN_WIDE)))
names(meas.list) <- long.to.wide.vars

###   First stretch out to get missings in log data
tmp_long <- melt(ILEARN_WIDE, id = c("ID", "CONTENT_AREA"), variable.name = "YEAR", measure=meas.list)

##  Exclude non-existent GRADE levels
tmp_long <- tmp_long[GRADE %in% 3:8]
# table(tmp_long[, GRADE, YEAR])

###   Fill in demographics
setkey(tmp_long, ID, YEAR)
tmp_long <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(tmp_long, ID),
                        tidyselect::all_of(demographics), .direction="downup")))

tmp_long[, GRADE := as.integer(GRADE)]
tmp_long[, YEAR := factor(YEAR)]
setattr(tmp_long$YEAR, "levels", c("2016", "2017", "2018", "2019", "2021"))
tmp_long[, ACHIEVEMENT_LEVEL := factor(ACHIEVEMENT_LEVEL, levels=c("Below", "Approaching", "At", "Above"))]
tmp_long[, SOCIO_ECONOMIC_STATUS := factor(SOCIO_ECONOMIC_STATUS, levels=c("Free", "Reduced", "Paid", NA))]

##    Final WIDE dataset with missing GRADE and demographics filled in
ILEARN_WIDE <- dcast(tmp_long,
                  ID + CONTENT_AREA ~ YEAR, sep=".", drop=FALSE, value.var=long.to.wide.vars)


#####
###   Plots
#####

plot_directory <- "assets/Rplots/participation/VIM_Plots"
if (!dir.exists(plot_directory)) dir.create(plot_directory, recursive=TRUE)


svg(file.path(plot_directory, "SS_Histograms-All_Grades.svg"))
par(mfrow = c(3, 1))
# 2019
histMiss(as.data.frame(ILEARN_WIDE[GRADE.2019 %in% 4:8, c("SCALE_SCORE.2018", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2018 - All Grades/Subjects", breaks=150, interactive=FALSE, only.miss=FALSE)
# abline(v=0, col="green", lwd=2)
histMiss(as.data.frame(ILEARN_WIDE[GRADE.2019 %in% 5:8, c("SCALE_SCORE.2017", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2017 - All Grades/Subjects", breaks=150, interactive=FALSE, only.miss=FALSE)

# 2021
histMiss(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 5:8, c("SCALE_SCORE.2019", "SCALE_SCORE.2021")]),
         main = "Missing 2021 to 2019 - All Grades/Subjects", breaks=150, interactive=FALSE, only.miss=FALSE)
# abline(v=0, col="green", lwd=2)
dev.off()

###   5th Grade - only elementary grade with possible priors in 2021
ela.cut.1yp <- SGPstateData[["IN"]][["Achievement"]][["Cutscores"]][["ELA.2019"]][["GRADE_4"]][2]
ela.cut.2yp <- SGPstateData[["IN"]][["Achievement"]][["Cutscores"]][["ELA.2019"]][["GRADE_3"]][2]
math.cut.1yp<- SGPstateData[["IN"]][["Achievement"]][["Cutscores"]][["MATHEMATICS.2019"]][["GRADE_4"]][2]
math.cut.2yp<- SGPstateData[["IN"]][["Achievement"]][["Cutscores"]][["MATHEMATICS.2019"]][["GRADE_3"]][2]

svg(file.path(plot_directory, "SS_Histograms-G5.svg"))
par(mfrow = c(3, 1))
# 2019
histMiss(as.data.frame(ILEARN_WIDE[GRADE.2019 %in% 5, c("SCALE_SCORE.2018", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2018 - 5th Grade", breaks=150, interactive=FALSE, only.miss=FALSE)
abline(v=ela.cut.1yp, col="green", lwd=2)
abline(v=math.cut.1yp, col="green", lwd=2)

histMiss(as.data.frame(ILEARN_WIDE[GRADE.2019 %in% 5, c("SCALE_SCORE.2017", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2017 - 5th Grade", breaks=150, interactive=FALSE, only.miss=FALSE)
abline(v=ela.cut.2yp, col="green", lwd=2)
abline(v=math.cut.2yp, col="green", lwd=2)

# 2021
histMiss(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 5, c("SCALE_SCORE.2019", "SCALE_SCORE.2021")]),
         main = "Missing 2021 to 2019 - 5th Grade", breaks=150, interactive=FALSE, only.miss=FALSE)
abline(v=ela.cut.2yp, col="green", lwd=2)
abline(v=math.cut.2yp, col="green", lwd=2)
dev.off()


svg(file.path(plot_directory, "SS_Histograms-Mid_Grades.svg"))
par(mfrow = c(3, 1))
# 2019
histMiss(as.data.frame(ILEARN_WIDE[GRADE.2019 %in% 6:8, c("SCALE_SCORE.2018", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2018 - Middle Grades/Subjects", breaks=150, interactive=FALSE, only.miss=FALSE)
histMiss(as.data.frame(ILEARN_WIDE[GRADE.2019 %in% 6:8, c("SCALE_SCORE.2017", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2017 - Middle Grades/Subjects", breaks=150, interactive=FALSE, only.miss=FALSE)

# 2021
histMiss(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 6:8, c("SCALE_SCORE.2019", "SCALE_SCORE.2021")]),
         main = "Missing 2021 to 2019 - Middle Grades/Subjects", breaks=150, interactive=FALSE, only.miss=FALSE)
dev.off()

# marginplot(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 5 & CONTENT_AREA == "ELA", c("SCALE_SCORE.2019", "SCALE_SCORE.2021")]))
# marginplot(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 5 & CONTENT_AREA == "MATHEMATICS", c("SCALE_SCORE.2019", "SCALE_SCORE.2021")]))
# marginplot(as.data.frame(ILEARN_WIDE[GRADE.2019 %in% 5 & CONTENT_AREA == "ELA", c("SCALE_SCORE.2018", "SCALE_SCORE.2019")]))
# marginplot(as.data.frame(ILEARN_WIDE[GRADE.2019 %in% 5 & CONTENT_AREA == "MATHEMATICS", c("SCALE_SCORE.2018", "SCALE_SCORE.2019")]))
#
# scattmatrixMiss(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 8 & CONTENT_AREA == "MATHEMATICS", c("SCALE_SCORE.2018", "SCALE_SCORE.2019", "SCALE_SCORE.2021")]), interactive=FALSE)
# scattmatrixMiss(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 8 & CONTENT_AREA == "MATHEMATICS", c("SCALE_SCORE.2016", "SCALE_SCORE.2017", "SCALE_SCORE.2019")]), interactive=FALSE)
#
# spineMiss(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 5:8, c("ACHIEVEMENT_LEVEL.2019", "SCALE_SCORE.2021")]), interactive=FALSE, only.miss=FALSE)
# spineMiss(as.data.frame(ILEARN_WIDE[GRADE.2019 %in% 5:8, c("ACHIEVEMENT_LEVEL.2017", "SCALE_SCORE.2019")]), interactive=FALSE, only.miss=FALSE)
# spineMiss(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 5:8, c("ETHNICITY.2019", "SCALE_SCORE.2021")]), interactive=FALSE, only.miss=FALSE)
# spineMiss(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 5:8, c("SOCIO_ECONOMIC_STATUS.2019", "SCALE_SCORE.2021")]), interactive=FALSE, only.miss=FALSE)
# spineMiss(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 5:8, c("SPECIAL_EDUCATION_STATUS.2019", "SCALE_SCORE.2021")]), interactive=FALSE, only.miss=FALSE)
# spineMiss(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 5:8, c("ENGLISH_LANGUAGE_LEARNER_STATUS.2019", "SCALE_SCORE.2021")]), interactive=FALSE, only.miss=FALSE)
# spineMiss(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 5:8, c("GENDER.2019", "SCALE_SCORE.2021")]), interactive=FALSE, only.miss=FALSE)

svg(file.path(plot_directory, "ILEARN_Mosaic_AchLev_SES.svg"))
mosaicMiss(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 5:8, c("ACHIEVEMENT_LEVEL.2019", "SOCIO_ECONOMIC_STATUS.2019", "SCALE_SCORE.2021")]),
           highlight = 3, plotvars = 1:2, miss.labels = FALSE)
dev.off()

svg(file.path(plot_directory, "ILEARN_Mosaic_Ethn_SES.svg"))
mosaicMiss(as.data.frame(ILEARN_WIDE[GRADE.2021 %in% 5:8, c("ETHNICITY.2019", "SOCIO_ECONOMIC_STATUS.2019", "SCALE_SCORE.2021")]),
          highlight = 3, plotvars = 1:2, miss.labels = FALSE, only.miss=FALSE)
dev.off()


###   Tables of Attrition and non-participation

attrit_2021 <-
  ILEARN_WIDE[GRADE.2021 %in% 5:8 & !is.na(SCALE_SCORE.2019),
                .(Pct_Attrit = round(sum(is.na(VALID_CASE.2021))/.N, 3)*100,
                  Total_Attrit = sum(is.na(VALID_CASE.2021)),
                  Total_Enrolled = sum(!is.na(VALID_CASE.2021))),
               keyby = .(CONTENT_AREA, GRADE.2021)]
setnames(attrit_2021, 2, "GRADE")
attrit_2021[, YEAR := "2019_to_2021"]

attrit_2019 <-
  ILEARN_WIDE[GRADE.2019 %in% 5:8 & !is.na(SCALE_SCORE.2017),
               .(Pct_Attrit = round(sum(is.na(VALID_CASE.2019))/.N, 3)*100,
                 Total_Attrit = sum(is.na(VALID_CASE.2019)),
                 Total_Enrolled = sum(!is.na(VALID_CASE.2019))),
               keyby = .(CONTENT_AREA, GRADE.2019)]
setnames(attrit_2019, 2, "GRADE")
attrit_2019[, YEAR := "2017_to_2019"]

attrit_2018 <-
  ILEARN_WIDE[GRADE.2018 %in% 5:8 & !is.na(SCALE_SCORE.2016),
               .(Pct_Attrit = round(sum(is.na(VALID_CASE.2018))/.N, 3)*100,
                 Total_Attrit = sum(is.na(VALID_CASE.2018)),
                 Total_Enrolled = sum(!is.na(VALID_CASE.2018))),
               keyby = .(CONTENT_AREA, GRADE.2018)]
setnames(attrit_2018, 2, "GRADE")
attrit_2018[, YEAR := "2016_to_2018"]

attrit_wide <- dcast(rbindlist(list(attrit_2018, attrit_2019, attrit_2021)),
                  CONTENT_AREA + GRADE ~ YEAR, sep=".", value.var = c("Pct_Attrit", "Total_Attrit", "Total_Enrolled"))

Report_Analyses[["participation"]][[assessment]][["state_attrition"]] <- attrit_wide


##    Attrition and non-participation
# ILEARN_WIDE[GRADE.2021 %in% 5:8 & !is.na(SCALE_SCORE.2019), .(round(sum(is.na(SCALE_SCORE.2021))/.N, 3)*100), keyby = .(CONTENT_AREA, GRADE.2021)]
# ILEARN_WIDE[GRADE.2019 %in% 5:8 & !is.na(SCALE_SCORE.2018), .(round(sum(is.na(SCALE_SCORE.2019))/.N, 3)*100), keyby = .(CONTENT_AREA, GRADE.2019)]
# ILEARN_WIDE[GRADE.2019 %in% 5:8 & !is.na(SCALE_SCORE.2017), .(round(sum(is.na(SCALE_SCORE.2019))/.N, 3)*100), keyby = .(CONTENT_AREA, GRADE.2019)]
#
# ##    Non-participation Only
# ILEARN_WIDE[VALID_CASE.2021 == "VALID_CASE" & GRADE.2021 %in% 5:8 & !is.na(SCALE_SCORE.2019), .(round(sum(is.na(SCALE_SCORE.2021))/.N, 3)*100), keyby = .(CONTENT_AREA, GRADE.2021)]
# ILEARN_WIDE[VALID_CASE.2019 == "VALID_CASE" & GRADE.2019 %in% 5:8 & !is.na(SCALE_SCORE.2017), .(round(sum(is.na(SCALE_SCORE.2019))/.N, 3)*100), keyby = .(CONTENT_AREA, GRADE.2019)]
# ILEARN_WIDE[VALID_CASE.2018 == "VALID_CASE" & GRADE.2018 %in% 5:8 & !is.na(SCALE_SCORE.2016), .(round(sum(is.na(SCALE_SCORE.2018))/.N, 3)*100), keyby = .(CONTENT_AREA, GRADE.2019)]
#
# ##    Attrition Only
# ILEARN_WIDE[GRADE.2021 %in% 5:8 & !is.na(SCALE_SCORE.2019), .(round(sum(is.na(VALID_CASE.2021))/.N, 3)*100), keyby = .(CONTENT_AREA, GRADE.2021)]
# ILEARN_WIDE[GRADE.2019 %in% 5:8 & !is.na(SCALE_SCORE.2017), .(round(sum(is.na(VALID_CASE.2019))/.N, 3)*100), keyby = .(CONTENT_AREA, GRADE.2019)]
