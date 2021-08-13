################################################################################
###                                                                          ###
###      Rhode Island  --  2021 Missing Data (enrollment and attrition)      ###
###                                                                          ###
################################################################################

###   General Setup (assumes running from ./Documentation directory)

if (!exists("Report_Analyses"))
    stop("Script assumes you have loaded an existing 'Report_Analyses' object.")

###   Load formated Report_Data
if (!exists("Report_Data"))  load("../Data/Report_Data.Rdata")

###   Locate the "Universal_Content" directory (assume woring directory is ./Documentation)
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

###   Declare an assessment flavor  --  does not extend directly to WIDA
assessment <- "State_Assessment"

prof.cut <- 2  #  which cutscore to get

###  Load required packages
require(SGP)
require(VIM)
require(cfaTools)
require(data.table)

###   Make copy of the data to change/clean
long_data <- copy(Report_Data[[assessment]])

long_data[, YEAR := tstrsplit(YEAR, "_", type.convert=TRUE, keep=2)]
long_data[, GRADE := as.integer(GRADE)]

###   clean up demographics for plots
setattr(long_data$ELL_STATUS, "levels", c("ELL: No", "ELL: Yes"))
setattr(long_data$ETHNICITY, "levels", c("W", "H", "A", "Mult", "NR", "B", "AI/AN", "NH/PI", "Other"))
# setattr(long_data$GENDER, "levels", c("Female", "Male", "Other", "Female", "Male", "Other"))
setattr(long_data$IEP_STATUS, "levels", c("IEP: No", "IEP: Yes", "504", NA))
setattr(long_data$FREE_REDUCED_LUNCH_STATUS, "levels", c("FRL: Yes", "FRL: No"))
long_data[, GENDER := as.character(GENDER)]
long_data[GENDER=="M", GENDER := "Male"]
long_data[GENDER=="F", GENDER := "Female"]

long_data <- droplevels(long_data)

###   Look at missingness related to prior scale scores and demographics.
long_data[, YEAR := factor(YEAR)]

##    Create a WIDE dataset

default.vars = c("VALID_CASE", "GRADE", "SCALE_SCORE", "SCALE_SCORE_ACTUAL", "ACHIEVEMENT_LEVEL")
demographics = c("IEP_STATUS", "ELL_STATUS", "FREE_REDUCED_LUNCH_STATUS", "GENDER", "ETHNICITY")
institutions = c("DISTRICT_NUMBER", "SCHOOL_NUMBER")

long.to.wide.vars <- c(default.vars, institutions, demographics)

wide_data <- dcast(long_data, ID + CONTENT_AREA ~ YEAR, sep=".", drop=FALSE, value.var=long.to.wide.vars)
##    Trim things down - remove cases with NAs in 2 most recent years
##    Nah - don't do this.  Want to see annual attrition in tables at bottom
# wide_data <- wide_data[!(is.na(SCALE_SCORE.2019) & is.na(SCALE_SCORE.2021))]

###   Probably only needed for missing data (student attrition?) analysis:
# summary(wide_data[, grep("GRADE", names(wide_data), value=T), with=FALSE])
for (i in 1:4) { # Do this a couple times
  wide_data[is.na(GRADE.2016), GRADE.2016 := GRADE.2017-1L]
  wide_data[is.na(GRADE.2017), GRADE.2017 := GRADE.2018-1L]
  wide_data[is.na(GRADE.2017), GRADE.2017 := GRADE.2019-2L]
  wide_data[is.na(GRADE.2018), GRADE.2018 := GRADE.2019-1L]
  wide_data[is.na(GRADE.2018), GRADE.2018 := GRADE.2021-3L]
  wide_data[is.na(GRADE.2019), GRADE.2019 := GRADE.2021-2L]

  wide_data[is.na(GRADE.2021), GRADE.2021 := GRADE.2019+2L]
  wide_data[is.na(GRADE.2021), GRADE.2021 := GRADE.2018+3L]
  wide_data[is.na(GRADE.2019), GRADE.2019 := GRADE.2018+1L]
  wide_data[is.na(GRADE.2019), GRADE.2019 := GRADE.2017+2L]
  wide_data[is.na(GRADE.2019), GRADE.2019 := GRADE.2016+3L]
  wide_data[is.na(GRADE.2018), GRADE.2018 := GRADE.2017+1L]
  wide_data[is.na(GRADE.2018), GRADE.2018 := GRADE.2016+2L]
  wide_data[is.na(GRADE.2017), GRADE.2017 := GRADE.2016+1L]
}

meas.list <- vector(mode = "list", length = length(long.to.wide.vars))
meas.list <- lapply(long.to.wide.vars, function(f) meas.list[[f]] <- grep(paste0(f, "[.]"), names(wide_data)))
names(meas.list) <- long.to.wide.vars

###   First stretch out to get missings in log data
tmp_long <- melt(wide_data, id = c("ID", "CONTENT_AREA"), variable.name = "YEAR", measure=meas.list)

##  Exclude non-existent GRADE levels
tmp_long <- tmp_long[GRADE %in% 3:8]
# table(tmp_long[, GRADE, YEAR])

setkey(tmp_long, ID, YEAR)
tmp_long <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(tmp_long, ID),
                          tidyselect::all_of(demographics), .direction="downup")))

tmp_long[, GRADE := as.integer(GRADE)]
tmp_long[, YEAR := factor(YEAR)]
setattr(tmp_long$YEAR, "levels", c("2016", "2017", "2018", "2019", "2021"))
tmp_long[, ACHIEVEMENT_LEVEL := factor(ACHIEVEMENT_LEVEL,
              levels=c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5",
                       "Not Meeting Expectations", "Partially Meeting Expectations",
                       "Meeting Expectations", "Exceeding Expectations"))]

setattr(tmp_long$ACHIEVEMENT_LEVEL, "levels",
          gsub(" Meeting", "", gsub(" Expectations", "", (gsub("Level ", "L", levels(tmp_long$ACHIEVEMENT_LEVEL))))))

##    Final WIDE dataset with missing GRADE and demographics filled in
wide_data <- dcast(tmp_long,
                  ID + CONTENT_AREA ~ YEAR, sep=".", drop=FALSE, value.var=long.to.wide.vars)


#####
###   Plots
#####

plot_directory <- "assets/Rplots/participation/VIM_Plots"
if (!dir.exists(plot_directory)) dir.create(plot_directory, recursive=TRUE)

###   5th Grade - only elementary grade with possible priors in 2021  (750 for all grades/subjects)
ela.cut.1yp <- SGPstateData[["RI"]][["Achievement"]][["Cutscores"]][["ELA.2017_2018"]][["GRADE_4"]][prof.cut]
ela.cut.2yp <- SGPstateData[["RI"]][["Achievement"]][["Cutscores"]][["ELA.2017_2018"]][["GRADE_3"]][prof.cut]
math.cut.1yp<- SGPstateData[["RI"]][["Achievement"]][["Cutscores"]][["MATHEMATICS.2017_2018"]][["GRADE_5"]][prof.cut]
math.cut.2yp<- SGPstateData[["RI"]][["Achievement"]][["Cutscores"]][["MATHEMATICS.2017_2018"]][["GRADE_4"]][prof.cut]

svg(file.path(plot_directory, "SS_Histograms-All_Grades.svg"))
par(mfrow = c(3, 2))
# 2019
histMiss(as.data.frame(wide_data[!is.na(VALID_CASE.2019) & GRADE.2019 %in% 4:8, c("SCALE_SCORE.2018", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2018 - Enrollment", breaks=50, interactive=FALSE, only.miss=FALSE)
histMiss(as.data.frame(wide_data[GRADE.2019 %in% 4:8, c("SCALE_SCORE.2018", "SCALE_SCORE.2019")]),
        main = "Missing 2019 to 2018 - Enrollment and Attrition", breaks=50, interactive=FALSE, only.miss=FALSE)

histMiss(as.data.frame(wide_data[!is.na(VALID_CASE.2019) & GRADE.2019 %in% 5:8, c("SCALE_SCORE.2017", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2017 - Enrollment", breaks=50, interactive=FALSE, only.miss=FALSE)
histMiss(as.data.frame(wide_data[GRADE.2019 %in% 5:8, c("SCALE_SCORE.2017", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2017 - Enrollment and Attrition", breaks=50, interactive=FALSE, only.miss=FALSE)

# 2021
histMiss(as.data.frame(wide_data[!is.na(VALID_CASE.2021) & GRADE.2021 %in% 5:8, c("SCALE_SCORE.2019", "SCALE_SCORE.2021")]),
         main = "Missing 2021 to 2019 - Enrollment", breaks=50, interactive=FALSE, only.miss=FALSE)
histMiss(as.data.frame(wide_data[GRADE.2021 %in% 5:8, c("SCALE_SCORE.2019", "SCALE_SCORE.2021")]),
         main = "Missing 2021 to 2019 - Enrollment and Attrition", breaks=50, interactive=FALSE, only.miss=FALSE)
dev.off()


svg(file.path(plot_directory, "SS_Histograms-ELA_G5.svg"))
par(mfrow = c(3, 2))
# 2019
histMiss(as.data.frame(wide_data[!is.na(VALID_CASE.2019) & GRADE.2019 == 5 & CONTENT_AREA == "ELA", c("SCALE_SCORE.2018", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2018 - Enrollment", breaks=35, interactive=FALSE, only.miss=FALSE)
abline(v=ela.cut.1yp, col="green", lwd=2)
histMiss(as.data.frame(wide_data[GRADE.2019 == 5 & CONTENT_AREA == "ELA", c("SCALE_SCORE.2018", "SCALE_SCORE.2019")]),
        main = "Missing 2019 to 2018 - Enrollment and Attrition", breaks=35, interactive=FALSE, only.miss=FALSE)
abline(v=ela.cut.1yp, col="green", lwd=2)

histMiss(as.data.frame(wide_data[!is.na(VALID_CASE.2019) & GRADE.2019 == 5 & CONTENT_AREA == "ELA", c("SCALE_SCORE.2017", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2017 - Enrollment", breaks=35, interactive=FALSE, only.miss=FALSE)
abline(v=ela.cut.2yp, col="green", lwd=2)
histMiss(as.data.frame(wide_data[GRADE.2019 == 5 & CONTENT_AREA == "ELA", c("SCALE_SCORE.2017", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2017 - Enrollment and Attrition", breaks=35, interactive=FALSE, only.miss=FALSE)
abline(v=ela.cut.2yp, col="green", lwd=2)

# 2021
histMiss(as.data.frame(wide_data[!is.na(VALID_CASE.2021) & GRADE.2021 == 5 & CONTENT_AREA == "ELA", c("SCALE_SCORE.2019", "SCALE_SCORE.2021")]),
         main = "Missing 2021 to 2019 - Enrollment", breaks=35, interactive=FALSE, only.miss=FALSE)
abline(v=ela.cut.2yp, col="green", lwd=2)
histMiss(as.data.frame(wide_data[GRADE.2021 == 5 & CONTENT_AREA == "ELA", c("SCALE_SCORE.2019", "SCALE_SCORE.2021")]),
         main = "Missing 2021 to 2019 - Enrollment and Attrition", breaks=35, interactive=FALSE, only.miss=FALSE)
abline(v=ela.cut.2yp, col="green", lwd=2)
dev.off()


svg(file.path(plot_directory, "SS_Histograms-Math_G6.svg"))
par(mfrow = c(3, 2))
# 2019
histMiss(as.data.frame(wide_data[!is.na(VALID_CASE.2019) & GRADE.2019 == 6 & CONTENT_AREA == "MATHEMATICS", c("SCALE_SCORE.2018", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2018 - Enrollment", breaks=35, interactive=FALSE, only.miss=FALSE)
abline(v=math.cut.1yp, col="green", lwd=2)
histMiss(as.data.frame(wide_data[GRADE.2019 == 6 & CONTENT_AREA == "MATHEMATICS", c("SCALE_SCORE.2018", "SCALE_SCORE.2019")]),
        main = "Missing 2019 to 2018 - Enrollment and Attrition", breaks=35, interactive=FALSE, only.miss=FALSE)
abline(v=math.cut.1yp, col="green", lwd=2)

histMiss(as.data.frame(wide_data[!is.na(VALID_CASE.2019) & GRADE.2019 == 6 & CONTENT_AREA == "MATHEMATICS", c("SCALE_SCORE.2017", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2017 - Enrollment", breaks=35, interactive=FALSE, only.miss=FALSE)
abline(v=math.cut.2yp, col="green", lwd=2)
histMiss(as.data.frame(wide_data[GRADE.2019 == 6 & CONTENT_AREA == "MATHEMATICS", c("SCALE_SCORE.2017", "SCALE_SCORE.2019")]),
         main = "Missing 2019 to 2017 - Enrollment and Attrition", breaks=35, interactive=FALSE, only.miss=FALSE)
abline(v=math.cut.2yp, col="green", lwd=2)

# 2021
histMiss(as.data.frame(wide_data[!is.na(VALID_CASE.2021) & GRADE.2021 == 6 & CONTENT_AREA == "MATHEMATICS", c("SCALE_SCORE.2019", "SCALE_SCORE.2021")]),
         main = "Missing 2021 to 2019 - Enrollment", breaks=35, interactive=FALSE, only.miss=FALSE)
abline(v=math.cut.2yp, col="green", lwd=2)
histMiss(as.data.frame(wide_data[GRADE.2021 == 6 & CONTENT_AREA == "MATHEMATICS", c("SCALE_SCORE.2019", "SCALE_SCORE.2021")]),
         main = "Missing 2021 to 2019 - Enrollment and Attrition", breaks=35, interactive=FALSE, only.miss=FALSE)
abline(v=math.cut.2yp, col="green", lwd=2)
dev.off()


# marginplot(as.data.frame(wide_data[, c("SCALE_SCORE.2019", "SCALE_SCORE.2021")]))
# scattmatrixMiss(as.data.frame(wide_data[, c("SCALE_SCORE.2018", "SCALE_SCORE.2019", "SCALE_SCORE.2021")]), interactive=FALSE)

# scattmatrixMiss(as.data.frame(wide_data[GRADE.2021 %in% 5, c("SCALE_SCORE.2019", "SCALE_SCORE.2021")]), interactive=FALSE)
# marginplot(as.data.frame(wide_data[GRADE.2021 %in% 5, c("SCALE_SCORE.2019", "SCALE_SCORE.2021")]))
# marginplot(as.data.frame(wide_data[GRADE.2019 %in% 5, c("SCALE_SCORE.2018", "SCALE_SCORE.2019")]))
#
# scattmatrixMiss(as.data.frame(wide_data[GRADE.2021 %in% 8, c("SCALE_SCORE.2018", "SCALE_SCORE.2019", "SCALE_SCORE.2021")]), interactive=FALSE)
#
# spineMiss(as.data.frame(droplevels(wide_data[, c("ACHIEVEMENT_LEVEL.2019", "SCALE_SCORE.2021")])), interactive=FALSE, only.miss=FALSE)
# spineMiss(as.data.frame(droplevels(wide_data[, c("ACHIEVEMENT_LEVEL.2018", "SCALE_SCORE.2019")])), interactive=FALSE, only.miss=FALSE)
# spineMiss(as.data.frame(droplevels(wide_data[, c("ACHIEVEMENT_LEVEL.2017", "SCALE_SCORE.2019")])), interactive=FALSE, only.miss=FALSE)
# spineMiss(as.data.frame(wide_data[, c("ETHNICITY.2019", "SCALE_SCORE.2021")]), interactive=FALSE, only.miss=FALSE)
# spineMiss(as.data.frame(wide_data[, c("IEP_STATUS.2019", "SCALE_SCORE.2021")]), interactive=FALSE, only.miss=FALSE)
# spineMiss(as.data.frame(wide_data[, c("FREE_REDUCED_LUNCH_STATUS.2019", "SCALE_SCORE.2021")]), interactive=FALSE, only.miss=FALSE)

# mosaicMiss(as.data.frame(droplevels(wide_data[GRADE.2021 == 5, c("FREE_REDUCED_LUNCH_STATUS.2019", "ACHIEVEMENT_LEVEL.2019", "SCALE_SCORE.2021")])),
#            highlight = 3, plotvars = 1:2, miss.labels = FALSE)
#
# mosaicMiss(as.data.frame(droplevels(wide_data[GRADE.2021 == 5, c("FREE_REDUCED_LUNCH_STATUS.2019", "ETHNICITY.2019", "SCALE_SCORE.2021")])),
#           highlight = 3, plotvars = 1:2, miss.labels = FALSE)

svg(file.path(plot_directory, "RI_Mosaic_AchLev_SES.svg"))
mosaicMiss(as.data.frame(droplevels(wide_data[GRADE.2021 %in% 5:8, c("ACHIEVEMENT_LEVEL.2019", "FREE_REDUCED_LUNCH_STATUS.2019", "SCALE_SCORE.2021")])),
           highlight = 3, plotvars = 1:2, miss.labels = FALSE)
dev.off()

svg(file.path(plot_directory, "RI_Mosaic_Ethn_SES.svg"))
mosaicMiss(as.data.frame(droplevels(wide_data[GRADE.2021 %in% 5:8, c("ETHNICITY.2019", "FREE_REDUCED_LUNCH_STATUS.2019", "SCALE_SCORE.2021")])),
          highlight = 3, plotvars = 1:2, miss.labels = FALSE, only.miss=FALSE)
dev.off()

svg(file.path(plot_directory, "RI_Mosaic_SwD_SES.svg"))
mosaicMiss(as.data.frame(droplevels(wide_data[GRADE.2021 %in% 5:8, c("IEP_STATUS.2019", "FREE_REDUCED_LUNCH_STATUS.2019", "SCALE_SCORE.2021")])),
          highlight = 3, plotvars = 1:2, miss.labels = FALSE, only.miss=FALSE)
dev.off()

svg(file.path(plot_directory, "RI_Mosaic_ELL_SES.svg"))
mosaicMiss(as.data.frame(droplevels(wide_data[GRADE.2021 %in% 5:8, c("ELL_STATUS.2019", "FREE_REDUCED_LUNCH_STATUS.2019", "SCALE_SCORE.2021")])),
          highlight = 3, plotvars = 1:2, miss.labels = FALSE, only.miss=FALSE)
dev.off()

svg(file.path(plot_directory, "RI_Mosaic_Gender_SES.svg"))
mosaicMiss(as.data.frame(droplevels(wide_data[GRADE.2021 %in% 5:8, c("GENDER.2019", "FREE_REDUCED_LUNCH_STATUS.2019", "SCALE_SCORE.2021")])),
          highlight = 3, plotvars = 1:2, miss.labels = FALSE, only.miss=FALSE)
dev.off()


#####
###   Tables of Attrition and non-participation
#####

attrit_2021 <-
  wide_data[GRADE.2021 %in% 5:8 & !is.na(SCALE_SCORE.2019),
                .(Pct_Attrit = round(sum(is.na(VALID_CASE.2021))/.N, 3)*100,
                  Total_Attrit = sum(is.na(VALID_CASE.2021)),
                  Total_Enrolled = sum(!is.na(VALID_CASE.2021))),
               keyby = .(CONTENT_AREA, GRADE.2021)]
setnames(attrit_2021, 2, "GRADE")
attrit_2021[, YEAR := "2019_to_2021"]

attrit_2019 <-
  wide_data[GRADE.2019 %in% 5:8 & !is.na(SCALE_SCORE.2017),
               .(Pct_Attrit = round(sum(is.na(VALID_CASE.2019))/.N, 3)*100,
                 Total_Attrit = sum(is.na(VALID_CASE.2019)),
                 Total_Enrolled = sum(!is.na(VALID_CASE.2019))),
               keyby = .(CONTENT_AREA, GRADE.2019)]
setnames(attrit_2019, 2, "GRADE")
attrit_2019[, YEAR := "2017_to_2019"]

attrit_2018 <-
  wide_data[GRADE.2018 %in% 5:8 & !is.na(SCALE_SCORE.2016),
               .(Pct_Attrit = round(sum(is.na(VALID_CASE.2018))/.N, 3)*100,
                 Total_Attrit = sum(is.na(VALID_CASE.2018)),
                 Total_Enrolled = sum(!is.na(VALID_CASE.2018))),
               keyby = .(CONTENT_AREA, GRADE.2018)]
setnames(attrit_2018, 2, "GRADE")
attrit_2018[, YEAR := "2016_to_2018"]

attrit_wide <- dcast(rbindlist(list(attrit_2018, attrit_2019, attrit_2021)),
                  CONTENT_AREA + GRADE ~ YEAR, sep=".", value.var = c("Pct_Attrit", "Total_Attrit", "Total_Enrolled"))

Report_Analyses[["participation"]][[assessment]][["state_attrition"]] <- attrit_wide
