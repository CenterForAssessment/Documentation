################################################################################
###                                                                          ###
###   Indiana -- Quantile Shift Analyses (Race, SES and Prior Achievement)   ###
###                                                                          ###
################################################################################

###   General Setup (assumes running from ./Documentation directory)

if (!exists("Report_Analyses")) {
  stop("Script assumes you have loaded or created a 'Report_Analyses' object.")
} else {
  if (!is.null(Report_Analyses[["Quantile_Shift"]][[assessment]])) Existing_Results <- Report_Analyses[["Quantile_Shift"]][[assessment]]
}
###   Load formated Report_Data
if (!exists("Report_Data")) stop("Script assumes you have loaded a 'Report_Data' object.")

###   Locate the "Universal_Content" directory (assume woring directory is ./Documentation)
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

###   Load required packages and custom functions
require(SGP)
require(rogme)
require(tibble)
require(ggplot2)
# require(WRS2)
require(data.table)
require(cfaTools)

"%w/o%" <- function(x,y) x[!x %in% y]

###   Load existing params or create by sourcing appropriate scripts
if (!exists("params")) {
  ###   Load required packages and custom functions
  require(SGP)
  require(Literasee)
  if (exists("assessment")) {tmp.assessment <- assessment; chg.assess <- TRUE} else chg.assess <- FALSE
  setwd("..")
  source("./Documentation/4_Make_Configs.R")
  setwd("./Documentation")
  params <- report.config$params
  source(knitr::purl(file.path(universal.content.path, "Learning_Loss_Analysis", "Child_RMD", "params.Rmd"), quiet=TRUE))
  file.remove("params.R")
  if (chg.assess) tmp.assessment -> assessment
}

current.year <- tail(params[["years"]][[assessment]], 1)
prior.year1  <- tail(params[["years"]][[assessment]], 2)[-2]

all.grades <- params[["grades"]][[assessment]]
sgp.grades <- params[["sgp.grades"]][[assessment]]
stat.grades<- setdiff(all.grades, sgp.grades)

content.areas <- params[["GL_subjects"]][[assessment]]

subgroup.min.size <- params[["min.size.district"]] # 50

###   Declare cutscores to use from SGPstateData
# if (assessment == "State_Assessment") {
#   assessment_cutscores <- list(
#     ELA = SGPstateData[["IN"]][["Achievement"]][["Cutscores"]][[paste0("ELA.2019")]],
#     MATHEMATICS = SGPstateData[["IN"]][["Achievement"]][["Cutscores"]][[paste0("MATHEMATICS.2019")]])
# }
# if (assessment == "ELP_Assessment") {
#   assessment_cutscores <- SGPstateData[["WIDA_IN"]][["Achievement"]][["Cutscores"]]
# }
# prof.cut <- which(params$proficient_levels[[assessment]]=="Proficient")[1]-1


###   Make copy of the data to change/clean
long_data <- copy(Report_Data[[assessment]][YEAR %in% c(prior.year1, current.year)])

long_data <- long_data[!is.na(SCALE_SCORE)]  #  All HSF/GES work is done with non-NA values
long_data[, SCALE_SCORE := round(SCALE_SCORE, 0)] # 2016 - 2018 EQUATED scores are not rounded!  Makes for ugly individual_sf tables...


###   Clean up some of the demographic variables
long_data[, ENGLISH_LANGUAGE_LEARNER_STATUS := gsub(" English Proficient", "_EP", ENGLISH_LANGUAGE_LEARNER_STATUS)]
long_data[ENGLISH_LANGUAGE_LEARNER_STATUS %in% c("Native English Speaking Immigrant", "Not an English Language Learner"),
                        ENGLISH_LANGUAGE_LEARNER_STATUS := "Not_ELL"]

long_data[, SOCIO_ECONOMIC_STATUS := plyr::mapvalues(SOCIO_ECONOMIC_STATUS,
              from=c("Free meals", "Reduced price meals", "Paid meals", "Unknown"),
              to  =c("FRL", "FRL", "Full_Price", NA))]

long_data[, ETHNICITY := as.character(ETHNICITY)]
long_data[, ETHNICITY := gsub("Native Hawaiian or Other Pacific Islander", "NH/PI", ETHNICITY)]
# c("American Indian", "Asian", "Black", "Hispanic", "Multiracial", "Native Hawaiian or Other Pacific Islander", "White")
# long_data[!ETHNICITY %in% c("Asian", "Black", "Hispanic", "White") & !is.na(ETHNICITY), ETHNICITY := "All Other"]

mode_of_instruction <- fread(file=file.path("..", "..", "..", "..", "Indiana", "Data", "Base_Files", "Student_Level_Mode_of_Instruction_20210715.csv"))
setnames(mode_of_instruction, c("ID", "INSTRUCTION_MODE"))
mode_of_instruction[, ID := as.character(ID)]

setkey(mode_of_instruction, ID)
setkey(long_data, ID)
long_data <- mode_of_instruction[long_data]
table(long_data[, MODE_OF_INSTRUCTION, INSTRUCTION_MODE], exclude=NULL)
table(long_data[, GRADE, INSTRUCTION_MODE], exclude=NULL)


demographics <- c("SPECIAL_EDUCATION_STATUS", "ENGLISH_LANGUAGE_LEARNER_STATUS",
                  "SOCIO_ECONOMIC_STATUS", "GENDER", "ETHNICITY", "INSTRUCTION_MODE")

for (dmg in demographics %w/o% c("SOCIO_ECONOMIC_STATUS", "INSTRUCTION_MODE")) {
  long_data[get(dmg)=="Unknown", (dmg) := NA]
}

setkey(long_data, ID, YEAR)
long_data <- data.table(dplyr::ungroup(tidyr::fill(dplyr::group_by(long_data, ID),
                          tidyselect::all_of(demographics), .direction="downup")))

for (dmg in demographics) {
  long_data[is.na(get(dmg)), (dmg) := "Unknown"]
}

table(long_data[, INSTRUCTION_MODE], exclude=NULL)
long_data[, INSTRUCTION_MODE := gsub("New ", "", INSTRUCTION_MODE)]

plot.dir <- file.path("assets", "Rplots", "Quantile_Shift", assessment)


#####
###   Independent Groups Shift Functions
#####

##    Create temporary subset of variable of interest WITHOUT missing values (required)
long_data_subset <-
    long_data[, .(ID, YEAR, GRADE, CONTENT_AREA, SCALE_SCORE, SGP_BASELINE,
                  SPECIAL_EDUCATION_STATUS, ENGLISH_LANGUAGE_LEARNER_STATUS, ETHNICITY,
                  SOCIO_ECONOMIC_STATUS, GENDER, INSTRUCTION_MODE, PRIOR_DECILE_2YEAR)]

setkey(long_data_subset, ID, YEAR)

long_data_subset[, YEAR := factor(YEAR)]
long_data_subset[, GRADE := factor(as.numeric(GRADE))]


#####
###      Hierarchical Shift Function -- `rogme` package
###                        &
###    Akinshin's Gamma Effect Sizes -- `cfaTools` package
#####

if (hsf.analysis) {

# comparison.years <- list(c(current.year, prior.year1)) # Only 2019 and 2021 have Demographics in Indiana
cmp.yrs <- c(current.year, prior.year1)  #  keep for potential multi-year expansion in other states
YEARS <- paste(c(current.year, prior.year1), collapse="_")
Subgrp_HSF <- vector("list", 1)
names(Subgrp_HSF) <- YEARS

comparison.groups <- list(
      c("ETHNICITY", "PRIOR_DECILE_2YEAR"), c("SOCIO_ECONOMIC_STATUS", "PRIOR_DECILE_2YEAR"),
      c("GENDER", "PRIOR_DECILE_2YEAR"), c("ENGLISH_LANGUAGE_LEARNER_STATUS", "PRIOR_DECILE_2YEAR"),
      c("SPECIAL_EDUCATION_STATUS", "PRIOR_DECILE_2YEAR"), c("INSTRUCTION_MODE", "PRIOR_DECILE_2YEAR"))

for (cmp.group in comparison.groups) {
  # for (tmp.focus in c("SGP_BASELINE", "SCALE_SCORE")) {
  tmp.focus <- "SGP_BASELINE"
  # cmp.group <- c("INSTRUCTION_MODE", "PRIOR_DECILE_2YEAR")

  for (CA in content.areas) {
      tmp.var <- paste(cmp.group, collapse="__") # "SES_PriorDecile_Grade" # "Ethn_PriorDecile_Grade"

      tmp.vars.to.get <- c("YEAR", "GRADE", cmp.group, tmp.focus)
      Temp_Subset <- na.omit(long_data_subset[CONTENT_AREA == CA, ..tmp.vars.to.get][,
                                SUBGRP_COUNT := .N, keyby = c("YEAR", "GRADE", cmp.group)])

      Temp_Subset <- Temp_Subset[SUBGRP_COUNT >= subgroup.min.size] # Use 50 for small
      Temp_Subset[, eval(tmp.var) := eval(parse(text=paste0("factor(paste(", paste(c(cmp.group, "GRADE"), collapse=", "), ", sep='--'))")))]

      ##    Exclude districts that only appear in 1 year
      sbgrp.yr1 <- unique(Temp_Subset[YEAR == cmp.yrs[2], get(tmp.var)])
      sbgrp.yr2 <- unique(Temp_Subset[YEAR == cmp.yrs[1], get(tmp.var)])
      only.yr1 <- sbgrp.yr1[!sbgrp.yr1 %in% sbgrp.yr2]
      only.yr2 <- sbgrp.yr2[!sbgrp.yr2 %in% sbgrp.yr1]

      Temp_Subset <- droplevels(Temp_Subset[!get(tmp.var) %in% c(only.yr1, only.yr2)])

      ##    Create Akinshin's Gamma Effect Size tables for comparison years
      tmp_ges_tbl <- Temp_Subset[,
          as.list(gammaEffectSizeLong(.SD, tmp.focus, cmp.yrs[2], cmp.yrs[1], digits=2)), keyby=tmp.var]
      tmp_n_tbl <- Temp_Subset[, .N, keyby=tmp.var]#[,
          # c("ETHNICITY", "PRIOR_DECILE_2YEAR", "GRADE") := tstrsplit(Ethn_PriorDecile_Grade, "--", fixed=TRUE)]
      Subgrp_HSF[[YEARS]][[tmp.var]][["GAMMA_ES"]][[tmp.focus]][[CA]] <- tmp_n_tbl[tmp_ges_tbl]

      # Subgrp_HSF[[YEARS]][[tmp.var]][["GAMMA_ES"]][[tmp.focus]][[CA]][GRADE == 6 & ETHNICITY == "Black"]

      # if (cmpgp == 1 | tmp.focus == "SGP_BASELINE") {
      trim.mean <- 0.01 # trim level for the mean - keep VERY small for GRADE comparisons

      Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]] <-
         hsf(as.data.frame(Temp_Subset),
             formula = as.formula(paste0(tmp.focus, " ~ YEAR + ", tmp.var)),
             qseq = 1:9/10,
             tr = trim.mean, # default is 0.2
             alpha = 0.05,
             qtype = 8,
             todo = c(2,1),
             null.value = 0,
             adj_method = "hochberg")

      # Change plot MAIN title
      Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]] <- tmp.comp <-
        paste(Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]],
              capwords(paste(c(CA, tmp.focus, "/", paste(cmp.group, collapse=" & ")), collapse=" ")))
      Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["PLOT"]] <-
        plot_hsf(Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]])

      # Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]] <- ""
      # Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["BOOT_PLOT"]] <-
      #   plot_hsf_pb_dist(Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]])
      # Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]] <- tmp.comp

      ##    Format subgroup results and split cmp.group into separate variables
      tmp.ges <- Subgrp_HSF[[YEARS]][[tmp.var]][["GAMMA_ES"]][[tmp.focus]][[CA]]
      setnames(tmp.ges, gsub("^Q_", "GES_Q_", names(tmp.ges)))
      tmp.sf <- data.table(
          levels(Temp_Subset[[tmp.var]]),
          t(round(Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]], 2)))
      setnames(tmp.sf, c(tmp.var, paste0("DIFF_Q_", 1:9*10)))

      tmp.tbl <- merge(tmp.sf, tmp.ges, by = tmp.var)
      tmp.tbl[, (c(cmp.group, "GRADE")) := tstrsplit(get(tmp.var), "--", fixed=TRUE)]
      tmp.tbl[, PRIOR_DECILE_2YEAR := factor(PRIOR_DECILE_2YEAR, levels = paste0("Decile_", 1:10))]

      tmp.tbl[, (tmp.var) := NULL]
      setcolorder(tmp.tbl, c(cmp.group, "GRADE", "N", c(rbind(paste0("DIFF_Q_", 1:9*10), paste0("GES_Q_", 1:9*10)))))
      setkeyv(tmp.tbl, c(cmp.group, "GRADE"))
      # tmp.tbl[GRADE == 6 & ENGLISH_LANGUAGE_LEARNER_STATUS == "Limited_EP"]

      Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["LONG_TABLE"]] <- tmp.tbl

      eval_expr <- paste0("Q_", 1:9*10, " = paste0(", paste0("round(DIFF_Q_", 1:9*10), ", 0), ' (', ", paste0("format(GES_Q_", 1:9*10), ", nsmall=2),')')")
      eval_expr <- setNames(eval_expr,  paste0("Q_", 1:9*10))
      format.tbl <- tmp.tbl[, lapply(eval_expr, function(f) eval(parse(text=f))), keyby=c(cmp.group, "GRADE", "N")]
      format.tbl[, PCT := round(prop.table(N)*100, 1), keyby=c(cmp.group %w/o% "PRIOR_DECILE_2YEAR", "GRADE")]
      format.tbl[, N := paste0(prettyNum(N, big.mark=","), "/", format(PCT, nsmall=1), "%")]
      format.tbl[, PCT := NULL]
      # format.tbl[GRADE == 6 & ETHNICITY == "Black"] # & SOCIO_ECONOMIC_STATUS == "FRL"] #

      Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["FORMAT_TABLE"]] <- format.tbl

      ##    Stochastic Dominance
      np <- nrow(tmp.tbl)
      nq <- length(Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["quantiles"]])
      pdmt0 <- apply(Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]] > 0, 2, sum)
      pdlt0 <- apply(Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]] < 0, 2, sum)

      Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["STOCH_DOM"]] <- list(
        c(paste0('In ',sum(pdmt0 == nq),' sub-group combinations (', round(100 * sum(pdmt0 == nq) / np, digits = 1), '%), all quantile differences are more than to zero'),
          paste0('In ',sum(pdlt0 == nq),' sub-group combinations (', round(100 * sum(pdlt0 == nq) / np, digits = 1), '%), all quantile differences are less than to zero')),
        c(round(100 * sum(pdmt0 == nq) / np, digits = 1), round(100 * sum(pdlt0 == nq) / np, digits = 1)))

  }   #   for(CA in content.areas)
  if (exists("Existing_Results")) {
    # modifyList
  }
}     #   for (cmp.group in comparison.groups)


#####
###   No GRADE -- Doesn't make sense for INSTRUCTION_MODE since only one year (???)

# for (cmp.group in comparison.groups) {
  # for (tmp.focus in c("SGP_BASELINE", "SCALE_SCORE")) {
  tmp.focus <- "SGP_BASELINE"
  cmp.group <- c("INSTRUCTION_MODE", "PRIOR_DECILE_2YEAR")

  for (CA in content.areas) {
      tmp.var <- paste(cmp.group, collapse="__") # "SES_PriorDecile_Grade" # "Ethn_PriorDecile_Grade"

      tmp.vars.to.get <- c("YEAR", cmp.group, tmp.focus)
      Temp_Subset <- na.omit(long_data_subset[CONTENT_AREA == CA, ..tmp.vars.to.get][,
                                SUBGRP_COUNT := .N, keyby = c("YEAR", cmp.group)])

      Temp_Subset <- Temp_Subset[SUBGRP_COUNT >= subgroup.min.size] # Use 50 for small
      Temp_Subset[, eval(tmp.var) := eval(parse(text=paste0("factor(paste(", paste(cmp.group, collapse=", "), ", sep='--'))")))]

      ##    Exclude districts that only appear in 1 year
      sbgrp.yr1 <- unique(Temp_Subset[YEAR == cmp.yrs[2], get(tmp.var)])
      sbgrp.yr2 <- unique(Temp_Subset[YEAR == cmp.yrs[1], get(tmp.var)])
      only.yr1 <- sbgrp.yr1[!sbgrp.yr1 %in% sbgrp.yr2]
      only.yr2 <- sbgrp.yr2[!sbgrp.yr2 %in% sbgrp.yr1]

      Temp_Subset <- droplevels(Temp_Subset[!get(tmp.var) %in% c(only.yr1, only.yr2)])

      ##    Create Akinshin's Gamma Effect Size tables for comparison years
      tmp_ges_tbl <- Temp_Subset[,
          as.list(gammaEffectSizeLong(.SD, tmp.focus, cmp.yrs[2], cmp.yrs[1], digits=2)), keyby=tmp.var]
      tmp_n_tbl <- Temp_Subset[, .N, keyby=tmp.var]
      Subgrp_HSF[[YEARS]][[tmp.var]][["GAMMA_ES"]][[tmp.focus]][[CA]] <- tmp_n_tbl[tmp_ges_tbl]

      # Subgrp_HSF[[YEARS]][[tmp.var]][["GAMMA_ES"]][[tmp.focus]][[CA]][GRADE == 6 & ETHNICITY == "Black"]

      # if (cmpgp == 1 | tmp.focus == "SGP_BASELINE") {
      trim.mean <- 0.01 # trim level for the mean - keep VERY small for GRADE comparisons

      Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]] <-
         hsf(as.data.frame(Temp_Subset),
             formula = as.formula(paste0(tmp.focus, " ~ YEAR + ", tmp.var)),
             qseq = 1:9/10,
             tr = trim.mean, # default is 0.2
             alpha = 0.05,
             qtype = 8,
             todo = c(2,1),
             null.value = 0,
             adj_method = "hochberg")

      # Change plot MAIN title
      Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]] <- tmp.comp <-
        paste(Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]],
              capwords(paste(c(CA, tmp.focus, "/", paste(cmp.group, collapse=" & ")), collapse=" ")))
      Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["PLOT"]] <-
        plot_hsf(Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]])

      # Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]] <- ""
      # Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["BOOT_PLOT"]] <-
      #   plot_hsf_pb_dist(Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]])
      # Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]] <- tmp.comp

      ##    Format district results and add DISTRICT_NUMBER
      tmp.ges <- Subgrp_HSF[[YEARS]][[tmp.var]][["GAMMA_ES"]][[tmp.focus]][[CA]]
      setnames(tmp.ges, gsub("^Q_", "GES_Q_", names(tmp.ges)))
      tmp.sf <- data.table(
          levels(Temp_Subset[[tmp.var]]),
          t(round(Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]], 2)))
      setnames(tmp.sf, c(tmp.var, paste0("DIFF_Q_", 1:9*10)))

      tmp.tbl <- merge(tmp.sf, tmp.ges, by = tmp.var)
      tmp.tbl[, (cmp.group) := tstrsplit(get(tmp.var), "--", fixed=TRUE)]
      tmp.tbl[, PRIOR_DECILE_2YEAR := factor(PRIOR_DECILE_2YEAR, levels = paste0("Decile_", 1:10))]

      tmp.tbl[, (tmp.var) := NULL]
      setcolorder(tmp.tbl, c(cmp.group, "N", c(rbind(paste0("DIFF_Q_", 1:9*10), paste0("GES_Q_", 1:9*10)))))
      setkeyv(tmp.tbl, cmp.group)
      # tmp.tbl[GRADE == 6 & ENGLISH_LANGUAGE_LEARNER_STATUS == "Limited_EP"]

      Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["LONG_TABLE"]] <- tmp.tbl

      eval_expr <- paste0("Q_", 1:9*10, " = paste0(", paste0("round(DIFF_Q_", 1:9*10), ", 0), ' (', ", paste0("format(GES_Q_", 1:9*10), ", nsmall=2),')')")
      eval_expr <- setNames(eval_expr,  paste0("Q_", 1:9*10))
      format.tbl <- tmp.tbl[, lapply(eval_expr, function(f) eval(parse(text=f))), keyby=c(cmp.group, "N")]
      format.tbl[, PCT := round(prop.table(N)*100, 1), keyby=c(cmp.group %w/o% "PRIOR_DECILE_2YEAR")]
      format.tbl[, N := paste0(prettyNum(N, big.mark=","), "/", format(PCT, nsmall=1), "%")]
      format.tbl[, PCT := NULL]
      # format.tbl[GRADE == 6 & ETHNICITY == "Black"] # & SOCIO_ECONOMIC_STATUS == "FRL"] #

      Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["FORMAT_TABLE"]] <- format.tbl

      ##    Stochastic Dominance
      np <- nrow(tmp.tbl)
      nq <- length(Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["quantiles"]])
      pdmt0 <- apply(Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]] > 0, 2, sum)
      pdlt0 <- apply(Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]] < 0, 2, sum)

      Subgrp_HSF[[YEARS]][[tmp.var]][["HShift"]][[tmp.focus]][[CA]][["STOCH_DOM"]] <- list(
        c(paste0('In ',sum(pdmt0 == nq),' sub-group combinations (', round(100 * sum(pdmt0 == nq) / np, digits = 1), '%), all quantile differences are more than to zero'),
          paste0('In ',sum(pdlt0 == nq),' sub-group combinations (', round(100 * sum(pdlt0 == nq) / np, digits = 1), '%), all quantile differences are less than to zero')),
        c(round(100 * sum(pdmt0 == nq) / np, digits = 1), round(100 * sum(pdlt0 == nq) / np, digits = 1)))

  }   #   for(CA in content.areas)
  if (exists("Existing_Results")) {
    # modifyList
  }
# }     #   for (cmp.group in comparison.groups)


################################################################################
###          Between Subgroup Member (Aggregate) Gamma Effect Sizes          ###
################################################################################

if (between.inst.ges) {

YEARS <- paste0(current.year, "_", prior.year1)

if (!hsf.analysis) {
  if (exists("Existing_Results")) {
    Subgrp_HSF <- Existing_Results[["Hierarchical_Shift"]]
  } else {
    Subgrp_HSF <- vector("list", 1)
    names(Subgrp_HSF) <- YEARS
  }
}

subgr_plot_dir <- file.path(plot.dir, "Subgroups")
if (!dir.exists(subgr_plot_dir)) dir.create(subgr_plot_dir, recursive=TRUE)

#####
###   Subgroups of Interest
#####

comparison.groups <- list(
      c("ETHNICITY", "PRIOR_DECILE_2YEAR"), c("SOCIO_ECONOMIC_STATUS", "PRIOR_DECILE_2YEAR"), c("GENDER", "PRIOR_DECILE_2YEAR"),
      c("ENGLISH_LANGUAGE_LEARNER_STATUS", "PRIOR_DECILE_2YEAR"), c("SPECIAL_EDUCATION_STATUS", "PRIOR_DECILE_2YEAR"))

# cmp.group
###   Between (similar) Subgroup Summaries

  tmp.var <- paste(cmp.group, collapse="__") # "SES_PriorDecile_Grade" # "Ethn_PriorDecile_Grade"
  tmp.pltnm <- sub("__", "_", sub("GENDER", "MF", sub("ENGLISH_LANGUAGE_LEARNER", "ELL", sub("SPECIAL_EDUCATION", "SWD", sub("PRIOR_DECILE_2YEAR", "PD2Y", sub("SOCIO_ECONOMIC", "SES", sub("ETHNICITY", "Ethn", gsub("_STATUS", "", tmp.var))))))))
  tmp.vars.to.get <- c("YEAR", "CONTENT_AREA", "GRADE", cmp.group, "SGP_BASELINE", "SCALE_SCORE")
  Temp_Subset <- na.omit(long_data_subset[CONTENT_AREA == CA, ..tmp.vars.to.get][,
                            SUBGRP_COUNT := .N, keyby = c("YEAR", "CONTENT_AREA", "GRADE", cmp.group)])

  # Temp_Subset <- Temp_Subset[SUBGRP_COUNT >= subgroup.min.size] # Use 50 for small
  # Temp_Subset[, eval(tmp.var) := eval(parse(text=paste0("factor(paste(", paste(c(cmp.group, "GRADE"), collapse=", "), ", sep='--'))")))]
  Temp_Subset[, eval(tmp.var) := eval(parse(text=paste0("factor(paste(", paste(cmp.group, collapse=", "), ", sep='--'))")))]

  ##    Exclude districts that only appear in 1 year
  sbgrp.yr1 <- unique(Temp_Subset[YEAR == cmp.yrs[2], get(tmp.var)])
  sbgrp.yr2 <- unique(Temp_Subset[YEAR == cmp.yrs[1], get(tmp.var)])
  only.yr1 <- sbgrp.yr1[!sbgrp.yr1 %in% sbgrp.yr2]
  only.yr2 <- sbgrp.yr2[!sbgrp.yr2 %in% sbgrp.yr1]

  Temp_Subset <- droplevels(Temp_Subset[!get(tmp.var) %in% c(only.yr1, only.yr2)])

  Temp_Summary <- Temp_Subset[, .(
        Mean_Status = mean(SCALE_SCORE, na.rm = TRUE),
        Median_Status = as.numeric(median(SCALE_SCORE, na.rm = TRUE)),
        Mean_Growth = mean(SGP_BASELINE, na.rm = TRUE),
        Median_Growth = as.numeric(median(SGP_BASELINE, na.rm = TRUE)),
        N_Status = sum(!is.na(SCALE_SCORE)),
        N_Growth = sum(!is.na(SGP_BASELINE))),
      keyby=c("CONTENT_AREA", "YEAR", tmp.var)]

for (CA in content.areas) {
  png(file.path(subgr_plot_dir, paste0("Subgroup_Status_Growth__", gsub("ematics", "", capwords(CA)), "_", tmp.pltnm, ".png")))
  par(mfrow = c(2, 2))

  pr.mn.st <- density(na.omit(Temp_Summary[N_Status >= subgroup.min.size & CONTENT_AREA == CA & YEAR == prior.year1, Mean_Status]))
  cr.mn.st <- density(na.omit(Temp_Summary[N_Status >= subgroup.min.size & CONTENT_AREA == CA & YEAR == current.year, Mean_Status]))
  plot(pr.mn.st, xlim = range(pr.mn.st$x, cr.mn.st$x), ylim = range(pr.mn.st$y, cr.mn.st$y),
       main = paste(gsub("ematics", "", capwords(CA)), "\n\nMean Scale Scores"), sub = paste("\nBlack =", prior.year1, ", Red =", current.year))
  lines(cr.mn.st, col="red")

  pr.md.st <- density(na.omit(Temp_Summary[N_Status >= subgroup.min.size & CONTENT_AREA == CA & YEAR == prior.year1, Median_Status]))
  cr.md.st <- density(na.omit(Temp_Summary[N_Status >= subgroup.min.size & CONTENT_AREA == CA & YEAR == current.year, Median_Status]))
  plot(pr.md.st, xlim = range(pr.md.st$x, cr.md.st$x), ylim = range(pr.md.st$y, cr.md.st$y),
       main = paste0("Subgroups with N >=", subgroup.min.size, "\n\nMedian Scale Scores"), sub = paste("\nBlack =", prior.year1, ", Red =", current.year))
  lines(cr.md.st, col="red")

  pr.mn.gr <- density(na.omit(Temp_Summary[N_Growth >= subgroup.min.size & CONTENT_AREA == CA & YEAR == prior.year1, Mean_Growth]))
  cr.mn.gr <- density(na.omit(Temp_Summary[N_Growth >= subgroup.min.size & CONTENT_AREA == CA & YEAR == current.year, Mean_Growth]))
  plot(pr.mn.gr, xlim = range(pr.mn.gr$x, cr.mn.gr$x), ylim = range(pr.mn.gr$y, cr.mn.gr$y), main= "Mean Baseline SGPs")
  lines(cr.mn.gr, col="red")

  pr.md.gr <- density(na.omit(Temp_Summary[N_Growth >= subgroup.min.size & CONTENT_AREA == CA & YEAR == prior.year1, Median_Growth]))
  cr.md.gr <- density(na.omit(Temp_Summary[N_Growth >= subgroup.min.size & CONTENT_AREA == CA & YEAR == current.year, Median_Growth]))
  plot(pr.md.gr, xlim = range(pr.md.gr$x, cr.md.gr$x), ylim = range(pr.md.gr$y, cr.md.gr$y), main= "Median Baseline SGPs")
  lines(cr.md.gr, col="red")

  dev.off()
}

Temp_Summary[N_Growth >= subgroup.min.size & Median_Growth > 60,]

for (CA in content.areas) {
  Subgrp_HSF[[YEARS]][[tmp.var]][["GAMMA_ES"]][["SCALE_SCORE"]][[CA]][["BETWEEN"]][["CONTENT_AREA"]][["MEAN"]] <-
    Temp_Summary[N_Status >= subgroup.min.size & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Mean_Status", prior.year1, current.year, digits=2))]#,
    # keyby="CONTENT_AREA"]
  Subgrp_HSF[[YEARS]][[tmp.var]][["GAMMA_ES"]][["SCALE_SCORE"]][[CA]][["BETWEEN"]][["CONTENT_AREA"]][["MEDIAN"]] <-
    Temp_Summary[N_Status >= subgroup.min.size & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Median_Status", prior.year1, current.year, digits=2))]

  Subgrp_HSF[[YEARS]][[tmp.var]][["GAMMA_ES"]][["SGP_BASELINE"]][[CA]][["BETWEEN"]][["CONTENT_AREA"]][["MEAN"]] <-
    Temp_Summary[N_Growth >= subgroup.min.size & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Mean_Growth", prior.year1, current.year, digits=2))]#,
  Subgrp_HSF[[YEARS]][[tmp.var]][["GAMMA_ES"]][["SGP_BASELINE"]][[CA]][["BETWEEN"]][["CONTENT_AREA"]][["MEDIAN"]] <-
    Temp_Summary[N_Growth >= subgroup.min.size & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Mean_Growth", prior.year1, current.year, digits=2))]
}

###   Subgroup by Grade Level Summaries
  Temp_Grade_Summary <- Temp_Subset[, .(
        Mean_Status = mean(SCALE_SCORE, na.rm = TRUE),
        Median_Status = as.numeric(median(SCALE_SCORE, na.rm = TRUE)),
        Mean_Growth = mean(SGP_BASELINE, na.rm = TRUE),
        Median_Growth = as.numeric(median(SGP_BASELINE, na.rm = TRUE)),
        N_Status = sum(!is.na(SCALE_SCORE)),
        N_Growth = sum(!is.na(SGP_BASELINE))),
      keyby=c("CONTENT_AREA", "GRADE", "YEAR", tmp.var)]

# Temp_Grade_Summary[, .(SD_Mean = sd(Mean_Status), SD_Median = sd(Median_Status)), keyby=c("CONTENT_AREA", "GRADE", "YEAR")]

for (CA in content.areas) {
  for (G in all.grades) {
    if (G > max(as.numeric(stat.grades))) {
      png(file.path(subgr_plot_dir, paste0("Subgroup_Status_Growth__", gsub("ematics", "", capwords(CA)), "_G", G, ".png")))
      par(mfrow = c(2, 2))
    } else {
      png(file.path(subgr_plot_dir, paste0("Subgroup_Status__", gsub("ematics", "", capwords(CA)), "_G", G, ".png")))
      par(mfrow = c(2, 1))
    }
    pr.mn.st <- density(na.omit(Temp_Grade_Summary[N_Status >= subgroup.min.size & CONTENT_AREA == CA & GRADE == G & YEAR == prior.year1, Mean_Status]))
    cr.mn.st <- density(na.omit(Temp_Grade_Summary[N_Status >= subgroup.min.size & CONTENT_AREA == CA & GRADE == G & YEAR == current.year, Mean_Status]))
    plot(pr.mn.st, xlim = range(pr.mn.st$x, cr.mn.st$x), ylim = range(pr.mn.st$y, cr.mn.st$y),
         main = paste(gsub("ematics", "", capwords(CA)), "Grade", G, "\n\nMean Scale Scores"), sub = "\nBlack = 2019, Red = 2021")
    lines(cr.mn.st, col="red")

    pr.md.st <- density(na.omit(Temp_Grade_Summary[N_Status >= subgroup.min.size & CONTENT_AREA == CA & GRADE == G & YEAR == prior.year1, Median_Status]))
    cr.md.st <- density(na.omit(Temp_Grade_Summary[N_Status >= subgroup.min.size & CONTENT_AREA == CA & GRADE == G & YEAR == current.year, Median_Status]))
    plot(pr.md.st, xlim = range(pr.md.st$x, cr.md.st$x), ylim = range(pr.md.st$y, cr.md.st$y),
         main = "Subgroups with N >= 15\n\nMedian Scale Scores", sub = "\nBlack = 2019, Red = 2021")
    lines(cr.md.st, col="red")

    if (G > max(as.numeric(stat.grades))) {
      pr.mn.gr <- density(na.omit(Temp_Grade_Summary[N_Growth >= subgroup.min.size & CONTENT_AREA == CA & GRADE == G & YEAR == prior.year1, Mean_Growth]))
      cr.mn.gr <- density(na.omit(Temp_Grade_Summary[N_Growth >= subgroup.min.size & CONTENT_AREA == CA & GRADE == G & YEAR == current.year, Mean_Growth]))
      plot(pr.mn.gr, xlim = range(pr.mn.gr$x, cr.mn.gr$x), ylim = range(pr.mn.gr$y, cr.mn.gr$y), main= "Mean Baseline SGPs")
      lines(cr.mn.gr, col="red")
      pr.md.gr <- density(na.omit(Temp_Grade_Summary[N_Growth >= subgroup.min.size & CONTENT_AREA == CA & GRADE == G & YEAR == prior.year1, Median_Growth]))
      cr.md.gr <- density(na.omit(Temp_Grade_Summary[N_Growth >= subgroup.min.size & CONTENT_AREA == CA & GRADE == G & YEAR == current.year, Median_Growth]))
      plot(pr.md.gr, xlim = range(pr.md.gr$x, cr.md.gr$x), ylim = range(pr.md.gr$y, cr.md.gr$y), main= "Median Baseline SGPs")
      lines(cr.md.gr, col="red")
    }
    dev.off()
}}

for (CA in content.areas) {
  Subgrp_HSF[[YEARS]][[tmp.var]][["GAMMA_ES"]][["SCALE_SCORE"]][[CA]][["BETWEEN"]][["CONTENT_AREA_by_GRADE"]][["MEAN"]] <-
    Temp_Grade_Summary[N_Status >= subgroup.min.size & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Mean_Status", prior.year1, current.year, digits=2)), keyby="GRADE"]
  Subgrp_HSF[[YEARS]][[tmp.var]][["GAMMA_ES"]][["SCALE_SCORE"]][[CA]][["BETWEEN"]][["CONTENT_AREA_by_GRADE"]][["MEDIAN"]] <-
    Temp_Grade_Summary[N_Status >= subgroup.min.size & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Median_Status", prior.year1, current.year, digits=2)), keyby="GRADE"]

  Subgrp_HSF[[YEARS]][[tmp.var]][["GAMMA_ES"]][["SGP_BASELINE"]][[CA]][["BETWEEN"]][["CONTENT_AREA_by_GRADE"]][["MEAN"]] <-
    na.omit(Temp_Grade_Summary[N_Growth >= subgroup.min.size & CONTENT_AREA == CA,
              as.list(gammaEffectSizeLong(.SD, "Mean_Growth", prior.year1, current.year, digits=2)), keyby="GRADE"])
  Subgrp_HSF[[YEARS]][[tmp.var]][["GAMMA_ES"]][["SGP_BASELINE"]][[CA]][["BETWEEN"]][["CONTENT_AREA_by_GRADE"]][["MEDIAN"]] <-
    na.omit(Temp_Grade_Summary[N_Growth >= subgroup.min.size & CONTENT_AREA == CA,
              as.list(gammaEffectSizeLong(.SD, "Median_Growth", prior.year1, current.year, digits=2)), keyby="GRADE"])
}

###   Merge in Subgrp_HSF to Report_Analyses
# Report_Analyses[["Quantile_Shift"]][[assessment]][["Hierarchical_Shift"]] <- Subgrp_HSF
}

names(Report_Analyses[["Quantile_Shift"]][[assessment]][["Hierarchical_Shift"]][[1]][[3]][[1]][[1]][[1]][[1]])
