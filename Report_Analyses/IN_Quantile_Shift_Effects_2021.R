################################################################################
###                                                                          ###
###    Indiana -- Quantile Shift Analyses (State, School and Corporation)    ###
###                                                                          ###
################################################################################

###   General Setup (assumes running from ./Documentation directory)

if (!exists("Report_Analyses"))
  load("../Data/Report_Analyses.Rdata")

###   Load formated Report_Data
if (!exists("Report_Data"))
  load("../Data/Report_Data.Rdata")

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
prior.year2  <- tail(params[["years"]][[assessment]], 3)[-c(2:3)]
prior.year3  <- tail(params[["years"]][[assessment]], 4)[-c(2:4)]
prior.year4  <- tail(params[["years"]][[assessment]], 5)[-c(2:5)]

all.grades <- params[["grades"]][[assessment]]
sgp.grades <- params[["sgp.grades"]][[assessment]]
stat.grades<- setdiff(all.grades, sgp.grades)

content.areas <- params[["GL_subjects"]][[assessment]]

###   Declare cutscores to use from SGPstateData
if (assessment == "State_Assessment") {
  assessment_cutscores <- list(
    ELA = SGPstateData[["IN"]][["Achievement"]][["Cutscores"]][[paste0("ELA.2019")]],
    MATHEMATICS = SGPstateData[["IN"]][["Achievement"]][["Cutscores"]][[paste0("MATHEMATICS.2019")]])
}
if (assessment == "ELP_Assessment") {
  assessment_cutscores <- SGPstateData[["WIDA_IN"]][["Achievement"]][["Cutscores"]]
}
prof.cut <- which(params$proficient_levels[[assessment]]=="Proficient")[1]-1

###   Make copy of the data to change/clean
long_data <- copy(Report_Data[[assessment]])
long_data[, SCALE_SCORE := round(SCALE_SCORE, 0)] # 2016 - 2018 EQUATED scores are not rounded!  Makes for ugly individual_sf tables...

plot.dir <- file.path("assets", "Rplots", "Quantile_Shift", assessment)

#####
###   Independent Groups Shift Functions
#####

if (beeswarm.plots) {

if (!dir.exists(file.path(plot.dir, "Grade_Level"))) dir.create(file.path(plot.dir, "Grade_Level"), recursive=TRUE)

long_data_subset <-
    long_data[VALID_CASE=="VALID_CASE" & YEAR %chin% c(prior.year1, current.year),
      .(ID, YEAR, GRADE, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER, SCALE_SCORE, SGP_BASELINE)]

setkey(long_data_subset, ID, YEAR)

long_data_subset[, YEAR := factor(YEAR)]
long_data_subset[, GRADE := factor(as.numeric(GRADE))]

###   Beeswarm Plots
###   Observations in the scatterplots are jittered based on their local density
###   Implemented with ggbeeswarm::geom_quasirandom

beeswarm_plots <- vector("list", length(all.grades))
names(beeswarm_plots) <- paste0("GRADE_", all.grades)

for (CA in content.areas) {
  for (G in all.grades) {
    tmp.cuts <- assessment_cutscores[[CA]][[paste0("GRADE_", G)]]
    for (tmp.focus in c("SGP_BASELINE", "SCALE_SCORE")) {
      tmp.vars.to.get <- c("YEAR", "GRADE", tmp.focus)
      tmp.df <- tibble(na.omit(long_data_subset[GRADE == G & CONTENT_AREA == CA, ..tmp.vars.to.get]))

      if (nrow(tmp.df) != 0) {
      ##    compute shift function
      switch(tmp.focus,
        SGP_BASELINE= beeswarm_plots[[paste0("GRADE_",G)]][[CA]][[tmp.focus]][["SHIFT"]] <-
            shifthd(data = tmp.df, formula = SGP_BASELINE ~ YEAR, nboot = 200),
        SCALE_SCORE = beeswarm_plots[[paste0("GRADE_",G)]][[CA]][[tmp.focus]][["SHIFT"]] <-
            shifthd(data = tmp.df, formula = SCALE_SCORE ~ YEAR, nboot = 200))

      p <- plot_scat2(data = tmp.df,
              formula = switch(tmp.focus, SGP_BASELINE = SGP_BASELINE ~ YEAR, SCALE_SCORE = SCALE_SCORE ~ YEAR),
              xlabel = "",
              ylabel = capwords(tmp.focus),
              alpha = (1000/nrow(tmp.df)),
              shape = 21,
              colour = "grey10",
              fill = "grey90")

      if (tmp.focus == "SCALE_SCORE")
        p <- p + geom_hline(yintercept=tmp.cuts[prof.cut], col="green", size=1)

      p <- plot_hd_links(p, beeswarm_plots[[paste0("GRADE_",G)]][[CA]][[tmp.focus]][["SHIFT"]][[1]],
              q_size = 0.75,
              md_size = 1.5,
              add_rect = TRUE,
              rect_alpha = 0.1,
              rect_col = "grey50",
              add_lab = TRUE,
              text_size = 5)

      p <- p + coord_flip()
      beeswarm_plots[[paste0("GRADE_",G)]][[CA]][[tmp.focus]][["PLOT"]] <- p
      }
    }
  }
}

for (G in stat.grades) {
  for (CA in content.areas) {
  # tmp.plots <- lapply(content.areas, function(f) beeswarm_plots[[paste0("GRADE_", G)]][[f]][["SCALE_SCORE"]][["PLOT"]])
  # if (length(params[["GL_camel"]][[assessment]]) > 1) tmp.labels <- params[["GL_camel"]][[assessment]] else tmp.labels <- NULL # times
    tmp.filename <- file.path(plot.dir, "Grade_Level", paste0("beeswarms_w_Decile_Shift__Grade_", G, "_", gsub("ematics", "", SGP::capwords(CA)), ".png"))
    ggsave(
      filename = tmp.filename,
      plot = beeswarm_plots[[paste0("GRADE_",G)]][[CA]][["SCALE_SCORE"]][["PLOT"]],
      width=7, height = 5, dpi=150, units="in", bg="transparent")
  }
}

for (G in sgp.grades) {
  for (CA in content.areas) {
  # if (length(params[["GL_camel"]][[assessment]]) > 1) tmp.labels <- rep(params[["GL_camel"]][[assessment]], each=2) else tmp.labels <- NULL # times
    tmp.filename <- file.path(plot.dir, "Grade_Level", paste0("beeswarms_w_Decile_Shift__Grade_", G, "_", gsub("ematics", "", SGP::capwords(CA)), ".png"))
    ggsave(
      filename = tmp.filename,
      plot=cowplot::plot_grid(
              beeswarm_plots[[paste0("GRADE_",G)]][[CA]][["SCALE_SCORE"]][["PLOT"]],
              beeswarm_plots[[paste0("GRADE_",G)]][[CA]][["SGP_BASELINE"]][["PLOT"]],
              ncol = 1, nrow = 2, labels = NULL),
      width=7, height = 5, dpi=150, units="in", bg="transparent")
  }
}

# save(beeswarm_plots, file=file.path(plot.dir, "Grade_Level", "beeswarm_plots.rda"))
Report_Analyses[["Quantile_Shift"]][[assessment]][["beeswarm_plots"]] <- beeswarm_plots
}  #  END beeswarm.plots

#####
###      Hierarchical Shift Function -- `rogme` package
###                        &
###    Akinshin's Gamma Effect Sizes -- `cfaTools` package
#####

if (hsf.analysis) {
skip.comp <- if (assessment == "State_Assessment") c(prior.year1, prior.year3) else NULL

comparison.years <- list(c(current.year, prior.year1), c(prior.year1, prior.year2), skip.comp)
                        #c(prior.year2, prior.year3), c(prior.year2, prior.year4), c(prior.year3, prior.year4))
comparison.years <- comparison.years[lengths(comparison.years) != 0]
HSF_Results <- vector("list", length(comparison.years))
names(HSF_Results) <- unlist(lapply(comparison.years, paste, collapse="_"))

for (cmpyr in seq(length(comparison.years))) {
  cmp.yrs <- comparison.years[[cmpyr]]
  YEARS <- paste(cmp.yrs, collapse="_")

  ##    Create temporary subset of variable of interest WITHOUT missing values (required)
  long_data_subset <-
      long_data[VALID_CASE=="VALID_CASE" & YEAR %chin% rev(cmp.yrs),
        .(ID, YEAR, GRADE, CONTENT_AREA, SCALE_SCORE, SGP_BASELINE, DISTRICT_NUMBER, SCHOOL_NUMBER)]

  setkey(long_data_subset, ID, YEAR)

  long_data_subset[, YEAR := factor(YEAR)]
  long_data_subset[, GRADE := factor(as.numeric(GRADE))]

  ##    Create Akinshin's Gamma Effect Size tables for comparison years
  for (tmp.focus in c("SGP_BASELINE", "SCALE_SCORE")) {
    HSF_Results[[YEARS]][["GRADE"]][["GAMMA_ES"]][[tmp.focus]] <- long_data_subset[,
      as.list(gammaEffectSizeLong(.SD, tmp.focus, cmp.yrs[2], cmp.yrs[1], digits=2)),
      keyby=c("CONTENT_AREA", "GRADE")]
  }

  for (CA in content.areas) {
    for (tmp.focus in c("SGP_BASELINE", "SCALE_SCORE")) {
      tmp.vars.to.get <- c("YEAR", "GRADE", tmp.focus)
      Grade_Subset <- as.data.frame(na.omit(long_data_subset[CONTENT_AREA == CA, ..tmp.vars.to.get]))

      if (cmpyr == 1 | tmp.focus == "SCALE_SCORE") {
      trim.mean <- 0.01 # trim level for the mean - keep VERY small for GRADE comparisons

      HSF_Results[[YEARS]][["GRADE"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]] <-
         hsf(Grade_Subset,
             formula = switch(tmp.focus,
               SGP_BASELINE = SGP_BASELINE ~ YEAR + GRADE,
               SCALE_SCORE = SCALE_SCORE ~ YEAR + GRADE),
             qseq = 1:9/10,
             tr = trim.mean, # default is 0.2
             alpha = 0.05,
             qtype = 8,
             todo = c(2,1),
             null.value = 0,
             adj_method = "hochberg")

      # Change plot MAIN title
      HSF_Results[[YEARS]][["GRADE"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]] <-
        paste(HSF_Results[[YEARS]][["GRADE"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]], capwords(paste(CA, tmp.focus)))
      HSF_Results[[YEARS]][["GRADE"]][["HShift"]][[tmp.focus]][[CA]][["PLOT"]] <-
        plot_hsf(HSF_Results[[YEARS]][["GRADE"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]])

      tmp.ges <- HSF_Results[[YEARS]][["GRADE"]][["GAMMA_ES"]][[tmp.focus]][CONTENT_AREA==CA]
      tmp.tbl1 <- data.table(na.omit(tmp.ges))
      tmp.tbl2 <- data.table(CONTENT_AREA = "", GRADE = "", # t(HSF_Results[[YEARS]][["GRADE"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]]))
                    matrix(paste0("(", t(round(HSF_Results[[YEARS]][["GRADE"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]], 2)), ")"), nrow(tmp.tbl1), 9))
      setnames(tmp.tbl2, names(tmp.tbl1))
      tmp.tbl2 <- data.table(sapply(tmp.tbl2, as.character))
      tmp.tbl1 <- data.table(sapply(tmp.tbl1, as.character))

      tmp.tbl <- data.table()
      for (k in seq(nrow(tmp.tbl1))) tmp.tbl <- rbindlist(list(tmp.tbl, tmp.tbl1[k,], tmp.tbl2[k,]))
      HSF_Results[[YEARS]][["GRADE"]][["HShift"]][[tmp.focus]][[CA]][["LONG_TABLE"]] <- tmp.tbl

      tmp.tbl1 <- t(na.omit(tmp.ges))[-c(1:2),]
      tmp.tbl2 <- round(HSF_Results[[YEARS]][["GRADE"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]], 2)
      # tmp.tbl2 <- matrix(paste0("(", HSF_Results[[YEARS]][["GRADE"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]], ")"), 9, ncol(tmp.tbl1))

      tmp.tbl <- data.table(cbind(tmp.tbl1, tmp.tbl2))
      setcolorder(tmp.tbl, c(rbind(seq(ncol(tmp.tbl1)), seq(ncol(tmp.tbl2))+ncol(tmp.tbl1))))
      setnames(tmp.tbl, c(paste0("G", t(na.omit(tmp.ges))[2,], "_ES"),
                          paste0("G", t(na.omit(tmp.ges))[2,], "_Diff"))[
                            c(rbind(seq(ncol(tmp.tbl1)), seq(ncol(tmp.tbl2))+ncol(tmp.tbl1)))])

      HSF_Results[[YEARS]][["GRADE"]][["HShift"]][[tmp.focus]][[CA]][["WIDE_TABLE"]] <- tmp.tbl


      ###   District Level
      trim.mean <- 0.2 # trim level for the mean - use default for DISTRICT and SCHOOL comparisons
      dist.vars.to.keep <- c("YEAR", "CONTENT_AREA", "GRADE", "DISTRICT_NUMBER", tmp.focus)
      District_Subset <- na.omit(long_data_subset[CONTENT_AREA == CA, ..dist.vars.to.keep][,
                                    COUNT_DIST := .N, keyby = c("YEAR", "DISTRICT_NUMBER")])

      ##    Exclude Districts (Corporations) with fewer than 50 students with valid scores/SGPs
      # length(unique(District_Subset[COUNT_DIST < 49, DISTRICT_NUMBER]))
      District_Subset <- District_Subset[COUNT_DIST >= params$min.size.district] # Use 50 for small

      ##    Exclude districts that only appear in 1 year
      dist.yr1 <- unique(District_Subset[YEAR == cmp.yrs[2], DISTRICT_NUMBER])
      dist.yr2 <- unique(District_Subset[YEAR == cmp.yrs[1], DISTRICT_NUMBER])
      only.yr1 <- dist.yr1[!dist.yr1 %in% dist.yr2]
      only.yr2 <- dist.yr2[!dist.yr2 %in% dist.yr1]

      District_Subset <- District_Subset[!DISTRICT_NUMBER %in% c(only.yr1, only.yr2)]
      # District_Subset[, as.list(summary(COUNT_DIST)), keyby="YEAR"]

      District_Subset[, DISTRICT_NUMBER := factor(DISTRICT_NUMBER)]

      ##    Create District Level Gamma Effect Size tables for comparison years
      HSF_Results[[YEARS]][["DISTRICT"]][["GAMMA_ES"]][[tmp.focus]][[CA]][["WITHIN"]][["CONTENT_AREA"]] <- na.omit(
          District_Subset[,
                as.list(gammaEffectSizeLong(.SD, tmp.focus, cmp.yrs[2], cmp.yrs[1], digits=2)),
              keyby="DISTRICT_NUMBER"])

      HSF_Results[[YEARS]][["DISTRICT"]][["GAMMA_ES"]][[tmp.focus]][[CA]][["WITHIN"]][["CONTENT_AREA_by_GRADE"]] <- na.omit(
          District_Subset[,
                as.list(gammaEffectSizeLong(.SD, tmp.focus, cmp.yrs[2], cmp.yrs[1], digits=2)),
              keyby=c("DISTRICT_NUMBER", "GRADE")])

      ##    Run District Level Hierarchical Shift Function for comparison years
      HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]] <-
          hsf_pb(as.data.frame(District_Subset),
              formula = switch(tmp.focus,
                SGP_BASELINE = SGP_BASELINE ~ YEAR + DISTRICT_NUMBER,
                SCALE_SCORE = SCALE_SCORE ~ YEAR + DISTRICT_NUMBER),
              qseq = 1:9/10,
              tr = trim.mean,
              alpha = 0.05,
              qtype = 8,
              todo = c(2,1))

      ##    Create HSF plots
      HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]] <- tmp.comp <-
        paste(HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]], capwords(paste(CA, tmp.focus)), "(Corporation Level)")
      HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["LINE_PLOT"]] <-
        plot_hsf_pb(HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]])

      HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]] <- ""
      HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["BOOT_PLOT"]] <-
        plot_hsf_pb_dist(HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]])
      HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]] <- tmp.comp

      ##    Format district results and add DISTRICT_NUMBER
      HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["LONG_TABLE"]] <- data.table(
          levels(District_Subset$DISTRICT_NUMBER),
          t(round(HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]], 2)))
      setnames(HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["LONG_TABLE"]], c("DISTRICT_NUMBER", paste0("DIFF_Q", 1:9*10)))

      ##    Stochastic Dominance
      np <- length(unique(District_Subset$DISTRICT_NUMBER))
      nq <- length(HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["quantiles"]])
      pdmt0 <- apply(HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]] > 0, 2, sum)
      pdlt0 <- apply(HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]] < 0, 2, sum)

      HSF_Results[[YEARS]][["DISTRICT"]][["HShift"]][[tmp.focus]][[CA]][["STOCH_DOM"]] <- list(
        c(paste0('In ',sum(pdmt0 == nq),' corporations (', round(100 * sum(pdmt0 == nq) / np, digits = 1), '%), all quantile differences are more than to zero'),
          paste0('In ',sum(pdlt0 == nq),' corporations (', round(100 * sum(pdlt0 == nq) / np, digits = 1), '%), all quantile differences are less than to zero')),
        c(round(100 * sum(pdmt0 == nq) / np, digits = 1), round(100 * sum(pdlt0 == nq) / np, digits = 1)))

      rm(District_Subset);gc()

      ###   School Level
      trim.mean <- 0.2 # trim level for the mean - use default for DISTRICT and SCHOOL comparisons
      sch.vars.to.keep <- c("YEAR", "CONTENT_AREA", "GRADE", "SCHOOL_NUMBER", tmp.focus)
      School_Subset <- na.omit(long_data_subset[CONTENT_AREA == CA, ..sch.vars.to.keep][,
                                  COUNT_SCH := .N, keyby = c("YEAR", "SCHOOL_NUMBER")])

      ##    Exclude Schools with fewer than 15 students with valid scores/SGPs
      # length(unique(School_Subset[, SCHOOL_NUMBER]))
      School_Subset <- School_Subset[COUNT_SCH >= params$min.size.school]

      ##    Exclude schools that only appear in 1 year
      sch.yr1 <- unique(School_Subset[YEAR == cmp.yrs[2], SCHOOL_NUMBER])
      sch.yr2 <- unique(School_Subset[YEAR == cmp.yrs[1], SCHOOL_NUMBER])
      only.yr1 <- sch.yr1[!sch.yr1 %in% sch.yr2]
      only.yr2 <- sch.yr2[!sch.yr2 %in% sch.yr1]

      School_Subset <- School_Subset[!SCHOOL_NUMBER %in% c(only.yr1, only.yr2)]
      # School_Subset[, as.list(summary(COUNT_SCH)), keyby="YEAR"]

      School_Subset[, SCHOOL_NUMBER := factor(SCHOOL_NUMBER)]

      ##    Create School Level Gamma Effect Size tables for comparison years
      HSF_Results[[YEARS]][["SCHOOL"]][["GAMMA_ES"]][[tmp.focus]][[CA]][["WITHIN"]][["CONTENT_AREA"]] <- na.omit(
          School_Subset[,
                as.list(gammaEffectSizeLong(.SD, tmp.focus, cmp.yrs[2], cmp.yrs[1], digits=2)),
              keyby="SCHOOL_NUMBER"])

      HSF_Results[[YEARS]][["SCHOOL"]][["GAMMA_ES"]][[tmp.focus]][[CA]][["WITHIN"]][["CONTENT_AREA_by_GRADE"]] <- na.omit(
          School_Subset[,
                as.list(gammaEffectSizeLong(.SD, tmp.focus, cmp.yrs[2], cmp.yrs[1], digits=2)),
              keyby=c("SCHOOL_NUMBER", "GRADE")])

      ##    Run School Level Hierarchical Shift Function for comparison years
      HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]] <-
         hsf_pb(as.data.frame(School_Subset),
                formula = switch(tmp.focus,
                  SGP_BASELINE = SGP_BASELINE ~ YEAR + SCHOOL_NUMBER,
                  SCALE_SCORE = SCALE_SCORE ~ YEAR + SCHOOL_NUMBER),
                qseq = 1:9/10,
                tr = trim.mean,
                alpha = 0.05,
                qtype = 8,
                todo = c(2,1))

      ##    Create HSF plots
      HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]] <- tmp.comp <-
        paste(HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]], capwords(paste(CA, tmp.focus)), "(School Level)")
      HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["LINE_PLOT"]] <-
        plot_hsf_pb(HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]])

      HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]] <- ""
      HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["BOOT_PLOT"]] <-
        plot_hsf_pb_dist(HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]])
      HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["comparison"]] <- tmp.comp

      ##    Format school results and add SCHOOL_NUMBER
      HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["LONG_TABLE"]] <- data.table(
          levels(School_Subset$SCHOOL_NUMBER),
          t(round(HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]], 2)))
      setnames(HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["LONG_TABLE"]], c("SCHOOL_NUMBER", paste0("DIFF_Q", 1:9*10)))

      ##    Stochastic Dominance
      np <- length(unique(School_Subset$SCHOOL_NUMBER))
      nq <- length(HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["quantiles"]])
      pdmt0 <- apply(HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]] > 0, 2, sum)
      pdlt0 <- apply(HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["HSF"]][["individual_sf"]] < 0, 2, sum)

      HSF_Results[[YEARS]][["SCHOOL"]][["HShift"]][[tmp.focus]][[CA]][["STOCH_DOM"]] <- list(
        c(paste0('In ',sum(pdmt0 == nq),' schools (', round(100 * sum(pdmt0 == nq) / np, digits = 1), '%), all quantile differences are more than to zero'),
          paste0('In ',sum(pdlt0 == nq),' schools (', round(100 * sum(pdlt0 == nq) / np, digits = 1), '%), all quantile differences are less than to zero')),
        c(round(100 * sum(pdmt0 == nq) / np, digits = 3), round(100 * sum(pdlt0 == nq) / np, digits = 3)))

      rm(School_Subset);gc()
      } # END if nrow
    }   # END tmp.focus
  }     # END Content Area
}       # END comparison.years
if (!between.inst.ges) {
  Report_Analyses[["Quantile_Shift"]][[assessment]][["Hierarchical_Shift"]] <- HSF_Results
}
}       # END hsf.analysis


# ###   Full District (largest population here)
# long_data_subset <-
#     long_data[VALID_CASE=="VALID_CASE" & YEAR %chin% c(prior.year1, current.year),
#       .(ID, YEAR, GRADE, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER, SCALE_SCORE, SGP_BASELINE)] # SGP, SCALE_SCORE_ORIGINAL
#
# setkey(long_data_subset, ID, YEAR)
#
# long_data_subset[, COUNT_DIST := .N, keyby = c("YEAR", "DISTRICT_NUMBER")]
# long_data_subset[, as.list(summary(COUNT_DIST)), keyby="YEAR"]
#
# tmp.dist.num <- unique(long_data_subset[COUNT_DIST == max(COUNT_DIST), DISTRICT_NUMBER])
#
# tmp.focus <- "SGP_BASELINE"
# sch.vars.to.keep <- c("YEAR", "CONTENT_AREA", "SCHOOL_NUMBER", tmp.focus)
# School_Subset <- na.omit(long_data_subset[CONTENT_AREA == CA & DISTRICT_NUMBER == tmp.dist.num, ..sch.vars.to.keep])
#
# School_Subset[, YEAR := factor(YEAR)]
# School_Subset[, SCHOOL_NUMBER := factor(SCHOOL_NUMBER)]
#
# tmp.dist.sgp <- hsf_pb(as.data.frame(School_Subset),
#                        formula = switch(tmp.focus,
#                          SGP_BASELINE = SGP_BASELINE ~ YEAR + SCHOOL_NUMBER,
#                          SCALE_SCORE = SCALE_SCORE ~ YEAR + SCHOOL_NUMBER),
#                         qseq = 1:9/10,
#                         tr = trim.mean,
#                         alpha = 0.05,
#                         qtype = 8,
#                         todo = c(2,1))
#
# tmp.dist.sgp$comparison <- "2021 - 2019 Baseline SGPs - Schools in One Large Corporation" # "2021 - 2019 Scale Scores - Schools in One Large Corporation" # Change plot MAIN title #
# tmp.dist.indv.sf <- plot_hsf_pb(tmp.dist.sgp)
# tmp.dist.pbd <- plot_hsf_pb_dist(tmp.dist.sgp)
#
# ##    Stochastic Dominance
# np <- length(unique(School_Subset$SCHOOL_NUMBER))
# nq <- length(tmp.dist.sgp$quantiles)
#
# pdmt0 <- apply(tmp.dist.sgp$individual_sf > 0, 2, sum)
# print(paste0('In ',sum(pdmt0 == nq),' participants (',round(100 * sum(pdmt0 == nq) / np, digits = 1),'%), all quantile differences are more than to zero'))
#
# pdlt0 <- apply(tmp.dist.sgp$individual_sf < 0, 2, sum)
# print(paste0('In ',sum(pdlt0 == nq),' participants (',round(100 * sum(pdlt0 == nq) / np, digits = 1),'%), all quantile differences are less than to zero'))


################################################################################
###          Between District/School (Aggregate) Gamma Effect Sizes          ###
################################################################################

if (between.inst.ges) {

if (!hsf.analysis) {
  HSF_Results <- Report_Analyses[["Quantile_Shift"]][[assessment]][["Hierarchical_Shift"]]
}

long_data_subset <-
    long_data[VALID_CASE=="VALID_CASE" & YEAR %chin% c(prior.year1, current.year),
      .(ID, YEAR, GRADE, CONTENT_AREA, DISTRICT_NUMBER, SCHOOL_NUMBER, SCALE_SCORE, SGP_BASELINE)]

setkey(long_data_subset, ID, YEAR)

long_data_subset[, YEAR := factor(YEAR)]
long_data_subset[, GRADE := factor(as.numeric(GRADE))]

YEARS <- paste0(current.year, "_", prior.year1)

dst_plot_dir <- file.path(plot.dir, "District_Level")
sch_plot_dir <- file.path(plot.dir, "School_Level")
if (!dir.exists(dst_plot_dir)) dir.create(dst_plot_dir, recursive=TRUE)
if (!dir.exists(sch_plot_dir)) dir.create(sch_plot_dir, recursive=TRUE)

#####
###   Districs
#####

###   All-District Level Summaries
District_Summary <- long_data_subset[YEAR %in% c(prior.year1, current.year),
    .(YEAR, CONTENT_AREA, DISTRICT_NUMBER, SCALE_SCORE, SGP_BASELINE)][, .(
        Mean_Status = mean(SCALE_SCORE, na.rm = TRUE),
        Median_Status = as.numeric(median(SCALE_SCORE, na.rm = TRUE)),
        Mean_Growth = mean(SGP_BASELINE, na.rm = TRUE),
        Median_Growth = as.numeric(median(SGP_BASELINE, na.rm = TRUE)),
        N_Status = sum(!is.na(SCALE_SCORE)),
        N_Growth = sum(!is.na(SGP_BASELINE))),
      keyby=c("CONTENT_AREA", "YEAR", "DISTRICT_NUMBER")]

for (CA in content.areas) {
  png(file.path(dst_plot_dir, paste0("District_Level_Status_Growth__", gsub("ematics", "", capwords(CA)), ".png")))
  par(mfrow = c(2, 2))

  pr.mn.st <- density(na.omit(District_Summary[N_Status >= params$min.size.district & CONTENT_AREA == CA & YEAR == prior.year1, Mean_Status]))
  cr.mn.st <- density(na.omit(District_Summary[N_Status >= params$min.size.district & CONTENT_AREA == CA & YEAR == current.year, Mean_Status]))
  plot(pr.mn.st, xlim = range(pr.mn.st$x, cr.mn.st$x), ylim = range(pr.mn.st$y, cr.mn.st$y),
       main = paste(gsub("ematics", "", capwords(CA)), "\n\nMean Scale Scores"),
       sub = paste("\nBlack =", prior.year1, ", Red =", current.year))
  lines(cr.mn.st, col="red")

  pr.md.st <- density(na.omit(District_Summary[N_Status >= params$min.size.district & CONTENT_AREA == CA & YEAR == prior.year1, Median_Status]))
  cr.md.st <- density(na.omit(District_Summary[N_Status >= params$min.size.district & CONTENT_AREA == CA & YEAR == current.year, Median_Status]))
  plot(pr.md.st, xlim = range(pr.md.st$x, cr.md.st$x), ylim = range(pr.md.st$y, cr.md.st$y),
       main = paste0("Districts with N >=", params$min.size.district, "\n\nMedian Scale Scores"),
       sub = paste("\nBlack =", prior.year1, ", Red =", current.year))
  lines(cr.md.st, col="red")

  pr.mn.gr <- density(na.omit(District_Summary[N_Growth >= params$min.size.district & CONTENT_AREA == CA & YEAR == prior.year1, Mean_Growth]))
  cr.mn.gr <- density(na.omit(District_Summary[N_Growth >= params$min.size.district & CONTENT_AREA == CA & YEAR == current.year, Mean_Growth]))
  plot(pr.mn.gr, xlim = range(pr.mn.gr$x, cr.mn.gr$x), ylim = range(pr.mn.gr$y, cr.mn.gr$y), main= "Mean Baseline SGPs")
  lines(cr.mn.gr, col="red")
  
  pr.md.gr <- density(na.omit(District_Summary[N_Growth >= params$min.size.district & CONTENT_AREA == CA & YEAR == prior.year1, Median_Growth]))
  cr.md.gr <- density(na.omit(District_Summary[N_Growth >= params$min.size.district & CONTENT_AREA == CA & YEAR == current.year, Median_Growth]))
  plot(pr.md.gr, xlim = range(pr.md.gr$x, cr.md.gr$x), ylim = range(pr.md.gr$y, cr.md.gr$y), main= "Median Baseline SGPs")
  lines(cr.md.gr, col="red")

  dev.off()
}

for (CA in content.areas) {
  HSF_Results[[YEARS]][["DISTRICT"]][["GAMMA_ES"]][["SCALE_SCORE"]][[CA]][["BETWEEN"]][["CONTENT_AREA"]][["MEAN"]] <-
    District_Summary[N_Status >= params$min.size.district & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Mean_Status", prior.year1, current.year, digits=2))]#,
    # keyby="CONTENT_AREA"]
  HSF_Results[[YEARS]][["DISTRICT"]][["GAMMA_ES"]][["SCALE_SCORE"]][[CA]][["BETWEEN"]][["CONTENT_AREA"]][["MEDIAN"]] <-
    District_Summary[N_Status >= params$min.size.district & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Median_Status", prior.year1, current.year, digits=2))]

  HSF_Results[[YEARS]][["DISTRICT"]][["GAMMA_ES"]][["SGP_BASELINE"]][[CA]][["BETWEEN"]][["CONTENT_AREA"]][["MEAN"]] <-
    District_Summary[N_Growth >= params$min.size.district & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Mean_Growth", prior.year1, current.year, digits=2))]#,
  HSF_Results[[YEARS]][["DISTRICT"]][["GAMMA_ES"]][["SGP_BASELINE"]][[CA]][["BETWEEN"]][["CONTENT_AREA"]][["MEDIAN"]] <-
    District_Summary[N_Growth >= params$min.size.district & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Mean_Growth", prior.year1, current.year, digits=2))]
}

###   District by Grade Level Summaries
District_Grade_Summary <- long_data_subset[YEAR %in% c(prior.year1, current.year),
    .(YEAR, CONTENT_AREA, GRADE, DISTRICT_NUMBER, SCALE_SCORE, SGP_BASELINE)][, .(
        Mean_Status = mean(SCALE_SCORE, na.rm = TRUE),
        Median_Status = as.numeric(median(SCALE_SCORE, na.rm = TRUE)),
        Mean_Growth = mean(SGP_BASELINE, na.rm = TRUE),
        Median_Growth = as.numeric(median(SGP_BASELINE, na.rm = TRUE)),
        N_Status = sum(!is.na(SCALE_SCORE)),
        N_Growth = sum(!is.na(SGP_BASELINE))),
      keyby=c("CONTENT_AREA", "GRADE", "YEAR", "DISTRICT_NUMBER")]

# District_Grade_Summary[, .(SD_Mean = sd(Mean_Status), SD_Median = sd(Median_Status)), keyby=c("CONTENT_AREA", "GRADE", "YEAR")]

for (CA in content.areas) {
  for (G in all.grades) {
    if (G > max(as.numeric(stat.grades))) {
      png(file.path(dst_plot_dir, paste0("District_Level_Status_Growth__", gsub("ematics", "", capwords(CA)), "_G", G, ".png")))
      par(mfrow = c(2, 2))
    } else {
      png(file.path(dst_plot_dir, paste0("District_Level_Status__", gsub("ematics", "", capwords(CA)), "_G", G, ".png")))
      par(mfrow = c(2, 1))
    }
    pr.mn.st <- density(na.omit(District_Grade_Summary[N_Status >= params$min.size.district & CONTENT_AREA == CA & GRADE == G & YEAR == prior.year1, Mean_Status]))
    cr.mn.st <- density(na.omit(District_Grade_Summary[N_Status >= params$min.size.district & CONTENT_AREA == CA & GRADE == G & YEAR == current.year, Mean_Status]))
    plot(pr.mn.st, xlim = range(pr.mn.st$x, cr.mn.st$x), ylim = range(pr.mn.st$y, cr.mn.st$y),
         main = paste(gsub("ematics", "", capwords(CA)), "Grade", G, "\n\nMean Scale Scores"),
         sub = "\nBlack = 2019, Red = 2021")
    lines(cr.mn.st, col="red")
    pr.md.st <- density(na.omit(District_Grade_Summary[N_Status >= params$min.size.district & CONTENT_AREA == CA & GRADE == G & YEAR == prior.year1, Median_Status]))
    cr.md.st <- density(na.omit(District_Grade_Summary[N_Status >= params$min.size.district & CONTENT_AREA == CA & GRADE == G & YEAR == current.year, Median_Status]))
    plot(pr.md.st, xlim = range(pr.md.st$x, cr.md.st$x), ylim = range(pr.md.st$y, cr.md.st$y),
         main = "Districts with N >= 15\n\nMedian Scale Scores",
         sub = "\nBlack = 2019, Red = 2021")
    lines(cr.md.st, col="red")

    if (G > max(as.numeric(stat.grades))) {
      pr.mn.gr <- density(na.omit(District_Grade_Summary[N_Growth >= params$min.size.district & CONTENT_AREA == CA & GRADE == G & YEAR == prior.year1, Mean_Growth]))
      cr.mn.gr <- density(na.omit(District_Grade_Summary[N_Growth >= params$min.size.district & CONTENT_AREA == CA & GRADE == G & YEAR == current.year, Mean_Growth]))
      plot(pr.mn.gr, xlim = range(pr.mn.gr$x, cr.mn.gr$x), ylim = range(pr.mn.gr$y, cr.mn.gr$y), main= "Mean Baseline SGPs")
      lines(cr.mn.gr, col="red")

      pr.md.gr <- density(na.omit(District_Grade_Summary[N_Growth >= params$min.size.district & CONTENT_AREA == CA & GRADE == G & YEAR == prior.year1, Median_Growth]))
      cr.md.gr <- density(na.omit(District_Grade_Summary[N_Growth >= params$min.size.district & CONTENT_AREA == CA & GRADE == G & YEAR == current.year, Median_Growth]))
      plot(pr.md.gr, xlim = range(pr.md.gr$x, cr.md.gr$x), ylim = range(pr.md.gr$y, cr.md.gr$y), main= "Median Baseline SGPs")
      lines(cr.md.gr, col="red")
    }
    dev.off()
}}

for (CA in content.areas) {
  HSF_Results[[YEARS]][["DISTRICT"]][["GAMMA_ES"]][["SCALE_SCORE"]][[CA]][["BETWEEN"]][["CONTENT_AREA_by_GRADE"]][["MEAN"]] <-
    District_Grade_Summary[N_Status >= params$min.size.district & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Mean_Status", prior.year1, current.year, digits=2)), keyby="GRADE"]
  HSF_Results[[YEARS]][["DISTRICT"]][["GAMMA_ES"]][["SCALE_SCORE"]][[CA]][["BETWEEN"]][["CONTENT_AREA_by_GRADE"]][["MEDIAN"]] <-
    District_Grade_Summary[N_Status >= params$min.size.district & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Median_Status", prior.year1, current.year, digits=2)), keyby="GRADE"]

  HSF_Results[[YEARS]][["DISTRICT"]][["GAMMA_ES"]][["SGP_BASELINE"]][[CA]][["BETWEEN"]][["CONTENT_AREA_by_GRADE"]][["MEAN"]] <-
    na.omit(District_Grade_Summary[N_Growth >= params$min.size.district & CONTENT_AREA == CA,
              as.list(gammaEffectSizeLong(.SD, "Mean_Growth", prior.year1, current.year, digits=2)), keyby="GRADE"])
  HSF_Results[[YEARS]][["DISTRICT"]][["GAMMA_ES"]][["SGP_BASELINE"]][[CA]][["BETWEEN"]][["CONTENT_AREA_by_GRADE"]][["MEDIAN"]] <-
    na.omit(District_Grade_Summary[N_Growth >= params$min.size.district & CONTENT_AREA == CA,
              as.list(gammaEffectSizeLong(.SD, "Median_Growth", prior.year1, current.year, digits=2)), keyby="GRADE"])
}

#####
###   Schools
#####

###   All-School Level Summaries
School_Summary <- long_data_subset[YEAR %in% c(prior.year1, current.year),
    .(YEAR, CONTENT_AREA, SCHOOL_NUMBER, SCALE_SCORE, SGP_BASELINE)][, .(
        Mean_Status = mean(SCALE_SCORE, na.rm = TRUE),
        Median_Status = as.numeric(median(SCALE_SCORE, na.rm = TRUE)),
        Mean_Growth = mean(SGP_BASELINE, na.rm = TRUE),
        Median_Growth = as.numeric(median(SGP_BASELINE, na.rm = TRUE)),
        N_Status = sum(!is.na(SCALE_SCORE)),
        N_Growth = sum(!is.na(SGP_BASELINE))),
      keyby=c("CONTENT_AREA", "YEAR", "SCHOOL_NUMBER")]

# School_Summary[, .(SD_Mean = sd(Mean_Status), SD_Median = sd(Median_Status)), keyby=c("CONTENT_AREA", "YEAR")]

for (CA in content.areas) {
  png(file.path(sch_plot_dir, paste0("School_Level_Status_Growth__", gsub("ematics", "", capwords(CA)), ".png")))
  par(mfrow = c(2, 2))

  pr.mn.st <- density(na.omit(School_Summary[N_Status >= params$min.size.school & CONTENT_AREA == CA & YEAR == prior.year1, Mean_Status]))
  cr.mn.st <- density(na.omit(School_Summary[N_Status >= params$min.size.school & CONTENT_AREA == CA & YEAR == current.year, Mean_Status]))
  plot(pr.mn.st, xlim = range(pr.mn.st$x, cr.mn.st$x), ylim = range(pr.mn.st$y, cr.mn.st$y),
               main = paste(gsub("ematics", "", capwords(CA)), "\n\nMean Scale Scores"),
               sub = paste("\nBlack =", prior.year1, ", Red =", current.year))
  lines(cr.mn.st, col="red")

  pr.md.st <- density(na.omit(School_Summary[N_Status >= params$min.size.school & CONTENT_AREA == CA & YEAR == prior.year1, Median_Status]))
  cr.md.st <- density(na.omit(School_Summary[N_Status >= params$min.size.school & CONTENT_AREA == CA & YEAR == current.year, Median_Status]))
  plot(pr.md.st, xlim = range(pr.md.st$x, cr.md.st$x), ylim = range(pr.md.st$y, cr.md.st$y),
       main = paste0("Schools with N >=", params$min.size.school, "\n\nMedian Scale Scores"),
       sub = paste("\nBlack =", prior.year1, ", Red =", current.year))
  lines(cr.md.st, col="red")

  pr.mn.gr <- density(na.omit(School_Summary[N_Growth >= params$min.size.school & CONTENT_AREA == CA & YEAR == prior.year1, Mean_Growth]))
  cr.mn.gr <- density(na.omit(School_Summary[N_Growth >= params$min.size.school & CONTENT_AREA == CA & YEAR == current.year, Mean_Growth]))
  plot(pr.mn.gr, xlim = range(pr.mn.gr$x, cr.mn.gr$x), ylim = range(pr.mn.gr$y, cr.mn.gr$y), main= "Mean Baseline SGPs")
  lines(cr.mn.gr, col="red")

  pr.md.gr <- density(na.omit(School_Summary[N_Growth >= params$min.size.school & CONTENT_AREA == CA & YEAR == prior.year1, Median_Growth]))
  cr.md.gr <- density(na.omit(School_Summary[N_Growth >= params$min.size.school & CONTENT_AREA == CA & YEAR == current.year, Median_Growth]))
  plot(pr.md.gr, xlim = range(pr.md.gr$x, cr.md.gr$x), ylim = range(pr.md.gr$y, cr.md.gr$y), main= "Median Baseline SGPs")
  lines(cr.md.gr, col="red")

  dev.off()
}

for (CA in content.areas) {
  HSF_Results[[YEARS]][["SCHOOL"]][["GAMMA_ES"]][["SCALE_SCORE"]][[CA]][["BETWEEN"]][["CONTENT_AREA"]][["MEAN"]] <-
    School_Summary[N_Status >= params$min.size.school & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Mean_Status", prior.year1, current.year, digits=2))]#,
    # keyby="CONTENT_AREA"]
  HSF_Results[[YEARS]][["SCHOOL"]][["GAMMA_ES"]][["SCALE_SCORE"]][[CA]][["BETWEEN"]][["CONTENT_AREA"]][["MEDIAN"]] <-
    School_Summary[N_Status >= params$min.size.school & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Median_Status", prior.year1, current.year, digits=2))]

  HSF_Results[[YEARS]][["SCHOOL"]][["GAMMA_ES"]][["SGP_BASELINE"]][[CA]][["BETWEEN"]][["CONTENT_AREA"]][["MEAN"]] <-
    School_Summary[N_Growth >= params$min.size.school & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Mean_Growth", prior.year1, current.year, digits=2))]#,
  HSF_Results[[YEARS]][["SCHOOL"]][["GAMMA_ES"]][["SGP_BASELINE"]][[CA]][["BETWEEN"]][["CONTENT_AREA"]][["MEDIAN"]] <-
    School_Summary[N_Growth >= params$min.size.school & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Mean_Growth", prior.year1, current.year, digits=2))]
}

###   School by Grade Level Summaries

School_Grade_Summary <- long_data_subset[YEAR %in% c(prior.year1, current.year),
    .(YEAR, CONTENT_AREA, GRADE, SCHOOL_NUMBER, SCALE_SCORE, SGP_BASELINE)][, .(
        Mean_Status = mean(SCALE_SCORE, na.rm = TRUE),
        Median_Status = as.numeric(median(SCALE_SCORE, na.rm = TRUE)),
        Mean_Growth = mean(SGP_BASELINE, na.rm = TRUE),
        Median_Growth = as.numeric(median(SGP_BASELINE, na.rm = TRUE)),
        N_Status = sum(!is.na(SCALE_SCORE)),
        N_Growth = sum(!is.na(SGP_BASELINE))),
      keyby=c("CONTENT_AREA", "GRADE", "YEAR", "SCHOOL_NUMBER")]

# School_Grade_Summary[, .(SD_Mean = sd(Mean_Status), SD_Median = sd(Median_Status)), keyby=c("CONTENT_AREA", "GRADE", "YEAR")]

for (CA in content.areas) {
  for (G in all.grades) {
    if (G > max(as.numeric(stat.grades))) {
      png(file.path(sch_plot_dir, paste0("School_Level_Status_Growth__", gsub("ematics", "", capwords(CA)), "_G", G, ".png")))
      par(mfrow = c(2, 2))
    } else {
      png(file.path(sch_plot_dir, paste0("School_Level_Status__", gsub("ematics", "", capwords(CA)), "_G", G, ".png")))
      par(mfrow = c(2, 1))
    }
    pr.mn.st <- density(na.omit(School_Grade_Summary[N_Status >= params$min.size.school & CONTENT_AREA == CA & GRADE == G & YEAR == prior.year1, Mean_Status]))
    cr.mn.st <- density(na.omit(School_Grade_Summary[N_Status >= params$min.size.school & CONTENT_AREA == CA & GRADE == G & YEAR == current.year, Mean_Status]))
    plot(pr.mn.st, xlim = range(pr.mn.st$x, cr.mn.st$x), ylim = range(pr.mn.st$y, cr.mn.st$y),
         main = paste(gsub("ematics", "", capwords(CA)), "Grade", G, "\n\nMean Scale Scores"), sub = "\nBlack = 2019, Red = 2021")
            # main = paste("School Level Achievement and Growth:", gsub("ematics", "", capwords(CA)), "Grade", G, "\n\nMean Scale Scores"))
    lines(cr.mn.st, col="red")

    pr.md.st <- density(na.omit(School_Grade_Summary[N_Status >= params$min.size.school & CONTENT_AREA == CA & GRADE == G & YEAR == prior.year1, Median_Status]))
    cr.md.st <- density(na.omit(School_Grade_Summary[N_Status >= params$min.size.school & CONTENT_AREA == CA & GRADE == G & YEAR == current.year, Median_Status]))
    plot(pr.md.st, xlim = range(pr.md.st$x, cr.md.st$x), ylim = range(pr.md.st$y, cr.md.st$y),
         main = "Schools with N >= 15\n\nMedian Scale Scores", sub = "\nBlack = 2019, Red = 2021")
    lines(cr.md.st, col="red")

    if (G > max(as.numeric(stat.grades))) {
      pr.mn.gr <- density(na.omit(School_Grade_Summary[N_Growth >= params$min.size.school & CONTENT_AREA == CA & GRADE == G & YEAR == prior.year1, Mean_Growth]))
      cr.mn.gr <- density(na.omit(School_Grade_Summary[N_Growth >= params$min.size.school & CONTENT_AREA == CA & GRADE == G & YEAR == current.year, Mean_Growth]))
      plot(pr.mn.gr, xlim = range(pr.mn.gr$x, cr.mn.gr$x), ylim = range(pr.mn.gr$y, cr.mn.gr$y), main= "Mean Baseline SGPs")
      lines(cr.mn.gr, col="red")

      pr.md.gr <- density(na.omit(School_Grade_Summary[N_Growth >= params$min.size.school & CONTENT_AREA == CA & GRADE == G & YEAR == prior.year1, Median_Growth]))
      cr.md.gr <- density(na.omit(School_Grade_Summary[N_Growth >= params$min.size.school & CONTENT_AREA == CA & GRADE == G & YEAR == current.year, Median_Growth]))
      plot(pr.md.gr, xlim = range(pr.md.gr$x, cr.md.gr$x), ylim = range(pr.md.gr$y, cr.md.gr$y), main= "Median Baseline SGPs")
      lines(cr.md.gr, col="red")
    }
    dev.off()
}}

for (CA in content.areas) {
  HSF_Results[[YEARS]][["SCHOOL"]][["GAMMA_ES"]][["SCALE_SCORE"]][[CA]][["BETWEEN"]][["CONTENT_AREA_by_GRADE"]][["MEAN"]] <-
    School_Grade_Summary[N_Status >= params$min.size.school & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Mean_Status", prior.year1, current.year, digits=2)), keyby="GRADE"]
  HSF_Results[[YEARS]][["SCHOOL"]][["GAMMA_ES"]][["SCALE_SCORE"]][[CA]][["BETWEEN"]][["CONTENT_AREA_by_GRADE"]][["MEDIAN"]] <-
    School_Grade_Summary[N_Status >= params$min.size.school & CONTENT_AREA == CA,
      as.list(gammaEffectSizeLong(.SD, "Median_Status", prior.year1, current.year, digits=2)), keyby="GRADE"]

  HSF_Results[[YEARS]][["SCHOOL"]][["GAMMA_ES"]][["SGP_BASELINE"]][[CA]][["BETWEEN"]][["CONTENT_AREA_by_GRADE"]][["MEAN"]] <-
    na.omit(School_Grade_Summary[N_Growth >= params$min.size.school & CONTENT_AREA == CA,
              as.list(gammaEffectSizeLong(.SD, "Mean_Growth", prior.year1, current.year, digits=2)), keyby="GRADE"])
  HSF_Results[[YEARS]][["SCHOOL"]][["GAMMA_ES"]][["SGP_BASELINE"]][[CA]][["BETWEEN"]][["CONTENT_AREA_by_GRADE"]][["MEDIAN"]] <-
    na.omit(School_Grade_Summary[N_Growth >= params$min.size.school & CONTENT_AREA == CA,
              as.list(gammaEffectSizeLong(.SD, "Median_Growth", prior.year1, current.year, digits=2)), keyby="GRADE"])
}

###   Merge in HSF_Results to Report_Analyses
Report_Analyses[["Quantile_Shift"]][[assessment]][["Hierarchical_Shift"]] <- HSF_Results
}

# Indiana_HSF[["2021_2019"]][["SCHOOL"]][["MATHEMATICS"]][["SGP_BASELINE"]][["LINE_PLOT"]]
# Indiana_HSF[["2021_2019"]][["SCHOOL"]][["MATHEMATICS"]][["SGP_BASELINE"]][["BOOT_PLOT"]]
