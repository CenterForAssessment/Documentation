#-------------------------------------------------------------------------------
#Description: Create Customized Analaysis for IN for participation, enrollment
#etc.
#Authors: Nathan Dadey
#Last Modified on: 7/5/2021
#Notes:
  #Previously Created Variables
    #ACHIEVEMENT_ProfandAbove
  #Could standardize using last years means and SDs
  #Get prior year scale scores & Achievement Levels, only based on prior IDs
  #SCALE_SCORE_GRADEPRIOR

#I. Setup ----------------------------------------------------------------------
  #A. General Set Up  ----------------------------------------------------------

  if (!exists("Report_Analyses"))
      stop("Script assumes you have either loaded an existing 'Report_Analyses' object.")

  ###   Load formated Report_Data
  if (!exists("Report_Data"))  load("../Data/Report_Data.Rdata")

  ###   Locate the "Universal_Content" directory (assume woring directory is ./Documentation)
  universal.content.path <- file.path("..", "..", "..", "Universal_Content")

  if (!exists("assessment")) assessment <- "State_Assessment"

  ##    Read in Nathan's configs from `parmas` development
  # source("Report_Analyses/IN_Temporary_Meta_Data.R")  #  source(file="CustomContent/Indiana.R")

  #Create Priors (2021 Only)
  #need test case

  #A. Remove Duplicated Cases in 2019 and 2021 ---------------------------------
  long_prior <- Report_Data[[assessment]][YEAR %in% c("2019", "2021")]
  dup.ids    <- which(duplicated(long_prior[,c("YEAR", "CONTENT_AREA", "ID")]))
  dup.ids    <- long_prior[dup.ids,c("YEAR", "CONTENT_AREA", "ID")]

  #for each of these, there is a VALID_CASE for each invalid
  long_prior <- Report_Data[[assessment]][!(VALID_CASE == "INVALID_CASE" & YEAR %in% c("2019", "2021") &
                           ID %in% dup.ids$ID)][YEAR %in% c("2019", "2021")]

  #B. Get Prior Scale Scores and Achievement Levels
  long_2019 <- long_prior[YEAR == "2019", c("YEAR", "CONTENT_AREA", "ID",
                                            "VALID_CASE", "GRADE",
                                            "SCALE_SCORE", "ACHIEVEMENT_ProfandAbove",
                                            "ACHIEVEMENT_LEVEL")]
  setnames(long_2019, names(long_2019)[4:8], paste0("PRIOR_", names(long_2019)[4:8]))
  long_2019[, YEAR := NULL]

  long_2021 <- long_prior[YEAR == "2021"]
  long_2021 <- merge(x=long_2021, y=long_2019, all.x=TRUE, all.y=FALSE,
                     by=c("CONTENT_AREA", "ID"))

  long_2021[, NOT_TESTED := ifelse(is.na(SCALE_SCORE), 1, 0)]

  long_2019 <- Report_Data[[assessment]][YEAR=="2019",]
  long_prior <- rbindlist(list(long_2021, long_2019), fill=TRUE)

  ##C. Recode Subgroup info to make unique -------------------------------------
  #Otherwise tables choke later



#II. Particpation Exploration --------------------------------------------------
  #A. Examination based on Prior Scale Scores
  level.tab <- table(long_2021[CONTENT_AREA == "ELA", PRIOR_ACHIEVEMENT_LEVEL, NOT_TESTED])
  level.tab[1,] <- level.tab[1,]/sum(level.tab[1,])*100
  level.tab[2,] <- level.tab[2,]/sum(level.tab[2,])*100


  #B. Overall Table by Content  -- Table 1A.
  source(file.path(universal.content.path, "Learning_Loss_Analysis", "Functions", "func_fullTableState.R")) # only uses tmprms$demos$names

  overall_part <- data.frame(GRADE=NA, PRIOR_ACHIEVEMENT_LEVEL=NA)
  full_count <- data.frame()

  for (subj in tmprms$subjects) {
    tmp.part <- stateParticipationTable(long_data = long_prior,
                                        subject   = subj,
                                        params    = tmprms)
    tmp.part <- tmp.part[-(nrow(tmp.part)),]
    tmp.part$PRIOR_ACHIEVEMENT_LEVEL[which(is.na(tmp.part$PRIOR_ACHIEVEMENT_LEVEL))] <- "No Prior Level"
    p.table1 <- dcast(data=tmp.part, GRADE + PRIOR_ACHIEVEMENT_LEVEL ~ YEAR, value.var="PERCENT_TESTED") #+ SUBGROUP
    names(p.table1)[3:4] <- paste0(subj, ".", names(p.table1)[3:4])

    overall_part <- merge(x= overall_part, y=p.table1, all.y = TRUE, by=c("GRADE", "PRIOR_ACHIEVEMENT_LEVEL"))
    tmp.part$CONTENT_AREA <- subj
    full_count <- rbind.fill(full_count, tmp.part)
  }

  ##  Assuming this for 'p'?
  # p <- Report_Analyses[["participation"]][[assessment]][["full_table"]]
  # p[,NUMBER_NOT_TESTED := NUMBER_STUDENTS - NUMBER_TESTED]
  # p.subset <- p[LEVEL        == "State" &
  #               CONTENT_AREA == subject & # ? assume "ELA" or "MATHEMATICS"
  #               GRADE        == "All" &
  #               SUBGROUP     == "All" &
  #               ACHIEVEMENT_LEVEL_PRIOR == "All" &
  #               DISTRICT_NUMBER         == "All" &
  #               SCHOOL_NUMBER           == "All",]
  # p.table1 <- dcast(data=p.subset, GRADE + SUBGROUP ~ YEAR, value.var=value.var) # What's 'value.var' here?



#III. Appendix A ---------------------------------------------------------------
  #A. Cumlative Plots ----------------------------------------------------------
  p.school <- long_2021[,
                        list(LEVEL            = "School",
                             DISTRICT_NUMBER       = unique(DISTRICT_NUMBER),
                             GRADE                 = "All",
                             SUBGROUP              = "All",
                             NUMBER_STUDENTS       = length(SCALE_SCORE),
                             NUMBER_TESTED         = sum(!is.na(SCALE_SCORE)),
                             NUMBER_NONTESTED      = sum(is.na(SCALE_SCORE)),
                             NUMBER_AT_ABOVE_PROF  = sum(ACHIEVEMENT_ProfandAbove == "Proficient", na.rm=TRUE)),
                        #Add in demographics

                        by  = list(CONTENT_AREA, YEAR, SCHOOL_NUMBER)]
  p.school.missing <- p.school[NUMBER_NONTESTED != 0]

  p.school.missing <- p.school[p.school$NUMBER_NONTESTED != 0,]
  p.school.missing <- p.school.missing[order(CONTENT_AREA, -NUMBER_NONTESTED)]
  p.school.missing[,CUMLATIVE_NONTESTED := cumsum(NUMBER_NONTESTED), by=CONTENT_AREA]
  p.school.missing[,SCHOOL_DUMMY := 1:.N, by=CONTENT_AREA]

  # p.school.missing[CONTENT_AREA == "MATHEMATICS",]

  plot_directory <- "./assets/Rplots/participation"
  if (!dir.exists(plot_directory)) dir.create(plot_directory, recursive=TRUE)

  for (subj in tmprms$subjects) { # params$GL_subjects[[assessment]]
    png(file.path(plot_directory, paste0("School_cummulative_nonparticipation__", assessment, "_", subj, ".png")))
    par(mar=c(5.1,5.1,2.1,2.1)) # bottom, left, top and right
    plot(x=p.school.missing[CONTENT_AREA == subj, c("SCHOOL_DUMMY", "CUMLATIVE_NONTESTED")],
         xlab="School", ylab="Cumlative Number of\n Non-Participating Students", type="l")
    dev.off()
  }

  #B. School Level Relationship Plots

  ## Can't reporduce - not sure what 'ela_stack' is...

  # g <- ggplot(data = ela_stack, aes(percent, PERCENT_TESTED)) + geom_point()
  # g <- g + facet_wrap( ~ variable.name,  ncol = 3) +  labs( y = "Percent Partipcation", x = "Percent") +
  #   geom_smooth(method = "loess", aes(color="All Data")) +
  #   geom_smooth(data = ela_stack_sub, method = "loess", aes(color="0%>Percent<95%"))  +
  #   theme(legend.position="bottom")
  # g


  #B. Impute Scores ------------------------------------------------------------
  #B1 Create Look Up Table
  score.tab <- long_2021[,list(MIN_SCORE =min(SCALE_SCORE, na.rm=TRUE),
                               MAX_SCORE =max(SCALE_SCORE, na.rm=TRUE)),
                         by=list(CONTENT_AREA, GRADE)]
  setkey(score.tab, CONTENT_AREA, GRADE)
  #score.tab$MIN_LEVEL <- "Below Proficiency"
  #score.tab$MAX_LEVEL <- "Above Proficiency"

  #B2 Replace Scores
  long_2021[, SCALE_SCORE_altMIN := SCALE_SCORE]
  long_2021[, SCALE_SCORE_altMAX := SCALE_SCORE]
  long_2021[, ACHIEVEMENT_ProfandAbove_altMIN := ACHIEVEMENT_ProfandAbove]
  long_2021[, ACHIEVEMENT_ProfandAbove_altMAX := ACHIEVEMENT_ProfandAbove]

  long_2021[is.na(ACHIEVEMENT_LEVEL), ACHIEVEMENT_ProfandAbove_altMIN := "Not Proficient"]
  long_2021[is.na(ACHIEVEMENT_LEVEL), ACHIEVEMENT_ProfandAbove_altMAX := "Proficient"]

  for (i in tmprms$subjects) { # params$GL_subjects[[assessment]]
    for(j in tmprms$grade_values){
      #ELA
      long_2021[CONTENT_AREA == i & GRADE == j & is.na(SCALE_SCORE),
                  SCALE_SCORE_altMIN := score.tab[CONTENT_AREA == i & GRADE == j, MIN_SCORE]]
      long_2021[CONTENT_AREA == i & GRADE == j & is.na(SCALE_SCORE),
                  SCALE_SCORE_altMAX := score.tab[CONTENT_AREA == i & GRADE == j, MAX_SCORE]]
    }
  }

  #B3 School Summary  --  Table 3A.
  comp_table <- long_2021[, list(NUMBER_STUDENTS       = length(SCALE_SCORE),
                                 NUMBER_TESTED         = sum(!is.na(SCALE_SCORE)),
                                 NUMBER_NONTESTED      = sum(is.na(SCALE_SCORE)),
                                 MEAN_SCALE_SCORE         = sum(SCALE_SCORE, na.rm=TRUE)/sum(!is.na(SCALE_SCORE)),
                                 SD_SCALE_SCORE           = sd(SCALE_SCORE, na.rm=TRUE),
                                 MEAN_SCALE_SCORE_altMIN  = mean(SCALE_SCORE_altMIN),
                                 MEAN_SCALE_SCORE_altMAX  = mean(SCALE_SCORE_altMAX),

                                 PERCENT_PROFICENT = sum(ACHIEVEMENT_ProfandAbove == "Proficient", na.rm=TRUE)/sum(!is.na(ACHIEVEMENT_ProfandAbove))*100,
                                 PERCENT_PROFICENT_altMIN = sum(ACHIEVEMENT_ProfandAbove_altMIN == "Proficient", na.rm=TRUE)/length(ACHIEVEMENT_ProfandAbove)*100,
                                 PERCENT_PROFICENT_altMAX = sum(ACHIEVEMENT_ProfandAbove_altMAX == "Proficient", na.rm=TRUE)/length(ACHIEVEMENT_ProfandAbove)*100),
                            by  = list(CONTENT_AREA, GRADE)] #CONTENT_AREA, GRADE

  comp_table$MEAN_SCALE_SCORE_minDIF  <- comp_table$MEAN_SCALE_SCORE - comp_table$MEAN_SCALE_SCORE_altMIN
  comp_table$MEAN_SCALE_SCORE_maxDIF  <- comp_table$MEAN_SCALE_SCORE - comp_table$MEAN_SCALE_SCORE_altMAX
  comp_table$PERCENT_PROFICENT_minDIF <- comp_table$PERCENT_PROFICENT - comp_table$PERCENT_PROFICENT_altMIN #Lower, so this shoudl be positve, right?
  comp_table$PERCENT_PROFICENT_maxDIF <- comp_table$PERCENT_PROFICENT - comp_table$PERCENT_PROFICENT_altMAX

  comp_table <- comp_table[,c( "CONTENT_AREA", "GRADE",
                               "MEAN_SCALE_SCORE", "SD_SCALE_SCORE",
                               "MEAN_SCALE_SCORE_altMIN", "MEAN_SCALE_SCORE_minDIF",

                               "MEAN_SCALE_SCORE_altMAX", "MEAN_SCALE_SCORE_maxDIF",

                               "PERCENT_PROFICENT",
                               "PERCENT_PROFICENT_altMIN", "PERCENT_PROFICENT_minDIF",

                               "PERCENT_PROFICENT_altMAX", "PERCENT_PROFICENT_maxDIF"
                               )]

  #A2.D School Level
  #table(long_2021[GRADE == j & CONTENT_AREA == "MATHEMATICS" & is.na(ACHIEVEMENT_LEVEL),"ACHIEVEMENT_LEVEL_altMIN"])
  comp_table_schools <-
      long_2021[,list(NUMBER_STUDENTS       = length(SCALE_SCORE),
                      NUMBER_TESTED         = sum(!is.na(SCALE_SCORE)),
                      NUMBER_NONTESTED      = sum(is.na(SCALE_SCORE)),
                      MEAN_SCALE_SCORE         = sum(SCALE_SCORE, na.rm=TRUE)/sum(!is.na(SCALE_SCORE)),
                      SD_SCALE_SCORE           = sd(SCALE_SCORE, na.rm=TRUE),
                      MEAN_SCALE_SCORE_altMIN  = mean(SCALE_SCORE_altMIN),
                      MEAN_SCALE_SCORE_altMAX  = mean(SCALE_SCORE_altMAX),

                      PERCENT_PROFICENT = sum(ACHIEVEMENT_ProfandAbove == "Proficient", na.rm=TRUE)/sum(!is.na(ACHIEVEMENT_ProfandAbove))*100,
                      PERCENT_PROFICENT_altMIN = sum(ACHIEVEMENT_ProfandAbove_altMIN == "Proficient", na.rm=TRUE)/length(ACHIEVEMENT_ProfandAbove)*100,
                      PERCENT_PROFICENT_altMAX = sum(ACHIEVEMENT_ProfandAbove_altMAX == "Proficient", na.rm=TRUE)/length(ACHIEVEMENT_ProfandAbove)*100
  ),
  by  = list(CONTENT_AREA, SCHOOL_NUMBER, GRADE)] #CONTENT_AREA, GRADE

  comp_table_schools[, MEAN_SCALE_SCORE_minDIF := MEAN_SCALE_SCORE - MEAN_SCALE_SCORE_altMIN]
  comp_table_schools[, MEAN_SCALE_SCORE_maxDIF := MEAN_SCALE_SCORE - MEAN_SCALE_SCORE_altMAX]
  comp_table_schools[, PERCENT_PROFICENT_minDIF := PERCENT_PROFICENT - PERCENT_PROFICENT_altMIN] #Lower, so this shoudl be positve, right?
  comp_table_schools[, PERCENT_PROFICENT_maxDIF := PERCENT_PROFICENT - PERCENT_PROFICENT_altMAX]

  comp_table_schools <- comp_table_schools[NUMBER_STUDENTS >= tmprms$nsize_rule,]
  comp_table_schools <- comp_table_schools[NUMBER_NONTESTED != 0,]

  # Quantiles - Table 4A.
  out_tab <- NULL
  for(i in c("MEAN_SCALE_SCORE_minDIF", "MEAN_SCALE_SCORE_maxDIF", "PERCENT_PROFICENT_minDIF", "PERCENT_PROFICENT_maxDIF")){
    temp_tab <- comp_table_schools[,as.list(quantile(eval(parse(text = i)),
                                                     probs= seq(0.1, .9, .1), na.rm = TRUE)),
                                   by = list(CONTENT_AREA, GRADE)]
    temp_tab$variable <- i
    out_tab <- rbind(out_tab, temp_tab)
  }

Report_Analyses[["participation"]][[assessment]][["overall_participation_rates"]] <- as.data.table(overall_part) # 1A
Report_Analyses[["participation"]][[assessment]][["overall_participation_counts"]] <- as.data.table(full_count)
Report_Analyses[["participation"]][[assessment]][["state_minmax_diff_mean_pctp"]] <- comp_table # already data.table
Report_Analyses[["participation"]][[assessment]][["school_minmax_diff_quantiles"]] <- out_tab
