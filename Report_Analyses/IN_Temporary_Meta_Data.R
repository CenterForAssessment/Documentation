###  setup_data chunk

  ##    SOME MOVED TO 2_Report_Data.R

#C2. Parameter Information
tmprms <- list()
tmprms$years <- as.numeric(unique(Report_Data[[assessment]][, YEAR]))
tmprms$years <- sort(tmprms$years) #right now, using what is in the data
tmprms$grade_values <- c(3:8) #grade levels
tmprms$subjects <- unique(Report_Data[[assessment]][, CONTENT_AREA])
tmprms$nsize_rule <- 15
tmprms$achievement_Levels <- SGPstateData[["IN"]][["Achievement"]][["Levels"]][["Labels"]]


#A3. Demographics
tmprms$demos <- list(names=c("ETHNICITY",
                             "SOCIO_ECONOMIC_STATUS",
                             "ENGLISH_LANGUAGE_LEARNER_STATUS",
                             "SPECIAL_EDUCATION_STATUS",
                             "GENDER" # Renamed based on SGP package conventions
                             ))


#D. Data Labels ------------------------------------------------------------
tmprms$demos$values <- vector("list", length(tmprms$demos$names))
names(tmprms$demos$values) <- tmprms$demos$names

for(i in tmprms$demos$names){ #i <- tmprms$demos$names[2]
      #Get Labels
      tmprms$demos$values[[i]] <- as.character(unique(Report_Data[[assessment]][, eval(parse(text = i))]))
      tmprms$demos$values[[i]] <- tmprms$demos$values[[i]][!is.na(tmprms$demos$values[[i]])]

      #Reorder values to define the base case, i.e, define the appropriate contrasts
      #for regression later (note, this may break if data does not contain the right
      #codes for the subgroups)
      reindex.demos <- grep("Paid meals|General Education|Not|No|NO|white|Male|White", tmprms$demos$values[[i]])
      reindex.demos <- c(tmprms$demos$values[[i]][reindex.demos],
                        tmprms$demos$values[[i]][-reindex.demos])

      tmprms$demos$values[[i]] <- reindex.demos
}
