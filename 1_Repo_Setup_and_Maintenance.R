#####
###   Set up the Demonstration Learning Loss Analysis Report directory
###   Install and update `R` packages required to run analyses and create report.
###   Copy Literasee package assets, custom content templates, and other scripts.
#####

###   Set R working directory to the Documentation folder
setwd("./Documentation")

###   Locate the "Universal_Content" directory
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

###   Install/update packages used in the Learning Loss Report
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Meta_Data", "Report_Packages.R"))


###   Set up new report directory
###   An initial setup will include copying the Literasee package assets and
###   any custom RMD templates (from "Universal_Content" or another, similar, state)

###   Load packages required for report setup
# require(Literasee)
#
# template.path <- file.path(universal.content.path, "Learning_Loss_Analysis", "Child_RMD", "Template_Custom_Content")
# setupReportDirectory(custom.content.path = template.path)

###   It may be necessary to occasionally update Literasee package assets.
# updateAssets()

###   Alternatively, one can update custom content and/or Literasee assets
# setupReportDirectory(new.report = FALSE, update.assets = TRUE, custom.content.path = template.path, overwrite.custom=FALSE)

setwd("..")
