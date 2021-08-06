#####
###   Produce Indiana Learning Loss Analysis Report using `bookdown` and `pagedreport`
#####

###   Set up your R working directory
setwd("./Documentation")

###   Load formated Report_Data
if (!exists("Report_Data")) load("../Data/Report_Data.Rdata")
if (!exists("Report_Analyses")) load("../Data/Report_Analyses.Rdata")

###   Load required packages
require(Literasee)
# require(SGP)


#####
###    Render the report using `bookdown`
#####

bookdown::render_book(".", "bookdown::gitbook")

#  Serve the site directory on a local host to see the results:
servr::httw(dir = "site", watch = "site", port=4224)
# servr::daemon_stop()


#####
###    Render the report using `pagedown`
#####

rmarkdown::render(file.path("report", "Indiana_Academic_Impact_Analysis.Rmd"))
pagedown::chrome_print(file.path("report", "Indiana_Academic_Impact_Analysis.html"), wait=10, timeout=60)

###   Needs updating/revision
# rmarkdown::render(file.path("report", "Participation_Analyses_APPENDIX_A.Rmd"))
# unlink(file.path("report", "_bookdown.yml")) #  Need to remove - seems to mess up subsequent attempts to re-render the `bookdown` site ...
# pagedown::chrome_print(file.path("report", "Participation_Analyses_APPENDIX_A.html"), wait=10, timeout=60) # , options = list(pageRanges='1-10') # last (blank) page issue fixed in dev pagedown varsion

###  TBD
# rmarkdown::render(file.path("report", "Goodness_of_Fit_Plots_APPENDIX_A.Rmd"))
# unlink(file.path("report", "_bookdown.yml")) #  Need to remove - seems to mess up subsequent attempts to re-render the `bookdown` site ...
# pagedown::chrome_print(file.path("report", "Goodness_of_Fit_Plots_APPENDIX_A.html"), wait=10, timeout=60) # , options = list(pageRanges='1-10') # last (blank) page issue fixed in dev pagedown varsion

###  Copy report to the bookdown site for download links
if (!file.exists(file.path("site", "downloads"))) dir.create(file.path("site", "downloads"))
file.copy(file.path("report", "Indiana_Academic_Impact_Analysis.pdf"), file.path("site", "downloads"), overwrite = TRUE)
file.copy(file.path("report", "Participation_Analyses_APPENDIX_A.pdf"), file.path("site", "downloads"), overwrite = TRUE)
# file.copy(file.path("report", "Goodness_of_Fit_Plots_APPENDIX_A.pdf"), file.path("site", "downloads"), overwrite = TRUE)

setwd("..")
