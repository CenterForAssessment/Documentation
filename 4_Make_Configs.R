#####
###   Set up the Learning Loss Analysis Report Configuration and Content Lists
###   Identify necessary meta-data and parameters required to run the report.
###   Create/customize/complete the required YAML and RMD file config lists
#####

###   Set up your R working directory
setwd("./Documentation")

###   Load required package(s)
require(Literasee)

###   Locate the "Universal_Content" directory
universal.content.path <- file.path("..", "..", "..", "Universal_Content")

###
###   Merge custom and universal config lists
###

##   Remove existing objects before (re)running
if (exists("report.config")) rm(report.config)
if (exists("rmd.files")) rm(rmd.files)

##   The "custom.config" list is created to supply unique client/state info.
##   It can also be used to override some of the Universal settings (authors, etc.)

custom.config <- list(
  client.info = list(
    state.name = "Colorado",
    state.abv = "CO",
    city.name = "Denver",
    organization = "Colorado Department of Education",
    org.head = "Commissioner Katy Anthes, Ph.D.",
    github.repo = "CenterForAssessment/SGP_Research/tree/master/Colorado/Learning_Loss_Analysis",
    acknowledgements = "the entire staff of the CDE Accountability Analytics Office, particularly Marie Huchton and Josh Perdue,"
  ),
  # Override defaults for author/Affil
  top.level = list(             # Title/subtitle, author.names, author.affil, date
    title = "Academic Impact in a Year of Disruptions",
    subtitle = "Student Achievement and Growth during the COVID-19 Pandemic",
    draft = TRUE  #  default if TRUE - "DRAFT REPORT -- DO NOT CITE OR CIRCULATE" #
  ),
  params = list(
    # draft.text = "ALTERNATE DRAFT TEXT",
    keyword   =  "academic impact", # should be lower case.  Camel applied as needed in params.Rmd or can be customized as keyword_camel
    min.size.school = 15,
    min.size.district = 50,
    sgp.abv = list( # SGP package abbreviation for accessing relevant SGPstateData meta-data.
      State_Assessment = "CO",
      College_Entrance = "CO",
      ELP_Assessment = "WIDA_CO"
    ),
    years = list(
      State_Assessment = as.character(c(2017:2019, 2021)),
      College_Entrance = as.character(c(2017:2019, 2021)),
      ELP_Assessment = as.character(2017:2021)
    ),
    GL_subjects = list(
      State_Assessment = c("ELA", "MATHEMATICS"),
      College_Entrance = c("ELA_PSAT_9", "ELA_PSAT_10", "ELA_SAT",
                           "MATHEMATICS_PSAT_9", "MATHEMATICS_PSAT_10", "MATHEMATICS_SAT"),
      ELP_Assessment = "READING"
    ),
    GL_text = list(
      State_Assessment = "ELA and mathematics",
      College_Entrance = "PSAT and SAT in ELA and Mathematics", # Colorado School Day PSAT and SAT
      ELP_Assessment = "Overall ELP"
    ),
    test.name = list(
      State_Assessment = "Colorado Measures of Academic Success",
      College_Entrance = "College Board SAT",
      ELP_Assessment = "WIDA ACCESS"
    ),
    test.abv = list(
      State_Assessment = "CMAS",
      College_Entrance = "PSAT/SAT",
      ELP_Assessment = "ACCESS"
    ),
    test.url = list(
      State_Assessment = "www.cde.state.co.us/assessment/cmas",
      College_Entrance = "www.cde.state.co.us/assessment/sat-psat",
      ELP_Assessment = "www.cde.state.co.us/assessment/ela"
    ),
    grades = list(
      State_Assessment = as.character(3:8),
      College_Entrance = as.character(9:11),
      ELP_Assessment = as.character(0:12)
    ),
    demographics = list(
      State_Assessment = c("ETHNICITY", "GENDER", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS", "GIFTED_TALENTED_PROGRAM_STATUS"),
      College_Entrance = c("ETHNICITY", "GENDER", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS", "GIFTED_TALENTED_PROGRAM_STATUS"),
      ELP_Assessment  =  c("ETHNICITY", "GENDER", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS", "GIFTED_TALENTED_PROGRAM_STATUS")
    ),
    gof.path = list(
      State_Assessment = file.path("..", "Goodness_of_Fit"),
      College_Entrance = file.path("..", "Goodness_of_Fit"),
      ELP_Assessment  =  file.path("..", "..", "ACCESS", "Goodness_of_Fit")
    ),
    sgp.max.order = list(
      State_Assessment = 2L,
      College_Entrance = c(2L),
      ELP_Assessment = 2L
    )
  )
)


##   The following script will merge the report.config (universal) and custom.config lists and return 'report.config' to be used in next steps
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Meta_Data", "Report_Configs.R"))

##   The following script will merge the rmd.files (universal) and custom.files lists and return 'rmd.files' to be used in next steps
# custom.files <- list(...) # override defaults if desired.  Otherwise a message that universal list will be used.
# source(file.path(universal.content.path, "Learning_Loss_Analysis", "Meta_Data", "Report_Content.R"))

##    Besides adding/reordering Rmd files though custom.files, one can request a
##    subset of files. This will result in a truncated report, allowing chapter/section
##    editing/development. You always need to include `setup.Rmd` and `params.Rmd`!

custom.files <- list(
  report = list(
    file.order = c("setup.Rmd", "params.Rmd", "0_Executive_Summary.Rmd")),
  appendices = c())

source(file.path(universal.content.path, "Learning_Loss_Analysis", "Meta_Data", "Report_Content.R"))


#####
###    Create the .yml and .Rmd "master/parent" documents for the `bookdown` site and `pagedown` report
#####

createReportScripts(report_config=report.config, rmd_file_list=rmd.files)

###   Save report YAML and file configurations
save(list=c("report.config", "rmd.files"), file = "CO_Report_Configuration_MetaData.rda")
setwd("..")
