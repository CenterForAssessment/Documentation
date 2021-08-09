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
    state.name = "Demonstration COVID", # required at a minimum
    state.abv = "D.C.", # for cover page, not SGPstateData
    city.name = "Washington",
    organization = "Demonstration Department of Education",
    org.head = "Joseph R. Biden, Jr.",
    github.repo = "CenterForAssessment/SGP_Research/tree/master/Demonstration/Learning_Loss_Analysis",
    acknowledgements = "the entire staff of the DDoE Assessment and Accountability Office, and particularly Maggie Q. Public,"
  ),
  # Override defaults for author/Affil
  top.level = list(  #  Title/subtitle, author.names, author.affil, date
    title = "Example Academic Impact Analysis",
    subtitle = "Student Achievement and Growth during the COVID-19 Pandemic",
    draft = TRUE  #  default if TRUE - "DRAFT REPORT -- DO NOT CITE OR CIRCULATE" #
  ),
  ##  `params` are passed to R and executed internally to create Universal/Customized
  ##  text or used to run further analyses on the Report_Data.  Many of these can
  ##  be created internally in the `params.Rmd` script. This list is semi-exhaustive
  ##  of what can be supplied to the .Rmd.
  params = list(
    # draft.text = "ALTERNATE DRAFT TEXT",
    keyword = "academic impact", # should be lower case.  Camel applied as needed in params.Rmd or can be customized as keyword_camel
    min.size.school = 15,  #  N size cutoff - exclude SCHOOLs with fewer than X students from summaries/analyses
    min.size.district = 50, # N size cutoff - exclude DISTRICTs with fewer than X students from summaries/analyses
    sgp.abv = list( # SGP package abbreviation for accessing relevant SGPstateData meta-data.
      State_Assessment = "DEMO_COVID",
      College_Entrance = c(),
      ELP_Assessment = c(),
      Interim_Assessment = c()
    ),
    years = list(
      State_Assessment = as.character(c(2017:2019, 2021)),
      College_Entrance = c(),
      ELP_Assessment = c(),
      Interim_Assessment = c()
    ),
    GL_subjects = list(
      State_Assessment = c("ELA", "MATHEMATICS"),
      College_Entrance = c(),
      ELP_Assessment = c(),
      Interim_Assessment = c()
    ),
    GL_text = list(
      State_Assessment = "ELA and mathematics",
      College_Entrance = c(),
      ELP_Assessment = c(),
      Interim_Assessment = c()
    ),
    test.name = list(
      State_Assessment = "Demonstration Student Assessment Program",
      College_Entrance = c(),
      ELP_Assessment = c(),
      Interim_Assessment = c()
    ),
    test.abv = list(
      State_Assessment = "DEMO_COVID",
      College_Entrance = c(),
      ELP_Assessment = c(),
      Interim_Assessment = c()
    ),
    test.url = list(
      State_Assessment = "https://centerforassessment.github.io/SGPdata/",
      College_Entrance = c(),
      ELP_Assessment = c(),
      Interim_Assessment = c()
    ),
    grades = list(
      State_Assessment = as.character(3:8),
      College_Entrance = c(),
      ELP_Assessment = c(),
      Interim_Assessment = c()
    ),
    demographics = list(
      State_Assessment = c("ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS", "GENDER"),
      College_Entrance = c(),
      ELP_Assessment  =  c(),
      Interim_Assessment = c()
    ),
    gof.path = list(
      State_Assessment = file.path("..", "Goodness_of_Fit"),
      College_Entrance = c(),
      ELP_Assessment  =  c(),
      Interim_Assessment = c()
    )
  )
)

##   The following script will merge the report.config (universal) and custom.config lists and return 'report.config' to be used in next steps
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Meta_Data", "Report_Configs.R"))

##   The following script will merge the rmd.files (universal) and custom.files lists and return 'rmd.files' to be used in next steps
# custom.files <- list(...) # override defaults if desired.  Otherwise a message that universal list will be used.
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Meta_Data", "Report_Content.R"))

##    Besides adding/reordering Rmd files though custom.files, one can request a
##    subset of files. This will result in a truncated report, allowing chapter/section
##    editing/development. You always need to include `setup.Rmd` and `params.Rmd`!

# custom.files <- list(
#   report = list(
#     file.order = c("setup.Rmd", "params.Rmd", "0_Executive_Summary.Rmd")),
#   appendices = c())
#
# source(file.path(universal.content.path, "Learning_Loss_Analysis", "Meta_Data", "Report_Content.R"))


#####
###    Create the .yml and .Rmd "master/parent" documents for the `bookdown` site and `pagedown` report
#####

createReportScripts(report_config=report.config, rmd_file_list=rmd.files)

###   Save report YAML and file configurations
save(list=c("report.config", "rmd.files"), file = "Report_Configuration_MetaData.rda")
setwd("..")
