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
    state.name = "Indiana", # required at a minimum
    state.abv = "IN", # for cover page, not SGPstateData
    city.name = "Indianapolis",
    organization = "Indiana Department of Education",
    org.head = "Dr. Katie Jenner, Secretary of Education",
    github.repo = "CenterForAssessment/SGP_Research/tree/master/Indiana/Learning_Loss_Analysis",
    acknowledgements = "the entire staff of Office of School Accountability, and particularly Maggie Paino, Brenda Erbse, Vanessa Deveau Bachle and Ron Sandlin,"
  ),
  # Override defaults for author/Affil
  top.level = list(  #  Title/subtitle, author.names, author.affil, date
    title = "Education in Indiana During a Year of Disruptions",
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
      State_Assessment = "IN",
      College_Entrance = c(),
      ELP_Assessment = "WIDA_IN",
      Interim_Assessment = c()
    ),
    years = list(
      State_Assessment = as.character(c(2016:2019, 2021)),
      College_Entrance = c(),
      ELP_Assessment = as.character(2017:2021),
      Interim_Assessment = c("2021.1", "2021.2")
    ),
    GL_subjects = list(
      State_Assessment = c("ELA", "MATHEMATICS"),
      College_Entrance = c(),
      ELP_Assessment = "READING",
      Interim_Assessment = c("ELA", "MATHEMATICS")
    ),
    GL_text = list(
      State_Assessment = "ELA and mathematics",
      College_Entrance = c(),
      ELP_Assessment = "Overall ELP",
      Interim_Assessment = "ELA and mathematics"
    ),
    test.name = list(
      State_Assessment = "Indiana Learning Evaluation and Readiness Network",
      College_Entrance = c(),
      ELP_Assessment = "WIDA ACCESS",
      Interim_Assessment = "NWEA Measures of Academic Progress"
    ),
    test.abv = list(
      State_Assessment = "ILEARN",
      College_Entrance = c(),
      ELP_Assessment = "WIDA",
      Interim_Assessment = "MAP Growth"
    ),
    test.url = list(
      State_Assessment = "https://www.in.gov/doe/",
      College_Entrance = c(),
      ELP_Assessment = "https://wida.wisc.edu/memberships/consortium/in",
      Interim_Assessment = "https://www.nwea.org/map-growth/"
    ),
    grades = list(
      State_Assessment = as.character(3:8),
      College_Entrance = c(),
      ELP_Assessment = as.character(0:12),
      Interim_Assessment = as.character(0:10)
    ),
    demographics = list(
      State_Assessment = c("SPECIAL_EDUCATION_STATUS", "ENGLISH_LANGUAGE_LEARNER_STATUS", "SOCIO_ECONOMIC_STATUS", "GENDER", "ETHNICITY"),
      College_Entrance = c(),
      ELP_Assessment  =  c(),
      Interim_Assessment = c("SPECIAL_EDUCATION_STATUS", "ENGLISH_LANGUAGE_LEARNER_STATUS", "SOCIO_ECONOMIC_STATUS", "GENDER", "ETHNICITY")
    ),
    gof.path = list(
      State_Assessment = file.path("..", "..", "..", "..", "Indiana", "Goodness_of_Fit"),
      College_Entrance = c(),
      ELP_Assessment  =  file.path("..", "..", "..", "..", "WIDA_IN", "Goodness_of_Fit"),
      Interim_Assessment = c()
    ),
    sgp.grades = list(
      Interim_Assessment = as.character(0:10) # Even Kindergarten has Fall to Winter growth
    ),
    sgp.max.order = list(
      State_Assessment = 2L,
      College_Entrance = c(),
      ELP_Assessment = 2L,
      Interim_Assessment = 1L
    )
  )
)

##   The following script will merge the report.config (universal) and custom.config lists and return 'report.config' to be used in next steps
source(file.path(universal.content.path, "Learning_Loss_Analysis", "Meta_Data", "Report_Configs.R"))

##   The following script will merge the rmd.files (universal) and custom.files lists and return 'rmd.files' to be used in next steps
# custom.files <- list(...) # override defaults if desired.  Otherwise a message that universal list will be used.

custom.files <- list(
  report = list(
    file.order = c(
      "setup.Rmd",
      "params.Rmd",
      "0_Executive_Summary.Rmd"#, # implies 0_Executive_Summary_Text.Rmd
      # "1_Intro_Background.Rmd",
      # "1_Intro_Legislative_Charge.Rmd",
      # "1_Intro_Research_Questions.Rmd",
      # "1_Intro_Data_Sources.Rmd",
      # "1_Intro_Methods.Rmd",
      # "2_Participate__Overview.Rmd",
      # "2_Participate_Enrolled_Students.Rmd"#,
      # "2_Participate_Counts.Rmd",
      # "2_Participate_Mode_of_Instruction.Rmd",
      # "2_Participate_Attendance.Rmd",
      # "2_Participate_School_Closures.Rmd",
      # "3_Impact__Overview.Rmd",
      # "3_Impact_Achievement_Analysis.Rmd",
      # "3_Impact_Achievement_Overview.Rmd",
      # "3_Impact_Growth_Analysis.Rmd",
      # "3_Impact_Growth_Overview.Rmd",
      # "3_Impact_Synthesis.Rmd",
      # "4_Discussion__Overview.Rmd",
      # "9_Summary.Rmd"
    ),
    references = NULL),
  appendices = list(
    # A = list(
    #   title = "Participation Analyses",
    #   file.order = c(
    #     "setup_participation_appendix.Rmd",   #  Should be appendix specific (counter override, etc.)
    #     "params.Rmd",  #  Could be appendix specific - params_appendix_a.Rmd
    #     "Appendix_Participation_Intro.Rmd",
    #     "Appendix_Participation_by_School.Rmd",
    #     "Appendix_Participation_MinMax_Replace.Rmd"
    #   ),
    #   references = NULL
    # )
  )
)

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
