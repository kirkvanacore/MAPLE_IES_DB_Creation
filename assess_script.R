#### Uploading Assessment/student Meta Data Tables ####

####Installing & Loading Packages###
#create list of packagess
packages = c(
  "tidyverse",
  "plyr",
  "ggExtra",
  "xts",
  "lubridate",
  "readxl",
  "data.table",
  "RSQLite",
  "DBI"
)
#load install
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
) 
rm(package.check, packages)

ifnull <- function(x,y) ifelse(is.na(x), y, x)
options(scipen = 100)


### Connect to SQLite DB ####
ies_research_con <- dbConnect(RSQLite::SQLite(), "ies_research schema/maple_ies_research.db")


## load assessment data
assess <- read.csv("DATA20220202_4092 copy.csv")
colnames(assess)


### Save Crosswalk as csv
write.csv(assess, "ies_research schema/assess.csv")

### Save crosswalk in maple_ies_research db

RSQLite::dbWriteTable(ies_research_con, "assess", assess)

#  Mabe in the fure I will clean this data set up a bit more, but for now i'm just saving this as an assess file
# ### Create Student meta data
# sm <- assess %>%
#   select(StuID,
#          KEEP,
#          Gender,
#          FEMALE,
#          MALE,
#          RACE,
#          raceEthnicity,
#          raceEthnicityFed,
#          hispanicEthnicity,
#          EIP,
#          EL_PARENT_DENIED,
#          ESOL_C,
#          ESOL_FORMER,
#          ESOL,
#          GIFTED,
#          IEP,
#          IST,
#          LEPM1,
#          LEPM2,
#          LEPM3,
#          LEPM4,
#          SECTION504,
#          SST,
#          IN5,
#          RESOURCE,
#          RANDOMIZED,
#          rdm_condition,
#          condition_assignment,
#          FH2T,
#          DragonBox,
#          Instant,
#          Delay,
#          SchIDPre,
#          TeaIDPre_within_school,
#          TeaIDPre,
#          ClaIDPre,
#          SecIDPre,
#          SchIDEnd,
#          TeaIDEnd_within_school,
#          TeaIDEnd,
#          ClaIDEnd,
#          SecIDEnd,
#          inperson,
#          virtual,
#          courseName,
#          movement,
#          stay_INPERSON,
#          stay_VIRTUAL,
#          INP_VIR,
#          VIR_INP,
#          DROPSCH1,
#          DROPSCH2,
#          INPRETEST,
#          INPOSTTEST,
#          COND4343,
#          COND4092,
#          COND3972,
#          COND3591,
#          COND3249,
#          COND2849,
#          COND1849,
#          G5Sch1ID,
#          G5Sch2ID,
#          G6Sch1ID,
#          G6Sch2ID)
# 
# 
# sa_wide <- assess %>%
#   select(StuID,
#          AbsentDays5,
#          PresentDays5,
#          UnexcusedDays5,
#          MOBILE5,
#          MEMBERSHIPDAYS5,
#          ADM5,
#          ADA5,
#          Absencesavg.Daily5,
#          PercentInAttendance5,
#          IN6,
#          AbsentDays6,
#          PresentDays6,
#          UnexcusedDays6,
#          MOBILE6,
#          MEMBERSHIPDAYS6,
#          ADM6,
#          ADA6,
#          Absencesavg.Daily6,
#          PercentInAttendance6,
#          Membership.Days7,
#          Absent.Days7,
#          Present.Days7,
#          ADM7,
#          ADA,
#          Unexcused.Days7,
#          Absences.avg..Daily7,
#          Percent.In.Attendance7)
# 
# sa_long <- reshape(sa_wide, 
#                    direction = "long",
#                    varying = 
#                      list(c(),
#                           )
#                    id.vars = c("AbsentDays5", "AbsentDays6", "Absent.Days7"),
#                    c("PresentDays5", "PresentDays6", "Present.Days7"),
#                    c("UnexcusedDays5", "UnexcusedDays6", "Unexcused.Days7"),
#                    c("MOBILE5", "MOBILE6"),
#                    c("MEMBERSHIPDAYS5", "MEMBERSHIPDAYS6", "Membership.Days7"),
#                    c("ADM5", "ADM6", "ADM7")
#                    c("ADA5", "ADA6", "ADA")
#                    c("Absencesavg.Daily5", "Absencesavg.Daily6",  "Absencesavg.Daily7"),
#                    c("PercentInAttendance5", "PercentInAttendance6", "PercentInAttendance7"),
#                    v.names= c())


