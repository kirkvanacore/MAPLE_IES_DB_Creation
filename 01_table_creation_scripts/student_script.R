library(dplyr)
library(mice)
library(RSQLite)
library(DBI)

#### Create Functions ####
# get rid of "." because it messess with the queries
colClean <- function(x){   colnames(x) = gsub("\\.", "_", colnames(x)); x } 


### Create Functions
TESTING <- FALSE
# Simple function that takes in database connection, table name and new data to over ride
overwrite_table <- function(connection, table_name, data) {
    if (!TESTING){
        RSQLite::dbWriteTable(connection, table_name, data, overwrite = T)
        print(paste0("Created Table: ", table_name))
    } else {
        print("In Testing Mode: Nothing added to the database")
    }
    
}

#### Load Data ####
# master csv file to breakdown
master_csv <- read.csv("02_data_source_files/DATA20220202_4092.csv", na.strings = c("#NULL!"))
master_csv <- colClean(master_csv)

colnames(master_csv)
colnames(df)

# load demo data
df<- read.csv('02_data_source_files/roster_demographic_2022_09_20_N=4,343 - Sheet1.csv', na.strings = c("#NULL!"))
df <- colClean(df)

#### Connecting to Database ####
# Creating a connection SQLite database
ies_research_con <- dbConnect(RSQLite::SQLite(), "ies_research schema/maple_ies_research.db")


### Divide Variables ####
# Defining the colnames to breakdown the large data with

##### student_demo ####
student_demo <- df %>%
  dplyr::select(
    StuID,
    Gender,
    FEMALE,
    MALE,
    race_ethnicity,
    Hispanic,
    IEP,
    EIP,
    EL_PARENT_DENIED,
    ESOL_C,
    ESOL_FORMER,
    ESOL,
    GIFTED
  )
md.pattern(student_demo)

##### student_roster ####


student_roster <- master_csv %>% dplyr::select(
  StuID,
  SchIDPre,
  TeaIDPre_within_school,
  TeaIDPre,
  ClaIDPre,
  SecIDPre,
  SchIDEnd,
  TeaIDEnd_within_school,
  TeaIDEnd,
  ClaIDEnd,
  SecIDEnd,
  RANDOMIZED,
  RESOURCE,
  DROPSCH1,
  DROPSCH2,
  INPRETEST,
  INPOSTTEST,
  COND4343,
  COND4092,
  COND3972,
  COND3591,
  COND3249,
  COND2849,
  COND1849,
  rdm_condition,
  condition_assignment,
  FH2T,
  DragonBox,
  Instant,
  Delay,
  inperson,
  virtual,
  courseName,
  movement,
  stay_INPERSON,
  stay_VIRTUAL,
  INP_VIR,
  VIR_INP,
  KEEP,
  IST,
  LEPM1,
  LEPM2,
  LEPM3,
  LEPM4,
  SECTION504,
  SST,
  G5Sch1ID,
  G5Sch2ID,
  G6Sch1ID,
  G6Sch2ID
)

md.pattern(student_roster)

##### student_attendance ####
student_attendance <- df %>%
  dplyr::select(StuID,
                IN5,
                AbsentDays5,
                PresentDays5,
                UnexcusedDays5,
                MOBILE5,
                MEMBERSHIPDAYS5,
                ADM5,
                ADA5,
                Absencesavg_Daily5,
                PercentInAttendance5,
                IN6,
                AbsentDays6,
                PresentDays6,
                UnexcusedDays6,
                MOBILE6,
                MEMBERSHIPDAYS6,
                ADM6,
                ADA6,
                Absencesavg_Daily6,
                PercentInAttendance6,
                MEMBERSHIPDAYS7,
                AbsentDays7 = ,
                PresentDays7,
                ADM7,
                ADA7,
                UnexcusedDays7,
                Absencesavg_Daily7,
                PercentInAttendance7)

##### student_fidelity ####
student_fidelity <- master_csv %>%
  dplyr::select(StuID,
              fidelity_started_sum,
              fidelity_complete_sum,
              fidelity_complete_percent,
              started_intro_survey,
              complete_intro_survey,
              started_assignment_2,
              complete_assignment_2,
              started_assignment_3,
              complete_assignment_3,
              started_assignment_4,
              complete_assignment_4,
              started_assignment_5,
              complete_assignment_5,
              started_common_mid,
              complete_common_mid,
              started_assignment_7,
              complete_assignment_7,
              started_assignment_8,
              complete_assignment_8,
              started_assignment_9,
              complete_assignment_9,
              started_assignment_10,
              complete_assignment_10,
              started_assignment_11,
              complete_assignment_11,
              started_closing_survey,
              complete_closing_survey,
              started_delayed_posttest,
              complete_delayed_posttest)

# ### Create Tables ####
# assess_student <- dplyr::select(master_csv,assesment)
# md.pattern(assess_student)
# 
# student_fidelity <- dplyr::select(master_csv,fidelity)
# md.pattern(student_fidelity)
# 
# 
# student_roster <- dplyr::select(master_csv,roster)
# md.pattern(student_roster)
# 
# student_attendance <- dplyr::select(master_csv,attendance)
# md.pattern(student_attendance)



### Saving as CSV and uploading to DB ####

write.csv(student_fidelity,"ies_research schema/student_fidelity.csv")
overwrite_table(ies_research_con, "student_fidelity", student_fidelity)

write.csv(student_demo,"ies_research schema/student_demo.csv")
overwrite_table(ies_research_con, "student_demo", student_demo)

write.csv(student_roster,"ies_research schema/student_roster.csv")
overwrite_table(ies_research_con, "student_roster", student_roster)

write.csv(student_attendance,"ies_research schema/student_attendance.csv")
overwrite_table(ies_research_con, "student_attendance", student_attendance)
