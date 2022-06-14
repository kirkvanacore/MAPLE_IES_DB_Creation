library(dplyr)
library(mice)

# Needed breakdowns: 
# student_assess
# student_fidelity
# student_demo
# student_roster
# Student_attendance
# 

 
# ## TODO Remove
# setwd("C:/Users/sidpr/Desktop/Work/Summer/DataProcessingMAPLE")


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
master_csv <- read.csv("DATA20220202_4092.csv", na.strings = c("#NULL!"))
master_csv <- colClean(master_csv)
colnames(master_csv)
#### Connecting to Database ####
# Creating a connection SQLite database
ies_research_con <- dbConnect(RSQLite::SQLite(), "ies_research schema/maple_ies_research.db")


### Divide Variables ####
# Defining the colnames to breakdown the large data with

##### student_demo ####
demographic <- c("StuID",
          "RACE",
          "Gender",
          "FEMALE",
          "MALE",
          "raceEthnicity",
          "raceEthnicityFed",
          "hispanicEthnicity",
          "IEP",
          "EIP",
          "EL_PARENT_DENIED",
          "ESOL_C",
          "ESOL_FORMER",
          "ESOL",
          "GIFTED")

##### student_roster ####
roster <- c("StuID",
            "SchIDPre",
            "TeaIDPre_within_school",
            "TeaIDPre",
            "ClaIDPre",
            "SecIDPre",
            "SchIDEnd",
            "TeaIDEnd_within_school",
            "TeaIDEnd",
            "ClaIDEnd",
            "SecIDEnd",
            "RANDOMIZED",
            "RESOURCE",
            "DROPSCH1",
            "DROPSCH2",
            "INPRETEST",
            "INPOSTTEST",
            "COND4343",
            "COND4092",
            "COND3972",
            "COND3591",
            "COND3249",
            "COND2849",
            "COND1849",
            "rdm_condition",
            "condition_assignment",
            "FH2T",
            "DragonBox",
            "Instant",
            "Delay",
            "inperson",
            "virtual",
            "courseName",
            "movement",
            "stay_INPERSON",
            "stay_VIRTUAL",
            "INP_VIR",
            "VIR_INP",
            "KEEP",
            "IST",
            "LEPM1",
            "LEPM2",
            "LEPM3",
            "LEPM4",
            "SECTION504",
            "SST",
            "G5Sch1ID",
            "G5Sch2ID",
            "G6Sch1ID",
            "G6Sch2ID"
           )
##### student_attendance ####
attendance <- c("StuID",
                "IN5",
                "AbsentDays5",
                "PresentDays5",
                "UnexcusedDays5",
                "MOBILE5",
                "MEMBERSHIPDAYS5",
                "ADM5",
                "ADA5",
                "Absencesavg_Daily5",
                "PercentInAttendance5",
                "IN6",
                "AbsentDays6",
                "PresentDays6",
                "UnexcusedDays6",
                "MOBILE6",
                "MEMBERSHIPDAYS6",
                "ADM6",
                "ADA6",
                "Absencesavg_Daily6",
                "PercentInAttendance6",
                "Membership_Days7",
                "Absent_Days7",
                "Present_Days7",
                "ADM7",
                "ADA",
                "Unexcused_Days7",
                "Absences_avg__Daily7",
                "Percent_In_Attendance7")

##### student_fidelity ####
fidelity <- c("StuID",
              "fidelity_started_sum",
              "fidelity_complete_sum",
              "fidelity_complete_percent",
              "started_intro_survey",
              "complete_intro_survey",
              "started_assignment_2",
              "complete_assignment_2",
              "started_assignment_3",
              "complete_assignment_3",
              "started_assignment_4",
              "complete_assignment_4",
              "started_assignment_5",
              "complete_assignment_5",
              "started_common_mid",
              "complete_common_mid",
              "started_assignment_7",
              "complete_assignment_7",
              "started_assignment_8",
              "complete_assignment_8",
              "started_assignment_9",
              "complete_assignment_9",
              "started_assignment_10",
              "complete_assignment_10",
              "started_assignment_11",
              "complete_assignment_11",
              "started_closing_survey",
              "complete_closing_survey",
              "started_delayed_posttest",
              "complete_delayed_posttest")

##### student_assess ####
assesment <- c("StuID",
               "Scale_Score5",
               "Performance_Level5",
               "Scale_Score7",
               "Performance_Level7",
               "pre_total_math_score",
               "pre_percentage_math_score",
               "pre_sub_P_score",
               "pre_sub_C_score",
               "pre_sub_F_score",
               "pre_math_completed_num",
               "pre_math_completed_percent",
               "pre_total_time_on_tasks",
               "pre_avg_time_on_tasks",
               "pre_MA_total_score",
               "pre_MA_avg_score",
               "pre_MSE_total_score",
               "pre_MSE_avg_score",
               "pre_PS_tasks_total_score",
               "pre_PS_part1_score",
               "pre_PS_part2E_score",
               "pre_PS_part2NE_score",
               "pre_PS_completed_num",
               "pre_PS_completed_percent",
               "pre_PS_total_RT_sec",
               "pre_PS_total_RT_min",
               "pre_PS_avg_RT_sec",
               "mid_total_math_score",
               "mid_percentage_math_score",
               "mid_sub_P_score",
               "mid_sub_C_score",
               "mid_sub_F_score",
               "mid_math_completed_num",
               "mid_math_completed_percent",
               "mid_total_time_on_tasks",
               "mid_avg_time_on_tasks",
               "mid_MA_total_score",
               "mid_MA_avg_score",
               "mid_negative_reaction_score",
               "mid_numerical_confindence_score",
               "mid_worry_score",
               "mid_PS_tasks_total_score",
               "mid_PS_part1_score",
               "mid_PS_part2E_score",
               "mid_PS_part2NE_score",
               "mid_PS_completed_num",
               "mid_PS_completed_percent",
               "mid_PS_total_RT_sec",
               "mid_PS_total_RT_min",
               "mid_PS_avg_RT_sec",
               "post_total_math_score",
               "post_percentage_math_score",
               "post_sub_P_score",
               "post_sub_C_score",
               "post_sub_F_score",
               "post_math_completed_num",
               "post_math_completed_percent",
               "post_total_time_on_tasks",
               "post_avg_time_on_tasks",
               "post_MA_total_score",
               "post_MA_avg_score",
               "post_MSE_total_score",
               "post_MSE_avg_score",
               "post_PS_tasks_total_score",
               "post_PS_part1_score",
               "post_PS_part2E_score",
               "post_PS_part2NE_score",
               "post_PS_completed_num",
               "post_PS_completed_percent",
               "post_PS_total_RT_sec",
               "post_PS_total_RT_min",
               "post_PS_avg_RT_sec",
               "delayed_total_math_score",
               "delayed_percentage_math_score",
               "delayed_sub_P_score",
               "delayed_sub_C_score",
               "delayed_sub_F_score",
               "delayed_math_completed_num",
               "delayed_math_completed_percent",
               "delayed_total_time_on_tasks",
               "delayed_avg_time_on_tasks",
               "delayed_PS_tasks_total_score",
               "delayed_PS_part1_score",
               "delayed_PS_part2E_score",
               "delayed_PS_part2NE_score",
               "delayed_PS_completed_num",
               "delayed_PS_completed_percent",
               "delayed_PS_total_RT_sec",
               "delayed_PS_total_RT_min",
               "delayed_PS_avg_RT_sec")
    
    

### Create Tables ####
assess_student <- dplyr::select(master_csv,assesment)
md.pattern(assess_student)

student_fidelity <- dplyr::select(master_csv,fidelity)
md.pattern(student_fidelity)

student_demo <- dplyr::select(master_csv,demographic)
md.pattern(student_demo)

student_roster <- dplyr::select(master_csv,roster)
md.pattern(student_roster)

student_attendance <- dplyr::select(master_csv,attendance)
md.pattern(student_attendance)



### Saving as CSV and uploading to DB ####

write.csv(assess_student,"ies_research schema/assess_student.csv")
overwrite_table(ies_research_con, "assess_student", assess_student)

write.csv(student_fidelity,"ies_research schema/student_fidelity.csv")
overwrite_table(ies_research_con, "student_fidelity", student_fidelity)

write.csv(student_demo,"ies_research schema/student_demo.csv")
overwrite_table(ies_research_con, "student_demo", student_demo)

write.csv(student_roster,"ies_research schema/student_roster.csv")
overwrite_table(ies_research_con, "student_roster", student_roster)

write.csv(student_attendance,"ies_research schema/student_attendance.csv")
overwrite_table(ies_research_con, "student_attendance", student_attendance)
