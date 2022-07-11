
#### CROSSWALK TABLE CREATION #####
# these crosswalks were created by Craig and then motfied to add more student level IDs


#### Installing & Loading Packages ####
#create list of packages
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


### Crosswalk_Student ####

### Load 
student <- read.csv("02_data_source_files/Crosswalk_CSVs_20220201/Students-Table 1.csv", na.strings = c("", " "))
colnames(student)

### add condition using the assess file 
assess <- read.csv("02_data_source_files/roster_demographic_2022_06_16_N=4,343.csv", na.strings = c("", " "))
colnames(assess)

student <- student %>%
  select(
    STUDID, 
    student_id, 
    StuID) %>%
    left_join(assess%>%
          select(StuID,
                 rdm_condition,
                 condition_assignment ),
          by = "StuID")

### NEED TO ADD 
  # User_id from FH2T
  # assistments_user_id



#### ADDING FH2T User_id
FH2T<-read.csv("04_data_archive/OLD_FH2T_aggregation_overall_merge_2021_03_25.csv") %>%
  select(
    student_number,
    student_id,
    userID,
    o_num_visit
  )
colnames(FH2T)

student <- student %>%
  left_join(FH2T %>%
              select(
                student_number,
                userID,
                
              ),
            by = c("STUDID"="student_number")
            )



table(is.na(student$userID))
table(is.na(FH2T$userID))
# there are 501 students who were assigned to the FH2T Condition, who did not have any activity in FH2T

### ADDING ASSISTments 
# need to get the student ids from this data
old_logs <- read.csv("04_data_archive/assistments_ies_dataset_2021_ 0322_fromAnthony.csv")
colnames(old_logs)
length(unique(old_logs$student_id))
length(unique(old_logs$assistments_user_id))


student<-student %>%
  left_join(old_logs %>%
              select(student_id,
                     assistments_user_id) %>%
            distinct(),
            by = c("student_id"))
length(unique(old_logs$student_id))
table(is.na(student$assistments_user_id)) # all IDS transferred

# final cleaning before save

student <- student %>%
  select(StuID,
         STUDID,
         student_id,
         assistments_user_id,
         userID,
         condition_assignment,
         rdm_condition) %>%
  dplyr::rename(fh2t_user_id = userID) 
  

### Save Crosswalk as csv
write.csv(cross, "ies_research schema/crosswalk_student.csv")

### Save crosswalk in maple_ies_research db

DBI::dbWriteTable(ies_research_con, "crosswalk_student", student, overwrite = T)


### Crosswalk_teacher ####

### Load 
teacher <- read.csv("02_data_source_files/Crosswalk_CSVs_20220201/Teachers-Table 1.csv", na.strings = c("", " ")) %>%
  select(TeaID, Original_Teacher, TeaID_within_school)
colnames(teacher)
sapply(teacher, anyNA)


### Save Crosswalk as csv
write.csv(teacher, "ies_research schema/crosswalk_teacher.csv")

### Save crosswalk in maple_ies_research db

DBI::dbWriteTable(ies_research_con, "crosswalk_teacher", teacher, overwrite = T)


### Crosswalk_school ####

### Load 
school <- read.csv("02_data_source_files/Crosswalk_CSVs_20220201/Schools-Table 1.csv", na.strings = c("", " ")) %>%
  select(SchID, Original_School )
colnames(school)
sapply(school, anyNA)


### Save Crosswalk as csv
write.csv(teacher, "ies_research schema/crosswalk_school.csv")

### Save crosswalk in maple_ies_research db

DBI::dbWriteTable(ies_research_con, "crosswalk_school", school, overwrite = T)


### Crosswalk_school ####

### Load 
school <- read.csv("02_data_source_files/Crosswalk_CSVs_20220201/Schools-Table 1.csv", na.strings = c("", " ")) %>%
  select(SchID, Original_School )
colnames(school)
sapply(school, anyNA)


### Save Crosswalk as csv
write.csv(teacher, "ies_research schema/crosswalk_school.csv")

### Save crosswalk in maple_ies_research db

DBI::dbWriteTable(ies_research_con, "crosswalk_school", school, overwrite = T)


### Crosswalk_class ####

### Load 
class <- read.csv("02_data_source_files/Crosswalk_CSVs_20220201/Classes-Table 1.csv", na.strings = c("", " ")) %>%
  select(ClaID, Original_Class )
colnames(class)
sapply(class, anyNA)


### Save Crosswalk as csv
write.csv(teacher, "ies_research schema/crosswalk_class.csv")

### Save crosswalk in maple_ies_research db

DBI::dbWriteTable(ies_research_con, "crosswalk_class", class, overwrite = T)


### Crosswalk_class ####

### Load 
class <- read.csv("02_data_source_files/Crosswalk_CSVs_20220201/Classes-Table 1.csv", na.strings = c("", " ")) %>%
  select(ClaID, Original_Class )
colnames(class)
sapply(class, anyNA)


### Save Crosswalk as csv
write.csv(teacher, "ies_research schema/crosswalk_class.csv")

### Save crosswalk in maple_ies_research db

DBI::dbWriteTable(ies_research_con, "crosswalk_class", class, overwrite = T)


### Crosswalk_section ####

### Load 
section <- read.csv("02_data_source_files/Crosswalk_CSVs_20220201/Sections-Table 1.csv", na.strings = c("", " "))%>%
  select(SecID, Original_Section )
colnames(section)
sapply(section, anyNA)


### Save Crosswalk as csv
write.csv(teacher, "ies_research schema/crosswalk_section.csv")

### Save crosswalk in maple_ies_research db

DBI::dbWriteTable(ies_research_con, "crosswalk_section", section, overwrite = T)






