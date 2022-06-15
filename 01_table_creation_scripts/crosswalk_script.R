#### ADDING TO CROSSWALK ####

####Installing & Loading Packages###
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

### Load 
cross <- read.csv("02_data_source_files/Crosswalk_CSVs_20220201/Students-Table 1.csv", na.strings = c("", " "))
colnames(cross)

### add condition using the assess file 
assess <- read.csv("02_data_source_files/DATA20220202_4092.csv")
colnames(assess)

cross <- cross %>%
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

cross <- cross %>%
  left_join(FH2T %>%
              select(
                student_number,
                userID,
                
              ),
            by = c("STUDID"="student_number")
            )



table(is.na(cross$userID))
table(is.na(FH2T$userID))
# there are 501 students who were assigned to the FH2T Condition, who did not have any activity in FH2T

### ADDING ASSISTments 
# need to get the student ids from this data
old_logs <- read.csv("04_data_archive/assistments_ies_dataset_2021_ 0322_fromAnthony.csv")
colnames(old_logs)
length(unique(old_logs$student_id))
length(unique(old_logs$assistments_user_id))


cross<-cross %>%
  left_join(old_logs %>%
              select(student_id,
                     assistments_user_id) %>%
            distinct(),
            by = c("student_id"))

table(is.na(cross$assistments_user_id))

# final cleaning before save

cross <- cross %>%
  select(StuID,
         STUDID,
         student_id,
         assistments_user_id,
         userID,
         condition_assignment,
         rdm_condition) %>%
  dplyr::rename(fh2t_user_id = userID)
  

### Save Crosswalk as csv
write.csv(cross, "ies_research schema/student_id_crosswalk.csv")

### Save crosswalk in maple_ies_research db

RSQLite::dbWriteTable(ies_research_con, "student_id_crosswalk", cross, overwrite = T)

