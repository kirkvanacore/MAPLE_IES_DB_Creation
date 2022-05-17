#### ASSISTment Table Creation ####
### based on Kirk's Script ###

# This script takes the raw ASSISTment log file data along with the ASSISTment problem meta data,
# organizes and aggregates it into into tables for the maple_ies_research SQLite database.
# Final Outputs are both tables and csvs for:
    #assist_problems_meta
    #assist_student_action_logs
    #assist_student_problem_attempt
    #assist_student_problem
    #assist_raw_logs


TESTING <- FALSE


## TODO Remove
setwd("C:/Users/sidpr/Desktop/Work/Summer/DataProcessingMAPLE")

####Installing & Loading Packages###
## Kirk's code ###
#create list of packages
packages <- c(
  "tidyverse",
  "dplyr",
  "ggExtra",
  "xts",
  "lubridate",
  "readxl",
  "data.table",
  "RSQLite",
  "DBI",
  "mice"
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
rm(package.check, packages) # cleaning environment

# Simple function that takes in database connection, table name and new data to over ride
overwrite_table <- function(connection, table_name, data) {
    if (!TESTING){
        RSQLite::dbWriteTable(connection, table_name, data, overwrite = T)
        print(paste0("Created Table: ", table_name))
    } else {
        print("In Testing Mode: Nothing added to the database")
    }
    
}

# Creating a connection SQLite database
con <- RSQLite::dbConnect(RSQLite::SQLite(), "ies_research_schema/maple_ies_research.db")
# Lists all the tables in the database
RSQLite::dbListTables(con)

### Loading ASSISTment raw logs csv file ###
raw_log <- read.csv("csvData/IES_action_logs_with_hints.csv")

### CREATING a table for assist_raw_logs ###
overwrite_table(con, "assist_raw_logs", raw_log)

### Loading Master Cross-Walk file ###
master_cross <- read.csv("csvData/MASTER_crosswalk.csv")

### Loading ASSISTment Cross-Walk file ###
assist_cross <- read.csv("csvData/MAPLE_IES_STUDY_students_crosswalk.csv")


## Merging ASSISTment cross-walk onto the Master cross-walk ##
## This is lets us link raw log with "condition_assignment" and "StuID" ##
## Performing left join onto master_cross (ensures all data from master_cross is preserved) ##
gen_cross <- dplyr::inner_join(master_cross, assist_cross, by = "student_id")
## Dropping unneeded columns ##
gen_cross <- dplyr::select(gen_cross, -c(STUDID, fh2t_user_id, X.y, X.x, school_id, teacher, student_id))

# Cleaning environment
rm(assist_cross,master_cross)

## Merging the generated cross-walk onto raw log data (assist_raw_logs) ##
## Performing a inner-join onto raw log, on assistments_user_id (in crosswalk) and user_id (in log file)
action_logs <- dplyr::inner_join(raw_log, gen_cross, by = c("user_id" = "assistments_user_id"))

# Testing to make sure all the content of crosswalk have matched onto raw_log #
# If the test fails, the program will exit #
good_merge_flag <- all(gen_cross$assistments_user_id %in% raw_log$user_id)

if (!good_merge_flag){
    print("ERROR (merging crosswalk and raw_log): NOT all assistment_user_ids from the crosswalk is present in the raw log file")
    print("Quiting due to error")
    q()
}

print("MERGE (merging crosswalk and raw_log): All assistment_user_ids from the crosswalk is present in the raw log file")

## Removing rows in which action_name is numerical ##
## Assuming that numerical action_name is a system_action (ie made by server), not student_action ##

action_logs <- subset(action_logs, is.na(as.numeric(action_logs$action_name)))

## Removing unneeded columns from action_logs ##
## Removing according to the list of needed column names in Data_dictionary ##

action_logs <- dplyr::select(action_logs, -c(rowid, ordering, action_ar, 
                                            action_name, work_span, offset_seconds, offset_start, 
                                            offset_last, milli_time, uid, user_id, original))

## Cleaning action_time column of action_logs ##
## Some dates appear to be set pre-2000, replacing those values with NA ##


action_logs$action_time[as_datetime(action_logs$action_time) < as_datetime("2000-01-01 00:00:00")] <- NA

### CREATING a table for assist_student_action_logs ###
overwrite_table(con, "assist_student_action_logs", action_logs)

    
### Finally Disconnect at the end ###
print("Disconnecting from Database")
RSQLite::dbDisconnect(con)