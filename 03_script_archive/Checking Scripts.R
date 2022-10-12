
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
  "DBI",
  "mice",
  "lme4"
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

#### CHECK FROM HERE TO THERE DATA ####
# This script checks perviously aggigated FH2T data agains new data

### Connect to SQLite DB ####
ies_research_con <- dbConnect(RSQLite::SQLite(), "ies_research schema/maple_ies_research.db")


fh2t_CHECK <- read.csv("FH2T_Check_Problem_data.csv")
colnames(fh2t_CHECK)

table(fh2t_CHECK$problem)
fh2t_CHECK$problem_id <- as.integer(gsub("p", "", fh2t_CHECK$problem))
table(fh2t_CHECK$problem_id)

student_problemn  <- dbGetQuery(ies_research_con, 
                          "Select cw.student_id,
                                sp.*     
                          from fh2t_student_problem sp
                          left join student_id_crosswalk cw on cw.StuID = sp.StuID ")
# the fh2t_CHECK data was perviously created from a wide file of aggrigated problem level fh2t data
# the fh2t_CHECK data set has one row per possible problem whereas the student_problem data has one row per problem attempted
# to make these dat sets comperbale I am dropiing all problems were students did
colnames(student_problemn)
table(is.na(fh2t_CHECK$clover_first))
table((fh2t_CHECK$clover_first))

table(is.na(student_problemn$first_clovers))
table((student_problemn$first_clovers))

CHECK <- student_problemn %>% 
  select(student_id,
         StuID,
         problem_id,
         total_errors,
         first_num_errors,
         first_clovers,
         final_clovers,
         first_num_resets,
         first_num_errors,
         first_num_steps,
         first_errors_within_replay_attempt,
         total_replay,
         start_time,
         end_time) %>%
  left_join(fh2t_CHECK
            %>%
              select(problem_id,
                     student_id,
                     start_state,
                     goal_state,
                     problem_best_step,
                     clover_first,
                     clover_last,
                     user_first_step,
                     first_efficiency,
                     first_error,
                     total_error,
                     completed,
                     goback),
            by = c("student_id", "problem_id"))

table(CHECK$completed, CHECK$clover_first)
table(CHECK$completed, CHECK$first_clovers)
table(CHECK$completed, is.na(CHECK$first_clovers)) #there 1633 instances where the new data has clovers, but the old data says they didn't comeple the problem
table(CHECK[CHECK$first_multiple_visits_within_attempt == 0, ]$completed, 
      is.na(CHECK[CHECK$first_multiple_visits_within_attempt == 0, ]$first_clovers))

table((CHECK$clover_first == 0), CHECK$first_clovers)
table(CHECK$clover_first == 0, is.na(CHECK$first_clovers))

table(CHECK$first_num_resets == 0, (CHECK$clover_first == 0), CHECK$first_clovers)
table(CHECK$clover_first == 0, is.na(CHECK$first_clovers))

table(CHECK$first_multiple_visits_within_attempt == 0)
table(is.na(CHECK$first_multiple_visits_within_attempt ))
table(is.na(CHECK$first_multiple_visits_within_attempt ))

table(is.na(CHECK$first_multiple_visits_within_attempt ), 
      is.na(CHECK$first_clovers))


CHECK$FLAG_completed <- ifelse(CHECK$completed == 0 & is.na(CHECK$first_clovers) == F, 1, 0)

class(CHECK$end_time)
library(lubridate)
CHECK$datetime_end <- as_datetime(as.POSIXct(CHECK$end_time/1000, origin="1970-01-01"))
CHECK$datetime_start <- as_datetime(as.POSIXct(CHECK$start_time/1000, origin="1970-01-01"))

table( CHECK$FLAG_completed, (CHECK$datetime_end) > "2021-03-25 16:33:07", CHECK$datetime_start > "2021-03-25 16:33:07")


CHECK$CloversSame <- CHECK$first_clovers == CHECK$clover_first
table(CHECK[CHECK$clover_first != 0, ]$CloversSame)/length(CHECK[CHECK$clover_first != 0, ]$CloversSame)

table( 
  CHECK$problem_best_step == CHECK$user_first_step, CHECK$clover_first, CHECK$completed) # there seem to be some problems with these data
                    # all TRUE for this statement should == 3 (full clovers) or 0 if they did not complete the problem
table(CHECK$problem_best_step == CHECK$first_num_steps, CHECK$first_clovers)

table(is.na((CHECK$CloversSame)))
table(CHECK[CHECK$clover_first != 0, ]$problem_id, CHECK[CHECK$clover_first != 0, ]$CloversSame)

table(CHECK$clover_first, CHECK$first_num_resets > 0)

### CHECK ERRORS ####
# total errors
CHECK$ErrorsSame <- CHECK$total_error == CHECK$total_errors
table(CHECK$ErrorsSame)/length(CHECK$ErrorsSame)
table(CHECK$problem_id, CHECK$ErrorsSame)

# error first
table(CHECK$first_num_errors == CHECK$first_error)/length(CHECK$first_error)
    # first 
table(CHECK[CHECK$first_num_resets == 0, ]$first_num_errors == CHECK[CHECK$first_num_resets == 0, ]$first_error)/length((CHECK[CHECK$first_num_resets == 0, ]$first_num_errors == CHECK[CHECK$first_num_resets == 0, ]$first_error))

table(CHECK$first_num_errors == CHECK$first_errors_within_replay_attempt)

CHECK$any_replay <- ifelse(CHECK$total_replay > 0 , 1, 0)
table(CHECK$any_replay == CHECK$goback)/length(CHECK$goback)
table(CHECK$goback)
table(CHECK$total_replay, CHECK$goback)
require(lme4)



summary(
glmer(goback ~ as.factor(clover_first) +
      
        (1 | problem_id) +
        (1 | student_id)
        ,
             family = binomial()
      ,
      data = fh2t_CHECK[fh2t_CHECK$clover_first != 0 &
                        fh2t_CHECK$completed == 1
                        , ]
      ))


summary(
glmer(any_replay ~ as.factor(first_clovers) +
        first_num_errors +
        
        (1 | problem_id) +
        (1 | student_id)
      ,
      family = binomial()
      ,
      data = CHECK[CHECK$clover_first != 0 &
                     CHECK$completed == 1, ]
))


library(lubridate)
datetime <- as_datetime(as.POSIXct(time/1000, origin="1970-01-01"))