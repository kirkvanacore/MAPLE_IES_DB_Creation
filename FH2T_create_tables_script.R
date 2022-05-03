#### FH2T Table Creation ####

# load libraries
require(readxl)
library(data.table) 
require(plyr)
require(dplyr)
library(xts)
library(lubridate)
ifnull <- function(x,y) ifelse(is.na(x), y, x)
options(scipen = 100)

### LOAD DATA ####
# load original logs from David @ GM
data_files <- list.files("fh2t_logs")  # Identify file names
data_files                                                    # Print file names
for(i in 1:length(data_files)) {                              # Head of for-loop
  assign(paste0("data", i),                                   # Read and store data frames
         read.csv(paste0("fh2t_logs/",
                         data_files[i]),
         na.strings = c("", " ")))
}
ls(pattern = "data[1-9]+")

# merge all log files
logs<-do.call(rbind.fill, mget(ls(pattern = "data[1-9]+")))
rm(list = setdiff(ls(), c("logs", "ifnull"))) # clean environment

## load assessment data
assess <- read.csv("DATA20220202_4092 copy.csv")
colnames(assess)

# load crosswalk
cross<- read.csv("ies_research schema/student_id_crosswalk.csv")


# load FH2T problem metadata
problems <- read.csv("FH2T_Efficacy_Masterlist/worlds 1-14 Sep30-Table 1.csv", na.strings = c(""))
    # cleaning problem files

###### SAVE "fh2t_raw_logs.csv FILE ######
write.csv(logs, "ies_research schema/fh2t_raw_logs.csv")
logs<-read.csv("ies_research schema/fh2t_raw_logs.csv")

#### Build fh2t_problems_meta ####
colnames(problems)
table(problems$Video)
table(problems$New.Video)
table(problems$ID)
table(problems$problem)


fpm <- problems %>%
  filter(Video != 39) %>%
  mutate(video = ifelse(Video == "YES", 1, 0)) %>%
  rename("problem_id" = ID,
         "world_id" = worldID,
         "world_problem_num" = worldProblem,
        "start_state"  = problem,
        "answer_state" = answer,
        "optimal_step" = bestStep,
        "instruction_text1"=instructionText,
        "instruction_text2"=instructionText2,
        "hint_text"=hintText
        ) %>%
  select(
    problem_id,
    world_id,
    world_problem_num,
    start_state,
    answer_state,
    optimal_step,
    instruction_text1,
    instruction_text2,
    hint_text
  )





# create flags for optional and tuuiral poblems
# Optional problems - always the last 4 in each world (18 problems per world):
#   15-18, 33-36, 51-54, 69-72, 87-90, 105-108, 123-126, 141-144, 159-162, 177-180, 195-198, 213-216, 231-234, 249-2502
optional <- c(15, 16, 17, 18, 
              33, 34, 35, 36, 
              51, 52, 53, 54, 
              69, 70, 71, 72, 
              87, 88, 89, 90, 
              105, 106, 107, 108, 
              123, 124, 125, 126, 
              141, 142, 143, 144, 
              159, 160, 161,162, 
              177, 178, 179, 180, 
              195, 196, 197, 198, 
              213, 214, 215, 216, 
              231, 232, 233, 234, 
              249, 250, 251, 252)

fpm$optional <- ifelse(fpm$problem_id %in% (optional), 1, 0)
rm(optional)
table(fpm$optional)

# Tutorial problems - see https://docs.google.com/spreadsheets/d/13HE9CteznJbBOjC0V-JS6BWh0TQwWXhZt3uPSLt_8FE/edit#gid=0 – any problem with <img src> tag in instructionText (or in-game, may be the problems with stars)
#   1, 3, 8, 11, 19, 21, 28, 37, 45, 47, 55, 57, 58, 62, 64, 91, 92, 97, 99, 101, 103, 127, 145, 146, 163, 164, 168, 171, 181, 185, 190, 199, 200, 206, 211, 217, 223, 224, 246
tutorial<-  c(
  1,
  3,
  8,
  11,
  19,
  21,
  28,
  37,
  45,
  47,
  55,
  57,
  58,
  62,
  64,
  91,
  92,
  97,
  99,
  101,
  103,
  127,
  145,
  146,
  163,
  164,
  168,
  171,
  181,
  185,
  190,
  199,
  200,
  206,
  211,
  217,
  223,
  224,
  246
)

fpm$tutorial <- ifelse(fpm$problem_id %in% (tutorial), 1, 0)

table(fpm$tutorial)


###### SAVE "fh2t_student_action_logs.csv FILE #######
write.csv(fpm, "ies_research schema/fh2t_problems_meta.csv") 
colnames(fpm)
fpm<-read.csv("ies_research schema/fh2t_problems_meta.csv") 

#### CHECK/CLEAN LOGS DATA ####
colnames(logs)
table(logs$type)
table(logs$subtype) # has all the requested types
table(logs$automatic) # true means that the user/student didn't take any actions - WILL PURGE LATER
table(logs$automatic, 
      logs$subtype) # only for reset - consistent with what David said


# correct number of student 
length(unique(logs$user_id)) # There are more than I expected - maybe this includes researcher QAs

# make sure that we have all of the study FH2T users 

  # confirm that all the students how we expect to be in the logs are in the logs 
  FH2T_check <- cross %>%
    inner_join(logs %>%
      select(user_id) %>%
      distinct(),
      by = c("fh2t_user_id" = "user_id"))
    
  table(is.na(cross$fh2t_user_id)) 
  length(unique(FH2T_check$fh2t_user_id)) # confined
  rm(FH2T_check)

  
# correct number of student 
length(unique(logs$user_id)) # There are more than I expected - maybe this includes researcher QAs
  
# drop users who are not in the study
  
  logs <- logs %>%
    inner_join(cross %>% 
                 select("fh2t_user_id",
                       "StuID"),
               by = c("user_id" = "fh2t_user_id"))

  length(unique(logs$user_id))  

  
# correct number of problems 
table(logs$problem_id)
length(unique(logs$problem_id))
max(logs$problem_id) - length(unique(logs$problem_id))

# event variables
table(logs$type)
table(logs$subtype)
table(logs$subtype, logs$type) # type doesn't seems as relevant




#### Build fh2t_student_action_logs ####

logs_cln <- logs %>%
  filter(is.na(automatic ))%>%
  rename("world_id"= assignment_id,
         "world_problem_num"= assignment_problem_id) 
  select(X_id,
         StuID,
         canvas_id,
         trial_id,
         problem_id,
         world_id,
         world_problem_num,
         time,
         timestamp,
         dur,
         type,
         subtype,
         old_state,
         new_state,
         method,
         mistake,
         target_ascii,
         action) %>%
  arrange(StuID, time) 

###### SAVE "fh2t_student_action_logs.csv FILE #######
write.csv(logs_cln, "ies_research schema/fh2t_student_action_logs.csv") 
colnames(logs)
logs_cln<-read.csv("ies_research schema/fh2t_student_action_logs.csv") 

#### Build fh2t_student_problem_attempt ####
logs_cln <- logs_cln %>%
  ungroup()%>%
  arrange(StuID, time) %>%
  filter(subtype != "leave") %>% # leave is duplicate of complete and it is
                                 # screwing with the start/end time calculations
  dplyr::group_by(StuID, problem_id) %>% # this grouping is only for the lag section, 
                                           # so that the lags don't cross over into other problems
  dplyr::mutate(
    # this is lagged such that the flag starts on the first action after the complete will be
    replay_attempt_flag = ifnull(lag(ifelse(subtype == "complete", 1, 0)),0), 
    # this is a indicator that the next problem attempt was started due to a reset
    reset = ifnull(lag(ifelse(subtype == "reset", 1, 0)), 0),
    #this offset will be used to create the end time when I aggregate all of the data at the action level
    time_offset = lag(time) ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    # this creates a flag for when a student completed the problem 
    completed_flag = ifnull((ifelse(subtype == "complete", 1, 0)),0),
    step = ifelse(subtype == "rewrite", 1, 0),
    errors = ifelse(subtype == "mistake", 1, 0),
    reset_replay = ifelse(replay_attempt_flag == 1, 1,
                                    ifelse(reset == 1, 1, 0)) ,# this creates a flag for whenever a starts a new
    library =ifelse(subtype == "library" & action == "open", 1, 0),
    hint =ifelse(subtype == "hint", 1, 0),
  ) %>%
  dplyr::group_by(StuID, problem_id) %>%
  dplyr::mutate(
                # replay_attempt_num includes only instances where students go back after replaying
                replay_attempt_num = cumsum(replay_attempt_flag)+1,
                # attempt number includes both attempts for reset and attempt for replay
                attempt_num = cumsum(reset_replay)+1,
                
                ) %>% 
  dplyr::group_by(StuID, problem_id, replay_attempt_num) %>%  
  dplyr::mutate(
    # number of resets within the 
    resets_within_replay_attempt = sum(reset)
    
  ) %>%
  dplyr::group_by(StuID, problem_id, attempt_num) %>%  
  dplyr::mutate(num_steps = sum(step),
                num_errors = sum(errors),
                replay_attempt = sum(replay_attempt_flag),
                attempt_dur = sum(ifnull(dur, 0)),### NOTE THAT THIS IS ONLY A REACTION TIME AND SHOULD NOT BE 
                                                  ### USED AS TOTAL ATTEMPT TIME. I AM ONLY USING THIS TO DROP
                                                  ### FALSE ATTEMPTS WITH NULL DURATION TIMES 
                                  
                completed_dur_attempt = sum(completed_flag)
                ) %>%
  filter(attempt_dur > 0) # this excluded the last leave which is not an actual attempt



# checking work
# example1 <- logs_cln %>%
#   filter(StuID == 1686, 
#          problem_id ==25) %>%
#   select(StuID, 
#          problem_id,
#          attempt_num,
#          time,
#          time_offset,
#          replay_attempt,
#          completed_dur_attempt,
#          subtype,
#          dur,
#          reset_replay,
#          action,
#          replay_attempt_num,
#          step,
#          num_steps,
#          resets_within_replay_attempt,
#          errors,
#          num_errors,
#          trial_id  ) %>%
#   arrange(time)
# rm(example1)


spa <- logs_cln %>%
  dplyr::group_by(StuID, problem_id, attempt_num) %>%
  summarise(
    start_time = ifelse( min(attempt_num) == 1, min(time),
                         ifelse(
                           max(replay_attempt) == 0, min(time_offset), min(time))
                         ),
    end_time = max(time),
    num_steps = sum(step),
    num_errors = sum(errors),
    num_hints = sum(hint),
    num_libary_open = sum(library),
    replay_attempt = max(replay_attempt),
    completed_dur_attempt = max(completed_dur_attempt),
    resets_within_replay_attempt = max(resets_within_replay_attempt) # will drop this before saving, but need for student_problem_aggregation 
  )  %>% #### add Clover data
  left_join(fpm %>%
              select(problem_id,
                     optimal_step),
            by = c("problem_id")
            ) %>%
  mutate(steps_over_optimal = num_steps-optimal_step,
         clovers = ifelse(completed_dur_attempt == 0, NA,
                          ifelse(steps_over_optimal <= 0, 3, 
                          ifelse(steps_over_optimal <= 2, 2, 
                          1)))) %>%
  select(-steps_over_optimal)



###### SAVE "fh2t_student_problem_attempt.csv FILE #######
write.csv(spa %>%
            select(-resets_within_replay_attempt), "ies_research schema/fh2t_student_problem_attempt.csv") 



#### Build fh2t_student_problem ####

sp <- spa %>%
  group_by(StuID, problem_id) %>%
  dplyr::summarise(
    start_time = min(start_time),
    end_time = max(end_time),
    total_num_attempts = max(attempt_num),
    total_errors = sum(num_errors),
    total_hints = sum(num_hints),
    total_libary_open = sum(num_libary_open),
    total_completed_attempts = sum(completed_dur_attempt),
    total_resets = sum(ifelse(replay_attempt == 1, 0, 1)),
    total_replay = sum(replay_attempt)
  ) %>%
  left_join(
    spa %>%
      group_by(StuID, problem_id) %>%
      dplyr::filter(completed_dur_attempt == 1) %>%
      dplyr::mutate(
        best_attempt = min(num_steps),
        best_attempt_num = max(ifelse(best_attempt == num_steps,  attempt_num, 0))
      ) %>%
    dplyr::filter(attempt_num == best_attempt_num) %>%
        dplyr::select(StuID,
             problem_id,
             num_steps,
             clovers,
             num_errors,
             num_hints,
             resets_within_replay_attempt,
             num_libary_open,
             start_time,
             end_time
             ) %>%
      rename(num_resets=resets_within_replay_attempt) %>%
      rename_with( ~ paste0("best_", .x)),
    by = c("StuID" = "best_StuID", 
           "problem_id" = "best_problem_id")
  ) %>%
  left_join(
    spa %>%
      group_by(StuID, problem_id) %>%
      filter(completed_dur_attempt == 1) %>%
      dplyr::mutate(
        first_completed_attempt = min(attempt_num)
      ) %>%
      filter(attempt_num == first_completed_attempt) %>%
      select(StuID,
             problem_id,
             num_steps,
             clovers,
             num_errors,
             num_hints,
             resets_within_replay_attempt,
             num_libary_open,
             start_time,
             end_time) %>%
      rename(num_resets=resets_within_replay_attempt) %>%
      rename_with( ~ paste0("first_", .x)),
    by = c("StuID" = "first_StuID", 
           "problem_id" = "first_problem_id")
  ) %>%
  left_join(
    spa %>%
      group_by(StuID, problem_id) %>%
      filter(completed_dur_attempt == 1) %>%
      dplyr::mutate(
        first_completed_attempt = max(attempt_num)
      ) %>%
      filter(attempt_num == first_completed_attempt) %>%
      select(StuID,
             problem_id,
             num_steps,
             clovers,
             num_errors,
             num_hints,
             resets_within_replay_attempt,
             num_libary_open,
             start_time,
             end_time) %>%
      rename(num_resets=resets_within_replay_attempt) %>%
      rename_with( ~ paste0("final_", .x)),
    by = c("StuID" = "final_StuID", 
           "problem_id" = "final_problem_id")
  )

  

###### SAVE "fh2t_student_action_logs.csv FILE #######
write.csv(spa %>%
            select(-resets_within_replay_attempt), "ies_research schema/fh2t_student_problem.csv") 




table(as_datetime(logs_cln$time) == logs_cln$timestamp)
head(as.POSIXct(logs_cln$time, origin = "1970-01-01 00:00:00"))
head(logs_cln$time)
head(as_datetime(logs_cln$time))s
head(as_datetime(logs_cln$timestamp))
