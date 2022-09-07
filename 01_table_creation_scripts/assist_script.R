#### assist Table Creation ####

# This script takes the data sets from ASSISTments pulled by Jack Gonsalves on June 13th


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
  "mice"
  ,"psych",
  "stringr",
  "sjmisc",
  "campfin"
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
check <- function(X) { list(
  class = class(X),
  distint_count = length(unique(X)),
  (if  (class(X) == "numeric")
   {
    descibe = psych::describe(X)
  } else {
    table = head(table(X), 20)
  }),
  
  is_na=table(is.na(X)))
}
options(digits.secs = 3) 

### Connect to SQLite DB ####
# set db path
ies_research_con<- dbConnect(RSQLite::SQLite(), "ies_research schema/maple_ies_research.db")


#### load crosswalk #####
# need this to switch out the assistment user ids to the StuIDs
#cross<- read.csv("ies_research schema/student_id_crosswalk.csv")
cross<-dbGetQuery(ies_research_con, 
                  "select * from crosswalk_student")
colnames(cross)

#### problem meta data ####
##### load data #####
meta<-read.csv("02_data_source_files/problem list_IES_2020_final/ASSISTments-Table 1.csv",  na.strings = c("n/a", "n/a ", "", "N/A"))
colnames(meta)
length(unique(meta$Problem.ID))

##### add problem meta data
assignments <- data.frame (assignment_seq  = c(2,
                                               3,
                                               4,
                                               5,
                                               7,
                                               8,
                                               9,
                                               10,
                                               11,
                                               2,
                                               3,
                                               4,
                                               5,
                                               7,
                                               8,
                                               9,
                                               10,
                                               11 ), 
                           problem_set_id = c('PSABBP68',
                                     'PSABBMRV',
                                     'PSABBM2W',
                                     'PSABBMSN',
                                     'PSABBQZP',
                                     'PSABCFQ7',
                                     'PSABBMFD',
                                     'PSABCFRE',
                                     'PSABBRBK',
                                     'PSABC6S4',
                                     'PSABC6ST',
                                     'PSABC6SU',
                                     'PSABC6SV',
                                     'PSABC6SW',
                                     'PSABC6SX',
                                     'PSABC6SY',
                                     'PSABC6SZ',
                                     'PSABC6S2')
)


##### add condition meta data
condition <- data.frame (condition  = c("Instant",
                                          "Instant",
                                          "Instant",
                                          "Instant",
                                          "Instant",
                                          "Instant",
                                          "Instant",
                                          "Instant",
                                          "Instant",
                                               
                                           "Delay",
                                          "Delay",
                                          "Delay",
                                          "Delay",
                                          "Delay",
                                          "Delay",
                                          "Delay",
                                          "Delay",
                                          "Delay" ), 
                           problem_set_id = c('PSABBP68',
                                              'PSABBMRV',
                                              'PSABBM2W',
                                              'PSABBMSN',
                                              'PSABBQZP',
                                              'PSABCFQ7',
                                              'PSABBMFD',
                                              'PSABCFRE',
                                              'PSABBRBK',
                                              
                                              'PSABC6S4',
                                              'PSABC6ST',
                                              'PSABC6SU',
                                              'PSABC6SV',
                                              'PSABC6SW',
                                              'PSABC6SX',
                                              'PSABC6SY',
                                              'PSABC6SZ',
                                              'PSABC6S2')
)


meta_cl <- meta %>%
  rename_all(funs(tolower(stringr::str_replace_all(.,'[.]', '_')))) %>% # clean variable names
  select(-contains("x")) %>% # drop extraneous columns
  dplyr::rename(
    assignment_seq = problem_set,
    research_problem_key = jieun_s_problem_id_2,
    question_text = question,
  ) %>%
  left_join(
    assignments,
    by = "assignment_seq"
  ) %>%
  left_join(
    condition,
    by = "problem_set_id"
  ) %>%
  mutate(
    problem_part = ifelse(subproblem == "A" | is.na(subproblem), 1,
                          ifelse(subproblem == "B", 2,
                                 ifelse(subproblem == "C", 3,
                                        ifelse(subproblem == "D", 4,
                                               ifelse(subproblem == "E", 5,
                                                      ifelse(subproblem =="F", 6,
                                                             ifelse(subproblem == "G", 7, NA
                                                                  ))))))),
    problem_type = tolower(ifelse(is.na(problem_type), "end",
                                  ifelse(problem_type == "problem", "question", problem_type))),
    response_type = tolower(ifelse(is.na(response_type) & is.na(correct_answer) == F, "numberic expressions", response_type)), # problem was missing a response type
    math_problem = ifelse(problem_type == "problem" & is.na(math_type) == F, 1, 0),
    graded = ifelse(math_problem == 1 & response_type != "ungraded open response", 1, 0)
    
  ) %>% select(
    condition,
    problem_set_id,
    assignment_seq,
    problem_id,
    problem_order, 
    problem_part,
    problem_number,
    curricula,
    math_type,
    math_problem,
    graded,
    research_problem_key,
    problem_name,
    problem_id,
    problem_type,
    question_text, 
    response_type, 
    choice_options, 
    correct_answer
  ) %>%
  arrange(assignment_seq, problem_order, problem_part)

table(is.na(meta_cl$problem_set_id))
table(is.na(meta_cl$condition))


# check missingness pattern
sapply(meta_cl, anyNA)


###### SAVE "assist_problems_meta.csv FILE #######
write.csv(meta, "ies_research schema/assist_problems_meta.csv")

###### Write fh2t_problems_meta table ######
# if (dbExistsTable(ies_research_con, "assist_problems_meta"))
#   dbRemoveTable(ies_research_con, "assist_problems_meta")
RSQLite::dbWriteTable(ies_research_con, "assist_problems_meta", meta_cl, overwrite = T)

#### assist_raw_logs ####

##### load data ####
logs<-read.csv("02_data_source_files/ASSISTments_IES Study Data 7-13-2022_from Jack/Assignment Actions with not split data.csv", 
               na.strings = c("", "NULL")) 
  # not that this 
colnames(logs)
length(unique(logs$problem_id))

# the response has NAs when 
table(logs$action)
table(grepl("response",logs$action), logs$action)
table(grepl("response",logs$action), is.na(logs$submitted_response))
  # missing responses when the action was a response

#### code that Sid wrote to fix this problem
# After some examination it seems like the the reason for this error was because of -ve answers submitted by kids.
# I.E all rows with Submitted_response == "", is rows where the student had a negative answer, the - sign messed with the code it seems.

# The response is always in the 4th position of the action array
get_submitted_res <- function(check_action) {
  action_ar <- unlist(strsplit(check_action, " - "))
  return(trimws(action_ar[4]))
}
logs <- logs %>%
  mutate(
    submitted_response = sapply(check_action,get_submitted_res)
  )
table(grepl("response",logs$action), is.na(logs$submitted_response))



##### switch student ids ####
logs <- logs %>%
  left_join(cross %>%
              dplyr::select(assistments_user_id,
                     StuID),
            by = c("student_id" = "assistments_user_id")) %>%
  dplyr::rename( assistments_user_id =student_id) %>%
  arrange(StuID, problem_start_time, problem_part, action_number)


##### SAVE assist_raw_logs.csv FILE ####
write.csv(logs, "ies_research schema/assist_raw_logs.csv")

##### Write assist_raw_logs table ######
if (dbExistsTable(ies_research_con, "assist_raw_logs"))
  dbRemoveTable(ies_research_con, "assist_raw_logs")
RSQLite::dbWriteTable(ies_research_con, "assist_raw_logs", logs, overwrite = T)

#### assist_action_logs ####


    ##### MISSING STUDENTS IDS ####

    table(is.na(logs$StuID))
    table(is.na(logs$StuID))/length(logs$StuID) 
    
    student_check <- logs %>%
      select(assistments_user_id,
             StuID) %>%
      distinct() 
    
    write.csv(student_check, "missing_assistment_students.csv")
    
    table(is.na(student_check$StuID)) # there are 574 assistment user_ids ids that are not in that we do not have StuID connected to
    table(is.na(student_check$StuID))/length(student_check$assistments_user_id) # this includes ~ 33% of the users
    ### SOLUTION--> these are from the poliet study and should be dropped
    
    
    ##### MISSING PROBLEM IDS ####

    # further checks/questions
      # extra problem ids --> not in the problem meta data
    missing_ids <- logs %>% 
      select(problem_id) %>%
      #mutate(exists_logs = 1) %>%
      anti_join(meta %>%
                  select(Problem.ID),
                by= c("problem_id"= "Problem.ID")) %>%
      distinct()
    
    logs_missing_problem_ids <- logs %>%
      filter(problem_id %in% missing_ids$problem_id)
    class(logs_missing_problem_ids$problem_start_time)
    range(
      logs_missing_problem_ids$problem_start_time
    )
    hist(logs_missing_problem_ids$problem_start_time, breaks = 100)
    
    
    missing_porblme_logs<- logs %>% 
      #mutate(exists_logs = 1) %>%
      anti_join(meta_cl %>%
                  select(problem_id),
                by= c("problem_id"))
    
    table(is.na(missing_porblme_logs$StuID))

    # SOLUTION: determined that these were either deleted problems or instructional messages - not important-- will drop
    
  



# converts problem start time to datetime
class(logs$problem_start_time)
head(logs$problem_start_time, 10)
head(as_datetime(logs$problem_start_time, tz = "UTC"), 10)
logs$problem_start_time<- as_datetime(logs$problem_start_time, tz = "UTC")
range(logs$problem_start_time)




# milli_offset set NAs (should only occur on the and last action action)
table(is.na(logs$milli_offset), logs$action)
# na when: comment, end, help stuck, resume, start
  # is this the time between the last action taken and the current row action?


##### cleaning ####
# rename experiment_id as problem log id
### Drop student_id, arrange variables
### Drop missing student and problem ids 

colnames(logs)
logs_cln <- logs %>%
  filter(
    is.na(logs$StuID) == F, # drop piolet students
    !problem_id %in% missing_porblme_logs$problem_id # check this --> dropped a lot
  ) %>%
  mutate(
    problem_start_time = as_datetime(problem_start_time, tz = "UTC"),
    n = 1
  ) %>%
  dplyr::group_by(StuID,
           experiment_id,
           problem_id,
           problem_part) %>%
  dplyr::mutate(
    action_number = cumsum(n)
  ) %>%
  select(StuID,
         experiment_id,
         problem_id,
         problem_part,
         problem_start_time,
         action_number,
         action,
         submitted_response,
         milli_offset      
  ) %>%
  dplyr::rename(
    problem_set_id = experiment_id 
  ) %>%
  arrange(
    StuID,
    problem_set_id,
    problem_id,
    problem_part,
    action_number
  )
sapply(logs_cln, anyNA)

table(logs_cln$action, is.na(logs_cln$submitted_response))


##### SAVE assist_action_logs.csv FILE ####
write.csv(logs_cln, "ies_research schema/assist_student_action_logs.csv")

##### Write assist_action_logs table ######
if (dbExistsTable(ies_research_con, "assist_student_action_logs"))
  dbRemoveTable(ies_research_con, "assist_student_action_logs")
RSQLite::dbWriteTable(ies_research_con, "assist_student_action_logs", logs_cln, overwrite = T)


#### assist_student_problem ####

##### load ####
problems <- read.csv("02_data_source_files/ASSISTments_IES Study Data 7-13-2022_from Jack/Problem Logs.csv", 
                     na.strings = c("", "NULL"))
colnames(problems)
problems$dups<-duplicated( paste(problems$experiment_id, problems$problem_id, problems$problem_part, problems$student_id))
table(problems$dups)



problems <- problems%>%
  dplyr::ungroup() %>%
  dplyr::left_join(cross %>%
              dplyr::select(assistments_user_id,
                            StuID),
            by = c("student_id" = "assistments_user_id")) %>%
  dplyr::filter(
    is.na(StuID) == F,
    !problem_id %in% missing_porblme_logs$problem_id # check this --> dropped a lot
  ) %>%
  dplyr::mutate(
    dup = duplicated( paste(experiment_id, student_id, problem_id, problem_part, start_time))
  ) 


table(problems$dup)
table(problems$problem_id, problems$dup) # duplicates are only PRABKJD6 
table(problems$experiment_id, problems$dup) # occurs across problem_set_ids (assignments)
table(problems$num_hints_available < problems$hint_count)
table(problems[problems$problem_id == "PRABKJD6",]$num_hints_available)

  # KV Deduction: there was a problem with the with a group by statement 
  # because the pattern is that one dup has num_hints_available == 3 and the other == 1
  # and all the other data is the same

# DROP THE num_hints_available & DROP DUPS 
table(problems$problem_id != "PRABKJD6"  | problems$num_hints_available != 1)

problems_cln <- problems%>%
  ungroup() %>%
  dplyr::filter((problem_id != "PRABKJD6" | num_hints_available != 1))%>%
  dplyr::rename(
    problem_set_id = experiment_id
  ) %>%
  dplyr::left_join(meta_cl %>%
              dplyr::select(problem_id, 
                            problem_set_id,
                            problem_part,
                            math_problem,
                            problem_type,
                            response_type,
                            response_type,
                            graded),
            by = c("problem_set_id", "problem_id", "problem_part") 
  ) 
  # the NAs in the meta data are due to missing data in the original file, but it is only for 
  

problem_logs_agg <- logs_cln %>%
  left_join(meta_cl %>%
              dplyr::select(problem_id, 
                            problem_set_id,
                            problem_part,
                            math_problem,
                            problem_type,
                            graded),
            by = c("problem_set_id", "problem_id", "problem_part")
  ) %>%
  ungroup() %>%
  dplyr::group_by(StuID,
                  problem_set_id,
                  problem_id,
                  problem_part) %>%
  dplyr::mutate(
    hint_num = cumsum(ifelse(action == "hint", 1, 0)),
      attempt_num = ifelse(action != "correct response" &
                             action != "incorrect response",
                           NA,
                           cumsum(
                             ifelse(action == "correct response" |
                                      action == "incorrect response", 1, 0)
                           ))
    ) %>%
  dplyr::group_by(StuID,
                  problem_set_id,
                  problem_id,
                  problem_part) %>%
  dplyr::summarise(
    num_starts = sum(ifelse(action == "start", 1, 0)),
    num_end = sum(ifelse(action == "end", 1, 0)),
    num_resumes = sum(ifelse(action == "resume", 1, 0)),
    
    attempt_num_max = max(ifnull(attempt_num, 0)),
    correct_response_first_attempt_before_hint = ifelse(graded == 0 | attempt_num_max == 0, NA, 
                                                       sum(ifelse(hint_num == 0 & attempt_num == 1 & action == "correct response", 1, 0)) ),
    correct_response_first_attempt_after_hint = ifelse(graded == 0 | attempt_num_max == 0, NA, 
                                            sum(ifelse(attempt_num == 1 & action == "correct response", 1, 0)) ),
    correct_response_any = ifelse(graded == 0 | attempt_num_max == 0, NA, 
                                ifelse(  sum(ifelse(action == "correct response", 1, 0)) > 0, 1, 0) ),
    bottom_out_hint = ifelse(
      graded == 0 | attempt_num_max == 0, NA, 
      sum(ifelse(action == "answer_hint", 1, 0)))
    ) %>%
  distinct()

table(problem_logs_agg$correct_response_first_attempt_before_hint)
table(problem_logs_agg$correct_response_first_attempt_after_hint)
table(problem_logs_agg$correct_response_any)

table(problem_logs_agg$correct_response_first_attempt_before_hint <= 
        problem_logs_agg$correct_response_first_attempt_after_hint)

table(problem_logs_agg$correct_response_first_attempt_after_hint <= 
        problem_logs_agg$correct_response_any)
# converts problem start time to datetime
class(problems$start_time)
head(problems$start_time, 10)
head(as_datetime(problems$start_time, tz = "UTC"), 10)
problems$start_time<- as_datetime(problems$start_time, tz = "UTC")
range(problems$start_time)



# merge aggravation for logs with problem data provided by assignments
assist_student_problem <- (problems_cln) %>% 
  dplyr::mutate(
    total_time = ifelse(
      as.numeric(difftime( as_datetime(end_time), as_datetime(start_time),  units = "secs"))/(360) > 1, NA, # if the time is greater than an hour NULL
      as.numeric(difftime( as_datetime(end_time), as_datetime(start_time),  units = "secs")))*1000,
    first_response_time = ifelse(first_response_time/(3.6e+6) > 1, NA, first_response_time)
    )  %>%
  dplyr::left_join(
    (problem_logs_agg),
    by = c("StuID", 
           "problem_set_id",
           "problem_id",
           "problem_part")
  ) 

table(is.na(assist_student_problem$student_id))


colnames(assist_student_problem)

### check total time ####
describe(as.numeric( (assist_student_problem$total_time/6000))) # in mins
table(round(as.numeric((assist_student_problem$total_time/(3.6e+6))), 0)) # none over an hour
table(round(as.numeric((assist_student_problem$total_time/(6000))), 0) > 30) 
hist((as.numeric((assist_student_problem$total_time/(6000)))), breaks = 1000, xlim = c(0, 20))

### check first response time ####
describe(as.numeric( (assist_student_problem$first_response_time/6000))) # in mins
table(round(as.numeric((assist_student_problem$first_response_time/(3.6e+6))), 3)) # none over an hour
table(round(as.numeric((assist_student_problem$first_response_time/(6000))), 0) > 30) 
hist((as.numeric((assist_student_problem$first_response_time/(6000)))), breaks = 10000, xlim = c(0, 20))

##### Check Aggregations ####

## Correctness 


table(is.na(assist_student_problem$correct))
table(is.na(assist_student_problem$correct), assist_student_problem$attempt_count > 0)
  # only NA when attempt count is zero

table(is.na(assist_student_problem$correct), assist_student_problem$graded) 
  # there are a lot of problems with correctness when they are not supposed to be graded (not suprizing)

# alignment between correctness form assignments and my aggregation
table(assist_student_problem$correct, assist_student_problem$correct_response_first_attempt_before_hint)
  # there are 4 discrepancies --> all involve resumes --> adding a resume flag
table( assist_student_problem$correct, assist_student_problem$correct_response_first_attempt_before_hint, assist_student_problem$num_resumes > 0)
  # Discrepancies only a problem when students paused and then resumed

# DECISION: keep both variables - but make ungraded problem's correctness NA




## Attempt Count
# does my attempt count match their attempt count? - no 
table(assist_student_problem$attempt_num_max == assist_student_problem$attempt_count)

# its not because of resumes :( 
assist_student_problem %>%
  dplyr::group_by(num_resumes) %>%
  dplyr::summarise(
     TR = sum(ifelse(attempt_num_max == attempt_count, 1, 0)),
     FL = sum(ifelse(attempt_num_max != attempt_count, 1, 0))
  )
assist_student_problem$attempt_count_FLAGG <- ifelse(assist_student_problem$attempt_num_max != assist_student_problem$attempt_count, 1, 0)

head(
  assist_student_problem %>%
    filter(attempt_count_FLAGG == 1)
)

# its is only for ungraded open response questions --> Assignments isn't counting these as attempts
assist_student_problem %>%
  dplyr::group_by(response_type) %>%
  dplyr::summarise(
    TR = sum(ifelse(attempt_num_max == attempt_count, 1, 0)),
    FL = sum(ifelse(attempt_num_max != attempt_count, 1, 0))
  )

# DECISION: user the aggregated attempt count because it more accurately reflects the students experience



### hints
table(assist_student_problem$num_hints_available, assist_student_problem$num_hints_meta)
table(assist_student_problem$num_hints_available< assist_student_problem$hint_count)
table(assist_student_problem$num_hints_meta< assist_student_problem$hint_count)
# it seems as though num hints in meta data is less accurate than num hints available in data from assignments

# are thee multiple num_hints_available possibilities for a problem 
hints <- assist_student_problem %>%
  dplyr::group_by(problem_id, problem_set_id) %>%
  dplyr::summarise(
    min_hints = min(num_hints_available),
    max_hints = max(num_hints_available),
    hints_avl_differences = (min_hints != max_hints )
  )


check <- assist_student_problem %>%
  filter(is.na(correct) == F,
         attempt_count == 0 )

# bottom out hints
table(assist_student_problem$answer_given, assist_student_problem$bottom_out_hint)
  # answer given aligns with bottom out hints
table(assist_student_problem$answer_given, assist_student_problem$bottom_out_hint, (assist_student_problem$num_resumes > 0))
# will make bottom out hint an binary indicator

# reorg to only include variables we want 
assist_student_problem <- assist_student_problem %>%
  mutate(
    flag = ifelse(is.na(correct_response_any) &
                   is.na(correct) == F &
                   attempt_count == 0, 1, 0),
    correct = ifelse(graded == 0 | flag == 1, NA, correct),
    correct_response_any = ifelse(correct_response_any >= 1, 1, 0),
    
    bottom_out_hint = ifelse(bottom_out_hint > 0, 1, 0)
  ) %>%
  select(
    StuID,
    assistments_reference_id,
    problem_set_id,
    problem_id,
    problem_part,
    problem_type,
    graded,
    start_time,
    end_time,
    total_time,
    first_response_time,
    assistments_correctness = correct,
    correct_response_first_attempt_before_hint,
    correct_response_first_attempt_after_hint,
    correct_response_any,
    hint_count,
    num_hints_available,
    num_attempts = attempt_num_max,
    num_resumes,
    bottom_out_hint
  ) 


summary(as.numeric(assist_student_problem$total_time))
table(is.na(assist_student_problem$total_time), assist_student_problem$num_attempts)


table(assist_student_problem$correct_response_any, assist_student_problem$num_attempts)
table(assist_student_problem$problem_id, is.na(assist_student_problem$problem_type)) 
# missing meta data for PRABKHMM  --> it is an end 




##### SAVE assist_student_problem.csv FILE ####
write.csv(assist_student_problem, "ies_research schema/assist_student_problem.csv")

##### Write assist_problem_action_logs table ######
if (dbExistsTable(ies_research_con, "assist_student_problem"))
  dbRemoveTable(ies_research_con, "assist_student_problem")
RSQLite::dbWriteTable(ies_research_con, "assist_student_problem", assist_student_problem, overwrite = T)


### assist_student ####
      # number of assignments
      # num problems
      # percent of total problems complete
      # num hints
      # num bottom out hints
      # num attempts
      # correct_first_attempt 
      # avg time on task
      # sum time on task

assist_student <- assist_student_problem %>%
  dplyr::left_join(
    meta_cl %>%
      dplyr::group_by(problem_id) %>%
      dplyr::filter(
        problem_type %in% c( "problem", "question")
      ) %>%
      dplyr::summarise(
        last_problem_part = max(problem_part)
      ),
    by = "problem_id"
  ) %>%
  dplyr::filter(
    problem_type %in% c( "problem", "question")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    problem_id_part = paste(problem_set_id, problem_id, problem_part, sep = "_"),
    max_problem_part = max(problem_part),
    complete_problem_flag = ifelse(problem_part == last_problem_part
                                   & num_attempts >= 1,
                                   
                                   1, 0 
    )
  ) %>%
  dplyr::group_by(StuID) %>%
  dplyr::summarise(
    
    num_assignments_started = length(unique(problem_set_id)),
    
    # problems
    num_problems_started = length(unique(paste(problem_set_id, problem_id))),
    num_problems_attempted = sum(complete_problem_flag),
    
    
    # problem parts
    num_problem_parts_started = length(unique(problem_id_part)),
    num_problem_parts_attempted =sum(ifelse(num_attempts >= 1, 1, 0)),

    # graded problems
    num_graded_problems_started = sum(ifelse(problem_part == 1 & graded == 1, 1, 0)),
    num_graded_problems_attempted = sum(ifelse(num_attempts >= 1 & problem_part == 1 & graded == 1, 1, 0)),
    per_graded_problems_attempted = num_graded_problems_attempted/219, # i'm not sure this the right denominator
    
    # graded problem parts 
    num_graded_problem_parts_started = sum(graded),
    num_graded_problem_parts_attempted = sum(ifelse(graded== 1 & num_attempts >= 1, 1, 0)),
    per_graded_problem_parts_attempted = num_graded_problem_parts_attempted/308, # i'm not sure this the right denominator
    
    # correctness
    num_correct_response_first_attempt_before_hint = sum(correct_response_first_attempt_before_hint, na.rm = T),
    num_correct_response_first_attempt_after_hint = sum(correct_response_first_attempt_after_hint, na.rm = T),
    num_correct_response_any = sum(correct_response_any, na.rm = T),
    
    #
    avg_accuracy_first_attempt_before_hint = ifelse(num_graded_problem_parts_attempted == 0, NA,
                                                    round(num_correct_response_first_attempt_before_hint/num_graded_problem_parts_attempted, 2)),
    avg_accuracy_first_attempt_after_hint = ifelse(num_graded_problem_parts_attempted == 0, NA, 
                                                   round(num_correct_response_first_attempt_after_hint/num_graded_problem_parts_attempted, 2)),
    avg_correct_response_any = ifelse(num_graded_problem_parts_attempted == 0, NA, 
                                      round(num_correct_response_any/num_graded_problem_parts_attempted, 2)),
    
    # hints
    total_hints_accessed = sum(hint_count, na.rm = T),
    per_available_hints_accessed = total_hints_accessed/sum(num_hints_available),
    num_problem_parts_hints_accessed = sum(ifelse(hint_count >= 1, 1, 0), na.rm = T),
    num_problem_parts_used_bottom_out_hint = sum(ifelse(bottom_out_hint >= 1, 1, 0), na.rm = T),
    
    # time 
    avg_first_response_time = mean(first_response_time, na.rm = T),
    avg_problem_time = mean(total_time, na.rm = T),
    total_time = sum(total_time, na.rm = T)
    )

length(unique(meta_cl[meta_cl$grade == 1,]$problem_id))
length(unique(paste(meta_cl$problem_id, meta_cl$problem_part, sep = "_")))
length(unique(paste(meta_cl[meta_cl$grade == 1,]$problem_id, meta_cl[meta_cl$grade == 1,]$problem_part, sep = "_")))

# how many graded problems  are there in each condition --> 218
length(unique(meta_cl[
                        meta_cl$condition == "Instant"
                      ,]$problem_id))

length(unique(meta_cl[meta_cl$grade == 1 &
                        meta_cl$condition == "Delay"
                      ,]$problem_id))

# how many graded problems  are there in each condition --> 218
length(unique(meta_cl[meta_cl$grade == 1 &
                              meta_cl$condition == "Instant"
                            ,]$problem_id))

length(unique(meta_cl[meta_cl$grade == 1 &
                        meta_cl$condition == "Delay"
                      ,]$problem_id))


# how many graded problem parts are there in each condition --> 308
length(unique(paste(meta_cl[meta_cl$grade == 1 &
                              meta_cl$condition == "Instant"
                            ,]$problem_id,
                    meta_cl[meta_cl$grade == 1 &
                              meta_cl$condition == "Instant"
                            ,]$problem_part, sep = "_")))

length(unique(paste(meta_cl[meta_cl$grade == 1 &
                              meta_cl$condition == "Delay"
                            ,]$problem_id,
                    meta_cl[meta_cl$grade == 1 &
                              meta_cl$condition == "Delay"
                            ,]$problem_part, sep = "_")))


#### data checks #####

# problems
summary(assist_student$num_problems_started)
hist(assist_student$num_problems_started)
summary(assist_student$num_problems_attempted)
table(assist_student$num_problems_started >= assist_student$num_problems_attempted)



# problem parts
summary(assist_student$num_problem_parts_started )
summary(assist_student$num_problem_parts_attempted)

# graded problems
summary(assist_student$num_graded_problems_started)
summary(assist_student$num_graded_problems_attempted)
table(assist_student$num_graded_problems_started >= assist_student$num_graded_problems_attempted)

summary(assist_student$per_graded_problems_attempted)
hist(assist_student$per_graded_problem_parts_attempted)


# graded problem parts 
summary(assist_student$num_graded_problem_parts_started)
hist(assist_student$num_graded_problem_parts_started)
summary(assist_student$num_graded_problem_parts_attempted)
hist(assist_student$num_graded_problem_parts_attempted)

summary(assist_student$per_graded_problem_parts_attempted)

# correctness
summary(assist_student$num_correct_response_first_attempt_before_hint)
summary(assist_student$num_correct_response_first_attempt_after_hint)
summary(assist_student$num_correct_response_any)

table(assist_student$num_correct_response_first_attempt_before_hint <= assist_student$num_correct_response_first_attempt_after_hint)
table(assist_student$num_correct_response_first_attempt_after_hint <= assist_student$num_correct_response_any)

hist(assist_student$num_correct_response_first_attempt_before_hint)
hist(assist_student$num_correct_response_first_attempt_after_hint)
hist(assist_student$num_correct_response_any)


#
summary(assist_student$avg_accuracy_first_attempt_before_hint)
        summary(assist_student$avg_accuracy_first_attempt_after_hint)
                summary(assist_student$avg_correct_response_any)

table(is.na(assist_student$avg_accuracy_first_attempt_before_hint), assist_student$num_problem_parts_attempted == 0)


                # support
summary(assist_student$num_hints_accessed)
summary(assist_student$num_problem_parts_hints_accessed)
summary(assist_student$per_available_hints_accessed)
summary(assist_student$num_problems_parts_used_bottom_out_hint)       
hist(assist_student$num_problems_parts_used_bottom_out_hint)


# time 
summary(assist_student$avg_first_response_time/60000)
table(is.na(assist_student$avg_first_response_time), (assist_student$num_problem_parts_attempted == 0))
hist(as.numeric(assist_student$avg_first_response_time)/60000, breaks = 100000, xlim =c(0, 5))

  summary(as.numeric(assist_student$avg_problem_time))
  table(is.na(assist_student$avg_problem_time), (assist_student$num_problem_parts_attempted == 0))
  hist(as.numeric(assist_student$avg_problem_time)/60000, breaks = 100, xlim =c(0, 5))
  
  table(as.numeric(assist_student$avg_first_response_time) <= as.numeric(assist_student$avg_problem_time))
  
hist(as.numeric(assist_student$avg_problem_time)/60000, breaks = 100000, xlim =c(0, 5))
table(as.numeric(assist_student$avg_problem_time)/60000 > 24*60)
  

summary(assist_student$total_time/3600000)
table(is.na(assist_student$avg_first_response_time), (assist_student$num_problem_parts_attempted == 0))
hist(as.numeric(assist_student$total_time)/3600000, breaks = 1000, xlim =c(0, 5))
plot(as.numeric(assist_student$total_time)/3600000, assist_student$num_assignments_started)

# num problems correct* should always be > or = to total problem parts attempted

table(assist_student$num_graded_problem_parts_started >= assist_student$num_correct_response_first_attempt_before_hint)
table(assist_student$num_graded_problem_parts_attempted >= assist_student$num_correct_response_first_attempt_before_hint)


table(assist_student$num_problems_started - assist_student$num_problems_attempted) # max difference of 9 because that is the number of assignments they have finsihed

table(assist_student$num_graded_problems_started < assist_student$num_problems_started)

table(assist_student$avg_accuracy_first_attempt_after_hint == assist_student$avg_accuracy_first_attempt_before_hint)

summary(assist_student$avg_accuracy_first_attempt_before_hint)
hist(assist_student$avg_accuracy_first_attempt_before_hint, breaks = 100)
summary(assist_student$avg_accuracy_first_attempt_after_hint)
hist(assist_student$avg_accuracy_first_attempt_after_hint, breaks = 100)
summary(assist_student$avg_correct_response_any)
hist(assist_student$avg_correct_response_any, breaks = 100)

## SAVE assist_problem_action_logs.csv FILE ####
write.csv(assist_student, "ies_research schema/assist_student.csv")

##### Write assist_problem_action_logs table ######
if (dbExistsTable(ies_research_con, "assist_student"))
  dbRemoveTable(ies_research_con, "assist_student")
RSQLite::dbWriteTable(ies_research_con, "assist_student", assist_student, overwrite = T)



