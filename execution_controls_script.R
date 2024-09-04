# Things the scheduler should do:
#   Break runs into categories- daily, weekly, monthly, quarterly, semi-annual, yearly
#   Determine based on the calendar day what run type.convert
#   Run type becomes a parameter to filter subtasks
#     creates a vector ex c("daily", "weekly") if last day of business week
#     creates a vector ex c("daily", "monthly") if last day of month
#   Create a file that saves with run info (so pretty much a log)
#     Should collect info if there are errors ie need robust error reporting
#   Stops run if there is already a run for that business day
#   Possibly check for duplicate tickets?

# 2017-08-16T14:21:00.000-0500 date string format
#as_datetime

#Dependencies####
library(tidyverse)
library(bizdays)
library(httr)
library(jsonlite)
library(readxl)
library(lubridate)

# supplied via commabd line

params = list(un = myargs[1], pw = myargs[2])



#Calendars####
#A way to automate holidays?
#base on fiscal year? - need to make custom year/half year calculations

fiscal_start = as_date("2023-11-01")
fiscal_end = fiscal_start %m+% months(12) - 1

#importing ticket information via excel template####

read_excel_allsheets = function(filename) {
  sheets = excel_sheets(filename)
  x = lapply(sheets, function(X)
    read_excel(filename, sheet = X))
  names(x) = sheets
  x
}

JIRA = read_excel_allsheets(
  "C:/Users/TAG2054/OneDrive - The Toronto-Dominion Bank/Desktop/JIRA.Reference.xlsx"
)



d = as.Date(JIRA$Holidays$Holidays)

# creating calendar to run ticket creation
# starts on first holiday given, ends on last

create.calendar(
  "TD",
  holidays = d,
  weekdays = c("saturday", "sunday"),
  start.date = first(d),
  end.date = last(d)
)

q1_end = adjust.previous(fiscal_start %m+% months(3) - 1, cal = "TD")
q2_end = adjust.previous(fiscal_start %m+% months(6) - 1, cal = "TD")
q3_end = adjust.previous(fiscal_start %m+% months(9) - 1, cal = "TD")
q4_end = adjust.previous(fiscal_start %m+% months(12) - 1, cal = "TD")

quarterly = c(q1_end, q2_end, q3_end, q4_end)

`semi-annual` = q2_end

yearly = q4_end

run_date = Sys.Date()

active_reports = JIRA$Reports %>%
  filter(start_date <= run_date) %>%
  select(-start_date)


log.path = paste0(
  "C:/Users/TAG2054/OneDrive - The Toronto-Dominion Bank/Desktop/R Objects/Ticket Creation/",
  Sys.Date()
)

is.daily = function(x) {
  if_else(is.bizday(x, cal = "TD"), "daily", NULL)
}

is.weekly = function(x) {
  tolower(trimws(paste0("weekly*", wday(run_date, label = TRUE, abbr = FALSE))))
}

is.monthly = function(x) {
  if_else(x == adjust.previous(ceiling_date(x, "months", change_on_boundary = FALSE) -
                                 1, cal = "TD"),
          "monthly",
          NULL)
}

is.quarterly = function(x) {
  if_else(x %in% quarterly, "quarterly", NULL)
}

is.semi_annual = function(x) {
  if_else(x == `semi-annual`, "semi-annual", NULL)
}

is.yearly = function(x) {
  if_else(x == yearly, "yearly", NULL)
}

determine.run = function(x) {
  c(
    is.daily(x),
    is.weekly(x),
    is.monthly(x),
    is.quarterly(x),
    is.semi_annual(x),
    is.yearly(x)
  ) %>%
    na.omit() %>%
    as.vector()
}

run_params = determine.run(run_date)

if (length(run_params) < 1) {
  quit(save = "no")
} else {
  print("Business Day")
}


###remember to open else clause


#test for bulk: parent_key = "LRMAUDIT-2507"



tickets = within(JIRA, rm("Reports", "Template", "Holidays", "DataValidation"))

report_names = names(tickets)



###### Checking for duplicates

check_date = run_date %m+% months(-3)

paginated.get = function(link){
  base.link = paste0(link, "&maxResults=0")
  results = GET(base.link, authenticate(params$un, params$pw))
  results = content(results, "text", encoding = "UTF-8")
  results = fromJSON(results, flatten = TRUE)$total
  
  limit = 500
  pages = ceiling(results/limit)
  
  start.at = seq(from = 0, by = limit, length.out = pages)
  
  links = paste0(link, "&maxResults=", limit, "&startAt=", start.at)
  
  get.fun = function(x){
    api = GET(x, authenticate(params$un, params$pw))
    apid = content(api, "text", encoding = "UTF-8")
    results = as.data.frame(fromJSON(apid, flatten = TRUE))
  }
  
  JIRA = lapply(links, get.fun)
  
  jira = do.call(rbind, JIRA)
  
  jira
  
}

all_tickets = paginated.get(paste0('https://track.td.com/rest/api/latest/search?jql=project%20%3D%20LRMAUDIT%20AND%20issuetype%20%3D%20Story%20AND%20status%20!%3D%20canceled%20AND%20%22Target%20start%22%20%3E%20%22', check_date, '%22&fields=customfield_10006,customfield_24100'))

duplicates = all_tickets %>%
  filter(issues.fields.customfield_24100 == as.character(run_date)) %>%
  pull(issues.fields.customfield_10006)


clean.tickets = function(x) {
 
  # to direct to test environment
  x = x %>%
    mutate(Epic_Link = "LRMAUDIT-13418")
  ######  
  
      x = x %>%
        filter(`JIRA_Include?` == "yes", 
               Epic_Link %in% active_reports$Key,
               !(Epic_Link %in% duplicates)) %>%
        mutate(across(.cols = everything(),as.character),
               KC = if_else(Key != "no",
                       "Non-Key",
                       "Key"),
              Assignee = if_else(is.na(as.character(Assignee)),
                             "-1" ,
                             as.character(Assignee)),
          `Control Detail Description` = if_else(is.na(`Control Detail Description`),
                                                 "Placeholder",
                                                 `Control Detail Description`),
          Frequency = tolower(trimws(if_else(is.na(DoTW),
                                             paste0(Frequency),
                                             paste0(Frequency, "*", DoTW))))
        ) %>%
        select(
          Project,
          Type,
          Epic_Link,
          `JIRA_Include?`,
          `Control Detail Description`,
          `Sub-Task Summary`,
          Assignee,
          Frequency,
          KC
        ) %>%
        rename(Include = `JIRA_Include?`,
               Description = `Control Detail Description`,
               Summary = `Sub-Task Summary`) %>%
        filter(Frequency %in% run_params)
      x
    
    
}

cleaned_tickets = lapply(tickets, clean.tickets)

## ticket check
# check for filled columns for project, type, epic_link, control description, summary, frequency, assignee

# check_tickets = function(x) {
#   check1 = all(x$Project == x$Project[1])
#   check2 = x$Type[1] == "Story" & all(x$Type[-1] == "Sub-task")
#   check3 = all(x$Epic_Link == x$Epic_Link[1])
#   check4 = 
# }

drop_empty = function(x) {
  if (nrow(x) < 1) {
    x = NULL
  } else {
    x
  }
}

cleaned_tickets = compact(lapply(cleaned_tickets, drop_empty))

#error




# When fully automating this, should implement a data check to stop the POST if the data is wrong


#posting to jira####

jira_story_url = 'https://track.td.com/rest/api/latest/issue'
jira_bulk_url = 'https://track.td.com/rest/api/latest/issue/bulk'

story.JSON = function(x) {
  body_text = list(
    "fields" = list(
      "project" = list("key" = x$Project),
      "summary" = x$Summary,
      "description" = x$Description,
      "issuetype" = list("name" = x$Type),
      "assignee" = list("name" = x$Assignee),
      "customfield_10006" = x$Epic_Link,
      "customfield_24100" = as_datetime(run_date)
      
    )
  )
  toJSON(body_text, pretty = TRUE, auto_unbox = TRUE)
}

subtask.list = function(x) {
  body_text =
    list(
      "update" = NULL,
      "fields" = list(
        "project" = list("key" = x$Project),
        "parent" = list("key" = parent_key),
        "summary" = x$Summary,
        "description" = x$Description,
        "issuetype" = list("name" = x$Type),
        "labels" = list(x$KC),
        "assignee" = list("name" = x$Assignee)
      )
    )
  
  body_text
}

bulk.JSON = function(x) {
  out = list("issueUpdates" =
               lapply(x, subtask.list))
  toJSON(out, pretty = TRUE, auto_unbox = TRUE)
}

#this function acts on the list "cleaned_tickets"

creation_log = data.frame()

post.to.JIRA = function(x) {
  
  log_row = c()
  log_row = append(log_row, x$Epic_Link[1])
  
  parent = x %>%
    filter(Type == "Story") %>%
    mutate(Summary = paste0(Summary, " - ", run_date))
  
  subtasks.df = x %>%
    filter(Type == "Sub-task")
  
  subtasks.list = subtasks.df %>%
    group_by(Summary) %>%
    group_split()
  
  parent.post = POST(
    url = jira_story_url,
    body = story.JSON(parent),
    authenticate(params$un, params$pw),
    add_headers("Content-Type" = "application/json")
  )
  parent_key <<- if (status_code(parent.post) == 201) {
    fromJSON(rawToChar(parent.post$content))$key
  } else {
    "Error"
  }
  
  
  if (parent_key == "Error") {} else {
        bulk_subtasks = bulk.JSON(subtasks.list)
        
        subtasks.post = POST(
          url = jira_bulk_url,
          body = bulk_subtasks,
          authenticate(params$un, params$pw),
          add_headers("Content-Type" = "application/json")
    )
  }
  
  log_row = append(log_row, parent_key)
  
  creation_log <<- rbind(creation_log, log_row)
  
  }


lapply(cleaned_tickets, post.to.JIRA)

names(creation_log) = c("epic", "parent")


# https://track.td.com/browse/LRMAUDIT-13791

dput(error.log, log.path)
