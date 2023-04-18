library(vroom)
library(tidyverse)
library(data.table)
library(dplyr)
library(tictoc)

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

patent_tbl <- vroom(
  file       = "patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

class(patent_tbl)
setDT(patent_tbl)
class(patent_tbl)
patent_tbl %>% glimpse()

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

class(assignee_tbl)
setDT(assignee_tbl)
class(assignee_tbl)
assignee_tbl %>% glimpse()

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)
patent_assignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

class(patent_assignee_tbl)
setDT(patent_assignee_tbl)
class(patent_assignee_tbl)
patent_assignee_tbl %>% glimpse()

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

uspc_tbl <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

# Question 1
## Patent Dominance: What US company / corporation has the most patents? 
## List the 10 US companies with the most assigned/granted patents in 2014.

## Rename column
setnames(assignee_tbl, "id", "assignee_id")

## Merge data
combined_data <- merge(x = patent_assignee_tbl, y = assignee_tbl, 
                       by    = "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)
combined_data %>% glimpse()

## Only US companies -> Type 2
combined_data_us <- combined_data %>%
  filter(type == "2")

combined_data_us$assignee_id %>% 
  unique()

## Count
combined_data_us %>%
  group_by(organization) %>%
  summarise(
    count = n(),
  ) %>%
  ungroup() %>%
  arrange(desc(count)) %>% 
  head(10)

# Question 2
## Recent patent acitivity: 
## What US company had the most patents granted in April 2014? 
## List the top 10 companies with the most new granted patents for April 2014

## Rename column
setnames(patent_tbl, "id", "patent_id")

## Merge data with date
tic()
combined_data_us_date <- merge(x = combined_data_us, y = patent_tbl, 
                       by    = "patent_id", 
                       all.x = TRUE, 
                       all.y = FALSE)
toc()

combined_data_us_date %>% glimpse()

## Only April 2014
combined_data_us_date$date <- as.Date(combined_data_us_date$date, format= "%Y-%m-%d")

combined_data_us_april <- subset(combined_data_us_date, date> "2014-04-01" & date < "2014-04-30")

## Count
combined_data_us_april %>%
  group_by(organization) %>%
  summarise(
    count = n(),
  ) %>%
  ungroup() %>%
  arrange(desc(count)) %>% 
  head(10)


# Question 3
## Innovation in Tech: What is the most innovative tech sector? 
## For the top 10 companies (worldwide) with the most patents, 
## what are the top 5 USPTO tech main classes?

## Merge patent_assignee and assignee data by assignee_id
combined_data_3 <- merge(x = patent_assignee_tbl, y = assignee_tbl, 
                               by    = "assignee_id", 
                               all.x = TRUE, 
                               all.y = FALSE)
combined_data_3 %>% glimpse()

## Merge with uspc_tbl
combined_data_3_uspc <- merge(x = uspc_tbl, y = combined_data_3, 
                         by    = "patent_id", 
                         all.x = TRUE, 
                         all.y = FALSE)
## Top 5 main classes 
combined_data_3_uspc %>%
  group_by(mainclass_id) %>%
  summarise(
    count = n(),
  ) %>%
  ungroup() %>%
  arrange(desc(count)) %>% 
  head(5)

## Top 10 companies worldwide with most patents
combined_data_3 %>%
  group_by(organization) %>%
  summarise(
    count = n(),
  ) %>%
  ungroup() %>%
  arrange(desc(count)) %>% 
  head(10)



