# Data cleaning for google sheets with metrics on ENPA and ENCV
# usage.

library(tidyverse)
library(readxl)

## Reading in the data
usage_one_date <- readxl::read_xlsx("input/Usage Data.xlsx", skip = 2, n_max = 2,
                               col_types = "date") |>
  mutate(`Key metrics` = c("end_date", "start_date"))
usage_one_other <- readxl::read_xlsx("input/Usage Data.xlsx", skip = 2, n_max = 11,
                                    col_types = "text") |>
  filter(row_number() > 2) |> filter(!is.na(`Key metrics`))
usage_one_date <- usage_one_date |> pivot_longer(cols = -`Key metrics`,
                                 names_to = "State", values_to = "value") |>
  pivot_wider(id_cols = State, names_from = `Key metrics`,
              values_from = value)

usage_one_other |> pivot_longer(cols = -`Key metrics`,
                               names_to = "State", values_to = "value")
usage_one_other <- usage_one_other |> mutate(
  parameter_name = case_when(
    `Key metrics` == "Estimated number of active users" ~
      "num_users_count",
    `Key metrics` == "Cumulative number of users used codes (published keys)" ~
      "cases_named_contacts_count",
    `Key metrics` == "Cumulative number of self-report codes issued" ~
      "self_report_codes_issued",
    `Key metrics` == "Number  of people used codes in Feb (published keys)" ~
      "cases_named_contacts_count",
    `Key metrics` == "Cumulative number of exposure notification sent" ~
      "contacts_reached_count",
    `Key metrics` == "Number of exposure notification sent during Feb" ~
      "contacts_reached_count"
  )
)

## Next we combine other and date data frames.  We use the start and end date
## joining using state.  We also have adjust the start date for key metrics
## that include "Feb" to be the first of February

usage_one_other_long <- usage_one_other |>
  pivot_longer(cols = setdiff(colnames(usage_one_other),
                              c("Key metrics", "parameter_name")) ,
                               names_to = "State", values_to = "value")

usage_one_long <- left_join(usage_one_date, usage_one_other_long,
                            by = "State")
usage_one_long$start_date[str_detect(usage_one_long$`Key metrics`, "Feb")] <- lubridate::ymd("2023-02-01")

last_90_idx <- grep(usage_one_long$value, pattern = "(90 days)")
usage_one_long[last_90_idx, ]$start_date <-
  usage_one_long[last_90_idx, ]$end_date - lubridate::days(90)

usage_one_long[last_90_idx, ]$value <-
  usage_one_long[last_90_idx, ]$value |>
  gsub(pattern = " \\(90 days\\)", replacement = "")


# Variable names from template:
# param_name, param_value, pm_location, source,
# pm_start_date, pm_end_date, param_type, source_text, Desc

usage_one_final <- usage_one_long |>
  filter(State != "Sum") |> mutate(
    value = as.numeric(value),
    param_type = "digital",
    source = "ENPA and ENCV metrics reported by States",
  ) |>
  select(-`Key metrics`, param_name = parameter_name,
         param_value = value, pm_location = State,
         pm_start_date = start_date, pm_end_date = end_date)

## Second data frame
## Note under the California column:
##    Codes issued and used represent the period after self-report
##    was introduced (2/14/22-9/22/22)
## Note on the WA Data: Here, I changed the value for ENs per key uploaded
## from 9-16 to 9 to 16 in the spreadsheet because otherwise it was
## interpreted as a date.

usage_two <- readxl::read_xlsx("input/EN metrics roll up (1).xlsx",
                               col_types = "text") |>
  filter(!is.na(WA)) |> mutate(
    param_name = case_when(
      `Key Metrics` == "Full date range for available metrics (start-end dates)" ~
        "date_range",
      `Key Metrics` == "Cumulative activations" ~ "cumulative_activations",
      `Key Metrics` == "Cumulative codes used (tokens claimed)" ~
        "cases_named_contacts_count",
      `Key Metrics` == "Cumulative ENs generated" ~
        "contacts_reached_count",
      `Key Metrics` == "Average ENs per code used" ~
        "contcts_named_ratio_mean_case_naming",
      `Key Metrics` == "% of ENs issued within 3 days of exposure" ~
        "percent_notified_three_days",
      `Key Metrics` == "% of ENs issued within 6 days of exposure" ~
        "percent_notified_six_days",
      `Key Metrics` == "number of months" ~ "number_of_months"
    )
  )

usage_two <- usage_two |> t() |> as.data.frame()
colnames(usage_two) <- usage_two[nrow(usage_two), ]
usage_two <- usage_two[-1, ]
usage_two$state <- rownames(usage_two)
usage_two <- usage_two |>
  filter(!(state %in% c("TOTALS", "...13", "param_name")))

date_list <- usage_two$date_range |> str_split("-")
usage_two$pm_start_date <- date_list |> map_chr(1) |>
  lubridate::mdy()
usage_two$pm_end_date <- date_list |> map_chr(2) |>
  lubridate::mdy()

usage_two_long <- pivot_longer(usage_two |> select(-date_range),
                               cols = -c(state, pm_start_date, pm_end_date),
                               names_to = "param_name",
                               values_to = "param_value") |>
  mutate(param_value = as.numeric(param_value)) |>
  mutate(pm_location = state) |>
  select(-state) |>
  mutate(param_type = "digital",
         source = "ENPA and ENCV metrics reported by States")

comb_dat <- bind_rows(usage_one_final, usage_two_long)
comb_dat <- comb_dat |>
  mutate(pm_start_date = lubridate::ymd(pm_start_date),
         pm_end_date = lubridate::ymd(pm_end_date))

comb_dat |> group_by(pm_location, pm_start_date, pm_end_date) |>
  summarise(num_params = n()) |>
  ggplot(aes(y = pm_location, xmin = pm_start_date,
             xmax = pm_end_date, color = as.factor(num_params))) +
  geom_linerange(position = position_dodge2(width = 1)) + scale_x_date()

setdiff(
  usage_one_final$pm_location |> unique(),
  usage_two_long$pm_location |> unique()
)
setdiff(
  usage_two_long$pm_location |> unique(),
  usage_one_final$pm_location |> unique()
)

# The following are the groupings for each unique method that is needed
# to break down longer periods into smaller ones (when we get to it).....
# WA, MN, MA, HI, DC
# NV
# CA
# MD, MO
# NM
