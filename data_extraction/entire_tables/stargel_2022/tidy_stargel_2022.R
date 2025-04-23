# Data cleaning for "Case Investigation and Contact Tracing Efforts From
# Health Departments in the United States, November 2020 to December 2021."

library(tidyverse)

## Aggregate count data
ag_data <- read.csv("input/total_counts.csv")

ag_data_numeric <- ag_data |> mutate(across(-1, .fns = function(x) {
  as.numeric(stringr::str_remove_all(x, ","))
}))

ag_data_final <- ag_data_numeric |>
  pivot_longer(cols = -1, names_to = "date", values_to = "value") |>
  rename(parameter = "X") |> mutate(
    date = stringr::str_replace(pattern = "X", replacement = "", date),
    start_date = lubridate::ym(date),
    end_date = add_with_rollback(start_date, preserve_hms = TRUE,
                                 months(1), roll_to_first = TRUE),
    parameter = case_when(
      parameter == "Total # of Cases Reported to Health Departments" ~ "cases_assigned_count",
      parameter == "Total # of Cases Completing an Interview" ~ "cases_interviewed_count",
      parameter == "Total # of Cases that Provided at Least 1 Contact" ~ "cases_named_contacts_count",
      parameter == "Total # of Contacts Identified" ~ "contacts_named_count",
      parameter == "Total # of Contacts Notified" ~ "contacts_reached_count",
      parameter == "Total # of Cases known as Contacts within the previous 14 Days" ~ "cases_were_contacts_count",
      parameter == "Total # of Contacts Tested within 14 Days of Notification" ~ "contacts_tested_count",
    ),
    location = "64 HD accross US and collonies"
  ) |> select(-date) |> rename(param = parameter)

## Per-Department Summaries
dept_data <- read.csv("input/by_dept_ave.csv")

parse_params <- function(string) {
  split_string <- string |> str_split(pattern = ",|\n") |>
    map(.f = function(x) {
    x |> stringr::str_remove("\\(|\\)") |> str_remove("%")
  })
  fin_list <- list_transpose(split_string)
  return(data.frame("mean" = fin_list[[1]],
             "hdcount" = fin_list[[2]],
             "median" = fin_list[[3]]))
}

# Comparing between the two tables, it appears the means in the HD table
# is with respect to HD since the % of completed cases doesn't match between
# the two sources.

col_key <-
  c("Mean % of Cases Completing an Interview\n(n, median %)" = "hd_cases_interviewed_perc",
    "Mean % of Cases that Provided at Least 1 Contact during Interview\n(n, median %)" = "hd_percent_naming_contcts",
    "Mean # of Contacts Identified from Cases Completing an Interview (n, median #)" = "hd_contacts_named_ratio_mean",
    "Mean # of Contacts Identified from Cases Providing Contacts (n, median #)" = "hd_contacts_named_ratio_mean_cases_naming",
    "Mean % of Contacts Identified who were Notified (n, median %)" = "hd_contacts_reached_perc",
    "Mean % of Contacts Notified who are Tested within 14 days (n, median %)" = "hd_contacts_test_from_notified",
    "Mean % of Cases Reported to HD that were previously known as Contacts within the Past 14 Days\n(n, median %)" =
      "hd_cases_was_contacts_perc",
    "Mean # of reported days between speciment collection and report of case to health department\n(n, median #)" = "hd_cases_positive_from_test_mean",
    "Mean # of reported days between the report of case to health department and case interview completion (n, median #)" =
      "hd_cases_reached_from_test_mean",
    "Mean # of reported days between case interview completion and contact notification\n(n, median #)" =
      "hd_contacts_reached_from_named_mean")

expanded_data <- dept_data |> select(-X) |> map(.f = function(x) {
  month_df <- x |> parse_params()
  rownames(month_df) <- names(col_key)
  month_df <- suppressWarnings(
    month_df |> mutate(across(everything(), .fns = as.numeric))
  )
  month_df$param <- col_key
  # Converting from days to hours
  month_df$mean[8:10] <- month_df$mean[8:10] * 24
  month_df$median[8:10] <- month_df$median[8:10] * 24
  return_df <- month_df |> pivot_longer(cols = -param,
                           names_to = "type", values_to = "value") |>
    mutate(param = paste0(param, "_", type)) |>
    select(-type)
  return(return_df)
})

start_dates <- names(expanded_data) |> lubridate::my()
end_dates <- add_with_rollback(start_dates, preserve_hms = TRUE,
                             months(1), roll_to_first = TRUE)

names(expanded_data) <- NULL
list_with_dates <-
  list(expanded_data, as.list(start_dates), as.list(end_dates))

hd_perf_data <- list_transpose(list_with_dates) |> map(.f = function(x) {
  bind_cols(x[[1]], start_date = x[[2]], end_date = x[[3]])
}) |> do.call(what = bind_rows) |> mutate(
  location = "64 HD accross US and collonies"
)

## Workforce data
wf_data <- read.csv("input/case_investigators_workforce_estimates.csv")

dates <- paste0(c(NA, rep(2020, 2), rep(2021, 12)), "-", wf_data[1, ])
colnames(wf_data) <- dates
wf_data <- wf_data[-1, ]
wf_data$param <- c("staff_hired_ci_count", "staff_hired_ct_count",
                   "staff_hired_ci_per_health_dept_mean",
                   "staff_hired_ct_per_health_dept_mean",
                   "cases_per_staff_month", "contacts_per_staff_month")

workforce_data <- wf_data |> select(-`NA-`) |>
  pivot_longer(-param, names_to = "start_date", values_to = "value") |>
  mutate(
    start_date = lubridate::ym(start_date),
    end_date = add_with_rollback(start_date, preserve_hms = TRUE,
                                 months(1), roll_to_first = TRUE),
    value = as.numeric(value),
    location = "64 HD accross US and collonies"
         )

final_combined <- bind_rows(workforce_data, ag_data_final, hd_perf_data) |>
  mutate(source = "Case Investigation and Contact Tracing Efforts From Health Departments in the United States, November 2020 to December 2021.") |>
  mutate(pm_start_date = format(start_date, "%m/%d/%Y"),
         pm_end_date = format(end_date, "%m/%d/%Y")) |>
  select(-start_date, -end_date) |>
  rename(param_name = param, param_value = value,
         pm_location = location)

write_csv(final_combined, file = "output/final_combined.csv")
