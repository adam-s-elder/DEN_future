# Calculations for AMIA abstract
library(tidyverse)

cid_table_times <-
  read.csv("../entire_tables/stargel_2022/output/final_combined.csv")

wide_time_df <- cid_table_times |> filter(
  param_name %in% c("contact_reached_from_named_mean",
                    "case_interviewed_from_positive_mean",
                    "case_positive_from_test_mean",
                    "cases_interviewed_count")
) |> select(param_name, param_value, pm_start_date) |>
  pivot_wider(names_from = "param_name", values_from = param_value)

wide_time_df |> mutate(
  contact_reached_from_named_mean = ifelse(
    is.na(contact_reached_from_named_mean), 24,
    contact_reached_from_named_mean
  )
) |> mutate(total_hours = contact_reached_from_named_mean +
              case_interviewed_from_positive_mean +
              case_positive_from_test_mean) |>
  summarise(
    mean_time = sum(total_hours * cases_interviewed_count) /
      sum(cases_interviewed_count) / 24
  )

wide_contact_df <- cid_table_times |> filter(
param_name %in% c("contact_reached_count", "case_named_contact_count")
) |> select(param_name, param_value, pm_start_date) |>
  pivot_wider(names_from = "param_name", values_from = param_value)
wide_contact_df |>
  summarise(total_cases_naming = sum(case_named_contact_count),
            total_contacts_reached = sum(contact_reached_count)) |>
  mutate(contacts_per_namer = total_contacts_reached / total_cases_naming)
