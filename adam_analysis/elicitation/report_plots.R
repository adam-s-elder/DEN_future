# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(patchwork)
source("../helper_functions/read_in_functions.R", chdir = TRUE)

article_level_elicit_data <- read_csv(
  file = "../intermediate_data/article_level_elicit.csv"
)
combined_data <- read_csv(
  file = "../../data_extraction/output/combined_data.csv"
)
month_level_elicit_data <- read_csv(
  file = "../intermediate_data/month_level_elicit.csv"
)
month_level_count_data <- read_csv(
  file = "../../data_extraction/output/split_month_data.csv"
)

monthly_stargel_compare_ten_params <-
  ten_param_starg_compare(month_level_elicit_data)
ggsave(
  "out/monthly_stargel_compare_ten_params.png",
  monthly_stargel_compare_ten_params,
  width = 17,
  height = 8
)

monthly_averages <- month_level_elicit_data |>
  mutate(count = cases_interviewed) |>
  group_by(pm_start_date, simp_pname) |>
  summarise(
    tot_sources = n(),
    non_missing_sources = sum(!is.na(count * param_value)),
    overall_count = sum(count, na.rm = TRUE),
    inclusive_ave = sum(count * param_value, na.rm = TRUE) / overall_count,
  ) |>
  mutate(count = overall_count)

count_data <-
  month_level_count_data |>
  filter(param_name == "cases_assigned_count") |>
  mutate(pm_start_date = lubridate::mdy(pm_start_date)) |>
  group_by(pm_start_date) |>
  summarise(
    inclusive_ave = sum(param_value, na.rm = TRUE)
  ) |>
  filter(pm_start_date %in% monthly_averages$pm_start_date)

max_val <- count_data$inclusive_ave |> max()

three_param_elictation <-
  monthly_averages |>
  make_elicit_plot("Elicitation Plot") +
  geom_line(
    data = count_data |> mutate(simp_pname = NA_character_),
    aes(x = pm_start_date, y = inclusive_ave / max_val),
    color = "black",
    linewidth = 0.25
  )
ggsave("out/three_param.svg", three_param_elictation, width = 8.5, height = 4)

contacts_per_namer <- monthly_averages |>
  make_sv_plot(
    'contacts_named_ratio_mean_cases_naming',
    "Average Contacts Named Per Case Naming"
  ) +
  geom_line(
    data = count_data |> mutate(simp_pname = NA_character_),
    aes(x = pm_start_date, y = 4.4 * inclusive_ave / max_val),
    color = "black",
    linewidth = 0.25
  )
ggsave("out/contacts_per.svg", contacts_per_namer, width = 8.5, height = 4)

contacts_per_case_assigned <- monthly_averages |>
  filter(inclusive_ave < 2) |>
  make_sv_plot(
    "contacts_reached_per_cases_assigned",
    "Average Number of Contacts Reached per Case Assigned"
  ) +
  geom_line(
    data = count_data |> mutate(simp_pname = NA_character_),
    aes(x = pm_start_date, y = 1.1 * inclusive_ave / max_val),
    color = "black",
    linewidth = 0.25
  )
ggsave(
  "out/contacts_per_case.svg",
  contacts_per_case_assigned,
  width = 8.5,
  height = 4
)

## Elicitation Against Capacity

elicit_multipanel_plot <- make_elicit_multipanel(combined_data)
ggsave(
  "out/elicit_multipanel.png",
  elicit_multipanel_plot,
  width = 17,
  height = 8
)

# Comparison to digital

two_plots <- elicitation_compare_digital(combined_data)

ggsave(
  "out/cont_per_case_naming.png",
  plot = two_plots$contacts_per_cases_naming_plot,
  width = 9,
  height = 3,
  dpi = 600
)
ggsave(
  "out/cont_per_case_assigned.png",
  plot = two_plots$contacts_notified_per_case,
  width = 9,
  height = 3,
  dpi = 600
)
