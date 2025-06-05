# Comparisons between digital and manual notification timeliness
library(tidyverse)
source("../adam_analysis/helper_functions/make_time_dfs.R")
source("../adam_analysis/helper_functions/plotting_functions.R")

long_monthly_with_impt <- read_csv("intermediate_data/monthly_manual_data.csv")
all_param_df <- read.csv("../data_extraction/manipulated_data/simplified_wide_df_with_date_loc_imputed.csv")
all_param_df$pm_end_date <- guess_dates(all_param_df$pm_end_date)
all_param_df$pm_start_date <- guess_dates(all_param_df$pm_start_date)

wa_verify_times <- read_csv("~/Desktop/etoen_wa_notify.csv")

waver <- wa_verify_times |> mutate(
    value = 24 * (mean_etoen),
    param_name = "test_to_notified")

manual_times <-
  long_monthly_with_impt |> group_by(month_year, metric) |>
  summarise(param_value = sum(average))

comb_data <- waver |>
  select(month_year = monthyear, param_name, param_value = value) |>
  mutate(type = "digital") |>
  bind_rows(manual_times |> mutate(type = "manual"))

comb_data |> filter(!(metric %in% "med")) |>
  ggplot(aes(x = month_year, y = param_value / 24,
             fill = type)) +
  geom_col(position = position_dodge2(preserve = "single"), width = 25) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_altair() +
  scale_fill_manual("", values =
                       ggthemes::tableau_color_pal("Tableau 10")(2),
                     breaks = c("digital", "manual"),
                     labels = c("Digital", "Manual")) +
  ylab("Days") +
  scale_x_date(date_breaks = "2 month",
               labels = scales::label_date("%b\n%Y")) +
  theme(
    legend.position = c(0.9, 0.9),
    legend.box = "horizontal",
    legend.direction = "horizontal",
    legend.justification = "right",
        axis.title.x = element_blank()) +
  ggtitle("Time from case symptom onset or contact exposure to contact notification")


## Old Plots (I think we remove these given they only have Stargel info)
#
# hd_times <- all_param_df |>
#   select(
#     pm_start_date,
#     test_to_positive = hd_cases_positive_from_test_mean_mean,
#     test_to_reached = hd_cases_reached_from_test_mean_mean,
#     contact_from_named = hd_contacts_reached_from_named_mean_mean,
#     contacted_to_test = hd_contacts_test_from_notified_mean
#   ) |> filter(!is.na(test_to_positive))
#
# hd_times_long <- hd_times |> pivot_longer(
#   cols = -pm_start_date,
#   names_to = "param_name",
#   values_to = "param_value"
# )
#
# hd_times_long |>
#   ggplot(aes(x = pm_start_date, y = param_value / 24, fill = param_name)) +
#   geom_col(position = "stack")
#
# hd_times_long |> filter(!is.na(param_value)) |>
#   ggplot(aes(
#     x = pm_start_date, y = param_value / 24,
#     color = param_name, group = param_name)) +
#   geom_line() + ylim(c(0, NA))
#
# all_param_df |> select(contains("_from_")) |> summarise(
#   across(everything(), ~ sum(!is.na(.x)))
# )
#
# hd_times_cleaned <- hd_times_long |> filter(param_name != "contacted_to_test") |>
#   mutate(param_value = ifelse(
#     is.na(param_value), 12, param_value))
#
#
# hd_times_cleaned |>
#   ggplot(aes(x = pm_start_date, y = param_value / 24,
#              fill = param_name)) +
#   geom_col(position = "stack")
