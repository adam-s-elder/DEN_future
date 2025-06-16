# Comparisons between digital and manual notification timeliness
library(tidyverse)
source("helper_functions/make_time_dfs.R")
source("helper_functions/plotting_functions.R")

# Single period time plots
library(tidyverse)
plot_data_folder <- "split_by_month"
impt_period_lens <- read_csv("intermediate_data/props_for_impts.csv")
impt_df <- read_csv("intermediate_data/props_for_impts.csv")
sp_times <- read_csv(paste0("intermediate_data/",
                            plot_data_folder,
                            "/single_period_times.csv"))


sp_times <- sp_times |> mutate(
   periods = as.factor(periods),
  periods = forcats::fct_recode(periods, !!!periods_to_label),
  periods = factor(x = periods,
                       levels = names(periods_to_label),
                       labels = names(periods_to_label)),
  en_type = ifelse(
    grepl("\\%\\%\\%digital", source),
    "Digital", "Manual")
  )

manual_monthly_filled <-
  calc_monthly_averages(sp_times |> filter(en_type == "Manual")) |>
  fill_missing_values(imputation_df = impt_df) |>
  group_by(month_year, metric) |> summarise(symptom_to_notified = sum(average))

wa_verify_times <- read_csv("~/Desktop/etoen_wa_notify.csv")

digital_monthly_filled <-
  calc_monthly_averages(
    sp_times |>
      filter(en_type == "Digital", grepl("California", source)))  |>
  fill_missing_values(imputation_df = impt_df) |>
  group_by(month_year, metric) |> summarise(symptom_to_notified = sum(average))

waver <- wa_verify_times |> mutate(
    value = 24 * (mean_etoen),
    param_name = "test_to_notified")


comb_data <- waver |>
  select(month_year = monthyear, param_name, symptom_to_notified = value) |>
  mutate(type = "WA Notify") |>
  bind_rows(manual_monthly_filled |> mutate(type = "Manual (All Sources)"),
            digital_monthly_filled |> mutate(type = "CA Notiy"))

comb_data |> filter(!(metric %in% "med")) |>
  ggplot(aes(x = month_year, y = symptom_to_notified / 24,
             fill = type)) +
  geom_col(position = position_dodge2(preserve = "single"), width = 25) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_altair() +
  scale_fill_manual("", values =
                       ggthemes::tableau_color_pal("Tableau 10")(3)
                     # breaks = c("digital", "manual", "other"),
                     # labels = c("Digital", "Manual", "Digital Other")
                    ) +
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
