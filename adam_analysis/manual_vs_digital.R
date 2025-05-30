# Comparisons between digital and manual notification timeliness

all_param_df <- read.csv("../data_extraction/manipulated_data/simplified_wide_df_with_date_loc_imputed.csv")
all_param_df$pm_end_date <- guess_dates(all_param_df$pm_end_date)
all_param_df$pm_start_date <- guess_dates(all_param_df$pm_start_date)

hd_times <- all_param_df |>
  select(
    pm_start_date,
    test_to_positive = hd_cases_positive_from_test_mean_mean,
    test_to_reached = hd_cases_reached_from_test_mean_mean,
    contact_from_named = hd_contacts_reached_from_named_mean_mean,
    contacted_to_test = hd_contacts_test_from_notified_mean
  ) |> filter(!is.na(test_to_positive))

hd_times_long <- hd_times |> pivot_longer(
  cols = -pm_start_date,
  names_to = "param_name",
  values_to = "param_value"
)


hd_times_long |> ggplot(aes(x = pm_start_date, y = param_value / 24,
                            fill = param_name)) + geom_col(position = "stack")

hd_times_long |> filter(!is.na(param_value)) |>
  ggplot(aes(
    x = pm_start_date, y = param_value / 24,
    color = param_name, group = param_name)) +
  geom_line() + ylim(c(0, NA))

all_param_df |> select(contains("_from_")) |> summarise(
  across(everything(), ~ sum(!is.na(.x)))
)

hd_times_cleaned <- hd_times_long |> filter(param_name != "contacted_to_test") |>
  mutate(param_value = ifelse(
    is.na(param_value), 12, param_value))


hd_times_cleaned |>
  ggplot(aes(x = pm_start_date, y = param_value / 24,
             fill = param_name)) +
  geom_col(position = "stack")

wa_verify_times <- read_csv("~/Desktop/etoen_wa_notify.csv")
wa_verify_inc <- read_csv("~/Desktop/main_analysis copy.csv")

waver <-
  wa_verify_times |> mutate(
    value = 24 * (mean_etoen - 2.79),
    param_name = "test_to_notified")




comb_data <- waver |>
  select(pm_start_date = monthyear, param_name, param_value = value) |> mutate(type = "digital") |>
  bind_rows(hd_times_cleaned |>
              mutate(pm_start_date = guess_dates(pm_start_date),
                     type = "manual"))

date_labs <- comb_data |>
  mutate(date_label = format(pm_start_date, "%b %Y")) |>
  select(pm_start_date, date_label) |> distinct()
date_lab_func <- function(x) {
  start_date_df <- data.frame(pm_start_date = lubridate::ymd(x))
  date_label <- left_join(start_date_df, date_labs) |>
    pull(date_label)
}

comb_data |>
  ggplot(aes(x = type, y = param_value / 24,
             fill = param_name)) +
  geom_col(position = "stack") +
  facet_wrap(~pm_start_date, nrow = 1, labeller = labeller(
    pm_start_date = date_lab_func)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_altair() + ggthemes::scale_fill_tableau()