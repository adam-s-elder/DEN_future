library(tidyverse)

all_param_df <- read.csv("../data_extraction/manipulated_data/simplified_wide_df_with_date_loc_imputed.csv")


hd_times <- all_param_df |>
  mutate(
    pm_start_date = lubridate::mdy(pm_start_date),
    pm_end_date = lubridate::mdy(pm_end_date)
  ) |> select(
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
                            fill = param_name)) +
  geom_col(position = "stack")

hd_times_long |> ggplot(aes(x = pm_start_date, y = param_value / 24,
                            color = param_name)) +
  geom_line() + ylim(c(0, NA))

all_param_df |> select(contains("_from_")) |> summarise(
  across(everything(), ~ sum(!is.na(.x)))
  )
