# Creating a split time key.
## One issue in creating timeliness evaluations is in the wide
## array of possible metrics reported, each of which overlap with one
## another.  To address this issue, we impute the subcomponents based on
library(tidyverse)
source("helper_functions/split_time_helpers.R")

dataset <- read_csv("../data_extraction/output/combined_data.csv")
dataset <- read_csv("../data_extraction/output/split_month_data.csv")

dataset <- dataset |> mutate(
  param_type = ifelse(is.na(param_type), "manual", param_type),
  source = paste0(source, "%%%", pm_location, "%%%", param_type)
)



# # Understanding time from Symptoms to testing
# sympt_split <- spliting_all |> filter(hd_summary == "sympt")
# sympt_split_cmpr <- spliting_all |> filter(periods == "BC")
#
# bind_rows(sympt_split, sympt_split_cmpr) |>
#   ggplot(aes(x = pm_start_date, y = param_value, color = periods)) +
#   geom_point() +
#   geom_smooth(method = lm, se = FALSE)
#
# period_aves <- bind_rows(sympt_split, sympt_split_cmpr) |> group_by(periods) |>
#   summarise(ave_time = mean(param_value, na.rm = TRUE))
#
# sympt_to_test_ave <- period_aves |>
#   filter(periods == "ABC") |> pull(ave_time) -
#   period_aves |> filter(periods == "BC") |> pull(ave_time)
# sympt_to_test_ave / 24

single_period_averages

impt_df <-
  bind_rows(bind_cols(periods = "A", ave_time = sympt_to_test_ave),
          single_period_averages |>
            filter(metric == "mean", hd_summary == "yes") |> ungroup() |>
            select(periods, ave_time)) |>
  mutate(prop_time = ave_time / sum(ave_time))


# post_sympt_props |>
#   ggplot(aes(x = pm_start_date, y = prop_time, fill = periods)) +
#   geom_col()
#
# post_sympt_props |>
#   ggplot(aes(x = pm_start_date, y = ave_time, fill = periods)) +
#   geom_col()

impt_df |> write.csv("intermediate_data/props_for_impts.csv", row.names = FALSE)
splitting_info |> write.csv("intermediate_data/splitting_info.csv", row.names = FALSE)
