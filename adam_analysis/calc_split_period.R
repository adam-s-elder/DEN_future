# Creating a split time key.
## One issue in creating timeliness evaluations is in the wide
## array of possible metrics reported, each of which overlap with one
## another.  To address this issue, we impute the subcomponents based on
library(tidyverse)

dataset <- read_csv("../data_extraction/output/combined_data.csv")
dataset <- dataset |> mutate(
  pm_start_date = lubridate::mdy(pm_start_date),
  pm_end_date = lubridate::mdy(pm_end_date),
  param_type = ifelse(is.na(param_type), "manual", param_type),
)

dataset <- dataset |> mutate(
  param_type = ifelse(is.na(param_type), "manual", param_type),
  source = paste0(source, "%%%", pm_location, "%%%", param_type))

splitting_info <- matrix(
  c("hd_contacts_test_from_notified_median",  "med", "E", "yes",
    "hd_cases_positive_from_test_mean_median", "med", "B", "yes",
    "hd_cases_reached_from_positive_mean_median", "med", "C", "yes",
    "hd_contacts_reached_from_named_mean_median", "med", "D", "yes",
    "cases_test_from_sympt_mean", "mean", "A", "no",
    "cases_test_from_sympt_med", "med", "A", "no",
    "cases_positive_from_test_mean", "mean", "B", "no",
    "cases_positive_from_test_med",  "med",  "B", "no",
    "hd_cases_positive_from_test_mean_mean", "mean", "B", "yes",
    "cases_reached_from_exposure_mean", "mean", "ABC", "no",
    "cases_reached_from_symptom_mean", "mean", "ABC", "sympt",
    "cases_reached_from_symptom_med", "med", "ABC", "sympt",
    "cases_reached_from_test_mean", "mean", "BC", "no",
    "cases_reached_from_test_med", "med", "BC", "no",
    "hd_cases_reached_from_positive_mean_mean", "mean", "C", "yes",
    "cases_reached_from_positive_max", "max", "C", "no",
    "cases_reached_from_positive_med", "med", "C", "no",
    "contacts_reached_from_exposure_mean", "mean", "ABCD", "no",
    "contacts_reached_from_exposure_med", "med", "ABCD", "no",
    "contacts_reached_from_symptom_med", "med", "ABCD", "sympt",
    "contacts_reached_from_test_mean", "mean", "BCD", "no",
    "contacts_reached_from_test_med", "med", "BCD", "no",
    "contacts_reached_from_positive_mean", "mean", "CD", "no",
    "contacts_reached_from_named_mean", "mean", "D", "no",
    "contacts_reached_from_named_med", "med", "D", "no",
    "hd_contacts_reached_from_named_mean_mean", "mean", "D", "yes",
    "hd_contacts_test_from_notified_mean", "mean", "E", "yes"
  ),
  byrow = TRUE, ncol = 4,
  dimnames = list(NULL,
                  c("param_name", "metric", "periods", "hd_summary"))) |>
  as.data.frame()

spliting_all <- dataset |> left_join(splitting_info, by = "param_name") |>
  filter(!is.na(periods))

timing_articles <- spliting_all |> pull(source) |> unique()
case_and_contact_counts <- dataset |> filter(source %in% timing_articles) |>
  filter(param_name %in% c(
    "cases_reached_count", "cases_interviewed_count",
    "contacts_reached_count", "contacts_interviewed_count"))
case_and_contact_counts$pm_start_date[
  case_and_contact_counts$param_value == 11003] <- lubridate::mdy("9/26/2021")

cc_long <- case_and_contact_counts |> pivot_wider(
  id_cols = c("source", "pm_start_date"),
  names_from = param_name, values_from = param_value
)

case_ci_ratio <- cc_long |>
  filter(!is.na(cases_reached_count), !is.na(cases_interviewed_count)) |>
  summarise(total_reached = sum(cases_reached_count),
            total_interviewed = sum(cases_interviewed_count)) |>
  mutate(ratio = total_interviewed / total_reached) |> pull(ratio)

contact_ci_ratio <- cc_long |>
  filter(!is.na(contacts_reached_count), !is.na(contacts_interviewed_count)) |>
  summarise(total_reached = sum(contacts_reached_count),
            total_interviewed = sum(contacts_interviewed_count)) |>
  mutate(ratio = total_interviewed / total_reached) |> pull(ratio)

count_df <- cc_long |> mutate(
  cases_interviewed = ifelse(
    is.na(cases_interviewed_count),
    cases_reached_count * case_ci_ratio,
    cases_interviewed_count),
  contacts_interviewed = ifelse(is.na(contacts_interviewed_count),
    contacts_reached_count * contact_ci_ratio,
    contacts_interviewed_count),
) |> select(source, cases_interviewed, contacts_interviewed, pm_start_date)

write.csv(count_df, "intermediate_data/timing_cc_counts.csv",
          row.names = FALSE)

single_period <- spliting_all |>
  filter(periods %in% c("A", "B", "C", "D"))

count_df |> filter(grepl("Francisco", source)) |> pull(source) ==
  single_period |> filter(grepl("Francisco", source)) |> pull(source)

single_period <-
  single_period |> left_join(count_df, by = c("source", "pm_start_date")) |>
  group_by(source) |>
  tidyr::fill(cases_interviewed, .direction = "updown") |>
  tidyr::fill(contacts_interviewed, .direction = "updown")

single_period_line_level <- single_period |> ungroup() |> mutate(
  prop_case_int = cases_interviewed / sum(cases_interviewed, na.rm = TRUE),
  prop_cont_int =
    contacts_interviewed / sum(contacts_interviewed, na.rm = TRUE)
) |> mutate(
  prop_to_use = ifelse(periods == "D", prop_cont_int, prop_case_int),
  count_to_use = ifelse(periods == "D", contacts_interviewed,
                        cases_interviewed)
)

single_period_averages <- single_period_line_level |>
  group_by(periods, metric, hd_summary) |> summarise(
  ave_time = sum(prop_to_use * param_value, na.rm = TRUE) /
    sum(prop_to_use, na.rm = TRUE),
  records_count = sum(!is.na(prop_to_use)),
  count_total = sum(count_to_use, na.rm = TRUE)
)

single_period_averages |>
  mutate(hd_metric = interaction(hd_summary, metric)) |>
  pivot_wider(id_cols = c("periods",), names_from = hd_metric,
              values_from = ave_time) |> View()

single_period_averages |>
  mutate(hd_metric = interaction(hd_summary, metric)) |>
  pivot_wider(id_cols = c("periods",), names_from = hd_metric,
              values_from = records_count) |> View()


# Understanding time from Symptoms to testing
sympt_split <- spliting_all |> filter(hd_summary == "sympt")
sympt_split[sympt_split$param_value == 1.24, "param_value"] <- 72
sympt_split_cmpr <- spliting_all |> filter(periods == "BC")

bind_rows(sympt_split, sympt_split_cmpr) |>
  ggplot(aes(x = pm_start_date, y = param_value, color = periods)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

period_aves <- bind_rows(sympt_split, sympt_split_cmpr) |> group_by(periods) |>
  summarise(ave_time = mean(param_value, na.rm = TRUE))

sympt_to_test_ave <- period_aves |>
  filter(periods == "ABC") |> pull(ave_time) -
  period_aves |> filter(periods == "BC") |> pull(ave_time)
sympt_to_test_ave / 24

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
