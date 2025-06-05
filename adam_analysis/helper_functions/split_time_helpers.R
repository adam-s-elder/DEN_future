
make_param_ss_count_df <- function(long_param_df) {
  case_and_contact_counts <- long_param_df |>
    filter(param_name %in% c(
      "cases_reached_count", "cases_interviewed_count",
      "contacts_reached_count", "contacts_interviewed_count"))
  case_and_contact_counts$pm_start_date[
    case_and_contact_counts$param_value == 11003] <-
    lubridate::mdy("9/26/2021")

  cc_long <- case_and_contact_counts |> pivot_wider(
    id_cols = c("source", "pm_start_date"),
    names_from = param_name, values_from = param_value)

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
  return(count_df)
}

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
# count_df |> filter(grepl("Francisco", source)) |> pull(source) ==
#   single_period |> filter(grepl("Francisco", source)) |> pull(source)

calc_split_df <- function(long_param_data, param_ss_df){
  spliting_all <- long_param_data |> filter(!is.na(periods))
  timing_articles <- spliting_all |> pull(source) |> unique()
  single_period <- spliting_all |>
    filter(periods %in% c("A", "B", "C", "D"))
  single_period <-
    single_period |> left_join(param_ss_df,
                               by = c("source", "pm_start_date")) |>
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
      records_count = sum(!is.na(prop_to_use) &
                            !is.na(param_value)),
      count_total = sum(
        count_to_use * as.numeric(!is.na(param_value)),
        na.rm = TRUE)
    )
  single_period_averages |>
    mutate(hd_metric = interaction(hd_summary, metric)) |>
    pivot_wider(id_cols = c("periods",), names_from = hd_metric,
                values_from = ave_time) |> knitr::kable() |> print()
  single_period_averages |>
    mutate(hd_metric = interaction(hd_summary, metric)) |>
    pivot_wider(id_cols = c("periods",), names_from = hd_metric,
                values_from = records_count) |> knitr::kable() |> print()
  impt_df <- single_period_averages |> filter(metric == "mean") |>
    filter(!is.na(ave_time)) |> group_by(periods) |>
    summarise(ave_time = sum(ave_time * count_total) /
                sum(count_total)) |> ungroup() |>
    mutate(prop_time = ave_time / sum(ave_time))
  return(list("all_metric" = single_period_averages,
              "impt_df" = impt_df))
}
