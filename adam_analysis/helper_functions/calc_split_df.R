calc_split_df <- function(long_param_data, param_ss_df) {
  spliting_all <- long_param_data |> filter(!is.na(periods))
  timing_articles <- spliting_all |> pull(source) |> unique()
  single_period <- spliting_all |>
    filter(periods %in% c("A", "B", "C", "D"))
  single_period <-
    single_period |>
    left_join(param_ss_df, by = c("source", "pm_start_date")) |>
    group_by(source) |>
    tidyr::fill(cases_interviewed, .direction = "updown") |>
    tidyr::fill(contacts_interviewed, .direction = "updown")
  single_period_line_level <- single_period |>
    ungroup() |>
    mutate(
      prop_case_int = cases_interviewed / sum(cases_interviewed, na.rm = TRUE),
      prop_cont_int = contacts_interviewed /
        sum(contacts_interviewed, na.rm = TRUE)
    ) |>
    mutate(
      prop_to_use = ifelse(periods == "D", prop_cont_int, prop_case_int),
      count_to_use = ifelse(
        periods == "D",
        contacts_interviewed,
        cases_interviewed
      )
    )
  single_period_averages <- single_period_line_level |>
    group_by(periods, metric, hd_summary) |>
    summarise(
      ave_time = sum(prop_to_use * param_value, na.rm = TRUE) /
        sum(prop_to_use, na.rm = TRUE),
      records_count = sum(
        !is.na(prop_to_use) &
          !is.na(param_value)
      ),
      count_total = sum(
        count_to_use * as.numeric(!is.na(param_value)),
        na.rm = TRUE
      )
    )
  single_period_averages |>
    mutate(hd_metric = interaction(hd_summary, metric)) |>
    pivot_wider(
      id_cols = c("periods", ),
      names_from = hd_metric,
      values_from = ave_time
    ) |>
    knitr::kable() |>
    print()
  single_period_averages |>
    mutate(hd_metric = interaction(hd_summary, metric)) |>
    pivot_wider(
      id_cols = c("periods", ),
      names_from = hd_metric,
      values_from = records_count
    ) |>
    knitr::kable() |>
    print()
  impt_df <- single_period_averages |>
    filter(metric == "mean") |>
    filter(!is.na(ave_time)) |>
    group_by(periods) |>
    summarise(
      ave_time = sum(ave_time * count_total) /
        sum(count_total)
    ) |>
    ungroup() |>
    mutate(prop_time = ave_time / sum(ave_time))
  return(list("all_metric" = single_period_averages, "impt_df" = impt_df))
}
