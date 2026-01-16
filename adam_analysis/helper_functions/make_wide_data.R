make_wide_data <- function(tidy_data, params_to_keep) {
  wide_data <- tidy_data |>
    mutate(
      pm_start_date = lubridate::mdy(pm_start_date),
      pm_end_date = lubridate::mdy(pm_end_date)
    ) |>
    relocate(pm_start_date, pm_end_date) |>
    filter(param_name %in% params_to_keep) |>
    mutate(param_type = ifelse(is.na(param_type), "manual", param_type)) |>
    select(
      pm_location,
      pm_start_date,
      pm_end_date,
      param_name,
      param_value,
      param_type,
      source
    ) |>
    pivot_wider(
      id_cols = c(
        "pm_location",
        "pm_start_date",
        "pm_end_date",
        "source",
        "param_type"
      ),
      values_from = "param_value",
      names_from = "param_name"
    )
  return(wide_data)
}
