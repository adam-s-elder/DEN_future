# Cacluate CDC reported cases
calc_cdc_cases <- function(case_counts, min_date, max_date) {
  us_case_counts <- case_counts |>
    mutate(
      start_date = lubridate::mdy(start_date),
      end_date = lubridate::mdy(end_date)
    ) |>
    group_by(start_date, end_date) |>
    summarise(
      total_cases = sum(new_cases, na.rm = TRUE)
    ) |>
    mutate(days_in_week = 7) |>
    uncount(weights = days_in_week, .id = "days_after") |>
    mutate(
      date = start_date + days((days_after - 1))
    ) |>
    group_by(start_date) |>
    mutate(day_cases = total_cases / 7) |>
    select(start_date, date, day_cases) |>
    mutate(month_year = lubridate::round_date(date, "month")) |>
    group_by(month_year) |>
    summarise(param_value = sum(day_cases, na.rm = TRUE))

  cdc_cases <- us_case_counts |>
    mutate(
      pm_start_date = month_year,
      param_name = "US_cases"
    ) |>
    select(-month_year) |>
    filter(pm_start_date >= min_date, pm_start_date <= max_date)
  return(cdc_cases)
}
