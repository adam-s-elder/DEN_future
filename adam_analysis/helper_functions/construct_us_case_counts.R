construct_us_case_counts <- function(file_path) {
  case_count_data <-
    read_csv(file_path) |>
    mutate(
      start_date = lubridate::mdy(start_date),
      end_date = lubridate::mdy(end_date)
    ) |>
    select(state, start_date, end_date, new_cases)

  ## Creating case count data to use
  us_case_counts <- case_count_data |>
    # mutate(start_date = lubridate::mdy(start_date),
    #        end_date = lubridate::mdy(end_date)) |>
    mutate(days_in_week = 7) |>
    uncount(weights = days_in_week, .id = "days_after") |>
    mutate(
      date = start_date + days((days_after - 1))
    ) |>
    group_by(start_date) |>
    mutate(day_cases = new_cases / 7) |>
    ungroup()

  us_case_counts <- bind_rows(
    us_case_counts,
    us_case_counts |>
      group_by(date) |>
      summarise(day_cases = sum(day_cases, na.rm = TRUE)) |>
      ungroup() |>
      mutate(state = "US")
  ) |>
    select(date, state, day_cases)
  return(us_case_counts)
}
