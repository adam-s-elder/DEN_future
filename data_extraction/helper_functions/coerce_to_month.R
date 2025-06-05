# This function takes the cleaned dataframe and splits out reported metrics
# into month long periods.  The need for this exists because we have created
# each of functions and visualizations to only consider start date.

split_into_months <- function(long_data, state_level_cases) {
  day_level_cases <- state_level_cases |> mutate(days_in_week = 7) |>
    uncount(weights = days_in_week, .id = "days_after") |>
    mutate(
      start_date = lubridate::mdy(start_date),
      date = start_date + days((days_after - 1))) |>
    group_by(state, start_date) |>
    mutate(day_cases = new_cases / 7) |>
    select(state, start_date, date, day_cases)
  us_cases <- day_level_cases |> group_by(date) |>
    summarise(day_cases = sum(day_cases),
              state_count = n())
  day_level_cases <- day_level_cases |> ungroup() |> select(-start_date) |>
    bind_rows(us_cases |> mutate(state = "US") |> select(-state_count))

  long_data <- long_data |>
    mutate(
      orig_start_date = lubridate::mdy(pm_start_date),
      orig_end_date = lubridate::mdy(pm_end_date),
      days_to_end = lubridate::ceiling_date(orig_start_date, "month") -
        orig_start_date,
      days_to_next = orig_end_date - lubridate::floor_date(orig_end_date, "month"),
      shifted_start_date = ifelse(
        days_to_end < 3,
        lubridate::ceiling_date(orig_start_date, "month"),
        orig_start_date) |>  lubridate::as_date(),
      shifted_end_date = ifelse(
        days_to_next < 3,
        lubridate::floor_date(orig_end_date, "month") - days(1),
        orig_end_date) |> lubridate::as_date(),
      month_span = 12 * (year(shifted_end_date) - year(shifted_start_date)) +
        month(shifted_end_date) - month(shifted_start_date) + 1,
      )
  expanded_data <-
    long_data |> group_by(row_number()) |> nest() |> pull(data) |>
    map(.f = function(row) {
      impt_type <- row$month_imputation_method
      expanded_dates <-
        row |> uncount(month_span, .id = "month_shift") |>
        mutate(
          month_shift = month_shift - 1,
          start_month_year = (lubridate::floor_date(
            (shifted_end_date %m+%
               ( -1 * months(max(month_shift) - month_shift))),
            "month")) |> lubridate::as_date(),
          end_month_year = (lubridate::ceiling_date(
            shifted_start_date %m+% months(month_shift),
            "month") - days(1)) |> lubridate::as_date(),
          spread_start_date = ifelse(
            month_shift == 0,
            shifted_start_date, start_month_year) |> lubridate::as_date(),
          spread_end_date = ifelse(
            month_shift == max(month_shift),
            shifted_end_date, end_month_year) |> lubridate::as_date()
        )
      expanded_dates <-
        expanded_dates |> left_join(state_translate, by = "pm_location")
      expanded_dates$period_cases <- NA_real_
      for (row_num in 1:nrow(expanded_dates)) {
        expanded_dates$period_cases[row_num] <- provide_case_counts(
          start_date = expanded_dates$spread_start_date[row_num],
          end_date = expanded_dates$spread_end_date[row_num],
          state = expanded_dates$state[row_num],
          dl_cases = day_level_cases)
      }
      return(expanded_dates)
    })
  return(expanded_data)
}

provide_case_counts <- function(state, start_date, end_date, dl_cases) {
  total_cases <- dl_cases |>
    filter(state == !!state,
           date >= !!start_date,
           date <= !!end_date) |>
    pull(day_cases) |> sum(na.rm = TRUE)
  return(total_cases)
}

# Location translation dataframe

state_translate <- c(
  "Maricopa" = "AZ",
  "Chicago" = "IL",
  "VT" = "VT",
  "San Francisco" = "CA",
  "New York" = "NY",
  "New Haven" = "CT",
  "state_a" = "US",
  "Members of large CA health care plan" = "CA",
  "Fulton County" = "GA",
  "New York City communities with higher than average COVID-19 incidence rates" = "NY",
  "United States*" = "US",
  "Maryland" = "MD",
  "Kansas" = "KS",
  "United States" = "US",
  "DE" = "DE",
  "WA" = "WA",
  "Mecklenburg" = "NC",
  "Randolph County" = "NC",
  "across US" = "US",
  "loc_N" = "US",
  "loc_M" = "US",
  "loc_L" = "US",
  "loc_K" = "US",
  "loc_J" = "US",
  "loc_I" = "US",
  "loc_H" = "US",
  "loc_G" = "US",
  "loc_F" = "US",
  "loc_E" = "US",
  "loc_D" = "US",
  "loc_C" = "US",
  "loc_B" = "US",
  "loc_A" = "US",
  "New York City" = "NY",
  "Philadelphia" = "PA",
  "central WA" = "WA",
  "San Fransico" = "CA",
  "Salt Lake City" = "UT",
  "New York except NYC" = "NY",
  "Boston University" = "MA",
  "Maimi University" = "FL",
  "North Carolina" = "NC",
  "Connecticut" = "CT",
  "Michigan" = "MI",
  "King County" = "WA",
  "California" = "CA",
  "Washington" = "WA",
  "Oak Ridge National Laboratory" = "TN",
  "Alaska" = "AK",
  "64 HD accross US and collonies" = "US",
  "New Hampshire" = "NH",
  "Georgia" = "GA",
  "Minnesota" = "MN",
  "Nebraska" = "NE",
  "Oregon" = "OR",
  "Nevada" = "NV",
  "South Carolina" = "SC",
  "Pennsylvania" = "PA",
  "Texas" = "TX",
  "Missouri" = "MO",
  "North Dakota" = "ND",
  "Mississippi" = "MS",
  "Utah" = "UT",
  "Tennessee" = "TN",
  "Massachusetts" = "MA",
  "Northern Mariana Islands (CNMI)" = "MP",
  "Delaware" = "DE",
  "Vermont" = "VT",
  "Oklahoma" = "OK",
  "Maine" = "ME",
  "Kentucky" = "KT",
  "Louisiana" = "LA",
  "West Virginia" = "WV",
  "District of Columbia" = "DC",
  "Iowa" = "IA",
  "Arkansas" = "AR",
  "Idaho" = "ID",
  "New Jersey" = "NJ",
  "Wisconsin" = "WI",
  "Colorado" = "CO",
  "Arizona" = "AZ",
  "Alabama" = "AL",
  "American Samoa" = "AS",
  "Florida" = "FL",
  "Guam" = "GU",
  "Hawaii" = "HI",
  "Illinois" = "IL",
  "Indiana" = "IN",
  "Montana" = "MT",
  "New Mexico" = "NM",
  "Northern Mariana Islands" = "MP",
  "Ohio" = "MP",
  "Puerto Rico" = "PR",
  "Rhode Island" = "RI",
  "South Dakota" = "SD",
  "U.S. Virgin Islands" = "VI",
  "Virginia" = "VA",
  "Wyoming" = "WY",
  "US Virgin Islands" = "VI",
  "CA" = "CA",
  "DC" = "DC",
  "MA" = "MA",
  "NV" = "NV",
  "AZ" = "AZ",
  "MN" = "MN",
  "HI" = "HI",
  "AK" = "AK",
  "NM" = "NM",
  "MO" = "MO",
  "MD" = "MD"
)

state_translate <- data.frame(
  pm_location = names(state_translate),
  state = state_translate, row.names = NULL
)
