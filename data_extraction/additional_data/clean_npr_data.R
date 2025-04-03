# Extracting the number of case investigators for each state
# From the NPR database.

may_2021 <- readxl::read_xlsx("../additional_data/npr_jhu_data.xlsx",
                              sheet = "May 2021")
sept_2021 <- readxl::read_xlsx("../additional_data/npr_jhu_data.xlsx",
                              sheet = "September 2021",
                              range = readxl::cell_rows(1:57))
dec_2021 <- readxl::read_xlsx("../additional_data/npr_jhu_data.xlsx",
                              sheet = "December 2021",
                              range = readxl::cell_rows(1:57))


may_2021 <- may_2021 |>
  select("staff_count" =
      `Current total number of contact tracing staff (not including reserve staff)`,
      state = Representing) |>
  mutate(date = lubridate::myd("05-2021-01")) |>
  mutate(n_staff_count = as.numeric(staff_count))

sept_2021 <- sept_2021 |>
  select("staff_count" =
           `As of September 23, 2020, how many total staff – part or full-time – will you have currently assigned to do contact tracing for COVID-19 cases across the state/territory (not including reserve staff)?`,
      state = Representing) |>
  mutate(date = lubridate::myd("09-2021-01")) |>
  mutate(n_staff_count = as.numeric(staff_count))

dec_2021 <- dec_2021 |>
  select("staff_count" =
           `Total number of contact tracing staff (not including reserve staff)`,
         state = `State/Territory`) |>
  mutate(date = lubridate::myd("12-2021-01")) |>
  mutate(n_staff_count = as.numeric(staff_count),
         staff_count = as.character(staff_count))

fin_df <- bind_rows(may_2021, sept_2021, dec_2021)

fin_df <- fin_df |> mutate(param_name = "staff_hired_ct_count",
                           # TODO: Update this
                           source = "NPR Survey") |>
  select(param_name, pm_start_date = date, param_value = n_staff_count,
         pm_location = state) |>
  mutate(pm_end_date = rollforward(
    pm_start_date, roll_to_first = TRUE, preserve_hms = TRUE))
write.csv(fin_df, "npr_data_clean.csv")


