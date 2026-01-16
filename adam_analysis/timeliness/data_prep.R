# Constructing data for timeliness analysis
library(tidyverse)
source("../helper_functions/read_in_functions.R", chdir = TRUE)

split_folder <- "split_by_month"
non_split_folder <- "no_split"

for (df_type_folder in c(split_folder, non_split_folder)) {
  if (df_type_folder == "split_by_month") {
    comb_data_loc <- "../../data_extraction/output/split_month_data.csv"
  } else {
    comb_data_loc <- "../../data_extraction/output/combined_data.csv"
  }
  start_data <- read.csv(comb_data_loc)
  if ("X" %in% colnames(start_data)) {
    start_data <- start_data |> select(-X)
  }
  start_data <- start_data |>
    mutate(
      pm_start_date = lubridate::mdy(pm_start_date),
      param_type = ifelse(is.na(param_type), "manual", param_type),
      source = paste0(source, "%%%", pm_location, "%%%", param_type),
      nest_source = source,
      nest_start_date = pm_start_date
    )
  param_count_df <- make_param_ss_count_df(start_data)
  write.csv(
    param_count_df,
    paste0("../intermediate_data/", df_type_folder, "/timing_cc_counts.csv"),
    row.names = FALSE
  )
  plot_data <- make_time_plot_data(start_data, param_count_df)
  impt_df <- plot_data$time_split_impt
  write_csv(
    impt_df,
    paste0(
      "../intermediate_data/",
      df_type_folder,
      "/time_impute_df.csv"
    )
  )

  sp_times <- read_csv(paste0(
    "../intermediate_data/",
    df_type_folder,
    "/single_period_times.csv"
  ))
  sp_times <- plot_data$single_period_times |>
    mutate(
      periods = as.factor(periods),
      periods = forcats::fct_recode(periods, !!!periods_to_label),
      periods = factor(
        x = periods,
        levels = names(periods_to_label),
        labels = names(periods_to_label)
      ),
      en_type = ifelse(
        grepl("%\\%\\%digital", source),
        "Digital",
        "Manual"
      )
    )

  write_csv(
    sp_times,
    paste0(
      "../intermediate_data/",
      df_type_folder,
      "/plot_single_period_times.csv"
    )
  )

  filled_monthly <- sp_times |>
    filter(en_type == "Manual") |>
    calc_monthly_averages() |>
    fill_missing_values(imputation_df = impt_df)

  write_csv(
    filled_monthly,
    paste0(
      "../intermediate_data/",
      df_type_folder,
      "/filled_single_period_times.csv"
    )
  )

  filled_monthly_sympt_to_notified <- filled_monthly |>
    group_by(month_year, metric) |>
    summarise(symptom_to_notified = sum(average))

  write_csv(
    filled_monthly_sympt_to_notified,
    paste0(
      "../intermediate_data/",
      df_type_folder,
      "/filled_time_to_notify.csv"
    )
  )
}
