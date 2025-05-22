library(tidyverse)
library(scales)
source("helper_functions/make_time_dfs.R")

timing_ss <- read_csv("intermediate_data/timing_cc_counts.csv")
impt_period_lens <- read_csv("intermediate_data/props_for_impts.csv")
split_info <- read.csv("intermediate_data/splitting_info.csv")
long_data <- read.csv("../data_extraction/output/combined_data.csv") |>
  select(-X)

kested_data <- long_data |> filter(grepl("_from_", param_name)) |>
  mutate(
    param_type = ifelse(is.na(param_type), "manual", param_type),
    source = paste0(source, "%%%", pm_location, "%%%", param_type),
    nest_source = source, nest_start_date = pm_start_date) |>
  group_by(nest_source, nest_start_date, param_type) |>
  tidyr::nest()

initial_imputation <- nested_data |>
  # filter(grepl("Campus", nest_source)) |>
  mutate(stacked_data = map(
    data, calc_time_df, split_info = split_info,
    ave_period_lens = impt_period_lens))

imputed_df <- initial_imputation$stacked_data |> do.call(what = bind_rows)

loc_df <- long_data |> filter(grepl("_from_", param_name)) |>
  mutate(param_type = ifelse(is.na(param_type), "manual", param_type),
    source = paste0(source, "%%%", pm_location,
                    "%%%", param_type)) |>
  select(source, pm_location) |>
  group_by(source, pm_location) |>
  filter(row_number() == 1)

comb_data <- imputed_df |>
  left_join(loc_df, by = "source") |> mutate(
    date = lubridate::mdy(pm_start_date)
  ) |> arrange(desc(date), pm_location)

final_df <- comb_data |>
  mutate(pm_start_date = lubridate::mdy(pm_start_date)) |>
  left_join(timing_ss, by = c("source", "pm_start_date"))

labeled_data <- final_df |> filter(metric != "max") |> mutate(
  digital = ifelse(grepl("digital", source), "digital", "manual"),
  pm_location = gsub(pattern = "communities with higher than average COVID-19 incidence rates", "", pm_location),
  comb_loc_date = factor(
    x = paste0("(", digital, ") ", pm_location, ",", date),
    levels = paste0("(", digital, ") ", pm_location, ",", date),
    labels = paste0("(", digital, ") ", pm_location, ", ", date)),
  periods = forcats::fct_rev(as.factor(periods)),
          impt_type_alpha = case_when(
            impt_type == "reported" ~ 0,
            impt_type == "inner" ~ 0.1,
            impt_type == "outer_abs" ~ 0.7,
            impt_type == "outer_rel" ~ 0.7)
) |> mutate(count = ifelse(periods == "D",
                           contacts_interviewed, cases_interviewed))

period_median_count <- labeled_data |>
  filter(df_type == "outer_abs", impt_type != 0.7) |>
  group_by(periods, metric) |>
  summarise(impt_median = median(count, na.rm = TRUE))

impt_count_data <- labeled_data |>
  left_join(period_median_count, by = c("periods", "metric")) |>
  group_by(periods, metric) |>
  mutate(
    impt_count = is.na(count),
    count = ifelse(impt_count, impt_median, count),
    metric_color = case_when(
      impt_count ~ "imputed",
      metric == "mean" ~ "mean",
      metric == "med" ~ "med"
  ))

sp_times <-  impt_count_data |>
  filter(!grepl("Using Automation", source)) |>
  filter(df_type == "outer_abs", impt_type_alpha != 0.7, metric != "max") |>
  mutate(
    month_year = lubridate::floor_date(pm_start_date, "month"),
    incl = 1 - as.numeric(impt_count),
  )

monthly_aves <- sp_times |> group_by(month_year, metric, periods) |>
  summarise(
    num_sources = n(),
    overall_count = sum(count),
    inclusive_ave = sum(count * param_value) / overall_count,
    exclusive_ave = sum(incl * count * param_value) / sum(incl * count)
  ) |> mutate(count = overall_count)

write.csv(imputed_df, "intermediate_data/all_period_times.csv",
          row.names = FALSE)

write.csv(sp_times, "intermediate_data/single_period_times.csv",
          row.names = FALSE)

write.csv(monthly_aves, "intermediate_data/monthly_averages.csv",
          row.names = FALSE)

