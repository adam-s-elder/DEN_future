library(tidyverse)
library(scales)
source("helper_functions/make_time_dfs.R")
source("helper_functions/split_time_helpers.R")

split_folder <- "split_by_month"
non_split_folder <- "no_split"

article_period_data <-
  read.csv("../data_extraction/output/combined_data.csv") |>
  select(-X) |>
  mutate(
    param_type = ifelse(is.na(param_type), "manual", param_type),
    source = paste0(source, "%%%", pm_location, "%%%", param_type),
    nest_source = source, nest_start_date = pm_start_date)

split_month_data <-
  read.csv("../data_extraction/output/split_month_data.csv") |>
  mutate(
    param_type = ifelse(is.na(param_type), "manual", param_type),
    source = paste0(source, "%%%", pm_location, "%%%", param_type),
    nest_source = source, nest_start_date = pm_start_date)

param_count_df <- make_param_ss_count_df(article_period_data)
param_count_df_month <- make_param_ss_count_df(split_month_data)
write.csv(
  param_count_df,
  paste0("intermediate_data/", non_split_folder, "/timing_cc_counts.csv"),
         row.names = FALSE)
write.csv(
  param_count_df_month,
  paste0("intermediate_data/", split_folder, "/timing_cc_counts.csv"),
         row.names = FALSE)

split_month_time_plot_data <-
  make_time_plot_data(split_month_data, split_info = splitting_info,
                      param_count_df_month)
non_split_time_plot_data <-
  make_time_plot_data(article_period_data, split_info = splitting_info,
                      param_count_df)

# Writing month_split_data
## Split by month

write.csv(split_month_time_plot_data$single_period_times,
          paste0("intermediate_data/split_by_month/single_period_times.csv"),
          row.names = FALSE)

## Not split by month
write.csv(non_split_time_plot_data$single_period_times,
          paste0("intermediate_data/no_split/single_period_times.csv"),
          row.names = FALSE)
