# Creating the elicitation data frames
library(tidyverse)
library(scales)

source("../helper_functions/read_in_functions.R", chdir = TRUE)

article_level_data <-
  read.csv("../../data_extraction/output/combined_data.csv") |>
  select(-X) |>
  mutate(
    param_type = ifelse(is.na(param_type), "manual", param_type),
    source = paste0(source, "%%%", pm_location, "%%%", param_type),
    nest_source = source,
    nest_start_date = pm_start_date
  )

split_month_data <-
  read.csv("../../data_extraction/output/split_month_data.csv") |>
  mutate(
    param_type = ifelse(is.na(param_type), "manual", param_type),
    source = paste0(source, "%%%", pm_location, "%%%", param_type),
    nest_source = source,
    nest_start_date = pm_start_date
  )

param_count_df <- make_param_ss_count_df(article_level_data)
param_count_df_month <- make_param_ss_count_df(split_month_data)

article_level_with_count <-
  add_var_sample_size(article_level_data, param_count_df)

month_level_with_count <-
  add_var_sample_size(split_month_data, param_count_df_month)

article_level_elicit_data <- clean_elic_data(article_level_with_count)
month_level_elicit_data <- clean_elic_data(month_level_with_count)

write_csv(
  article_level_elicit_data,
  file = "../intermediate_data/article_level_elicit.csv"
)
write_csv(
  month_level_elicit_data,
  file = "../intermediate_data/month_level_elicit.csv"
)
