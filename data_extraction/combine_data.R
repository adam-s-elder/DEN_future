library(tidyverse)
## First, read through all the directories in this folder.
pos_folds <- list.dirs("datasets", recursive = FALSE)

output_data <- pos_folds |> map(.f = function(x) {
  folds <- list.dirs(x, recursive = FALSE) |>
    gsub(pattern = x, replacement = "") |>
    gsub(pattern = "/", replacement = "")
  if (("input" %in% folds) & ("output" %in% folds)) {
    source(paste0(x, "/clean_data.R"), chdir = TRUE)
    fname <- list.files(paste0(x, "/output"))
    if (length(fname) == 0) {
      warning(x, " has an output directory ", "but no files in it.")
      output <- NULL
    } else {
      output <- read.csv(paste0(x, "/output/", fname))
    }
  } else {
    output <- NULL
  }
  return(output)
})

# Run Zotero query
source("zotero_sql_querry/zot_sql_querry.R", chdir = TRUE)
zotero_data <-
  read.csv("zotero_sql_querry/output/simplified_parameter_df.csv") |>
  mutate(
    pm_start_date = lubridate::mdy(pm_start_date),
    pm_end_date = lubridate::mdy(pm_end_date),
    pm_start_date = format(pm_start_date, "%m/%d/%Y"),
    pm_end_date = format(pm_end_date, "%m/%d/%Y")
  )

combined_data <- bind_rows(
  zotero_data |> select(-X), do.call(output_data, what = bind_rows)
) |> select(-X)

combined_data <- manual_error_fix(combined_data)

# Adding descriptions to variables
var_descr <- read.csv("datasets/variable_definitions.csv") |>
  mutate(Name = str_replace(pattern = "case_",
                            replacement = "cases_", Name),
         Name = str_replace(Name, "contact_", "contacts_"))
source("helper_functions/add_variable_description.R")

combined_data <- add_variable_description(combined_data, var_descr)
params_missing_description <-
  combined_data |> filter(is.na(Desc)) |> pull(param_name) |> unique()
params_missing_description

write.csv(combined_data, "output/combined_data.csv")

# Split the data into month-level data.
case_counts <- read_csv("datasets/baselines/weekly_metrics_by_state.csv")
month_level_data <- split_into_months(combined_data, case_counts)
split_month_data <- month_level_data |> do.call(what = bind_rows) |>
  group_by(source, param_name, param_type, pm_location,
           pm_start_date, pm_end_date) |>
  mutate(
    orig_value = param_value,
    param_value = case_when(
      month_imputation_method == "translate" ~ param_value,
      month_imputation_method == "spread_cases" ~ (period_cases * param_value) /
        sum(period_cases),
      month_imputation_method == "spread_even" ~ param_value / n(),
    ))

split_month_data <- split_month_data |> ungroup() |> group_by(
  start_month_year, end_month_year, pm_location,
  source, param_name, param_type, Desc, month_imputation_method
) |> summarise(
  num_unique_vals = n(),
  param_value = ifelse(
    unique(month_imputation_method) %in% "translate",
    mean(param_value), sum(param_value)
  ) ) |> ungroup() |>
select(pm_start_date = start_month_year,
       pm_end_date = end_month_year,
       pm_location, source, param_name, Desc, param_type,
       month_imputation_method, param_value)

split_month_data |> mutate(
  pm_start_date = format.Date(pm_start_date, "%m/%d/%Y"),
  pm_end_date = format.Date(pm_end_date, "%m/%d/%Y")
) |> write_csv(file = "output/split_month_data.csv")

# write_csv(file = "output/split_month_data.csv", x = split_month_data)

combined_data |> group_by(source, param_type, param_name,
                          pm_location, pm_start_date, pm_end_date) |>
  summarise(values = paste0(param_value, collapse = " AND "),
            n = length(unique(param_value))) |> filter(n > 1) |>
  select(-n) |> knitr::kable()
