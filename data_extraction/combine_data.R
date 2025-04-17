library(tidyverse)
zotero_data <- read.csv("zotero_sql_querry/output/simplified_parameter_df.csv")
stargel_data <-
  read.csv("entire_tables/stargel_2022/output/final_combined.csv") |>
  mutate(param_type = "manual")
npr_data <- read.csv("additional_data/npr_data_clean.csv") |>
  mutate(param_type = "manual")

combined_data <-
  bind_rows(stargel_data, npr_data |> select(-X),
          zotero_data |> select(-X))

# Adding descriptions to variables
var_descr <- read.csv("additional_data/variable_definitions.csv") |>
  mutate(Name = str_replace(pattern = "case_",
                            replacement = "cases_", Name),
         Name = str_replace(Name, "contact_", "contacts_"))
source("helper_functions/add_variable_description.R")

combined_data <- add_variable_description(combined_data, var_descr)
params_missing_description <-
  combined_data |> filter(is.na(Desc)) |> pull(param_name) |> unique()
params_missing_description

write.csv(combined_data, "output/combined_data.csv")

combined_data |> group_by(source, param_type, param_name,
                           pm_location, pm_start_date, pm_end_date) |>
  summarise(values = paste0(param_value, collapse = " AND "),
            n = length(unique(param_value))) |> filter(n > 1) |>
  select(-n) |> knitr::kable()
