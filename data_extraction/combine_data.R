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
)

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

combined_data |> group_by(source, param_type, param_name,
                           pm_location, pm_start_date, pm_end_date) |>
  summarise(values = paste0(param_value, collapse = " AND "),
            n = length(unique(param_value))) |> filter(n > 1) |>
  select(-n) |> knitr::kable()
