library(tidyverse)
zotero_data <- read.csv("zotero_sql_querry/output/simplified_parameter_df.csv")
stargel_data <- read.csv("entire_tables/stargel_2022/output/final_combined.csv")
npr_data <- read.csv("additional_data/npr_data_clean.csv")

combined_data <-
  bind_rows(stargel_data, npr_data |> select(-X),
          zotero_data |> select(-X))

write.csv(combined_data, "output/combined_data.csv")
