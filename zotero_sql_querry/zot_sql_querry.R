zotero_loc <- "~/Zotero/zotero.sqlite"
param_order <- c("param_value", "pm_start_date", "pm_end_date", "pm_location")

library(DBI)
library(RSQLite)
library(purrr)
library(dplyr)
library(rvest)
library(stringr)

portaldb <- dbConnect(RSQLite::SQLite(), zotero_loc)

dt_names <- dbListTables(portaldb)
name_list <- dt_names |> map(.f = function(df_name) {
  collect(tbl(portaldb, df_name)) |> colnames()
})
param_dictionary <-
  map2(name_list, dt_names, .f = function(x, y){
  bind_cols(param = x, dt_name = y)
}) |> do.call(what = rbind)
dbListFields(portaldb, "itemNotes")
dbListFields(portaldb, "tags")

annotation_table_df <- collect(tbl(portaldb, "itemAnnotations"))
annotation_table_df <- annotation_table_df |> mutate(
  param_value = stringr::str_extract(comment, pattern = "\\{\\{.*\\}\\}"),
  param_value = stringr::str_remove_all(param_value, "\\{|\\}")
)


# Tag data table
tag_idx <- collect(tbl(portaldb, "tags"))
item_tags <- collect(tbl(portaldb, "itemTags"))
collect(tbl(portaldb, "tags")) |> View()

annotations_and_tags <-
  left_join(item_tags,
            annotation_table_df |> select(-type), by = "itemID") |>
  filter(!is.na(comment))

annotations_and_tags <-
  annotations_and_tags |> left_join(tag_idx, by = "tagID")

# Find note's parent items
attachments_table_df <- collect(tbl(portaldb, "itemAttachments"))
attachment_ids <- annotations_and_tags$parentItemID |> unique()
annotations_and_tags <- left_join(annotations_and_tags,
          attachments_table_df |> filter(itemID %in% attachment_ids) |>
            rename(articleItemID = parentItemID,
                   parentItemID = itemID),
          by = "parentItemID")

parameter_item_idx <- annotations_and_tags |> filter(name == "parameter") |>
  pull(itemID)

parameter_df <- annotations_and_tags |>
  filter(itemID %in% parameter_item_idx) |>
  filter(name != "parameter")

parameter_notes_df <- full_join(
  item_tags |> filter(itemID %in% parameter_item_idx),
  parameter_df, relationship = "many-to-many", by = "itemID")

# Getting article title / names
item_data_df <- collect(tbl(portaldb, "itemData"))
item_data_values_df <- collect(tbl(portaldb, "itemDataValues"))
field_df <- collect(tbl(portaldb, "fields"))

wanted_field_df <- field_df |> filter(fieldName %in% c("title"))

value_id_df <- item_data_df |>
  filter(fieldID %in% wanted_field_df$fieldID) |>
  left_join(wanted_field_df, by = "fieldID")

wanted_field_value_df <-
  left_join(value_id_df, item_data_values_df, by = "valueID")

parameter_notes_df <- left_join(
  parameter_notes_df,
  wanted_field_value_df |> rename(articleItemID = itemID),
  by = "articleItemID")


parse_params <- function(str_vector) {
  split_list <- str_split(str_vector, pattern = ",")
  failed_parse_length <-
    which((split_list |> map(length) |> do.call(what = c)) != 4)
  if (length(failed_parse_length) > 0) {
    warning("Some parameters were not coded properly and have an incorrect format: \n",
            paste0(do.call(split_list[failed_parse_length], what = c),
                       collapse = "\n"),
            "\nCheck these rows: ",
            paste0(failed_parse_length, collapse = ", "))
  }
  split_list <- split_list[-failed_parse_length]
  split_param_df <- list_transpose(split_list) |>
    map(trimws) |> as.data.frame()
  colnames(split_param_df) <- param_order
  na_mat <- matrix(NA, nrow = length(failed_parse_length), ncol = 4)
  na_df <- as.data.frame(na_mat)
  colnames(na_df) <- colnames(split_param_df)
  split_param_df <- bind_rows(split_param_df, na_df)
  base_idx <- 1:length(str_vector)
  fix_idx <- c(setdiff(base_idx, failed_parse_length), failed_parse_length)
  return(split_param_df[match(base_idx, fix_idx), ])
}

parameter_notes_df <- bind_cols(
  parameter_notes_df |> rename(param_string = param_value),
  parse_params(parameter_notes_df$param_value)
)

collapsed_vals <- parameter_notes_df |> select(all_of(param_order)) |> apply(
  1, FUN = paste0, collapse = ", "
)

non_match <- which(parameter_notes_df$param_string != collapsed_vals)
non_match <- setdiff(non_match, which(is.na(parameter_notes_df$param_value)))
if (length(non_match) > 0) {
  warning("Some parsing failed. Check rows ",
          paste0(non_match, collapse = ", "))
}

simple_param_df <- parameter_notes_df |> select(
  param_name = name,
  all_of(param_order),
  source = value
) |> filter(!is.na(param_value))

write.csv(simple_param_df, file = "simplified_parameter_df.csv")
write.csv(parameter_notes_df, file = "full_parameter_df.csv")

# Notes Datatable (this has become depricated since I realized that
# we can directly access annotations and that single notes (created for
# whole articles rather than single annotations) wouldn't have all
# the information we wanted in as delineated of a format as desired).
#
# notes_table <- tbl(portaldb, "itemNotes")
# notes_table_df <- collect(notes_table)
# notes_list <- notes_table_df |> pull(note) |> map(read_html) |>
#   map(html_text2)
# one_annotation <- notes_table_df |> filter(itemID == max(itemID))
# notes_table_df <- notes_table_df |> mutate(
#   param_value = stringr::str_extract(note, pattern = "\\{\\{.*\\}\\}"),
#   param_value = stringr::str_remove_all(param_value, "\\{|\\}")
# )
# notes_table_df