zotero_loc <- "~/Zotero/zotero.sqlite"
param_order <- c("param_value", "pm_start_date", "pm_end_date", "pm_location")

library(DBI)
library(RSQLite)
library(purrr)
library(dplyr)
library(rvest)
library(stringr)

helper_scripts <- list.files("helper_functions/")
for (x in helper_scripts) {
  source(paste0("helper_functions/", x))
}

portaldb <- dbConnect(RSQLite::SQLite(), zotero_loc)

dt_names <- dbListTables(portaldb)
name_list <- dt_names |> map(.f = function(df_name) {
  collect(tbl(portaldb, df_name)) |> colnames()
})
names(name_list) <- dt_names
name_list |> keep(
  .p = function(x) any(grepl("itemID", x))
)

param_dictionary <-
  map2(name_list, dt_names, .f = function(x, y){
  bind_cols(param = x, dt_name = y)
}) |> do.call(what = rbind)

dbListFields(portaldb, "itemNotes")
dbListFields(portaldb, "libraries")
dbListFields(portaldb, "tags")

# Libraries
collection_table <- collect(tbl(portaldb, "collections"))
collection_items_table <- collect(tbl(portaldb, "collectionItems"))
sr_include_collection_id <-
  collection_table |> filter(collectionName == "sr_included_articles") |>
  pull(collectionID)
items_to_keep <-
  collection_items_table |> filter(collectionID == sr_include_collection_id) |>
  pull(itemID)

annotation_table_df <- collect(tbl(portaldb, "itemAnnotations"))
annotation_table_df <- annotation_table_df |> mutate(
  param_value = stringr::str_extract(comment, pattern = "\\{\\{.*\\}\\}"),
  param_value = stringr::str_remove_all(param_value, "\\{|\\}")
)

# Tag data table
tag_idx <- collect(tbl(portaldb, "tags"))
item_tags <- collect(tbl(portaldb, "itemTags"))
item_tag_names <- item_tags |> left_join(tag_idx, by = "tagID")

annotations_and_tags <-
  left_join(item_tag_names,
            annotation_table_df |> select(-type), by = "itemID") |>
  filter(!is.na(comment))

# annotations_and_tags <-
#   annotations_and_tags |> left_join(tag_idx, by = "tagID")

# Find note's parent items
attachments_table_df <- collect(tbl(portaldb, "itemAttachments"))
attachment_ids <- annotations_and_tags$parentItemID |> unique()
# Overall, there are annotations with tags and each annotation is made within
# a given pdf.  The tags are applied to the annotations, so we have the
# following hierarchy: tag <- annotation <- pdf <- journal article.

# One confusion is that annotations, pdfs, and articles are all "items"
# Attachments here are the pdfs and their parents are the articles.  We
# rename the itemID to parentItemID because the pdfs are the parents of the
# annotations.  The tags associated with each annotation have tagIDs.

annotation_parent_df <- attachments_table_df |>
  filter(itemID %in% attachment_ids) |>
  rename(articleItemID = parentItemID,
         parentItemID = itemID)

# Tags with information on the annotation ID (itemID) and
# article ID (articleItemID)
tag_df_w_aa <- left_join(annotations_and_tags, annotation_parent_df,
          by = "parentItemID") |>
  filter(articleItemID %in% items_to_keep)

parameter_item_idx <- tag_df_w_aa |> filter(name == "parameter") |>
  pull(itemID)

parameter_df <- tag_df_w_aa |>
  filter(itemID %in% parameter_item_idx) |>
  filter(name != "parameter")

# parameter_notes_df <- full_join(
#   item_tags |> filter(itemID %in% parameter_item_idx),
#   parameter_df, relationship = "many-to-many", by = "itemID")

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
  parameter_df,
  wanted_field_value_df |> rename(articleItemID = itemID),
  by = "articleItemID")

parameter_info_list <- parse_params(parameter_notes_df$param_value)
parameter_notes_df <- bind_cols(
  parameter_notes_df |> rename(param_string = param_value),
  parameter_info_list$param_df
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

en_type <- parameter_notes_df |> filter(name %in% c("manual", "digital")) |>
  select(name, itemID)

missed_item_ids <- en_type |> group_by(itemID) |> count() |> filter(n > 1) |>
  pull(itemID)
stop_message <- "The following item IDs (annotations) have been associated with both manual and digital contact tracing:\n"
if (length(missed_item_ids) > 0) stop(paste0(c(stop_message, missed_item_ids), collapse = "\n"))

parameter_notes_df <-
  parameter_notes_df |> filter(!(name %in% c("manual", "digital"))) |>
  left_join(en_type |> select(itemID, param_type = name), by = "itemID")

simple_param_df <- parameter_notes_df |> select(
  param_name = name,
  all_of(param_order),
  source = value,
  param_type
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