# Manual Error Fixes

manual_error_fix <- function(combined_data) {
  cd <- combined_data
  remove_row <- which(cd$param_name == "contacts_reached_from_test_med" &
          cd$param_value == 72 &
        grepl("COVID-19 Contact Tracing During", cd$source))
  cd <- cd[-remove_row, ]
  duplicate_row <- which(
    cd$param_name == "contacts_named_count" &
    cd$pm_start_date == "12/27/2020" &
    grepl("Unified", cd$source)
  )
  if (length(duplicate_row) > 1) {
    cd <- cd[-duplicate_row[2], ]
  }
  return(cd)
}
