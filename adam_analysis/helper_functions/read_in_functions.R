current_files <- list.files(full.names = TRUE)
for (file in current_files[-grep("read_in_functions.R", current_files)]) {
  source(file, chdir = TRUE)
}
