load_data <- function(name) {
  readRDS(file.path("data", paste0(name, ".rds")))
}


# -------------------------------------------------------------------
# create_update_data()
# Runs all numbered data_raw scripts in correct order (1.R → 2.R → 3.R → 4.R)
# independent of filenames
# -------------------------------------------------------------------

create_update_data <- function() {
  message("🔄 Rebuilding all data from data_raw scripts...")

  # list all scripts that begin with a number: "01_...", "1.R", "2_build..."
  scripts <- list.files(
    path = here::here("data_raw"),
    pattern = "^[0-9]+.*\\.R$",
    full.names = TRUE
  )

  # sort numerically: 1,2,3,4,...
  script_numbers <- as.numeric(gsub("^([0-9]+).*", "\\1", basename(scripts)))
  scripts <- scripts[order(script_numbers)]

  # execute in order
  for (s in scripts) {
    message("➡️ Running: ", basename(s))
    source(s, local = TRUE)
  }

  message("✅ Data build complete.")
}
