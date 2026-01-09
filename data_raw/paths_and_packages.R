if (!"pacman" %in% rownames(installed.packages())) {
  install.packages("pacman")
}

library(pacman)
p_load(
  tidyverse,
  stringr
)

#original text path stored in google drive
if (str_detect(getwd(), "goutsmed")) {
  if (str_detect(getwd(), "agoutsmedt")) {
    ejhet_project <- file.path(
      path.expand("~"),
      "Nextcloud",
      "ejhet_project"
    )
    jstor_data_path <- file.path(
      path.expand("~"),
      "Nextcloud",
      "jstor"
    )
    jstor_raw_data <- file.path(path.expand("~"), "data", "jstor") # I'm storing the raw data in a different folder because it's heavy.

    wos_data_path <- file.path(
      path.expand("~"),
      "data",
      "wos"
    )

    elsevier_data_path <- file.path(
      path.expand("~"),
      "Nextcloud",
      "Research",
      "data",
      "elsevier"
    )

    future_project <- file.path(
      path.expand("~"),
      "Nextcloud",
      "Research",
      "data",
      "model_futur"
    )
  } else {
    data_path <- file.path(path.expand("~"), "data", "ejhet_project")
    jstor_data_path <- file.path(path.expand("~"), "data", "jstor")
    jstor_raw_data <- jstor_data_path
    wos_data_path <- file.path(path.expand("~"), "data", "wos")
    elsevier_data_path <- file.path(path.expand("~"), "data", "elsevier")
  }
} else if (str_detect(getwd(), "D:/Dropbox/8")) {
  data_path <- "D:/Dropbox/8-Projets Quanti/1-R_Projects/Data/ejhet_quanti_method"
  general_data_path <- "D:/Dropbox/8-Projets Quanti/1-R_Projects/Data/1-General_data"
} else if (str_detect(getwd(), "E:/Dropbox/8")) {
  data_path <- "E:/Dropbox/8-Projets Quanti/1-R_Projects/Data/ejhet_quanti_method"
  general_data_path <- "E:/Dropbox/8-Projets Quanti/1-R_Projects/Data/1-General_data"
} else {
  if (str_detect(getwd(), "github_p")) {
    app_data_path <- "data"
    future_project <- "C:/cloud/data/model_futur"
    ejhet_project <- "C:/cloud/data/ejhet_project"
    jstor_raw_data <- "C:/cloud/data/jstor"
    jstor_data_path <- "C:/cloud/data/jstor"
    wos_data_path <- "C:/cloud/data/wos"
    istex_data <- "C:/cloud/data/istex"
    istex_data_path <- "C:/cloud/data/istex"
    elsevier_data <- "C:/cloud/data/elsevier"
    elsevier_data_path <- "C:/cloud/data/elsevier"
    embeddings_data <- "C:/cloud/data/econ_embeddings"
  } else {
    if (str_detect(getwd(), "github_w")) {
      app_data_path <- "data"
      future_project <- "C:/cloud/data/model_futur"
      ejhet_project <- "C:/cloud/data/ejhet_project"
      jstor_raw_data <- "C:/cloud/data/jstor"
      jstor_data_path <- "C:/cloud/data/jstor"
      wos_data_path <- "C:/cloud/data/wos"
      istex_data <- "C:/cloud/data/istex"
      istex_data_path <- "C:/cloud/data/istex"
      elsevier_data <- "C:/cloud/data/elsevier"
      elsevier_data_path <- "C:/cloud/data/elsevier"
      embeddings_data <- "C:/cloud/data/econ_embeddings"
    }
  }
}
