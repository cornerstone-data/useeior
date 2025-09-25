## Generate Nowcasted data objects from files generated in USEEIO repo and stored in USEEIO-input

source("data-raw/BEAData.R")

ls <- list("url" = "USEEIO",
           "date_accessed" = "",
           "date_last_modified" = "")

dir <- file.path(rappdirs::user_data_dir(), "USEEIO-input")
dir.create(dir, showWarnings = FALSE)

for(yr in c(2018:2023)) {
  name <- "U_out"
  df <- read.csv(file.path(dir, paste0(name, "_", yr, ".csv")))
  rownames(df) <- df[, 1]
  df <- df[, -1]
  names(df) <- gsub("^X", "", names(df))
  writeFile(df = df, year = yr,
            name = paste0("Detail_Use_", yr, "_PRO_BeforeRedef"), ls = ls,
            schema_year = 2017)

  name <- "U_imports_out"
  df <- read.csv(file.path(dir, paste0(name, "_", yr, ".csv")))
  rownames(df) <- df[, 1]
  df <- df[, -1]
  names(df) <- gsub("^X", "", names(df))
  writeFile(df = df, year = yr,
            name = paste0("Detail_Import_", yr, "_BeforeRedef"), ls = ls,
            schema_year = 2017)
  
  name <- "V_out"
  df <- read.csv(file.path(dir, paste0(name, "_", yr, ".csv")))
  rownames(df) <- df[, 1]
  df <- df[, -1]
  names(df) <- gsub("^X", "", names(df))
  df <- t(df) ## TRANSPOSE!
  writeFile(df = df, year = yr,
            name = paste0("Detail_Make_", yr, "_BeforeRedef"), ls = ls,
            schema_year = 2017)
}
