# Building reproducible analytical pipelines with R
# Chapter 3 - Project start
# Source: https://raps-with-r.dev/project_start.html

# load packages
library(dplyr)
library(purrr)
library(readxl)
library(stringr)
library(janitor)

# The url below points to an Excel file
# hosted on the book’s github repository
url <- "https://is.gd/1vvBAc"

raw_data <- tempfile(fileext = ".xlsx")

download.file(url, raw_data,
              method = "auto",
              mode = "wb")

sheets <- excel_sheets(raw_data)

read_clean <- function(..., sheet){
     read_excel(..., sheet = sheet) |>
          mutate(year = sheet)
}

raw_data <- map(
     sheets,
     ~read_clean(raw_data,
                 skip = 10,
                 sheet = .)
) |>
     bind_rows() |>
     clean_names()

raw_data <- raw_data |>
     rename(
          locality = commune,
          n_offers = nombre_doffres,
          average_price_nominal_euros = prix_moyen_annonce_en_courant,
          average_price_m2_nominal_euros = prix_moyen_annonce_au_m2_en_courant,
          average_price_m2_nominal_euros = prix_moyen_annonce_au_m2_en_courant
     ) |>
     mutate(locality = str_trim(locality)) |>
     select(year, locality, n_offers, starts_with("average"))

raw_data

# count 'Luxembourg' in 'locality'
raw_data |>
     filter(grepl("Luxembourg", locality)) |>
     count(locality)

# count 'Pétange'
raw_data |>
     filter(grepl("P.tange", locality)) |>
     count(locality)

# correct issues on 'locality' spelling
raw_data <- raw_data |>
     mutate(
          locality = ifelse(grepl("Luxembourg-Ville", locality),
                            "Luxembourg",
                            locality),
          locality = ifelse(grepl("P.tange", locality),
                            "Pétange",
                            locality)
     ) |>
     mutate(across(starts_with("average"),
                   as.numeric))

raw_data

# show NA values in 'average_price_nominal_euros'
raw_data |>
     filter(is.na(average_price_nominal_euros))

# remove rows with "Source" string in 'locality'
raw_data <- raw_data |>
     filter(!grepl("Source", locality))

# only keep the communes in 'raw_data'
commune_level_data <- raw_data |>
     filter(!grepl("nationale|offres", locality),
            !is.na(locality))

commune_level_data

# create a dataset with the national data
country_level <- raw_data |>
     filter(grepl("nationale", locality)) |>
     select(-n_offers)

offers_country <- raw_data |>
     filter(grepl("Total d.offres", locality)) |>
     select(year, n_offers)

country_level_data <- full_join(country_level, offers_country) |>
     select(year, locality, n_offers, everything()) |>
     mutate(locality = "Grand-Duchy of Luxembourg")

# webscrape Luxembourg commune table from Wikipedia
current_communes <- "https://is.gd/lux_communes" |>
     rvest::read_html() |>
     rvest::html_table() |>
     purrr::pluck(2) |>
     janitor::clean_names() |>
     dplyr::filter(name_2 != "Name") |>
     dplyr::rename(commune = name_2) |>
     dplyr::mutate(commune = stringr::str_remove(commune, " .$"))

current_communes

# Let’s see if we have all the communes in our dataset
setdiff(unique(commune_level_data$locality),
        current_communes$commune)

# former communes list
former_communes <- "https://is.gd/lux_former_communes" |>
     rvest::read_html() |>
     rvest::html_table() |>
     purrr::pluck(3) |>
     janitor::clean_names() |>
     dplyr::filter(year_dissolved > 2009)

former_communes

# harmonise former and current communes
communes <- unique(c(former_communes$name,
                     current_communes$commune))

# we need to rename some communes

# Different spelling of these communes between wikipedia and the data

communes[which(communes == "Clemency")] <- "Clémency"
communes[which(communes == "Redange")] <- "Redange-sur-Attert"
communes[which(communes == "Erpeldange-sur-Sûre")] <- "Erpeldange"
communes[which(communes == "Luxembourg City")] <- "Luxembourg"
communes[which(communes == "Käerjeng")] <- "Kaerjeng"
communes[which(communes == "Petange")] <- "Pétange"

# run test again
setdiff(unique(commune_level_data$locality),
        communes)
