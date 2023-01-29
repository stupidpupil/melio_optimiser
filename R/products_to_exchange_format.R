products_to_exchange_format <- function(products){

  biomarkers_map <- readr::read_csv("data-raw/biomarker_snomed_map.csv", col_types="cc")

  products <- products |>
    purrr::map(function(prod){
      prod$id = NULL

      prod$name = jsonlite::unbox(prod$title)
      prod$title = NULL

      prod$url = jsonlite::unbox(paste0("https://www.meliohealth.co.uk/product/", prod$handle))
      prod$handle = NULL

      prod$price_pence = jsonlite::unbox(prod$price_pence)
      prod$turnaround_days = jsonlite::unbox(prod$turnaround_days)

      prod$sampling_procedure = jsonlite::unbox("venous")

      prod$biomarkers <- tibble::tibble(biomarker_handle = prod$biomarkers) |>
        dplyr::left_join(biomarkers_map,  by="biomarker_handle") |>
        dplyr::pull("sctid") |>
        na.omit() |>
        unique()

      return(prod)
    }) |>
    purrr::keep(function(prod){
      length(prod$biomarkers) > 0
    })


  return(products)
}