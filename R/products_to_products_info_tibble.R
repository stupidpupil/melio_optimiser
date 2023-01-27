products_to_products_info_tibble <- function(products){
  products |> 
    purrr::map(function(p){list(
      product_handle = p$handle, 
      title = p$title, 
      price_pence = p$price_pence, 
      turnaround_days = p$turnaround_days
    )}) |> 
    purrr::map_dfr(function(x) {purrr::flatten(x)}) |>
    dplyr::left_join(products_to_products_biomarkers_matrix(products), by="product_handle")
}