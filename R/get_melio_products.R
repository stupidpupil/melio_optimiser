get_melio_products <- function() {
  get_melio_product_urls() |>
    purrr::map(get_melio_product_details)
}