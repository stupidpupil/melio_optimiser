get_melio_product_details <- function(melio_product_url){

  handle <- melio_product_url |> 
    stringr::str_match("([a-z0-9_-]+)$") |> (\(x) x[,2])()


  product_html <- rvest::read_html(melio_product_url)

  title <- product_html |> rvest::html_node("h1") |> rvest::html_text()

  turnaround_days <- product_html |> 
    rvest::html_nodes("span") |> 
    rvest::html_text() |> 
    stringr::str_match("^RESULTS .+(\\d-)?(\\d).+ DAYS$") |> 
    (\(x) x[,3])() |> as.integer() |> na.omit() 

  if(length(turnaround_days) > 0){
    turnaround_days <- turnaround_days |> dplyr::first()
  } else{
    turnaround_days <- NA_integer_
  }

  biomarker_urls <- product_html |> 
    rvest::html_nodes("div[id^='HealthAreas']") |> 
    rvest::html_nodes("a[href^='/blog']") |> 
    rvest::html_attr("href")

  biomarker_ids <- biomarker_urls |> 
    stringr::str_match("\\/blog\\/([a-z0-9_-]+)") |> 
    (\(x) x[,2])() |> unique()

  price_pence <- product_html |> rvest::html_nodes("span") |> 
    rvest::html_text() |> 
    stringr::str_match("^Buy £(\\d+.\\d{2})$") |> (\(x) x[,2])() |> 
    as.numeric() |> na.omit() |> dplyr::first() |> (\(x) as.integer(x*100))()

  list(
    title = title,
    handle = handle,
    biomarkers = biomarker_ids,
    turnaround_days = turnaround_days,
    price_pence = price_pence
  )

}