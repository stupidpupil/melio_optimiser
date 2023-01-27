get_melio_product_urls <- function(){
	front_page_url <- "https://www.meliohealth.co.uk/"

	rel_hrefs <- rvest::read_html(front_page_url) |> 
		rvest::html_nodes("a[href^='/product/']") |> 
		rvest::html_attr("href") |> unique() |> sort()

	rvest::url_absolute(rel_hrefs, front_page_url)
}