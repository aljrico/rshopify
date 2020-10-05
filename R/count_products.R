#' Count number of products
#'
#' @name count_products
#' @details This function will gather all product listed in the shopify website and count how many are they
#' @return integer indicating the number of products
ShopifyConnection$set("private", "count_products", function() {
  request_url <- paste0(private$base_url, "/products/count.json")

  # Send GET request
  response <- private$get_request(request_url = request_url)
  
  # Parse the response
  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  if (!is.null(result$errors)) stop(as.character(result$errors))
  result <- result$count
  
  return(result)
})
