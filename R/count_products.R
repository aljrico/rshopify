#' Count number of products
#'
#'
#' @details This function will gather all product listed in the shopify website and count how many are they
#' @name count_products
#'
#' @return integer indicating the number of products
ShopifyConnection$set("private", "count_products", function() {
  request_url <- paste0(private$base_url, "/products/count.json")

  # Send htt request
  suppressWarnings(
    response <-
      httr::GET(
        url = request_url,
        encode = "json",
        httr::authenticate(user = private$api_key, password = private$api_password)
      )
  )

  # Parse the response
  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  if (!is.null(result$errors)) stop(as.character(result$errors))
  result <- result$count
  
  return(result)
})
