#' Get request
#' @name get_request
#' @param request_url URL to be sent
#' @param parameters any specific parameters to be passed unto the 'query' parameter
#' @details Generic method that sends a GET type of request to the rest API
ShopifyConnection$set("private", "get_request", function(request_url, parameters = NULL) {
  
  # Send http request
  suppressWarnings(
    response <-
      httr::GET(
        url = request_url,
        encode = "json",
        httr::authenticate(user = private$api_key, password = private$api_password),
        query = parameters
      )
  )
  
  return(response)
})

#' Post request
#' @name post_request
#' @param request_url URL to be sent
#' @param parameters any specific parameters to be passed unto the 'query' parameter
#' @details Generic method that sends a GET type of request to the rest API
ShopifyConnection$set("private", "post_request", function(request_url, parameters = NULL, body = NULL) {

  # Send http request
  suppressWarnings(
    response <-
      httr::POST(
        url = request_url,
        encode = "json",
        httr::authenticate(user = private$api_key, password = private$api_password),
        query = parameters,
        body = body
      )
  )
  
  return(response)
})
