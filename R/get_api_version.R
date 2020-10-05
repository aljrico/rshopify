#' Get Latest Stable API Version
#'
#' Retrieve the latest stable api version
#' @name get_api_version
#' @details Shopify releases a new API version every 3 months at the beginning of the quarter. This function follows this rule to automatically infer the latest
#' @references \url{https://help.shopify.com/en/api/versioning#the-api-version-release-schedule}
#' @return character indicating the latest stable API version
ShopifyConnection$set("private", "get_api_version", function(){
  
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  api_versions <- seq.Date(
    from = as.Date(paste0(current_year - 1, "-10-01")),
    to = as.Date(paste0(current_year, "-10-01")),
    by = "3 months"
  )
  api_versions <- as.POSIXct(paste0(api_versions, " 17:00:00"), tz = "America/Chicago")
  current_time <- Sys.time()
  
  for(i in seq_along(api_versions)){
    if(current_time < api_versions[i]) break
  }
  
  version <- format(api_versions[i-1], "%Y-%m")
  
  return(version)
})
