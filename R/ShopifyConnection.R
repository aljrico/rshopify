#' This is a ShopifyConnection Class
#' 
#' This type of object will open a connection with a determined shopify app
#' 
#' @details Keep it secret, keep it safe
#' @docType class
#' @section Public fields:
#'
#' * `test`: this is a test yet.
#' 
#' @section Public Methods:
#' \describe{
#' \item{\bold{Orders} functions}{\itemize{
#'   \item \code{\link{OrdersDownloader}}
#' }}
#' }
#' @include OrdersDownloader.R
#' @export
ShopifyConnection <- R6::R6Class(
  "ShopifyConnection",
  public = list(

    #' @details if successful, will return a new \code{ShopifyConnection} object.
    #' 
    #' @param shop_url shop URL (e.g. 'https://blue-whale-coffee.myshopify.com')
    #' @param api_key API key
    #' @param api_password API password
    initialize = function(shop_url, api_key, api_password){
      
      # Store private credentials
      private$api_key      = api_key
      private$api_password = api_password
      private$shop_url     = shop_url
      
      # Build base URL
      private$api_version = private$get_api_version()
      private$base_url    = paste0(shop_url, "/admin/api/", private$api_version) # "/orders.json"
      
      # Check connection works
      tryCatch(
        private$count_products(), 
        error = function(e){
          private$log("Initial Connection has failed", "\n")
          stop(e)
        }  
      )
      
      private$log("Connection established successfully!")
    },
    get_orders = function(){
      od = OrdersDownloader$new(base_url = private$base_url, api_key = private$api_key, api_password = private$api_password)
      od$download()
    },
    fulfill_order = function(order_id, body){
      request_url <- glue::glue("{private$base_url}/orders/{order_id}/fulfillments.json")
      
      
      ful_body <- list()
      ful_body[["fulfillment"]] <- body
      
      response <- private$post_request(request_url, body = ful_body)
      
      clean_response <- response %>% httr::content()
      
      if(any(names(clean_response) %>% stringr::str_detect("error"))){
        stop(clean_response$error)
      }else{
        message(clean_response)
      }
    }
  ),
  private = list(
    
    # Private Fields
    api_version = NULL,
    base_url = NULL,
    
    # Private Credentials
    api_key = NULL,
    api_password = NULL,
    shop_url = NULL
    
  )
)
