#' This is a ShopifyOrder Class
#' 
#' This type of object will organise and manage the information of Shopify Orders
#' 
#' @details Keep it secret, keep it safe
#' @docType class
#' @export
ShopOrder <- R6::R6Class(
  "ShopOrder",
  public = list(
    code_name = NULL,
    header = NULL,
    shipping_address = NULL,
    shipping_lines = NULL,
    billing_address = NULL,
    line_items = NULL,
    access_token = NULL,
    path_file = NULL,
    initialize = function(this_order){
      
      # Save main fields
      self$header = this_order$header
      self$shipping_address = this_order$shipping_address
      self$billing_address = this_order$billing_address
      self$line_items = this_order$line_items
      self$shipping_lines = this_order$shipping_lines
      
      # Create code name
      creation_date = self$header$created_at %>% as.Date() %>% gsub("-", "", .)
      self$code_name = paste0(creation_date, "_", self$header$id)
      
      # Retrieve access_token
      # self$access_token = readRDS('token.rds')
      
      # Write file path
      # self$path_file <- paste0("Coffee Dropshipper/orders/")
      
    },
    fulfill = function(shopify_connection, body){
      
      if(!any(class(shopify_connection) == "ShopifyConnection")) stop("A proper ShopifyConnection class needs to be introduced")
      
      id <- self$header$id
      shopify_connection$fulfill_order(id, body)

    }
  )
)