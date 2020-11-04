test_that("Orders can be downloaded", {
  
  # Read locally saved credentials
  credentials <- readRDS('shop_credentials')
  
  # Establish connection
  sc <- ShopifyConnection$new(
    shop_url =  credentials$shop_url,
    api_key = credentials$api_key,
    api_password = credentials$api_password
  )
  
  # Download all pending orders
  sc$get_orders()
  
})
