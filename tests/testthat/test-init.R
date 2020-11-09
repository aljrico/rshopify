
# Read locally saved credentials
credentials <- readRDS('../testdata/shop_credentials')


test_that("connection can be made", {
  
  # Establish connection
  sc <<- ShopifyConnection$new(
    shop_url =  credentials$shop_url,
    api_key = credentials$api_key,
    api_password = credentials$api_password
  )
})
test_that("Orders can be downloaded", {
  
  # Download all pending orders
  sc$get_orders()
  
})