testthat::test_that("connection can be made", {
  # Read locally saved credentials
  credentials <- readRDS('shop_credentials')
  
  # Establish connection
  ShopifyConnection$new(
    shop_url =  credentials$shop_url,
    api_key = credentials$api_key,
    api_password = credentials$api_password
  )
})
