test_that("Orders can be downloaded", {
  
  # Establish Connection
  sc <- 
    ShopifyConnection$new(
    shop_url =  "https://blue-whale-coffee.myshopify.com",
    api_key = "5397112d5008ca1ba90b9962335a3558",
    api_password = "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"
  )
  
  # Download all pending orders
  sc$get_orders()
  
})
