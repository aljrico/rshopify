WhaleBrain = R6::R6Class(
  "WhaleBrain",
  public = list(
    shop_connection = NULL,
    orders = NULL,
    initialize = function(){
      self$establish_connection()
    },
    establish_connection = function(){
      self$shop_connection = ShopifyConnection$new(
        shop_url =  "https://blue-whale-coffee.myshopify.com",
        api_key = "5397112d5008ca1ba90b9962335a3558",
        api_password = "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"
      )
    },
    download_orders = function(){
      orders <- self$shop_connection$get_orders()
      
      # Map product id to roaster names
      map_roaster_names = rdrop2::drop_read_csv('Coffee Dropshipper/map_roaster_names')
      orders <- lapply(orders, function(o){
        o$line_items = o$line_items[map_roaster_names, on = "product_id", nomatch = 0]
        return(o)
      })
      
      # Backup orders on Dropbox
      sapply(orders, function(o){
        saveRDS(o, o$code_name)
        rdrop2::drop_upload(o$code_name, o$path_file, mode = "overwrite")
        file.remove(o$code_name)
      })
      
      # Save orders up
      self$orders = orders
    },
    write_labels = function(){
      map_labels = mapLabeller$new(self$orders)
      self$orders = map_labels$orders
    },
    send_email = function(receiver){
      email_builder = emailBuilder$new(self$orders)
      email_builder$send(receiver)
      
    }
  )
)
