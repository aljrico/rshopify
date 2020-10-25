mapLabeller = R6::R6Class(
  "mapLabeller",
  private = list(
    labels = NULL,
    extract_grind = function(li){
      grind <- li$variant_title %>% 
        as.character() %>% 
        stringr::str_to_lower() %>% 
        stringr::str_split(' / ') %>% 
        .[[1]] %>% 
        .[[2]]
      
      if(grind == "beans") grind = "whole"
      if(grind %>% stringr::str_detect('coarse')) grind = "coarse"
      return(grind)
    },
    extract_grams = function(li){
      li$grams %>% 
        as.character() %>% 
        return()
    },
    extract_blend = function(li){
      li$title %>% 
        stringr::str_remove('The Whale®') %>% 
        stringr::str_trim() %>% 
        stringr::str_to_lower() %>% 
        return()
    },
    write_label_field = function(line_items){
      n_items = nrow(line_items)
      
      # Loop through each item
      for(j in 1:n_items){
        blend  = private$extract_blend(line_items[j, ])
        grams  = private$extract_grams(line_items[j, ])
        grind  = private$extract_grind(line_items[j, ])
        
        blend_condition = stringr::str_detect(private$labels, blend)
        grams_condition = stringr::str_detect(private$labels, grams)
        grind_condition = stringr::str_detect(private$labels, grind)
        
        label_name = private$labels[blend_condition & grams_condition & grind_condition]
        line_items[j, label_field := label_name]
      }
      
      return(line_items)
    }
  ),
  public = list(
    orders = NULL,
    initialize = function(orders){
    
      # Download existing labels
      private$labels = rdrop2::drop_dir("Coffee Dropshipper/labels/pdf/")$name
      
      # Loop through every order
      n_orders = length(orders)
      for(i in 1:n_orders){
        
        # Get line items from this order
        line_items = orders[[i]]$line_items
        
        # Write label field on every item
        orders[[i]]$line_items = private$write_label_field(line_items)
      }
      self$orders = orders
    }
  )
)
