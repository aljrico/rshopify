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
        as.character() %>% 
        stringr::str_to_lower() %>% 
        stringr::str_split(" ") %>% 
        .[[1]] %>% 
        .[[length(.)]] %>% 
        # stringr::str_remove_all("the whale®") %>% 
        stringr::str_trim() %>% 
        return()
    },
    write_label_field = function(line_items){
      n_items = nrow(line_items)
      
      li <- data.table::copy(line_items)
      li[, label_field := character(.N)]
      
      # Loop through each item
      for(j in 1:n_items){
        blend  = private$extract_blend(li[j, ])
        grams  = private$extract_grams(li[j, ])
        grind  = private$extract_grind(li[j, ])
        
        blend_condition = stringr::str_detect(private$labels, blend)
        grams_condition = stringr::str_detect(private$labels, grams)
        grind_condition = stringr::str_detect(private$labels, grind)
        
        label_name = private$labels[blend_condition & grams_condition & grind_condition]
        li[j, label_field := label_name]
      }
      return(li)
    }
  ),
  public = list(
    orders = NULL,
    initialize = function(orders){
    
      # Download existing labels
      private$labels = rdrop2::drop_dir("Coffee Dropshipper/labels/pdf/")$name
      self$orders = orders
      
      # Map labels
      self$map_labels()

    },
    map_labels = function(){
      
      # Get static copy
      orders = data.table::copy(self$orders)
      
      # Loop through every order
      n_orders = length(orders)
      for(i in 1:n_orders){
        
        # Get line items from this order
        line_items = data.table::copy(orders[[i]]$line_items)
        
        # Write label field on every item
        orders[[i]]$line_items = private$write_label_field(line_items)
      }
      self$orders = orders
    }
  )
)