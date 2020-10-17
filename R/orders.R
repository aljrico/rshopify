## Downloads all orders
#' @rdname orders
get_orders <- function(){
  
  check_null <- function(orders){
    # If orders is a NULL data.table, exit this function early
    isNULLDT <- all.equal(orders, data.table::data.table())
    if(is.logical(isNULLDT) && isNULLDT == TRUE){
      private$log("No orders to be found")
      stop("Stopping the parsing because there are no orders.")
    }else{
      return(orders) 
    } 
  }
  remove_fulfilments <- function(orders){
    # This field causes problems.. remove it
    for(i in seq_along(orders$fulfillments)){
      if("receipt" %in% colnames(orders$fulfillments[[i]])){
        data.table::set(orders$fulfillments[[i]], j = "receipt", value = NULL)
      }
    }
    return(orders)
  }
  extract_subdataframes <- function(orders){
    # Extract sub data.frames
    extract <- c("discount_applications", "discount_codes", "tax_lines", "line_items", "fulfillments", "refunds", "shipping_lines")
    result <- vector(mode = "list", length = 1 + length(extract))
    names(result) <- c("orders", extract)
    for(df in extract){
      result[[df]] <- data.table::rbindlist(
        l = lapply(as.list(orders[[df]]), data.table::as.data.table),
        use.names = TRUE,
        fill = TRUE,
        idcol = TRUE
      )
      result[[df]]$order_id <- orders$id[result[[df]]$.id]
    }
    
    # Insert orders into result
    data.table::set(orders, j = extract, value = NULL)
    result[["orders"]] <- orders
    
    return(result)
  }
  unnest_columns <- function(dt){
    for(n in names(dt)){
      columns <- colnames(dt[[n]])
      for(col in columns){
        dt_length <- nrow(dt[[n]])
        if(dt_length == 0) next
        if(length(dt[[n]][[col]][[1]]) > 1){
          dt[[n]] <- dt[[n]] %>% tidyr::unnest_wider(col = col, names_sep = "_") %>% data.table::data.table()
        }
      }
    }
    return(dt)
  }
  
  request_url <- paste0(private$base_url, "/orders.json")
  # query_parameters <- list(...)
  response <- private$get_request(request_url)
  
  # Parse response
  # response %>% 
  #   httr::content() %>% 
  #   .$orders %>% 
  #   lapply(lapply, function(x)ifelse(is.null(x), NA, x)) %>% 
  #   data.table::rbindlist(use.names = TRUE, fill = TRUE) %>% # Collapse list of data.tables into a single data.table
  #   check_null() %>% 
  #   remove_fulfilments() %>% 
  #   extract_subdataframes() %>% 
  #   unnest_columns() %>% 
  #   return()
  
  
  raw_orders <- response %>% httr::content() %>% .$orders
  
  orders_list <- list()
  for(o in 1:length(raw_orders)){
    this_order <- raw_orders[[o]]
    
    clean_order <- list()
    order_header <- list()
    deep_tables <- c()
    field_names <- names(this_order)
    for(f in field_names){
      this_field <- this_order[[f]]
      if(length(this_field) == 0){
        order_header[[f]] <- NA
      }else if(length(this_field) == 1){
        if(length(this_field[[1]]) == 1){
          order_header[[f]] <- this_field[[1]]
        }else{
          deep_tables <- c(deep_tables, f)
        }
      }else{
        deep_tables <- c(deep_tables, f)
      }
    }
    
    clean_order[["header"]] <- order_header
    
    unnest_columns <- function(dt){
      columns <- colnames(dt)
      for(col in columns){
        dt_length <- nrow(dt)
        if(dt_length == 0) next
        if(length(dt[[col]][[1]]) > 1){
          dt <- dt %>% tidyr::unnest_wider(col = col, names_sep = "_") %>% data.table::data.table()
        }
      }
      
      return(dt)
    }
    make_prices_table <- function(my_order){
      shop_money <- list()
      shop_money$line_items <- my_order$total_line_items_price_set$shop_money %>% paste(., collapse = " ")
      shop_money$subtotal <- my_order$subtotal_price_set$shop_money %>% paste(., collapse = " ")
      shop_money$discounts <- my_order$total_discounts_set$shop_money %>% paste(., collapse = " ")
      shop_money$shiphping <- my_order$total_shipping_price_set$shop_money %>% paste(., collapse = " ")
      shop_money$taxes <- my_order$total_tax_set$shop_money %>% paste(., collapse = " ")
      shop_money$total_price_set <- my_order$total_price_set$shop_money %>% paste(., collapse = " ")
      
      presentment_money <- list()
      presentment_money$line_items <- my_order$total_line_items_price_set$presentment_money %>% paste(., collapse = " ")
      presentment_money$subtotal <- my_order$subtotal_price_set$presentment_money %>% paste(., collapse = " ")
      presentment_money$discounts <- my_order$total_discounts_set$presentment_money %>% paste(., collapse = " ")
      presentment_money$shiphping <- my_order$total_shipping_price_set$presentment_money %>% paste(., collapse = " ")
      presentment_money$taxes <- my_order$total_tax_set$presentment_money %>% paste(., collapse = " ")
      presentment_money$total_price_set <- my_order$total_price_set$presentment_money %>% paste(., collapse = " ")
      
      return(list(shop_money = shop_money, presentment_money = presentment_money))
    }
    
    for(d in deep_tables){
      
      if(d == "line_items"){
        line_items <- this_order[[d]]
        number_items <- length(line_items)
        items_list <- list()
        for(l in 1:number_items){
          this_item <- line_items[[l]] %>% 
            data.table::as.data.table() %>% 
            unnest_columns()
          
          # Clean rough columns
          this_item[, origin_location := NULL]
          this_item[, properties := NULL]
          this_item[, discount_allocations := NULL]
          this_item[, duties := NULL]
          this_item[, tax_lines := NULL]
          
          # Remove duplicated rows
          this_item = unique(this_item)
          
          # Add to the list
          items_list[[l]] = this_item
        }
        
        clean_order[[d]] <- items_list %>% data.table::rbindlist()
      }else if(d %in% c("customer")){
        clean_order[[d]] <- this_order[[d]]
      }else if(d %in% c("total_line_items_price_set", "subtotal_price_set", "total_discounts_set", "total_shipping_price_set", "total_tax_set", "total_price_set")){
        clean_order[["prices"]] <- make_prices_table(this_order)
      }else if(d %in% "shipping_lines"){
        clean_order[[d]] <- 
          this_order[[d]] %>% 
          .[[1]] %>% 
          data.table::as.data.table() %>% 
          unnest_columns()
      }else{
        clean_order[[d]] <- 
          this_order[[d]] %>% 
          data.table::as.data.table() %>% 
          unnest_columns()
      }
    }
    
    
    orders_list[[o]] <- clean_order
    
    
  }
  
  return(orders_list)
}