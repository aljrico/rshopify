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
  unnest_columns <- function(orders){
    nested_columns <- c("total_price_set", "subtotal_price_set", "total_shipping_price_set", "total_discounts_set", "total_line_items_price_set")
    
    
    for(n in names(orders)){
      columns <- colnames(orders[[n]])
      for(col in columns){
        dt_length <- nrow(orders[[n]])
        if(dt_length == 0) next
        if(length(orders[[n]][[col]][[1]]) > 1){
          orders[[n]] <- orders[[n]] %>% tidyr::unnest_wider(col = col, names_sep = "_") %>% data.table::data.table()
        }
      }
    }
    return(orders)
  }
  
  request_url <- paste0(private$base_url, "/orders.json")
  # query_parameters <- list(...)
  response <- private$get_request(request_url)
  
  # Parse response
  response %>% 
    httr::content() %>% 
    .$orders %>% 
    lapply(lapply, function(x)ifelse(is.null(x), NA, x)) %>% 
    data.table::rbindlist(use.names = TRUE, fill = TRUE) %>% # Collapse list of data.tables into a single data.table
    check_null() %>% 
    remove_fulfilments() %>% 
    extract_subdataframes() %>% 
    unnest_columns() %>% 
    return()
}