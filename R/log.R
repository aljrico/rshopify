#' Log message
#'
#'
#' @details This function simply prints a message
#' @name log
#' @export
#' @return integer indicating the number of products
ShopifyConnection$set("private", "log", function(msg, level = 1) {
  
  pre_msg <- ""
  if(level > 1){
    for(i in 1:(level - 1)) pre_msg <- paste0(pre_msg, '  ')
  }else{
    pre_msg <- "ShopifyConnection : "
  }
  
  full_msg <- paste0(pre_msg, msg, '  ~')
  
  message(full_msg)
})
