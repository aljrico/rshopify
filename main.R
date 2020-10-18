library(rshopify)
library(emayili)
library(glue)


shop_connection <- ShopifyConnection$new(
  shop_url =  "https://blue-whale-coffee.myshopify.com",
  api_key = "5397112d5008ca1ba90b9962335a3558",
  api_password = "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"
)

orders <- shop_connection$get_orders()

# Backup orders on Dropbox
sapply(orders, function(o){
  saveRDS(o, o$code_name)
  rdrop2::drop_upload(o$code_name, o$path_file, mode = "overwrite")
  file.remove(o$code_name)
})

# 
for(o in orders){

}

construct_email_html <- function(orders){
  
  construct_tables <- function(orders){
    all_tables <- lapply(orders, function(x) knitr::kable(x$line_items[, c("title", "quantity", "grams")], format = "html"))
    
    l <- length(all_tables)
    tables_html <- ""
    table_titles <- paste0("<h5>Order ", 1:l, "</h5>")
    
    for(i in 1:l){
      tables_html <- paste(tables_html, table_titles[[i]], all_tables[[i]], sep = "\n")
    }
    
    return(tables_html)
  }

  
  email_html <- glue::glue(
    "<!DOCTYPE html>
    <html>
    <head>
    <title>New orders coming in!</title>
    </head>
    <body>
    
    {construct_tables(orders)}
    
    </body>
    </html>
    "
  )
  
  return(email_html)
}

email_title <- "New orders coming in!"
email_table <- knitr::kable(o$line_items[, c("id", "title", "quantity", "grams")], format = "html")
email_html <- paste0("
               <!DOCTYPE html>
<html>
<head>
  <title>", email_title, "</title>
</head>
<body>

<h5>Order 1</h5>

</body>
</html>      
                     ")

email <- 
  envelope() %>%
  from("aljrico@gmail.com") %>% 
  to("aljrico@gmail.com") %>% 
  subject("🐋 The Whale Coffee: New oders coming in! ☕") %>% 
  html(construct_email_html(all_tables))

smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = "aljrico@gmail.com",
               password = Sys.getenv("gmail_password"))
smtp(email, verbose = TRUE)

library(blastula)
create_smtp_creds_file(file = "gmail_credentials", user = "aljrico@gmail.com", port = 465, provider = "gmail")


email_title <- md("The Whale Coffee: New order coming in!")
email_body <- knitr::kable(o$line_items[, c("id", "title", "quantity", "grams")], format = "html")
email_footer <- md("asdf")


email <- blastula::render_email('email_report.html')

email

email %>%
  smtp_send(
    to = "aljrico@gmail.com",
    from = "aljrico@gmail.com",
    subject = "Testing the `smtp_send()` function",
    credentials = creds_file("gmail_credentials")
  )
