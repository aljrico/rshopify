library(rshopify)
library(emayili)
library(glue)
library(data.table)
library(dplyr)

source('src/emailBuilder.R')
source('src/mapLabeller.R')
source('src/WhaleBrain.R')

# Initialize the Brain of the Whale
wb = WhaleBrain$new()

# Retrieve all orders in Shopify Store
wb$download_orders()

# Check 
n_orders = length(wb$orders)
if(n_orders > 0){
  wb$write_labels()
  wb$send_email(receiver = "aljrico@gmail.com")
  message("All emails sent.")
}else{
  message("No new orders today.")
}

