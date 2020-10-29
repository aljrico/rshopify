library(rshopify)
library(emayili)
library(glue)
library(data.table)
library(dplyr)

source('src/emailBuilder.R')
source('src/mapLabeller.R')
source('src/WhaleBrain.R')

wb = WhaleBrain$new()
wb$download_orders()
wb$write_labels()
wb$send_email(receiver = "aljrico@gmail.com")
