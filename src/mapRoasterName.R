# Define ids
kata_id <- as.character(o$line_items[title == "The Whale® Brown"]$product_id)
limini_id <- as.character(o$line_items[title == "The Whale® Noir"]$product_id)
blue_id <- as.character(o$line_items[title == "The Whale® Blond"]$product_id)

# Create maps
name_map <- data.table::data.table(
  product_id = c(blue_id, kata_id, limini_id),
  roaster_name = c("Blue Mountain Blend", "Kata Blend", "Limini Blend")
)

# Update files
data.table::fwrite(name_map, 'map_roaster_names')
rdrop2::drop_upload('map_roaster_names', 'Coffee Dropshipper/', mode = "overwrite")
file.remove('map_roaster_names')
