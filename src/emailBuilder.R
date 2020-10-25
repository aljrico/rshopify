emailBuilder = R6::R6Class(
  "emailBuilder",
  public = list(
    email_body = NULL,
    initialize = function(orders) {
      titles_html <- private$make_titles(orders)
      tables_html <- private$make_tables(orders)
      self$email_body <- private$build_email(titles_html, tables_html)
      private$download_credentials()
    },
    send = function(receiver) {

      # Create email object
      email_object <-
        emayili::envelope() %>%
        emayili::from(private$credentials$address) %>%
        emayili::to(receiver) %>%
        emayili::subject("The Whale Coffee: New oders coming in!") %>%
        emayili::html(self$email_body)

      # Build credentials object
      smtp <- emayili::server(
        host = "smtp.gmail.com",
        port = 465,
        username = private$credentials$address,
        password = private$credentials$password
      )

      # Send email
      smtp(email_object, verbose = FALSE)
    }
  ),
  private = list(
    credentials = NULL,
    download_credentials = function() {
      rdrop2::drop_download("Coffee Dropshipper/mail_credentials")
      private$credentials <- readRDS("mail_credentials")
      file.remove("mail_credentials")
    },
    make_titles = function(orders) {
      lapply(orders, function(o) {
        build_address <- function(address) {
          paste(address$name, " - ", address$address1, ", ", address$address2, ", ", address$city, ", ", address$zip, ", ", address$country, " (", address$country_code, ")", sep = "")
        }

        ba <- o$billing_address %>% build_address()
        sa <- o$shipping_address %>% build_address()

        glue::glue(
          "
    <hr>
    <h3> Coffee Order </h3>
    <p><b> Shipping Address: </b> {sa} </p>
    <p><b> Billing Address: </b> {ba} </p>
    "
        )
      })
    },
    make_tables = function(orders) {
      lapply(orders, function(o) {
        items_table <- o$line_items[, c("roaster_name", "quantity", "grams", "variant_title", "label_field")]
        items_table[, grind := gsub(".*/ ", "", variant_title)]
        items_table <- items_table[, c("roaster_name", "quantity", "grams", "grind", "label_field")]

        data.table::setnames(items_table, old = c("roaster_name"), new = c(""))
        knitr::kable(items_table, format = "html")
      })
    },
    build_email = function(titles, tables) {
      l <- length(titles)
      body <- ""

      for (i in 1:l) {
        body <- paste(body, titles[[i]], tables[[i]], sep = "\n")
      }

      return(body)
    }
  )
)
