emailBuilder <- R6::R6Class(
  "emailBuilder",
  public = list(
    email_body = NULL,
    initialize = function(orders) {

      # Extract label_names
      private$label_names <- private$get_label_names(orders)

      # Create html parts of the email
      titles_html <- private$make_titles(orders)
      tables_html <- private$make_tables(orders)

      # Build email body
      self$email_body <- private$build_email_body(titles_html, tables_html)

      # Store credentials
      private$download_credentials()
    },
    send = function(receiver) {
      attach_labels <-
        function(eo, ln) {
          rdrop2::drop_download(sprintf("Coffee Dropshipper/labels/pdf/%s", ln), overwrite = TRUE)
          eo <- eo %>% emayili::attachment(path = ln)
          file.remove(ln)
          return(eo)
        }

      # Create email object
      email_object <-
        emayili::envelope() %>%
        emayili::from(private$credentials$address) %>%
        emayili::to(receiver) %>%
        emayili::subject("The Whale Coffee: New oders coming in!") %>%
        emayili::html(self$email_body)

      # Attach label files
      # for (ln in private$label_names) {
      #   email_object <- email_object %>% attach_labels(ln)
      # }
      
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
    label_names = NULL,
    download_credentials = function() {
      rdrop2::drop_download("Coffee Dropshipper/mail_credentials", overwrite = TRUE)
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

        data.table::setnames(items_table, old = c("roaster_name", "label_field"), new = c("", "label file"))
        knitr::kable(items_table, format = "html")
      })
    },
    get_label_names = function(orders) {

      # Extract all needed labels
      label_names <-
        lapply(orders, function(o) o$line_items$label_field) %>%
        unlist() %>%
        unique()

      return(label_names)
    },
    build_email_body = function(titles, tables) {
      l <- length(titles)
      body <- ""

      for (i in 1:l) {
        body <- paste(body, titles[[i]], tables[[i]], sep = "\n")
      }

      return(body)
    }
  )
)
