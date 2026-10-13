#app/view/s5.R
box::use(
  shiny[h3, moduleServer, tagList, NS, br, fluidRow, column, tags,
         downloadButton, downloadHandler, req, observeEvent, textAreaInput,
         actionButton, uiOutput, HTML, reactiveVal, renderUI],
  dplyr[`%>%`],
  rhandsontable[rHandsontableOutput, renderRHandsontable, rhandsontable, hot_col,
                hot_to_r],
  shinyWidgets[sendSweetAlert],
)

box::use(
  app/logic/s5_parser[parse_s5_input, build_s5_manifest, s5_manifest_csv_lines],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      h3("S5 input", style = "text-align: center"),
      textAreaInput(
        ns("s5_input"),
        label = NULL,
        placeholder = paste0(
          "Une ligne par echantillon, 4 champs separes par espaces :\n",
          "CTRL BC SAMPLE_ID NOM\n",
          "CTL 22 26P1470 BENVENUTI\n",
          "PAN 23 26P1475 GEVAUDAN"
        ),
        rows = 8,
        width = "100%"
      ),
      actionButton(ns("load"), "Load", class = "btn-primary", width = "100%")
    ),
    br(), br(),
    fluidRow(
      h3("Preview table", style = "text-align: center"),
      uiOutput(ns("preview_ui"))
    ),
    br(), br(),
    fluidRow(
      column(width = 6, align = "center",
             actionButton(ns("reset"), "Reset table", width = "100%")),
      column(width = 6, align = "center",
             downloadButton(ns("dlS5"), "Download",
                            style = "width: 100%; display: block;"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    s5_data <- reactiveVal(NULL)             # data.frame du parser
    s5_errors <- reactiveVal(character(0))
    table_version <- reactiveVal(0)

    # --- Load / validation ---
    observeEvent(input$load, {
      req(input$s5_input)
      res <- parse_s5_input(input$s5_input)
      s5_data(res$data)
      s5_errors(res$errors)
      if (length(res$errors)) {
        sendSweetAlert(
          session = session,
          title = "Input S5 invalide",
          text = HTML(paste(res$errors, collapse = "<br>")),
          html = TRUE,
          type = "error"
        )
      }
      table_version(table_version() + 1)
    })

    # --- Reset ---
    observeEvent(input$reset, {
      s5_data(NULL)
      s5_errors(character(0))
      table_version(table_version() + 1)
    })

    # --- Preview (lecture seule + checkbox REPASSE) ---
    output$preview_ui <- renderUI({
      table_version()
      data <- s5_data()
      if (is.null(data) || !nrow(data)) {
        return(tags$p("Collez votre input puis cliquez sur « Load »."))
      }
      rHandsontableOutput(ns("s5_table"))
    })

    output$s5_table <- renderRHandsontable({
      data <- s5_data()
      tbl <- cbind(data, REPASSE = rep(FALSE, nrow(data)))
      rhandsontable(tbl, rowHeaders = FALSE, stretchH = "all", height = 400,
                    readOnly = TRUE) %>%
        hot_col("CTRL", width = 80) %>%
        hot_col("BC", width = 80) %>%
        hot_col("SAMPLE_ID", width = 120) %>%
        hot_col("NOM", width = 140) %>%
        hot_col("REPASSE", type = "checkbox", readOnly = FALSE, width = 90)
    })

    # --- Download : bloqué si erreurs (stopifnot -> la requete download echoue),
    #     lecture des cases REPASSE via la valeur handsontable ---
    output$dlS5 <- downloadHandler(
      filename = function() {
        paste0("S5-", Sys.Date(), ".csv")
      },
      content = function(con) {
        req(s5_data(), "Chargez d'abord un input (bouton Load).")
        stopifnot(length(s5_errors()) == 0)
        repasse <- rep(NA, nrow(s5_data()))
        if (!is.null(input$s5_table)) {
          ht <- hot_to_r(input$s5_table)
          if (!is.null(ht[["REPASSE"]])) {
            repasse <- as.logical(ht[["REPASSE"]])
          }
        }
        manifest <- build_s5_manifest(s5_data(), repasse)
        writeLines(s5_manifest_csv_lines(manifest), con)
      }
    )
  })
}
