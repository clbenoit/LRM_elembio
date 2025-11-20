#app/view/import_indexes.R

box::use(
  shiny[h3, moduleServer, tagList, NS, br, fluidRow, column, tags,
        downloadButton, downloadHandler, req, observeEvent,uiOutput,
        textAreaInput, actionButton, dataTableOutput, HTML, reactiveVal, renderUI],
  DT[renderDT, datatable],
  dplyr[filter, `%>%`, select, case_when, mutate, arrange, inner_join, rename,
        bind_rows, left_join, ungroup, group_by, row_number, slice],
  bslib[card, card_header, card_body, value_box, layout_column_wrap],
  utils[write.table, head, read.table],
  readr[read_tsv],
  stringr[str_detect],
  tibble[add_row],
  tidyr[pivot_wider, fill],
  rhandsontable[rHandsontableOutput, renderRHandsontable, rhandsontable, hot_col, hot_to_r],
  shinyWidgets[sendSweetAlert],
)

box::use(
  #app/logic/selectVariables[selectVariables],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    card(id = "mycard", width = 12, full_screen = FALSE, min_height = '250px',
         card_body(
           fluidRow(
            uiOutput(ns("table_ui"))),
            #rHandsontableOutput(ns("table_input"))),#),
            fluidRow(actionButton(ns("reset"), "Reset table"),
           ),
           fluidRow(
             dataTableOutput(ns("preview_table"))
           ),
           br(), br(),
           downloadButton(ns("dlManifest"))
           )
         ),
    br(),
  )
}

#' @export
server <- function(id, con, appData, main_session) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    default_data <- data.frame(
      SAMPLE_ID = character(1),
      SAMPLE_DESCRIPTION = character(1),
      WELL_ID = character(1),
      stringsAsFactors = FALSE
    )

    table_data <- reactiveVal(default_data)
    table_version <- reactiveVal(0)

    output$table_ui <- renderUI({
      table_version()
      rHandsontableOutput(ns("table_input"))
    })

    output$table_input <- renderRHandsontable({
      rhandsontable(table_data(), rowHeaders = NULL, stretchH = "all") %>%
        hot_col("SAMPLE_ID") %>%
        hot_col("SAMPLE_DESCRIPTION") %>%
        hot_col("WELL_ID")
    })

    observeEvent(input$table_input, {
      req(input$table_input)
      table_data(hot_to_r(input$table_input))   # always update table_data
      positions <- hot_to_r(input$table_input)
      print("POSITIONS ::")
      if (any(positions$SAMPLE_ID != "")) {
        positions <- positions %>% filter(if_any(everything(), ~ trimws(.) != ""))
        if (any(!str_detect(positions$SAMPLE_ID, "^[A-Za-z0-9-]+$"))) {
           sendSweetAlert(session = session, title = "Identifiant d'échantillon invalide",
                          text = HTML("Au moins un de vos échantillon contient un charactère spécial interdit ou bien un espace..."),
                          html = TRUE, type = "error")
          appData$data$positions <- NULL
          appData$data$manifest_samples_info <- data.frame(
            SAMPLE_ID = "<span style='color:red; font-weight:bold;'>Vérifiez vos noms d'échantillons</span>",
            SAMPLE_DESCRIPTION = "<span style='color:red; font-weight:bold;'>Vérifiez vos noms d'échantillons</span>",
            WELL_ID = "<span style='color:red; font-weight:bold;'>Vérifiez vos noms d'échantillons</span>",
            stringsAsFactors = FALSE
          )
        } else {
          appData$data$positions <- positions
        }
      } else {
        appData$data$positions <- NULL
        appData$data$manifest_samples_info <- NULL
      }
    })

    observeEvent(input$reset, {
      table_data(default_data)
      appData$data$positions <- NULL
      table_version(table_version() + 1)
    })

    observeEvent(c(appData$data$positions, appData$data$correspondances), {
      req(appData$data$positions, appData$data$correspondances)

      if(appData$selectors$analysis_name %in% c("TS65","Hema_M_L_CHUGA")){
        manifest_samples_info <- data.table::setDT(
          appData$data$positions %>%
            left_join(appData$data$correspondances, by = "WELL_ID") %>%
            select(c("SAMPLE_ID","SEQUENCE"))
        )

        manifest_samples_info <- manifest_samples_info %>%
          group_by(SAMPLE_ID) %>%
          mutate(Index = paste0("Index", row_number())) %>%
          pivot_wider(names_from = Index, values_from = SEQUENCE)

        if(!("Index2" %in% colnames(manifest_samples_info))){
          sendSweetAlert(session = session, title = "Aucun index trouvé !",
                         text = HTML("Êtes-vous sûr qu’au moins une des valeurs saisies dans la colonne WELL_ID correspond à celles attendues pour le kit XT-HS2 ?"),
                         html = TRUE, type = "error")
        } else {
          manifest_samples_info <- manifest_samples_info %>%
            ungroup() %>%
            rename(COL1 = "SAMPLE_ID", COL2 = "Index1", COL3 = "Index2") %>%
            mutate(COL4 = "1+2") %>%
            add_row(COL1 = "SampleName", COL2 = "Index1", COL3 = "Index2", COL4 = "Lane", .before = 1) %>%
            add_row(COL1 = "[SAMPLES]", .before = 1)

          appData$data$manifest_samples_info <- manifest_samples_info
        }

      } else if (appData$selectors$analysis_name %in% c("Myogre","Exomes")) {
        manifest_samples_info <- appData$data$positions %>%
          left_join(appData$data$correspondances, by = "WELL_ID")

        print(manifest_samples_info)
        print(unique(manifest_samples_info$SEQUENCE))
        if (length(unique(unique(manifest_samples_info$SEQUENCE))) == 1 && is.na(unique(manifest_samples_info$SEQUENCE))) {
        #if (unique(manifest_samples_info$SEQUENCE) == NA) {
          sendSweetAlert(session = session, title = "Aucun index trouvé !",
                         text = HTML("Êtes-vous sûr qu’au moins une des valeurs saisies dans la colonne WELL_ID correspond à celles attendues pour le kit XT-HS ?"),
                         html = TRUE, type = "error")
          manifest_samples_info <- NULL
        } else {
          manifest_samples_info <- manifest_samples_info %>%
            select(c("SAMPLE_ID","SEQUENCE")) %>%
            rename(COL1 = "SAMPLE_ID", COL2 = "SEQUENCE") %>%
            mutate(COL3 = "1+2") %>%
            add_row(COL1 = "SampleName", COL2 = "Index1", COL3 = "Lane", .before = 1) %>%
            add_row(COL1 = "[SAMPLES]", .before = 1)
        }
        appData$data$manifest_samples_info <- manifest_samples_info
      }
    })

    observeEvent(c(appData$data$manifest_samples_info, appData$data$template_settings), {
      req(appData$data$template_settings, appData$data$manifest_samples_info)

      manifest <- bind_rows(appData$data$template_settings,
                            appData$data$manifest_samples_info) %>%
        add_row(COL1 = "[SETTINGS]", .before = 1)

      appData$data$manifest <- manifest
    })

    output$dlManifest <- downloadHandler(
      filename = function() { paste0('Manifest-', Sys.Date(), '.csv') },
      content = function(con) {
        req(appData$data$manifest)
        write.table(
          appData$data$manifest,
          file = con, col.names = FALSE, na = "",
          sep = ",", quote = FALSE, row.names = FALSE
        )
      }
    )

    output$preview_table <- renderDT({
      req(appData$data$manifest_samples_info)
      datatable(appData$data$manifest_samples_info, rownames = FALSE, escape = FALSE)
    })

  })
}
