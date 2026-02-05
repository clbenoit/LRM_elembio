#app/view/import_indexes.R

box::use(
  shiny[h3, moduleServer, tagList, NS, br, fluidRow, column, tags,
        downloadButton, downloadHandler, req, observeEvent,uiOutput, div,
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
      LANE = "", 
      stringsAsFactors = FALSE
    )

    table_data <- reactiveVal(default_data)
    table_version <- reactiveVal(0)

    output$table_ui <- renderUI({
      table_version()
        rHandsontableOutput(ns("table_input"))
    })

    output$table_input <- renderRHandsontable({
      rhandsontable(
        table_data(),
        rowHeaders = NULL,
        stretchH = "all",
        height = 400
      ) %>%
        hot_col("SAMPLE_ID", width = 120) %>%
        hot_col("SAMPLE_DESCRIPTION", width = 80) %>%
        hot_col("WELL_ID", width = 120) %>%
        hot_col(
          "LANE",
          type = "dropdown",
          source = c("1+2", "1", "2"),
          width = 120
        )
    })

    observeEvent(input$table_input, {
      req(input$table_input)

      positions <- hot_to_r(input$table_input)
      table_data(positions) 

      if (any(positions$SAMPLE_ID != "")) {
        valid_positions <- positions %>% filter(if_any(everything(), ~ trimws(.) != ""))

        if (any(!str_detect(valid_positions$SAMPLE_ID, "^[A-Za-z0-9-_]+$"))) {
          sendSweetAlert(
            session = session,
            title = "Identifiant d'échantillon invalide",
            text = HTML("Au moins un de vos échantillons contient un caractère spécial interdit ou un espace."),
            html = TRUE, type = "error"
          )
          appData$data$positions <- NULL
          appData$data$manifest_samples_info <- data.frame(
            SAMPLE_ID = "<span style='color:red; font-weight:bold;'>Vérifiez vos noms d'échantillons</span>",
            SAMPLE_DESCRIPTION = "<span style='color:red; font-weight:bold;'>Vérifiez vos noms d'échantillons</span>",
            WELL_ID = "<span style='color:red; font-weight:bold;'>Vérifiez vos noms d'échantillons</span>",
            stringsAsFactors = FALSE
          )
          return(NULL)
        } else {
          appData$data$positions <- valid_positions
        }
      } else {
        appData$data$positions <- NULL
        appData$data$manifest_samples_info <- NULL
      }

      if (!is.null(appData$data$positions)) {
        lane_values <- appData$data$positions$LANE
        lane_values[is.na(lane_values)] <- ""  # NA devient vide pour le test

        if (any(!lane_values %in% c("1+2", "1", "2") & lane_values != "")) {
          sendSweetAlert(
            session = session,
            title = "Valeur de LANE invalide",
            text = HTML("Au moins une valeur de la colonne <b>LANE</b> n'est pas valide.<br>
                     Les valeurs autorisées sont : <b>1+2</b>, <b>1</b> ou <b>2</b>."),
            html = TRUE,
            type = "error"
          )

          appData$data$positions <- NULL
          appData$data$manifest_samples_info <- NULL
          return(NULL)
        }
      }
      
      if (!is.null(appData$data$positions)) {
        if (!length(appData$data$positions$SAMPLE_ID) == length(unique(appData$data$positions$SAMPLE_ID))) {
          sendSweetAlert(
            session = session,
            title = "Nom d'echantillon dupliqué",
            text = HTML("Au moins un de vos noms d'echantillon est présent deux fois dans vos données d'entrées."),
            html = TRUE,
            type = "error"
          )
          
          appData$data$positions <- NULL
          appData$data$manifest_samples_info <- NULL
          return(NULL)
        }
      }
      
      if (!is.null(appData$data$positions)) {
        if (!length(appData$data$positions$WELL_ID) == length(unique(appData$data$positions$WELL_ID))) {
          sendSweetAlert(
            session = session,
            title = "Nom de puit dupliqué",
            text = HTML("Au moins un de vos noms de puits est présent deux fois dans vos données d'entrées."),
            html = TRUE,
            type = "error"
          )
          
          appData$data$positions <- NULL
          appData$data$manifest_samples_info <- NULL
          return(NULL)
        }
      }
      
      
    })

    observeEvent(input$reset, {
      table_data(default_data)
      appData$data$positions <- NULL
      table_version(table_version() + 1)
    })

    observeEvent(c(appData$data$positions, appData$data$correspondances), {
      req(appData$data$positions, appData$data$correspondances)

      if(appData$selectors$analysis_name %in% c("TS65/GHEM-FFPE","Hema_M_L_CHUGA")){
        manifest_samples_info <- data.table::setDT(
          appData$data$positions %>%
            left_join(appData$data$correspondances, by = "WELL_ID") %>%
            select(c("SAMPLE_ID","SEQUENCE", "LANE"))
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
            ungroup() #%>%

         manifest_samples_info <- manifest_samples_info %>%
            rename(COL1 = "SAMPLE_ID", COL2 = "Index1", COL3 = "Index2", COL4 = "LANE") %>%
            select(COL1,COL2,COL3,COL4) %>%
            #mutate(COL4 = "1+2") %>%
            add_row(COL1 = "PhiX", COL2 = "ATGTCGCT", COL3 = "CTAGCTCG", COL4 = "1+2", .before = 1) %>%
            add_row(COL1 = "PhiX", COL2 = "CACAGATC", COL3 = "CACAGATC", COL4 = "1+2", .before = 1) %>%
            add_row(COL1 = "PhiX", COL2 = "GCACATAG", COL3 = "GACTACTA", COL4 = "1+2", .before = 1) %>%
            add_row(COL1 = "PhiX", COL2 = "TGTGTCGA", COL3 = "TGTCTGAC", COL4 = "1+2", .before = 1) %>%
            add_row(COL1 = "SampleName", COL2 = "Index1", COL3 = "Index2", COL4 = "Lane", .before = 1) %>%
            add_row(COL1 = "[SAMPLES]", .before = 1)

          appData$data$manifest_samples_info <- manifest_samples_info
        }

      } else if (appData$selectors$analysis_name %in% c("Myogre","Exomes")) {
        manifest_samples_info <- appData$data$positions %>%
          left_join(appData$data$correspondances, by = "WELL_ID")

        if (length(unique(unique(manifest_samples_info$SEQUENCE))) == 1 && is.na(unique(manifest_samples_info$SEQUENCE))) {
          sendSweetAlert(session = session, title = "Aucun index trouvé !",
                         text = HTML("Êtes-vous sûr qu’au moins une des valeurs saisies dans la colonne WELL_ID correspond à celles attendues pour le kit XT-HS ?"),
                         html = TRUE, type = "error")
          manifest_samples_info <- NULL
        } else {
          manifest_samples_info <- manifest_samples_info %>%
            select(c("SAMPLE_ID","SEQUENCE", "LANE")) %>%
            rename(COL1 = "SAMPLE_ID", COL2 = "SEQUENCE", COL3 = "LANE") #%>%

          manifest_samples_info <- manifest_samples_info %>%
            add_row(COL1 = "PhiX", COL2 = "ATGTCGCT", COL3 = "1+2", .before = 1) %>%
            add_row(COL1 = "PhiX", COL2 = "CACAGATC", COL3 = "1+2", .before = 1) %>%
            add_row(COL1 = "PhiX", COL2 = "GCACATAG", COL3 = "1+2", .before = 1) %>%
            add_row(COL1 = "PhiX", COL2 = "TGTGTCGA", COL3 = "1+2", .before = 1) %>%
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
      filename = function() { paste0('Manifest-',appData$selectors$analysis_name,"-", Sys.Date(), '.csv') },
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
        preview_table <- appData$data$manifest_samples_info %>%
          filter(COL1 != "PhiX")  %>%
          {
            colnames(.) <- as.character(.[2, ])
            .
          } %>%
          slice(-(1:2))
    },
    options = list(
      autoWidth = FALSE,
      columnDefs = list(
        list(width = "60px", targets = 0, className = "dt-center")
      )
    ))

  })
}
