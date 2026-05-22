#app/view/import_indexes.R
box::use(
  shiny[h3, moduleServer, tagList, NS, br, fluidRow, column, tags,
        downloadButton, downloadHandler, req, observeEvent,uiOutput, div,
        textAreaInput, actionButton, numericInput , dataTableOutput, HTML, reactiveVal, renderUI],
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
    fluidRow(
      shiny::h3("Input table", style = "text-align: center"),
      uiOutput(ns("table_ui"))),#,
    #br(), br(),
    fluidRow(
      #br(),
      div(
        style = "padding-top: 25px;",
      shiny::h3("Preview table", style = "text-align: center"),
      dataTableOutput(ns("preview_table")))),
    br(), br(),
    fluidRow(
      column(
        width = 6,
        align = "center",
        actionButton(ns("reset"), "Reset table", width = "100%")
      ),
      column(
        width = 6,
        align = "center",
        downloadButton(
          ns("dlManifest"), 
          label = "Download", 
          style = "width: 100%; display: block;"
        )
      )
    ),
    #br(),br(),br(),
  )
}

#' @export
server <- function(id, con, appData, main_session) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # --- 1. Logique de données ---
    
    create_empty_data <- function(n) {
      data.frame(
        SAMPLE_ID = rep("", n),
        SAMPLE_DESCRIPTION = rep("", n),
        WELL_ID = rep("", n),
        LANE = rep("1+2", n), 
        stringsAsFactors = FALSE
      )
    }
    
    table_data <- reactiveVal(create_empty_data(8))
    table_version <- reactiveVal(0) 
    
    # FONCTION DE VALIDATION CENTRALISÉE
    # Elle prend les données brutes du tableau et met à jour appData$data$positions
    validate_and_update <- function(positions) {
      if (any(positions$SAMPLE_ID != "" & !is.na(positions$SAMPLE_ID))) {
        valid_positions <- positions %>% 
          filter(if_any(everything(), ~ !is.na(.) & trimws(as.character(.)) != ""))
        
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
            appData$data$manifest_samples_info <- NULL
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
        
        appData$data$positions <- valid_positions
        return(TRUE)
      } else {
        appData$data$positions <- NULL
        return(FALSE)
      }
    }
    
    # --- 2. ObserveEvents ---
    # Generation / Modification du nombre de lignes
    observeEvent(appData$selectors$generate_rows, {  
      n_target <- appData$selectors$num_rows
      if (is.na(n_target) || n_target < 1) return(NULL)
      
      current_df <- table_data()
      n_current <- nrow(current_df)
      
      if (n_target > n_current) {
        updated_data <- rbind(current_df, create_empty_data(n_target - n_current))
      } else {
        updated_data <- current_df[1:n_target, ]
      }
      
      table_data(updated_data)
      validate_and_update(updated_data) # <--- Force la mise à jour de la preview
      table_version(table_version() + 1)
    })
    
    # Reset
    observeEvent(input$reset, {
      new_df <- create_empty_data(1)
      table_data(new_df)
      appData$data$positions <- NULL
      appData$data$manifest_samples_info <- NULL
      table_version(table_version() + 1)
    })
    
    # Saisie manuelle
    observeEvent(input$table_input, {
      req(input$table_input)
      positions <- hot_to_r(input$table_input)
      table_data(positions)
      validate_and_update(positions)
    })
    
    # --- 3. Sorties Tableaux ---
    output$table_ui <- renderUI({
      table_version()
      rHandsontableOutput(ns("table_input"))
    })
    
    output$table_input <- renderRHandsontable({
      rhandsontable(table_data(), rowHeaders = NULL, stretchH = "all", height = 400) %>%
        hot_col("SAMPLE_ID", width = 120) %>%
        hot_col("LANE", type = "dropdown", source = c("1+2", "1", "2"), width = 80)
    })
    
    # --- 4. Logique Manifest ---
    observeEvent(c(appData$data$positions, appData$data$correspondances), {
      req(appData$data$positions, appData$data$correspondances)
      
      if(appData$selectors$analysis_name %in% c("TS65/GHEM-FFPE","Hema_M_L_CHUGA","Hedera")){
        manifest_samples_info <- data.table::setDT(
          appData$data$positions %>%
            left_join(appData$data$correspondances, by = "WELL_ID") %>%
            select(c("SAMPLE_ID","SEQUENCE", "LANE"))
        )
        
        manifest_samples_info <- manifest_samples_info %>%
          group_by(SAMPLE_ID) %>%
          mutate(Index = paste0("Index", row_number())) %>%
          pivot_wider(names_from = Index, values_from = SEQUENCE)
        
        print(colnames(manifest_samples_info))
        if(!("Index2" %in% colnames(manifest_samples_info))){
          sendSweetAlert(session = session, title = "Aucun index trouvé !",
                         text = HTML("Êtes-vous sûr qu’au moins une des valeurs saisies dans la colonne WELL_ID correspond à celles attendues pour le kit XT-HS2 ?"),
                         html = TRUE, type = "error")
        } else {
          manifest_samples_info <- manifest_samples_info %>%
            ungroup() #%>%
          
          if(appData$selectors$analysis_name == "Hedera"){
            manifest_samples_info <- manifest_samples_info %>%
              rename(COL1 = "SAMPLE_ID", COL2 = "Index1", COL3 = "Index2", COL4 = "LANE") %>%
              select(COL1,COL2,COL3,COL4) %>%
              #mutate(COL4 = "1+2") %>%
              add_row(COL1 = "PhiX", COL2 = "ATGTCGCTAG", COL3 = "CTAGCTCGTA", COL4 = "1+2", .before = 1) %>%
              add_row(COL1 = "PhiX", COL2 = "CACAGATCGT", COL3 = "ACGAGAGTCT", COL4 = "1+2", .before = 1) %>%
              add_row(COL1 = "PhiX", COL2 = "GCACATAGTC", COL3 = "GACTACTAGC", COL4 = "1+2", .before = 1) %>%
              add_row(COL1 = "PhiX", COL2 = "TGTGTCGACA", COL3 = "TGTCTGACAG", COL4 = "1+2", .before = 1) %>%
              add_row(COL1 = "SampleName", COL2 = "Index1", COL3 = "Index2", COL4 = "Lane", .before = 1) %>%
              add_row(COL1 = "[SAMPLES]", .before = 1)
          } else {
            manifest_samples_info <- manifest_samples_info %>%
              rename(COL1 = "SAMPLE_ID", COL2 = "Index1", COL3 = "Index2", COL4 = "LANE") %>%
              select(COL1,COL2,COL3,COL4) %>%
              #mutate(COL4 = "1+2") %>%
              add_row(COL1 = "PhiX", COL2 = "ATGTCGCT", COL3 = "CTAGCTCG", COL4 = "1+2", .before = 1) %>%
              add_row(COL1 = "PhiX", COL2 = "CACAGATC", COL3 = "ACGAGAGT", COL4 = "1+2", .before = 1) %>%
              add_row(COL1 = "PhiX", COL2 = "GCACATAG", COL3 = "GACTACTA", COL4 = "1+2", .before = 1) %>%
              add_row(COL1 = "PhiX", COL2 = "TGTGTCGA", COL3 = "TGTCTGAC", COL4 = "1+2", .before = 1) %>%
              add_row(COL1 = "SampleName", COL2 = "Index1", COL3 = "Index2", COL4 = "Lane", .before = 1) %>%
              add_row(COL1 = "[SAMPLES]", .before = 1)
          }
          
          appData$data$manifest_samples_info <- manifest_samples_info
        }
        
      } else if (appData$selectors$analysis_name %in% c("Myogre","Exomes")) {
        manifest_samples_info <- appData$data$positions %>%
          left_join(appData$data$correspondances, by = "WELL_ID")
        
        print(unique(unique(manifest_samples_info$SEQUENCE)))
        print(unique(manifest_samples_info$SEQUENCE))
        
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
    
    output$preview_table <- renderDT({
      req(appData$data$manifest_samples_info)
      # On reprend exactement votre logique de filtrage du second code
      appData$data$manifest_samples_info %>%
        filter(COL1 != "PhiX") %>%
        {
          if (nrow(.) >= 2) {
            colnames(.) <- as.character(.[2, ])
            slice(., -(1:2))
          } else { . }
        }
    }, options = list(dom = 'tip', autoWidth = FALSE))
    
    observeEvent(c(appData$data$manifest_samples_info, appData$data$template_settings), {
      req(appData$data$template_settings, appData$data$manifest_samples_info)
      
      manifest <- bind_rows(appData$data$template_settings,
                            appData$data$manifest_samples_info) %>%
        add_row(COL1 = "[SETTINGS]", .before = 1)
      
      appData$data$manifest <- manifest
    })
    
    output$dlManifest <- downloadHandler(
      filename = function() { paste0('Manifest-', appData$selectors$analysis_name, "-", Sys.Date(), '.csv') },
      content = function(con) {
        req(appData$data$manifest)
        write.table(appData$data$manifest, file = con, col.names = FALSE, na = "", sep = ",", quote = FALSE, row.names = FALSE)
      }
    )
  })
}