#app/view/import_indexes.R
box::use(
  shiny[h3, moduleServer, tagList, NS, br, fluidRow, column, tags,
        downloadButton, downloadHandler, req, observeEvent,uiOutput, div,
        textAreaInput, actionButton, numericInput , dataTableOutput, HTML, reactive,
        reactiveVal, renderUI],
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
  app/logic/s5_parser[parse_s5_table, build_s5_manifest, build_s5_manifest_df,
                      s5_manifest_csv_lines],
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
    # espace (ligne sautée) entre le preview table et les boutons
    br(), br(), br(), br(),
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
    
    # RNASeq_S5 : schema S5 (tableau) a la place du schema standard
    is_s5 <- reactive(appData$selectors$analysis_name == "RNASeq_S5")

    create_empty_data_s5 <- function(n) {
      data.frame(
        ANALYSIS_TYPE = rep("", n),
        BC_NUMBER = rep("", n),
        SAMPLE_ID = rep("", n),
        PATIENT_ID = rep("", n),
        # checkbox : NA = non repasse (coche = TRUE)
        REPASSE = rep(NA, n),
        stringsAsFactors = FALSE
      )
    }

    # Bascule d'analyse -> reset du tableau au schema de l'analyse (valide par l'user)
    observeEvent(appData$selectors$analysis_name, {
      req(appData$selectors$analysis_name)
      n <- appData$selectors$num_rows
      n <- if (is.null(n) || is.na(n) || n < 1) 8L else as.integer(n)
      table_data(if (appData$selectors$analysis_name == "RNASeq_S5")
                   create_empty_data_s5(n) else create_empty_data(n))
      appData$data$positions <- NULL
      appData$data$manifest_samples_info <- NULL
      appData$data$manifest <- NULL
    })

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
    
    # VALIDATION RNASeq_S5 (parser pur + modal SweetAlert, style existant)
    validate_and_update_s5 <- function(df) {
      res <- parse_s5_table(df)
      if (length(res$errors) > 0) {
        sendSweetAlert(
          session = session,
          title = "Entrees RNASeq_S5 invalides",
          text = HTML(paste(res$errors, collapse = "<br>")),
          html = TRUE, type = "error"
        )
        appData$data$positions <- NULL
        appData$data$manifest_samples_info <- NULL
        appData$data$manifest <- NULL
        return(NULL)
      }
      appData$data$positions <- if (nrow(res$data) > 0) res$data else NULL
      if (is.null(appData$data$positions)) {
        # tableau vide (ou erreur) : pas de manifest en attente
        appData$data$manifest_samples_info <- NULL
        appData$data$manifest <- NULL
      }
    }

    # --- 2. ObserveEvents ---
    # Generation / Modification du nombre de lignes
    observeEvent(appData$selectors$generate_rows, {  
      n_target <- appData$selectors$num_rows
      if (is.na(n_target) || n_target < 1) return(NULL)
      
      current_df <- table_data()
      n_current <- nrow(current_df)
      
      empty_rows <- if (is_s5()) create_empty_data_s5 else create_empty_data
      if (n_target > n_current) {
        updated_data <- rbind(current_df, empty_rows(n_target - n_current))
      } else {
        updated_data <- current_df[1:n_target, ]
      }
      
      table_data(updated_data)
      if (is_s5()) validate_and_update_s5(updated_data) else validate_and_update(updated_data) # <--- Force la mise à jour de la preview
      table_version(table_version() + 1)
    })
    
    # Reset (schema de l'analyse courante ; en S5 le manifest est egalement vide)
    observeEvent(input$reset, {
      table_data(if (is_s5()) create_empty_data_s5(1) else create_empty_data(1))
      appData$data$positions <- NULL
      appData$data$manifest_samples_info <- NULL
      if (is_s5()) appData$data$manifest <- NULL
      table_version(table_version() + 1)
    })
    
    # Saisie manuelle
    observeEvent(input$table_input, {
      req(input$table_input)
      # Echo du widget de l'ANCIEN schema (bascule d'analyse en cours) : le
      # payload porte encore les colonnees precedentes -> on l'ignore, sinon
      # le parser du schema courant recoit un df heterogene et le rendu du
      # tableau bascule sur le mauvais schema.
      payload_cols <- if (!is.null(input$table_input$params$rColHeaders))
        as.character(unlist(input$table_input$params$rColHeaders))
        else character(0)
      expected_cols <- if (is_s5())
        c("ANALYSIS_TYPE", "BC_NUMBER", "SAMPLE_ID", "PATIENT_ID", "REPASSE")
        else c("SAMPLE_ID", "SAMPLE_DESCRIPTION", "WELL_ID", "LANE")
      if (!identical(sort(payload_cols), sort(expected_cols))) return(NULL)
      positions <- hot_to_r(input$table_input)
      table_data(positions)
      if (is_s5()) validate_and_update_s5(positions) else validate_and_update(positions)
    })
    
    # --- 3. Sorties Tableaux ---
    output$table_ui <- renderUI({
      table_version()
      rHandsontableOutput(ns("table_input"))
    })
    
    output$table_input <- renderRHandsontable({
      if (is_s5()) {
        rhandsontable(table_data(), rowHeaders = NULL, stretchH = "all", height = 400) %>%
          hot_col("ANALYSIS_TYPE", width = 120) %>%
          hot_col("BC_NUMBER", width = 90) %>%
          hot_col("SAMPLE_ID", width = 120) %>%
          hot_col("PATIENT_ID", width = 120) %>%
          hot_col("REPASSE", type = "checkbox", width = 90)
      } else {
        rhandsontable(table_data(), rowHeaders = NULL, stretchH = "all", height = 400) %>%
          hot_col("SAMPLE_ID", width = 120) %>%
          hot_col("LANE", type = "dropdown", source = c("1+2", "1", "2"), width = 80)
      }
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
            
            
            print("nanani")
            print(utils::head(manifest_samples_info))
            
            manifest_samples_info <- manifest_samples_info %>%
              rename(COL1 = "SAMPLE_ID", COL2 = "Index1", COL3 = "Index2", COL4 = "LANE") %>%
              select(COL1,COL2,COL3,COL4) %>%
              #mutate(COL4 = "1+2") %>%
              add_row(COL1 = "PhiX", COL2 = "ATGTCGCT", COL3 = "CTAGCTCG", COL4 = "1+2") %>%
              add_row(COL1 = "PhiX", COL2 = "CACAGATC", COL3 = "ACGAGAGT", COL4 = "1+2") %>%
              add_row(COL1 = "PhiX", COL2 = "GCACATAG", COL3 = "GACTACTA", COL4 = "1+2") %>%
              add_row(COL1 = "PhiX", COL2 = "TGTGTCGA", COL3 = "TGTCTGAC", COL4 = "1+2") %>%
              # add_row(COL1 = "PhiX", COL2 = "ATGTCGCT", COL3 = "CTAGCTCG", COL4 = "1+2", .before = 1) %>%
              # add_row(COL1 = "PhiX", COL2 = "CACAGATC", COL3 = "ACGAGAGT", COL4 = "1+2", .before = 1) %>%
              # add_row(COL1 = "PhiX", COL2 = "GCACATAG", COL3 = "GACTACTA", COL4 = "1+2", .before = 1) %>%
              # add_row(COL1 = "PhiX", COL2 = "TGTGTCGA", COL3 = "TGTCTGAC", COL4 = "1+2", .before = 1) %>%
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
    
    # Manifest RNASeq_S5 : pas de template/correspondances (pas de kit)
    observeEvent(appData$data$positions, {
      req(appData$data$positions)
      if (appData$selectors$analysis_name != "RNASeq_S5") return(NULL)
      appData$data$manifest_samples_info <-
        build_s5_manifest_df(appData$data$positions)
    })

    output$preview_table <- renderDT({
      req(appData$data$manifest_samples_info)
      # RNASeq_S5 : la preview n'affiche que les lignes de donnees ; l'entete du
      # tableau = les colonnes Ion (ligne 2 du CSV). La ligne de version et la
      # ligne d'entete ne sont utiles que dans le fichier telecharge.
      if (is_s5()) {
        s5 <- appData$data$manifest_samples_info
        s5 <- if (nrow(s5) >= 2) {
          colnames(s5) <- as.character(s5[2, ])
          slice(s5, -(1:2))
        } else s5
        return(s5) # rownames masquees via rownames = FALSE du renderDT ci-dessous
      }
      # On reprend exactement votre logique de filtrage du second code
      appData$data$manifest_samples_info %>%
        filter(COL1 != "PhiX") %>%
        {
          if (nrow(.) >= 2) {
            colnames(.) <- as.character(.[2, ])
            slice(., -(1:2))
          } else { . }
        }
    }, rownames = FALSE, options = list(dom = 'tip', autoWidth = FALSE))
    
    observeEvent(c(appData$data$manifest_samples_info, appData$data$template_settings), {
      req(appData$data$manifest_samples_info)
      # RNASeq_S5 : le manifest = les lignes du CSV Ion exactes (pas de bloc [SETTINGS]).
      # Caractere (pas df) : la ligne 1 du CSV n'a que 2 champs, write.table ecrirait
      # des champs vides en queue ("CSV Version (required),2.3,,,,,,,").
      if (is_s5()) {
        appData$data$manifest <-
          s5_manifest_csv_lines(build_s5_manifest(appData$data$positions))
        return(NULL)
      }
      req(appData$data$template_settings)
      
      manifest <- bind_rows(appData$data$template_settings,
                            appData$data$manifest_samples_info) %>%
        add_row(COL1 = "[SETTINGS]", .before = 1)
      
      appData$data$manifest <- manifest
    })
    
    output$dlManifest <- downloadHandler(
      filename = function() { paste0('Manifest-', appData$selectors$analysis_name, "-", Sys.Date(), '.csv') },
      content = function(con) {
        req(appData$data$manifest)
        if (is_s5()) {
          writeLines(appData$data$manifest, con)
          return(invisible())
        }
        write.table(appData$data$manifest, file = con, col.names = FALSE, na = "", sep = ",", quote = FALSE, row.names = FALSE)
      }
    )
  })
}