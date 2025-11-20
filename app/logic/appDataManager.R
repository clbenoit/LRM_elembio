box::use(
  R6[R6Class],
  shiny[reactiveValues, observeEvent, shinyOptions, req, reactive],
  RSQLite[SQLite],
  dplyr[`%>%`, filter, rename],
  stats[setNames],
  config[get],
  DBI[dbReadTable, dbGetQuery, dbExistsTable, dbExecute, dbSendQuery],
  utils[read.table],
  shinybusy[remove_modal_spinner, show_modal_spinner],
)

#' @export
appDataManager <- R6::R6Class(
  classname = "DataManager",
  public = list(
    con = NULL,
    selectors = reactiveValues(analysis_name = NULL),
    data = reactiveValues(template_settings = NULL, manifest_samples_info = NULL, manifest = NULL,
                          correspondances = NULL, positions = NULL),
    loadTemplates = function(analysis_name = NULL) {
      print("load manifest templates")
      req(analysis_name)
      print('analysis name')
      print(analysis_name)
      shinybusy::show_modal_spinner(
        spin = "double-bounce", color = "#112446",
        text = "Loading data")

         if(analysis_name %in% c("TS65","Hema_M_L_CHUGA")){
           kit_name <- "XT-HS2"
         } else if (analysis_name %in% c("Myogre","Exomes")){
           kit_name <- "XT-HS"
         }

        if (file.exists(paste0("app/data/Template_Settings_", kit_name, ".csv"))) {
          print("Loading templates")

         template_settings <- read.table(paste0("app/data/Template_Settings_", kit_name, ".csv"),
                                sep = ',',
                                header = TRUE,
                                )[1:5,]  %>%
           rename(COL1 = "X.SETTINGS.", COL2 = "X", COL3 = "X.1", COL4 = "X.2")

         self$data$template_settings <- template_settings
      }

      if (file.exists(paste0("app/data/index_correspondances_", kit_name, ".csv"))) {
        print("Loading correspondances")

        correspondances <- read.table(paste0("app/data/index_correspondances_", kit_name, ".csv"),
                                      sep = ',',
                                      header = TRUE
                                      )
        self$data$correspondances <- correspondances
      }
      remove_modal_spinner()
    }
  )
)
