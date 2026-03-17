box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, numericInput,
        shinyOptions, p , icon, observeEvent, selectizeInput, req, fluidRow, column,
        br, actionButton],
  config[get],
  cachem[cache_disk],
  DBI[dbConnect],
  RSQLite[SQLite],
  bslib[page_fluid, page_navbar, nav_panel, nav_spacer, nav_menu, nav_item,
        bs_theme],
)

box::use(
  app/logic/appDataManager[appDataManager],
  app/view/import_indexes,
)

# link_github <- tags$a(
#   icon("github"),"Code",
#   href = "https://github.com/clbenoit/LRM_elembio",
#   target = "_blank"
# )
# link_doc <- tags$a(
#   icon("book")," Documentation",
#   href = "https://clbenoit.github.io/portfolio/",
#   target = "_blank"
# )

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    page_navbar(
      title = "LRM_elembio",
      tags$style(shiny::HTML("
      .navbar {
          background: linear-gradient(to right, #E40303, #FF8C00, #FFED00, #008026, #004DFF, #750787);
        }
      ")),
      theme = bs_theme(
        bootswatch = "flatly",
        #bootswatch = "minty",
        bg = "#ffffff",
        fg = "#000000",
        danger = "#E40303",  # rouge
        warning = "#FF8C00", # orange
        success = "#FFED00",   # jaune
        info = "#008026",      # vert
        secondary = "pink",
        primary = "#750787",     # violet,
        base_font = "Comic Neue",
        heading_font = "Lobster"
      ),
      tags$link(rel = "stylesheet", href = "styles/fonts.css"),
      # theme = bs_theme(bootswatch = "darkly",
      #                  # bg = "#FCFDFD",
      #                  # fg = "rgb(25, 125, 85)"
      #
      underline = TRUE,
      nav_panel(title = "Manifest Builder",
                fluidRow(
                  column(
                    width = 4, 
                    selectizeInput(
                      ns("analysis_name"),
                      label = "Analysis selector", # Traduit: "Sélecteur d'analyse"
                      choices = c("Myogre", "Exomes", "TS65/GHEM-FFPE", "Hema_M_L_CHUGA"),
                      selected = "TS65/GHEM-FFPE",
                      width = "100%"
                    )
                  ),
                  column(
                    width = 4, 
                    numericInput(
                      ns("num_rows"), 
                      label = "Number of rows:", # Traduit: "Nombre de lignes :"
                      value = 8, 
                      min = 1, 
                      max = 112, 
                      width = "100%"
                    )
                  ),
                  column(
                    width = 4, 
                    br(), 
                    actionButton(
                      ns("generate_rows"), 
                      label = "Apply", # Déjà en anglais dans votre snippet
                      class = "btn-primary", 
                      width = "100%"
                    )
                  )
                ),
                div(
                  style = "padding-bottom: 25px;",
                fluidRow(import_indexes$ui(ns("import_indexes")))),
      ),
      nav_panel(title = "Bases2Fastqs",
                p("Coming soon")),
      nav_spacer(),
      nav_menu(
        title = "Links",
        align = "right",
        # nav_item(link_github),
        # nav_item(link_doc)
        # nav_item(link_BLABLA),
        # nav_item(link_BLABLA2)
      )
    ))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ## shiny options ##
    options(future.globals.maxSize = 10000*1024^2)
    # set up cache directory ##
    Sys.setenv(R_CONFIG_ACTIVE = "default")
    tempdir <- tempdir()
    if (get("cache_directory") ==  "default") {
      dir.create(file.path(tempdir, "cache"))
      print(paste0("using following cache directory : ", file.path(tempdir, "cache")))
      shinyOptions(cache = cache_disk(file.path(tempdir,"cache")))
    } else {
      print(paste0("using following cache directory : ",
                   get("cache_directory")))
      shinyOptions(cache = cache_disk(get("cache_directory")))
    }
    # Set up default user
    if(Sys.getenv("SHINYPROXY_USERNAME") == ""){
      Sys.setenv(SHINYPROXY_USERNAME = "Me")
    }

    appDataManager <- appDataManager$new()
    observeEvent(input$analysis_name,{
      req(input$analysis_name)
      print("observeer input$analysis_name")
      appDataManager$selectors$analysis_name <- input$analysis_name
      appDataManager$loadTemplates(analysis_name = appDataManager$selectors$analysis_name)
    })
    observeEvent(input$generate_rows,{
      req(input$generate_rows)
      req(input$num_rows)
      print("observeer input$generate_rows")
      appDataManager$selectors$num_rows <- input$num_rows
      appDataManager$selectors$generate_rows <- input$generate_rows
    })

    import_indexes$server("import_indexes", appData = appDataManager, main_session = session)

    })

}
