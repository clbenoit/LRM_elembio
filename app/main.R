box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput,
        shinyOptions, p , icon, observeEvent, selectizeInput, req],
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
      # theme = bs_theme(bootswatch = "cyborg",
      #                  fg = "#FFFFFF",
      #                  bg = "#000000"
      #                  ),
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
        #secondary = "#004DFF",   # bleu
        #secondary = "#FF66FF",   # bleu
        secondary = "pink",
        primary = "#750787",     # violet,
        # base_font = bslib::font_google("Comic Neue"),
        # heading_font = bslib::font_google("Lobster"),
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
                selectizeInput(ns("analysis_name"),
                               choices = c("Myogre","Exomes",
                                           "TS65","Hema_M_L_CHUGA"),
                               width = "100%",
                               selected = "TS65",
                               label = "Sélecteur d'analyse"),
                import_indexes$ui(ns("import_indexes"))),
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

    ## load database ##
    # db_name <- file.path(get("db_path"), paste0(get("prefix"), ".db"))
    # print(paste0("db name :", db_name))
    # con <- dbConnect(SQLite(), db_name)

    appDataManager <- appDataManager$new()
    #observeEvent(appDataManager$selectors$analysis_name,{
    #req(appDataManager$selectors$analysis_name)
    observeEvent(input$analysis_name,{
      req(input$analysis_name)
      print("observeer input$analysis_name")
      appDataManager$selectors$analysis_name <- input$analysis_name
      appDataManager$loadTemplates(analysis_name = appDataManager$selectors$analysis_name)
    })

    import_indexes$server("import_indexes", appData = appDataManager, main_session = session)

    })

}
