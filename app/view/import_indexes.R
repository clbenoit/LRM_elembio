#app/view/import_indexes.R

box::use(
  shiny[h3, moduleServer, tagList, NS, br, fluidRow, column, tags],
  dplyr[filter, `%>%`, select, case_when, mutate, arrange, inner_join, rename],
  bslib[card, card_header, card_body, value_box, layout_column_wrap],
)

box::use(
  #app/logic/selectVariables[selectVariables],

)

#' @export
#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    card(id = "mycard", width = 12, full_screen = FALSE, min_height = '250px',
         card_header("BlaBlaCardHeader"),
         card_body(
           fluidRow(
             layout_column_wrap(
               width = 1/2,
               "wrapped content"
             )
           ))),
    br(),
    # conditionalPanel(
    #   condition = sprintf("output['%s'] < 4", ns("sample_count")),
      column(width = 12,
             tags$div("Not enough samples to render the t-SNE plot. Please select at least 4 samples.",
                      style = "color: red; font-weight: bold; text-align: center;")
             ),
    #),
    # Conditional Panel to show the plot if there are at least 4 samples
    # conditionalPanel(
    #   condition = sprintf("output['%s'] >= 4", ns("sample_count")),
    #   column(width = 12,
    #          #plotlyOutput(ns("current_tsne_plot"), height = "800px")
    #          )
    # )
  )
}

#' @export
server <- function(id, con, appData, main_session) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

  })
}
