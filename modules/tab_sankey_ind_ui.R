## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## tab_sankey_ind_ui.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: UI code voor gantt individueel
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tab_sankey_ind_ui <- function(id) {

  ns <- NS(id)

  fluidPage(
    ## Plots ####
    column(
      width = 12,
      fluidRow(
        box(
          width = 12,
          withSpinner(
            plotOutput(ns("sankey")), type = 4)
        )
      )
    ),
    ## Filters + Bookmark ####
    ## INFO: Sidebar
    dashboardSidebar(
      ## TODO: Filters op jaar e.d. toevoegen
      sidebarMenu(
        pickerSankeyVar(id, dfCHO_doorstroom, df_config_sankey, "left_var"),
        uiOutput(ns("filter_links")),
        pickerSankeyVar(id, dfCHO_doorstroom, df_config_sankey, "right_var"),
        uiOutput(ns("filter_rechts"))
      )
    )
  )

}
