## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## tab_gantt_ind_ui.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: UI code voor gantt individueel
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tab_gantt_ind_ui <- function(id) {

  ns <- NS(id)

  fluidPage(
    ## Plots ####
    column(
      width = 12,
      fluidRow(
        box(
          width = 12,
          withSpinner(
            plotlyOutput(ns("gantt")), type = 4)
        )
      )
    ),
    ## Filters + Bookmark ####
    ## INFO: Sidebar
    dashboardSidebar(
      ## TODO: Filters op jaar e.d. toevoegen
      sidebarMenu(
        pickerGanttVar(id, "input_var", df_config_gantt),
        uiOutput(ns("filter_values")),
        uiOutput(ns("target_var"))

      )
    )
  )
}
