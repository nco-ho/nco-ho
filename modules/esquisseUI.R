esquisseUI <- function(id) {

  ns <- NS(id)
  # esquisse_ui(
  #   id = id,
  #   header = FALSE, # dont display gadget title
  #   container = esquisseContainer(height = "700px")
  # )

  fluidPage(
    column(
      width = 12,
      ## Plots ####
      esquisse_ui(
        id = ns("esquisse"),
        header = FALSE, # dont display gadget title
        container = esquisseContainer(height = "700px")
      )
    )#,
    ## TODO: Tijdelijk filters uitgezet
    ## Filters + Bookmark ####
    ## INFO: Sidebar
    # dashboardSidebar(
    #   sidebarMenu(
    #     menuItem(
    #       text = "Filters",
    #       startExpanded = TRUE,
    #       icon = icon("signal"),
    #       commonFiltersUI(id)
    #     )
    #   )
    # )
  )


}
