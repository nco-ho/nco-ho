## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## plotsWithFiltersAggregatedCompareUI.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: UI code voor plotsWithFilters module
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plotsWithFiltersAggregatedCompareUI <- function(id) {
  fluidPage(
    ## Plots ####
    column(
      width = 12,
      fluidRow(
        box(
          width = 12,
          withSpinner(
            plotlyOutput(NS(id, "plot_doel_geaggregeerd")), type = 4)
        ),
        tabBox(
          tags$script(HTML('
           $( document ).on("shiny:sessioninitialized", function(event) {
                $(\'span[data-toggle="tooltip"]\').tooltip({
                    html: true
                });
           });')),
          width = 12,
          id = "tab_ondersteunend",
          ### Tabs ####
          tabPanel(tagList(shiny::icon("circle-info")),
                   uiOutput(NS(id,"tabel_info"))),
          tabPanelTables(id),
          ## TODO Tabel popover uitgezet
          #        )) %>%
          # tabellenPopover(tabblad = "Tabel"),
          #tabPanel("Tabel", tableOutput(NS(id, "tabel"))),
          tabPanel(span("Samenstelling (%)",
                        title = "info tekst",`data-toggle` = "tooltip"),
                   withSpinner(plotlyOutput(NS(id,"plot_samenstelling_p")),
                               type = 4)),
            #tabellenPopover(tabblad = "Samenstelling percentages"),
          tabPanel(span("Samenstelling (#)",
                        title = "info tekst",`data-toggle` = "tooltip"),
                   withSpinner(plotlyOutput(NS(id,"plot_samenstelling_n")),
                               type = 4))
            #tabellenPopover(tabblad = "Samenstelling aantallen")
        )
      )
    ),
    ## Filters + Bookmark ####
    ## INFO: Sidebar
    dashboardSidebar(
      sidebarMenu(
        menuItem(
          text = "Doel variabele",
          # tabName = "analytics", # childfull menuItems ignore the tabName parameter, they use expandedName instead
          icon = icon("signal"),
          startExpanded = TRUE,
          pickerVar(id, "y_links"),
          pickerVar(id, "y_rechts")
        )
      ),
      sidebarMenu(
        menuItem(
          text = "Uitsplitsing",
          startExpanded = TRUE,
          icon = icon("signal"),
          commonDrilldownUI(id)

        )
      ),
      sidebarMenu(
        menuItem(
          text = "Filters",
          startExpanded = TRUE,
          icon = icon("signal"),
          commonFiltersUI(id)
        )
      )
    )
  )
}
