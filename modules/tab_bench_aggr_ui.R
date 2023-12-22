## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## tab_bench_aggr_ui.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: UI code voor plotsWithFilters module
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tab_bench_aggr_ui <- function(id) {

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
          width = 12,
          id = "tab_ondersteunend",
          ### Tabs ####
          tabPanel(tagList(shiny::icon("circle-info")),
                   uiOutput(NS(id,"tabel_info"))),
          tabPanelTables(id),
          ## TODO Tabellen tijdelijk uitgezet
          #tabellenPopover(tabblad = "Tabel"),
          #tabPanel("Tabel", tableOutput(NS(id, "tabel"))),
          tabPanel(span("Samenstelling (%)",
                        title = "info tekst",`data-toggle` = "tooltip"),
                   withSpinner(plotlyOutput(NS(id,"plot_samenstelling_p")),
                               type = 4)),
          tabPanel(span("Samenstelling (#)",
                        title = "info tekst",`data-toggle` = "tooltip"),
                   withSpinner(plotlyOutput(NS(id,"plot_samenstelling_n")),
                               type = 4))
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

          ## TODO: Tijdelijk uitgevinkt
          #pickerVar(id, "x"),
          pickerVar(id, "y")
          #pickerFilter(id, "color")
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
          ##' *INFO* This is some strange code. Actually, 'right' should also be 'commonFilter'.
          ## Compare only lacks a second group to filter on (because two variables are being
          ## compared), and the individual version has the possibility of multiple values, hence it
          ## is different.
          ## TODO Put off temporarily. Now we just VU All
          # pickerValues(
          #   id = id,
          #   role = "right",
          #   variable = "opleiding",
          #   selected = "M Business Analytics;INS_Opleidingsnaam_2002",
          #   multiple = FALSE
          # )
        )
      )
    )
  )

}

