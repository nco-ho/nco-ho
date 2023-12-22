## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## plotsWithFiltersIndividualCompareUI.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: UI code voor plotsWithFilters module
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plotsWithFiltersIndividualCompareUI <- function(id) {
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
          ## TODO Tabel popover uitgezet
          #        )) %>%
          # tabellenPopover(tabblad = "Tabel"),
          #tabPanel("Tabel", tableOutput(NS(id, "tabel"))),
          tabPanel(span("Samenstelling (%)",
                        title = "info tekst",`data-toggle` = "tooltip"),
                   withSpinner(plotlyOutput(NS(id,"plot_samenstelling_p")),
                               type = 4)),
          tabPanel(span("Samenstelling (#)",
                        title = "info tekst",`data-toggle` = "tooltip"),
                   withSpinner(plotlyOutput(NS(id,"plot_samenstelling_n")),
                               type = 4)),
          tabPanel(span("Spreiding (boxplot)",
                        title = "info tekst",`data-toggle` = "tooltip"),
            uiOutput(NS(id,"boxplot_spreiding_warning")),
            withSpinner(plotlyOutput(NS(id, "boxplot"), height = "auto"), type = 4)
          ),
          tabPanel(span("Spreiding",
                        title = "info tekst",`data-toggle` = "tooltip"),
            uiOutput(NS(id,"plot_spreiding_warning")),
            withSpinner(uiOutput(NS(id, "histogram"), height = "auto"), type = 4))
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
          pickerVar(id, "x"),
          pickerVar(id, "color", "Kleur")
        )
      ),
      sidebarMenu(
        menuItem(
          text = "Filters",
          startExpanded = TRUE,
          icon = icon("signal"),
          pickerValues(
            id = id,
            variable = "cohort",
            role = "both",
            selected = c(
              "2018;INS_Inschrijvingsjaar_EOI",
              "2019;INS_Inschrijvingsjaar_EOI",
              "2020;INS_Inschrijvingsjaar_EOI")
          ),
      #   )
      # ),
      # sidebarMenu(
      #   menuItem(
      #     text = "Filters",
      #     startExpanded = FALSE,
      #     icon = icon("signal"),
          pickerValues(
            id = id,
            role = "left",
            variable = "phase",
            selected = "B;INS_Opleidingsfase_BPM"
          ),
          pickerValues(
            id = id,
            role = "left",
            variable = "faculty",
            selected = "SBE;INS_Faculteit"
          ),
          pickerValues(
            id = id,
            role = "left",
            variable = "opleiding",
            selected = "B International Business Administration;INS_Opleidingsnaam_2002"

          )
        )
      )
    )
  )
}
