## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## plotsWithFiltersIndividualUI.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: UI code voor plotsWithFilters module
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plotsWithFiltersIndividualUI <- function(id) {
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
            withSpinner(plotlyOutput(NS(id, "boxplot"), height = "auto"), type = 4)),
          tabPanel(span("Spreiding",
                        title = "info tekst",`data-toggle` = "tooltip"),
            uiOutput(NS(id,"plot_spreiding_warning")),
            withSpinner(uiOutput(NS(id, "histogram"), height = "auto"), type = 4)),
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
          pickerVar(id, "y")
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
          text = "Filter periode",
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
          )
        )
      ),
      sidebarMenu(
        menuItem(
          text = "Filters Links",
          startExpanded = FALSE,
          icon = icon("signal"),
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
            ##' *INFO* Dit wordt overgeschreven door keep_only_relevant_values
            selected = "B International Business Administration;INS_Opleidingsnaam_2002"

          )
        )
      ),
      sidebarMenu(
        menuItem(
          text = "Filters Rechts",
          startExpanded = FALSE,
          icon = icon("signal"),
          pickerValues(
            id = id,
            role = "right",
            variable = "phase",
            selected = "B;INS_Opleidingsfase_BPM"
          ),
          pickerValues(
            id = id,
            role = "right",
            variable = "faculty",
            selected = "SBE;INS_Faculteit"
          ),
          pickerValues(
            id = id,
            role = "right",
            variable = "opleiding",
            ##' *INFO* Dit wordt overgeschreven door keep_only_relevant_values
            selected = "Alles"

          )
        )
      )
    )
  )
}
#
#     column(
#       width = 3,
#       fluidRow(
#         ## INFO: "Met de bookmark button kunt u een pagina, inclusief de filters, kleur en ingestelde assen, opslaan voor een volgende keer. Ze verschijnen rechts.",
#         ## TODO: bookmark opslaan + dynamische naam meegeven
#         box(
#           id = "bookmark_box",
#           width = 12,
#           headerBorder = FALSE,
#           actionButton(NS(id, "bookmark"), "Bewaar deze pagina", width = "100%")
#         ),
#         ## TODO: Move to css / js
#         ## Hide header box
#         tags$head(tags$style('#bookmark_box .box-header{ display: none}'))
#       ),
#       fluidRow(
#         box(
#           width = 12,
#           collapsible = TRUE,
#           title = "Kies variables",
#           column(width = 12,
#                  pickerVar(id, "x"),
#                  #pickerInput(NS(id, "x"), "X", choices = df %>% names()),
#                  pickerVar(id, "y"),
#                  pickerVar(id, "color")
#           )
#         )
#       ),
#       fluidRow(
#         box(
#           width = 12,
#           collapsible = TRUE,
#           title = "Doelgroep en Benchmark",
#           fluidRow(
#             column(
#               width = 6,
#
#               pickerValues(
#                 id = id,
#                 role = "left",
#                 variable = "faculty",
#                 selected = "SBE;INS_Faculteit"
#               )
#             ),
#             column(
#               width = 6,
#               pickerValues(
#                 id = id,
#                 role = "right",
#                 variable = "faculty",
#                 selected = "SBE;INS_Faculteit"
#               )
#             )
#           ),
#           fluidRow(
#             column(width = 6,
#                    pickerValues(
#                      id = id,
#                      role = "left",
#                      variable = "phase",
#                      selected = "B;INS_Opleidingsfase_BPM"
#                    )),
#             column(width = 6,
#                    pickerValues(
#                      id = id,
#                      role = "right",
#                      variable = "phase",
#                      selected = "B;INS_Opleidingsfase_BPM"
#                    ))
#           ),
#           fluidRow(
#             column(width = 6,
#                    pickerValues(
#                      id = id,
#                      role = "left",
#                      variable = "opleiding",
#                      selected = "B Bedrijfskunde;INS_Opleidingsnaam_2002"
#                    )),
#             column(width = 6,
#                    pickerValues(
#                      id = id,
#                      role = "right",
#                      variable = "opleiding",
#                      selected = "B Bedrijfskunde;INS_Opleidingsnaam_2002"
#                    ))
#           ),
#           fluidRow(
#             column(
#               width = 12,
#               pickerValues(
#                 id = id,
#                 variable = "cohort",
#                 role = "both",
#                 selected = c(
#                   "2018;INS_Inschrijvingsjaar_EOI",
#                   "2019;INS_Inschrijvingsjaar_EOI",
#                   "2020;INS_Inschrijvingsjaar_EOI")
#               )))
#         )
#       )
#     )
#   )
#
# }
