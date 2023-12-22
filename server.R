## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Server.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: Server shiny app NCO-HO project
##
## Afhankelijkheden:
##
## Datasets:
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO:
## 1) ___.
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 23-05-2022: PdO: Aanmaak bestand
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## TODO: Esquisse tijdelijk uitgezet
server <- function(input, output, session) {

  ## Common filters ####
  ## TODO Nu alleen gebruik op één tabblad
  common_drilldown_mod1 <- commonDrilldownServer("bench_aggr")
  common_filters_mod1 <- commonFiltersServer("bench_aggr")
  common_inputs_mod1 <- c(common_drilldown_mod1, common_filters_mod1)

  ## Tabbladen ####
  tab_bench_aggr_server("bench_aggr", common_inputs_mod1)

  plotsWithFiltersIndividualServer("bench_ind")

  common_drilldown_mod2 <- commonDrilldownServer("compare_aggr")
  common_filters_mod2 <- commonFiltersServer("compare_aggr")
  common_inputs_mod2 <- c(common_drilldown_mod2, common_filters_mod2)


  plotsWithFiltersAggregatedCompareServer("compare_aggr", common_inputs_mod2)
  plotsWithFiltersIndividualCompareServer("compare_ind")

  tab_gantt_ind_server("gantt")
  tab_sankey_ind_server("sankey")

  esquisseServer("explorer", session)

  # plotsWithFiltersIndividualCompareServer("individual_compare")

  # ## Common filters multiple tabs ####
  # ## Create objects with output of filter and dropdown for every tab
  # common_drilldown_mod1 <- commonDrilldownServer('toegankelijkheid')
  # common_drilldown_mod2 <- commonDrilldownServer('switchgedrag')
  # common_drilldown_mod3 <- commonDrilldownServer('arbeidsmarkt')
  #
  # common_filters_mod1 <- commonFiltersServer('toegankelijkheid')
  # common_filters_mod2 <- commonFiltersServer('switchgedrag')
  # common_filters_mod3 <- commonFiltersServer('arbeidsmarkt')
  #
  # #common_filters_mod4 <- commonFiltersServer("explorer")
  #
  # common_inputs_mod1 <- c(common_drilldown_mod1, common_filters_mod1)
  # common_inputs_mod2 <- c(common_drilldown_mod2, common_filters_mod2)
  # common_inputs_mod3 <- c(common_drilldown_mod3, common_filters_mod3)
  #
  # #common_inputs_mod4 <- c(common_drilldown_mod3, common_filters_mod4)
  #
  # ## TODO: Tijdelijke code
  #
  #
  # ## TODO
  # ## Update filters one by one to avoid cyclical dependency
  # observeEvent(common_inputs_mod1, {
  #   t2 <- plotsWithFiltersAggregatedServer('switchgedrag', common_inputs_mod1)
  # })
  #
  # observeEvent(common_inputs_mod2, {
  #   t3 <- plotsWithFiltersAggregatedServer('arbeidsmarkt', common_inputs_mod2)
  # })
  #
  # observeEvent(common_inputs_mod3, {
  #   ## Esquisse server needs tablist because it only renders when tab is selected in order
  #   ## to avoid delays
  #   t1 <- plotsWithFiltersAggregatedServer('toegankelijkheid', common_inputs_mod3)
  #   #req(input$tablist)
  #   #t4 <- esquisseServer('explorer', common_inputs_mod3, session)
  # })
  #
  # # observeEvent(common_inputs_mod4, {
  # #   t1 <- plotsWithFiltersAggregatedServer('toegankelijkheid', common_inputs_mod4)
  # # })

  ## UI elements ####
  ## Options for Spinner
  options(spinner.color = "#0073b7",
          spinner.color.background = "#ffffff",
          spinner.size = 1)

  ## Landingspagina
  output$text_landingspagina <- renderUI({includeHTML("www/text_landingspagina.html")})

  ## Based on: https://stackoverflow.com/a/55839639/6375668
  output$tabItems <- renderUI({

    ## TODO Uit global een lijst met namen tabs
    ## Op Gallery app niet nodig

    items <- c(
      list(
        tabItem(
          "landingspagina",
          box(width = 9,
              #id = "landingspagina",
              title = "Welkom!",
              uiOutput("text_landingspagina")
          ))
      ),
      list(
        tabItem(
          "bench_aggr",
          tab_bench_aggr_ui("bench_aggr")
        ),
        tabItem(
          "gantt",
          tab_gantt_ind_ui("gantt")
        ),
        tabItem(
          "sankey",
          tab_sankey_ind_ui("sankey")
        ),
        tabItem(
          "bench_ind",
          plotsWithFiltersIndividualUI("bench_ind")
        ),
        tabItem(
          "compare_aggr",
          plotsWithFiltersAggregatedCompareUI("compare_aggr")
        ),
        tabItem(
          "compare_ind",
          plotsWithFiltersIndividualCompareUI("compare_ind")
        ),
        tabItem(
          "explorer",
          esquisseUI("explorer")
        )
      )
    )

    do.call(tabItems, items)
  })

  ##' *INFO* doesn't happen automatically open when generated dynamically, see:
  ## https://github.com/rstudio/shinydashboard/issues/335
  isolate({updateTabItems(session, "tablist", "landingspagina")})


  output$menu <- renderMenu({
    sidebarMenu(
      id = "tablist",
      menuItem("landingspagina", tabName = "landingspagina"),
      menuItem("benchmark - aggr", tabName = "bench_aggr", selected = TRUE),
      menuItem("benchmark - ind", tabName = "bench_ind"),
      menuItem("vergelijk - aggr", tabName = "compare_aggr"),
      menuItem("vergelijk - ind", tabName = "compare_ind"),
      menuItem("doorstroom - gantt", tabName = "gantt"),
      menuItem("doorstroom - sankey", tabName = "sankey"),
      menuItem("explorer", tabName = "explorer")
    )

  })

  ## SQL database ####
  ## TODO: Temp uitgezet

  # ## Haal gegevens op
  # sTable_name <- "user_action"
  #
  # ## Bouw select query
  # sSelect_query <- paste0("SELECT * FROM ",
  #                         sTable_name,
  #                         ";")
  # ## Haal tabel op
  # dfUser_action <- dbGetQuery(con,
  #                             sSelect_query)
  #
  # print(dfUser_action)

}
