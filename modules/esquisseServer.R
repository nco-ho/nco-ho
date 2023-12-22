## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## esquisseServer.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: Server code voor plotsWithFilters module
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#esquisseServer <- function(id) {
esquisseServer <- function(id, session_server) {
  moduleServer(id, function(input, output, session) {

    # ## Common input ####
    # observe({
    #
    #   ## Filters
    #   updateSelectInput(
    #     session,
    #     'cohort_both',
    #     selected = default_inputs$cohort_both()
    #   )
    #
    #   updateSelectInput(
    #     session,
    #     'programme_left',
    #     selected = default_inputs$programme_left()
    #   )
    #
    # })
    #
    # ## Use filters to update data
    # dfFiltered <- reactive({
    #
    #   lFilter_inputs <-
    #     list(
    #       input$opleiding_left,
    #       input$cohort_both
    #     ) %>%
    #     ## Verwijder lege elementen
    #     discard(is.null)
    #
    #   ## Transformeer input naar filter elementen
    #   lFilter_elements <- map(lFilter_inputs, transform_input)
    #
    #   dfFiltered <- dfInstelling_individueel %>%
    #     filter_with_lists(lFilter_elements)
    #
    #   cat(nrow(dfFiltered))
    #
    #   return(dfFiltered)
    #
    # })

    ## Run only when filters change AND tab is actually on explorer
    ## Because this takes a bit longer
    # observeEvent(c(dfFiltered(),session_server$input$tablist),  {
    #
    #   req(session_server$input$tablist)
    #
    #   data_rv <- reactiveValues(data = dfFiltered(),
    #                             name = "test_name")
    #
    #
    #   if(session_server$input$tablist == "explorer") {
    #     esquisse_server(
    #       id = "esquisse",
    #       data_rv = data_rv,
    #       default_aes = c("fill", "color", "size", "group", "facet"),
    #       import_from = NULL
    #     )
    #   }
    # })

    visit <- FALSE

    observeEvent(session_server$input$tablist, {
      req(session_server$input$tablist)
      if(session_server$input$tablist == "explorer" & visit == FALSE) {

      data_rv <- reactiveValues(data = dfInstelling_individueel,
                                name = "test_name")

      esquisse_server(
        id = "esquisse",
        data_rv = data_rv,
        default_aes = c("fill", "color", "size", "group", "facet"),
        import_from = NULL,
        notify_warnings = NULL
      )

      visit <- TRUE

      }
    })


  })

}

