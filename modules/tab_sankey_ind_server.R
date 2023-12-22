


tab_sankey_ind_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ## Pas var aan
    output$filter_links <- renderUI({
      req(input$left_var)

      tagList(
        pickerSankeyValues(id, input$left_var, dfCHO_doorstroom, "links")
      )
    })

    ## Pas var aan
    output$filter_rechts <- renderUI({
      req(input$right_var)

      tagList(
        pickerSankeyValues(id, input$right_var, dfCHO_doorstroom, "rechts")
      )
    })

    ## Maak Sankey ####
    output$sankey <- renderPlot({

      ## Haal vereiste variables op. De left_var update ook het filter, dus vandaar de
      ## req voor filter en isolate voor de left / right var
      req(input$filter_links, input$filter_rechts)

      ## Bepaal variabelen
      left_var <- isolate(input$left_var)
      right_var <- isolate(input$right_var)
      multiple_filter_links <- input$filter_links
      multiple_filter_rechts <- input$filter_rechts

      ## Prepareer dataframe
      ## TODO Functie
      dfDoorstroom_grouped <- dfCHO_doorstroom %>%
        filter(!!sym(left_var) != "Onbekend",
               !!sym(right_var) != "Onbekend"
               ) %>%
        group_by(!!sym(left_var), !!sym(right_var)) %>%
        summarize(n = n()) %>%
        ungroup() %>%
        mutate(change = !!sym(left_var) != !!sym(right_var)) %>%
        filter(change == TRUE) %>%
        filter(!!sym(left_var) %in% multiple_filter_links,
               !!sym(right_var) %in% multiple_filter_rechts)

      title <- "Doorstrom van Bachelor naar Master"

      sankey_plot(
        dfDoorstroom_grouped,
        left_var,
        right_var,
        xlab("Doorstroom"),
        ylab("Frequentie"),
        "Bachelor",
        "Master",
        title
      )



    })
  })
}
