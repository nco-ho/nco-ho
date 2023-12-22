## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## plotsWithFiltersIndividualCompareServer.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: Server code voor plotsWithFilters module
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


plotsWithFiltersIndividualCompareServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ## INPUTS ###
    ### Set reactive variables ####
    kleur <- reactive({input$color})
    x <- reactive({input$x})
    y_links <- reactive({input$y_links})
    y_rechts <- reactive({input$y_rechts})

    faculty <- reactive({input$filter_faculty_left})
    phase <- reactive({input$phase_left})

    programmes <- reactive({input$opleiding_left})
    cohorts <- reactive({input$cohort_both})


    ## Verzamel variables voor latere grouping (muv y omdat deze moet worden geaggregeerd)
    ## TODO: VIS_Groep_naam is variabele die wordt gemaakt obv de filters
    ## en of het left of right is
    variables <- reactive({c(input$color, input$x, "VIS_Groep", "VIS_Groep_naam")})

    ### Basis dfs ####
    lRelevante_opleidingen <- reactive({
      keep_only_relevant_values(list(faculty(), phase()), "INS_Opleidingsnaam_2002", id)
    })

    ## Maak df obv dataset en filters
    dfLeft <- reactive({
      prep_df(
        list(
          faculty(),
          phase(),
          programmes(),
          cohorts()
        ),
        programmes(),
        dfInstelling_individueel,
        kleur(),
        "left"
      )
    })

    dfRight <- reactive({
      dfLeft() %>% mutate(VIS_Groep = "right")
    })

    dfBoth <- reactive({
      bind_both(dfLeft(), dfRight(), "comp", y_links(), y_rechts())
    })

    ### Summaries maken adhv selecties ####
    dfLeft_summarized <- reactive({
      prep_df_summ(dfLeft(), variables(), y_links())
    })

    dfRight_summarized <- reactive({
      prep_df_summ(dfRight(), variables(), y_rechts())
    })

    ## Maak summary van complete data
    dfBoth_summarized <- reactive({
      bind_both(dfLeft_summarized(), dfRight_summarized(), "comp", y_links(), y_rechts())
    })

    dfBoth_table <- reactive({
      bind_both_table(dfLeft_summarized(),
                      dfRight_summarized(),
                      y_links(),
                      y_rechts())
    })

    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## PLOTS MAKEN ####
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    ### Plot doel geaggregeerd ####
    output$plot_doel_geaggregeerd <-
      renderPlotly({

        wrapped_chart(dfBoth_summarized(),
                      x(),
                      "y",
                      kleur(),
                      id,
                      y_links(),
                      y_rechts())

      })

    ### Tabellen ####

    ## TODO: Wat is handige keuze om tabellen goed te stylen?
    ## TODO: Kolomnamen automatisch aanpassen mbv mapping table? Dit klopt nu
    ## niet wanneer verschillende y-variables worden geselecteerd
    ## DT met daarbij veel uitgevinkt om het toch simpel te houden?
    output$tabel <- renderDT({
      prep_table(y_links(), dfLeft(), dfBoth_table(), id, y_rechts())
    })

    ### Plot % Samenstelling ####
    output$plot_samenstelling_p <- renderPlotly({
      stacked_composition_bar_chart(dfLeft_summarized(),
                                    x(),
                                    kleur(),
                                    id,
                                    percentage = TRUE)

    })

    ### Plot # Samenstelling ####
    output$plot_samenstelling_n <- renderPlotly({
      stacked_composition_bar_chart(dfLeft_summarized(),
                                    x(),
                                    kleur(),
                                    id)
    })

    ### Spreiding plots ####
    output$boxplot <- renderPlotly({
      grid_boxplots(dfBoth(), x(), kleur(), "y", id, y_links(), y_rechts())
    })

    ## Maak wrapper om plot om warnings op te vangen
    lHistogram_quiet <- reactive({
      ## Run histograms quietly
      quietly_run(grid_histograms, dfBoth(), x(), kleur(), "y", id, y_links(), y_rechts())
    })

    ## TODO Oude code
    ## Render het plot (result)
    # output$plot_spreiding <- renderPlotly({
    #   lPlot_spreiding_met_warnings <- lHistogram_quiet()
    #   lPlot_spreiding_met_warnings$result
    #
    # })

    ## TODO onsuccesvol experiment
    # plots <- reactive({
    #   #req(list(dfBoth(), x(), kleur(), y_links(), y_rechts()))
    #   grid_histograms(dfBoth(), x(), kleur(), "y", id, y_links(), y_rechts())
    # })
    #
    # observeEvent(plots(), {
    #   walk(seq_along(plots()), function(i) {
    #     output[[paste0("plot_", i)]] <- renderPlotly({ plots()[[i]] })
    #   })
    # })

    output$histogram <- renderUI({
      # Generate the list of plots
      plots <- grid_histograms(dfBoth(), x(), kleur(), "y", id, y_links(), y_rechts())

      ## Set output elements for each plot
      walk(1:length(plots), function(i) {
        output[[paste0("plot_", i)]] <- renderPlotly({ plots[[i]] })
      })

      ## Run output code on each element and set namespace and height
      ##' *INFO*  Due to height setting of the output-elements the surrounding box also adapts
      output_list <- map(seq_along(plots), ~plotlyOutput(outputId = paste0(NS(id, "plot_"), .x), height = "250px")) %>%
        #flatten() %>%
        tagList()

      return(output_list)

    })




      # observe({
      #   walk(seq_along(plots()), function(i) {
      #     output[[paste0("plot_", i)]] <- renderPlotly({ plots()[[i]] })
      #   })
      # })

      # output$multi_plot <- renderUI({
      #   map(seq_along(plots()), ~plotlyOutput(outputId = paste0("plot_", .x))) %>%
      #     tagList()
      # })

      # output$multi_plot <- renderUI({
      #   map(seq_along(plots()), ~plotlyOutput(outputId = paste0("plot_", .x))) %>%
      #     tagList()
      # })

      # observe({
      #   walk(1:length(plots()), function(x) {
      #     output[[paste0("plot_", x)]] <- renderPlotly({ plots()[[x]] })
      #   })
      # })

    ## TODO: Bepalen hoe we dit handig doen
    ### Conditionele warning ####
    # output$plot_spreiding_warning <- renderUI({
    #   lPlot_spreiding_met_warnings <- lPlot_spreiding_met_warnings()
    #
    #   if (length(lPlot_spreiding_met_warnings$messages) > 0) {
    #     HTML(paste(c(unlist(lPlot_spreiding_met_warnings$messages), "beh"), collapse = "<br/>"))
    #     #verbatimTextOutput("text", text)
    #   }
    # })

    ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    ### Filters updaten ####
    ## NB: Vergelijkbaar met Tableau 'show only relevant values'
    ## update opleidingen obv faculty filter
    observeEvent(lRelevante_opleidingen(), {

      updatePickerInput(
        session = session,
        inputId = "programme_left",
        choices = lRelevante_opleidingen(),
        selected = lRelevante_opleidingen()[1]
      )
    })

    ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    ### Maak bookmark ####
    ## Het bookmarkbutton, dat creëert shiny alert
    observeEvent(input$bookmark, {

      shinyalert(
        "Onder welke naam wil je deze pagina opslaan",
        type = "input",
        ## TODO: bij Opslaan checken of die al bezet is en zo ja timestamp toevoegen
        ## TODO: Of de '1' afhankelijk maken van aantal bookmark == TRUE in database oid
        ## allemaal details ;-)
        inputPlaceholder = "bookmark 1"
      )
    })

    ## Shiny alert triggert een bookmark, verrijk deze
    observeEvent(input$shinyalert, {

      onBookmark(function(state) {

        ## Voeg extra waardes toe
        ## Naam om het later te kunnen herkennen
        ## Bookmark TRUE om onderscheid te maken tussen bookmarks die gebruiker
        ## wil bewaren en standaard logs
        ## NB: Misschien is TRUE ook niet nodig (naam wel of niet is al indicatie)
        state$values$bookmark_name <- input$shinyalert
        state$values$bookmark <- TRUE

      })

      ## TODO: Schakel later uit, zie volgende comment
      ## Geef url in pop-up weer
      session$doBookmark()

    })

    ## TODO: onBookmarked heeft als onzichtbaar element de url. cat kan dus
    ## worden vervangen door functie die obv url een insertion in database doet.
    ## Meerdere onBookmarked zijn ook mogelijk (kan voor testen handig zijn)
    ## (let op andere argumenten kunnen niet worden toegevoegd in de
    ## onBookmarked call, dus die moeten optioneel zijn)

    ## Bookmark triggert een onBookmarked, update hier de database
    # onBookmarked(cat)

  })
}
