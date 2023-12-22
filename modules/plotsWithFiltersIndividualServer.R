## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## plotsWithFiltersIndividualServer.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: Server code voor plotsWithFilters module
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


plotsWithFiltersIndividualServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ## INPUTS ####
    ### Set reactive variables ####
    kleur <- reactive({input$color})
    x <- reactive({input$x})
    y <- reactive({input$y})

    faculty_left <- reactive({input$filter_faculty_left})
    faculty_right <- reactive({input$filter_faculty_right})

    phase_left <- reactive({input$phase_left})
    phase_right <- reactive({input$phase_right})

    programmes_left <- reactive({input$opleiding_left})
    programmes_right <- reactive({input$opleiding_right})

    cohorts <- reactive({input$cohort_both})


    ## Verzamel variables voor latere grouping (muv y omdat deze moet worden geaggregeerd)
    ## TODO: VIS_Groep_naam is variabele die wordt gemaakt obv de filters
    ## en of het left of right is
    variables <- reactive({c(kleur(), x(), "VIS_Groep", "VIS_Groep_naam")})

    ### Basis dfs ####
    ## Supporting dfs (ook gebruikt voor updaten UI-element)
    lRelevante_opleidingen_left <- reactive({
      keep_only_relevant_values(list(faculty_left(), phase_left()), "INS_Opleidingsnaam_2002", id)
    })

    ## Maak left df obv dataset en filters
    dfLeft <- reactive({
      prep_df(
        list(
          faculty_left(),
          phase_left(),
          programmes_left(),
          cohorts()
        ),
        programmes_left(),
        dfInstelling_individueel,
        kleur(),
        "left"
      )
    })

    lRelevante_opleidingen_right <- reactive({
      keep_only_relevant_values(list(faculty_right(), phase_right()), "INS_Opleidingsnaam_2002", id)
    })

    ## Maak right df obv dataset en filters
    dfRight <- reactive({
      prep_df(
        list(
          faculty_right(),
          phase_right(),
          programmes_right(),
          cohorts()
        ),
        programmes_right(),
        dfInstelling_individueel,
        kleur(),
        "right"
      )
    })

    dfBoth <- reactive({
      bind_both(dfLeft(), dfRight(), id)
    })

    ### Summaries maken adhv selecties ####
    dfLeft_summarized <- reactive({
      prep_df_summ(dfLeft(), variables(), y())
    })

    dfRight_summarized <- reactive({
      prep_df_summ(dfRight(), variables(), y())
    })

    ## Maak summary van complete data
    dfBoth_summarized <- reactive({
      bind_both(dfLeft_summarized(), dfRight_summarized())
    })

    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## PLOTS MAKEN ####
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    ### Plot doel geaggregeerd ####
    output$plot_doel_geaggregeerd <-
      renderPlotly({
        wrapped_chart(dfBoth_summarized(),
                      x(),
                      y(),
                      kleur(),
                      id)
      })

    ### Tabellen ####
    output$tabel <- renderDT({
      prep_table(y(), dfLeft(), dfLeft_summarized(), id)
    })

    output$tabel_twee <- renderDT({
      prep_table(y(), dfRight(), dfRight_summarized(), id)
    })

    ### Plots Samenstellingen ####
    output$plot_samenstelling_p <- renderPlotly({
      stacked_composition_bar_chart(dfBoth_summarized(),
                                    x(),
                                    kleur(),
                                    id,
                                    percentage = TRUE,
                                    wrap = TRUE)
    })

    output$plot_samenstelling_n <- renderPlotly({
      stacked_composition_bar_chart(dfBoth_summarized(),
                                    x(),
                                    kleur(),
                                    id,
                                    wrap = TRUE)

    })

    ### Spreiding plots ####
    output$boxplot <- renderPlotly({
      grid_boxplots(dfBoth(), x(), kleur(), y(), id)
    })

    ## Maak wrapper om plot om warnings op te vangen
    lHistogram_quiet <- reactive({
      ## Run histograms quietly
      quietly_run(grid_histograms, dfBoth(), x(), kleur(), y(), id)
    })


    ## Render het plot (result)
    output$histogram <- renderUI({

      # Generate the list of plots
      plots <- grid_histograms(dfBoth(), x(), kleur(), y(), id)

      ## Set output elements for each plot
      walk(1:length(plots), function(i) {
        output[[paste0("plot_", i)]] <- renderPlotly({ plots[[i]] })
      })

      ##' *INFO*  Due to height setting of the output-elements the surrounding box also adapts
      unique_values_x <- unique(dfBoth()[[x()]])
      n_rows_grid <- length(unique_values_x)
      n_height <- 250 * n_rows_grid
      s_height <- paste0(n_height, "px")

      ## Run output code on each element and set namespace
      output_list <- map(seq_along(plots), ~plotlyOutput(outputId = paste0(NS(id, "plot_"), .x), width = "99%", height = s_height)) %>%
        #flatten() %>%
        tagList()

      return(output_list)



    })

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
    ## FILTER UPDATE ####
    ## NB: Vergelijkbaar met Tableau 'show only relevant values'
    ## update left opleidingen obv faculty filter
    observeEvent(lRelevante_opleidingen_left(), {

      updatePickerInput(
        session = session,
        inputId = "programme_left",
        choices = lRelevante_opleidingen_left(),
        selected = lRelevante_opleidingen_right()[[1]]
      )
    })

    observeEvent(lRelevante_opleidingen_right(), {

      updatePickerInput(
        session = session,
        inputId = "programme_right",
        choices = lRelevante_opleidingen_right()
      )
    })

    ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    ## BOOKMARKS ####
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
