## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## plotsWithFiltersAggregatedCompareServer.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: Server code voor plotsWithFilters module
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


plotsWithFiltersAggregatedCompareServer <- function(id, default_inputs) {
  moduleServer(id, function(input, output, session) {

    ## INPUTS ####
    ### Common input ####
    observe({

      ## Drilldown
      updateSelectInput(session,
                        'x',
                        selected = default_inputs$x())

      updateSelectInput(session,
                        'kleur',
                        selected = default_inputs$kleur())

      ## Filters
      updateSelectInput(session,
                        'cohort_both',
                        selected = default_inputs$cohort_both())

      updateSelectInput(session,
                        'programme_left',
                        selected = default_inputs$programme_left())

    })

    ### Set reactive variables ####
    kleur <- reactive({input$color})
    x <- reactive({input$x})
    y_links <- reactive({input$y_links})
    y_rechts <- reactive({input$y_rechts})

    programmes <- reactive({input$opleiding_left})
    cohorts <- reactive({input$cohort_both})

    ## Verzamel variables voor latere grouping (muv y omdat deze moet worden geaggregeerd)
    ## TODO: VIS_Groep_naam is variabele die wordt gemaakt obv de filters
    ## en of het left of right is
    ##variables <- reactive({c(input$color, input$x, "VIS_Groep_naam")})
    variables <- reactive({c(input$x, "INS_Splits_variabele_waarde", "VIS_Groep", "VIS_Groep_naam")})


    ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
    ## DATA PREP ####

    dfLeft <- reactive({
      prep_df(
        list(
          kleur(),
          programmes(),
          cohorts()
        ),
        programmes(),
        dfCombi_geaggregeerd,
        "INS_Splits_variabele_waarde",
        "left"
      )
    })

    dfRight <- reactive({
      dfLeft() %>% mutate(VIS_Groep = "right")
    })

    dfBoth <- reactive({
      bind_both(dfLeft(), dfRight(), id, y_links(), y_rechts())
    })

    dfLeft_summarized <- reactive({
      prep_df_summ_aggr(dfLeft(), variables(), y_links(), kleur())
    })

    dfRight_summarized <- reactive({
      prep_df_summ_aggr(dfRight(), variables(), y_rechts(), kleur())
    })

    dfBoth_table <- reactive({
      bind_both_table(dfLeft_summarized(),
                      dfRight_summarized(),
                      y_links(),
                      y_rechts())
    })

    dfBoth_summarized <- reactive({
      bind_both(dfLeft_summarized(), dfRight_summarized(), id, y_links(), y_rechts())
    })


    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## PLOTS MAKEN ####
    ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

    ### Tabel ####
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
