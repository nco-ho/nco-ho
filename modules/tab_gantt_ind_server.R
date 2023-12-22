## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## tab_gantt_ind_server.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: Server code voor througput module
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tab_gantt_ind_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ## Maak UI om te filteren op geselecteerde variabele
    output$filter_values <- renderUI({

      req(input$input_var)
      tagList(
        pickerGanttValues(id, input$input_var, dfCHO_doorstroom)
      )
    })

    ## Maak UI om te filteren op geselecteerde variabele
    output$target_var <- renderUI({

      req(input$input_var)

      tagList(
        pickerGanttVar(id, "target_var", df_config_gantt, input$input_var)
      )
    })


    ## Maak Gantt ####
    output$gantt <- renderPlotly({

      ## Haal vereiste variables op. De input_var update ook de filter_value, dus vandaar de
      ## req voor filter en isolate voor input
      req(input$filter, input$target_var)

      input_var_value <- isolate(input$input_var)
      filter_value <- input$filter


      split_var <- input$target_var
      position_label_y <- df_config_gantt %>% filter(input_var == input_var_value) %>% pull(position_y_label) %>% first()
      title_start = df_config_gantt %>% filter(input_var == input_var_value) %>% pull(title_start) %>% first()
      title_end = df_config_gantt %>% filter(input_var == input_var_value) %>% pull(title_end) %>% first()
      title = paste0(title_start, filter_value, title_end)

      ## TODO Functie
      ## Filter gantt based on selections
      dfCHO_doorstroom_gantt <- dfCHO_doorstroom %>%
        filter(!!sym(input_var_value) == filter_value) %>%
        group_by(!!sym(input_var_value), !!sym(split_var)) %>%
        summarize(n = n()) %>%
        ungroup() %>%
        ## Zet rijen op volgorde en geef ze vervolgens begin en eind percentage de verschillende
        ## beginnen waar de ander stopt
        arrange(desc(n)) %>%
        mutate(
          flow_perc = n / sum(n),
          flow_end_perc = cumsum(flow_perc),
          flow_start_perc = lag(flow_end_perc),
          flow_start_perc = replace_na(flow_start_perc, 0)
        ) %>%
        ## Limit number of values
        limit_n_values_gantt(split_var)

      x <- "flow_start_perc"
      xend <- "flow_end_perc"

      gantt_plot(dfCHO_doorstroom_gantt, x, xend, split_var, title, position_label_y)

    })
  })
}
