
commonFiltersServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    return(
      list(
        cohort_both = reactive({ input$cohort_both }),
        programme_left = reactive({ input$opleiding_left })
      )
    )
  })
}
