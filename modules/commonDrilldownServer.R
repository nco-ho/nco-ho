
commonDrilldownServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    return(
      list(
        x = reactive({ input$x }),
        kleur = reactive({ input$color })
      )
    )
  })
}
