commonDrilldownUI <- function(id) {

  tagList(
    pickerVar(id, "x"),
    pickerSplitVar(id)
  )
}
