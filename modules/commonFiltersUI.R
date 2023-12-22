commonFiltersUI <- function(id) {

  tagList(
    pickerValues(
      id = id,
      variable = "cohort",
      role = "both",
      selected = "2018;INS_Inschrijvingsjaar_EOI",
      multiple = FALSE
    ),
    pickerValues(
      id = id,
      role = "left",
      variable = "opleiding",
      selected = "M Business Administration;INS_Opleidingsnaam_2002",
      multiple = FALSE
    )
  )
}
