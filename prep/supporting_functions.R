


## TODO The following two functions have overlap in code
## TODO: Is pivot an appropriate name?
group_as_cohort <- function(dfAS, sAdditional_grouping_variable = NULL, pivot = FALSE) {

  ## TODO: Test of sAdditional_grouping_variable is single string. If it is multiple, the mutate
  ## rename will break
  ### If multiple is needed, different wrapper function

  ## TODO: Make sGroupingvariables an optional argument based on env variables

  if (is.null(sAdditional_grouping_variable)) {
    vGrouping_variables <- c(
      "INS_Inschrijvingsjaar_EOI", ## Aggregatieniveau
      "INS_Opleidingsnaam_2002", ## Aggregatieniveau
      #"INS_Opleidingsfase_BPM", ## Noodzakelijk voor onderscheid Bachelor - Premaster
      "INS_Faculteit", ## Bewaar deze variabele (is hierarchisch boven )

      "OPL_CBS_Richting_code",
      "OPL_CBS_Label_richting",
      "INS_Opleidingscode_actueel"

    )
  } else {
    vGrouping_variables <- c(
      "INS_Inschrijvingsjaar_EOI", ## Aggregatieniveau
      "INS_Opleidingsnaam_2002", ## Aggregatieniveau
      #"INS_Opleidingsfase_BPM", ## Noodzakelijk voor onderscheid Bachelor - Premaster
      "INS_Faculteit", ## Bewaar deze variabele (is hierarchisch boven )

      "OPL_CBS_Richting_code",
      "OPL_CBS_Label_richting",
      "INS_Opleidingscode_actueel",
      sAdditional_grouping_variable
    )
  }

  sTarget_variables <- c(
    "RES_Aantal_EC_tm_jaar_1",
    "RES_Aantal_EC_tm_voltooid",
    "RES_Aantal_EC_tm_nu",
    "RES_Gem_resultaat_tm_jaar_1",
    "RES_Gem_resultaat_tm_voltooid",
    "RES_Gem_resultaat_tm_nu",
    "RES_Gem_resultaat_geboekt_tm_P7"
  )

  ## Aggregeer obv vooropleiding, geslacht, opleidingsvorm (vol- deeltijd) en Nationaliteit_EER
  dfStudy_Grouped <- dfAS %>%
    #ungroup() %>%
    #filter(INS_Inschrijvingsjaar < Academic_year(now())) %>%
    group_by(across(all_of(c(
      vGrouping_variables
    )))) %>%
    aggregate_as_targeted(sTarget_variables) %>%
    #aggregate_as() %>%
    ungroup()

  ##
  if(pivot & is.null(sAdditional_grouping_variable)) {

    dfStudy_Grouped_pivot <- dfStudy_Grouped %>%
      ## Maak een kolom met variabele naam
      mutate(INS_Splits_variabele = "Alle") %>%
      ## Maak een kolom met de waarde
      mutate(INS_Splits_variabele_waarde = "Alle")

    return(dfStudy_Grouped_pivot)

    #stop("If you want to pivot you have to add an additional grouping variable")
  } else if (pivot & !is.null(sAdditional_grouping_variable)) {

    dfStudy_Grouped_pivot <- dfStudy_Grouped %>%
      ## Maak een kolom met variabele naam
      mutate(INS_Splits_variabele = sAdditional_grouping_variable) %>%
      ## Maak een kolom met de waarde
      rename(INS_Splits_variabele_waarde = sym(sAdditional_grouping_variable))

    return(dfStudy_Grouped_pivot)
  } else {
    return(dfStudy_Grouped)

  }
}

group_as_cohort_basic <- function(dfAS, sAdditional_grouping_variable = NULL, pivot = FALSE) {

  ## TODO: Test of sAdditional_grouping_variable is single string. If it is multiple, the mutate
  ## rename will break
  ### If multiple is needed, different wrapper function

  ## TODO: Make sGroupingvariables an optional argument based on env variables

  if (is.null(sAdditional_grouping_variable)) {
    vGrouping_variables <- c(
      "INS_Inschrijvingsjaar_EOI"#, ## Aggregatieniveau
      #"INS_Opleidingsnaam_2002", ## Aggregatieniveau
      #"INS_Opleidingsfase_BPM", ## Noodzakelijk voor onderscheid Bachelor - Premaster
      #"INS_Faculteit"#, ## Bewaar deze variabele (is hierarchisch boven )

      # "OPL_CBS_Richting_code",
      # "OPL_CBS_Label_richting",
      # "INS_Opleidingscode_actueel"

    )
  } else {
    vGrouping_variables <- c(
      "INS_Inschrijvingsjaar_EOI", ## Aggregatieniveau
      #"INS_Opleidingsnaam_2002", ## Aggregatieniveau
      #"INS_Opleidingsfase_BPM", ## Noodzakelijk voor onderscheid Bachelor - Premaster
      #"INS_Faculteit", ## Bewaar deze variabele (is hierarchisch boven )

      # "OPL_CBS_Richting_code",
      # "OPL_CBS_Label_richting",
      # "INS_Opleidingscode_actueel",
      sAdditional_grouping_variable
    )
  }

  # sTarget_variables <- c(
  #   "RES_Aantal_EC_tm_jaar_1",
  #   "RES_Aantal_EC_tm_voltooid",
  #   "RES_Aantal_EC_tm_nu",
  #   "RES_Gem_resultaat_tm_jaar_1",
  #   "RES_Gem_resultaat_tm_voltooid",
  #   "RES_Gem_resultaat_tm_nu",
  #   "RES_Gem_resultaat_geboekt_tm_P7"
  # )

  ## Aggregeer obv vooropleiding, geslacht, opleidingsvorm (vol- deeltijd) en Nationaliteit_EER
  dfStudy_Grouped <- dfAS %>%
    #ungroup() %>%
    #filter(INS_Inschrijvingsjaar < Academic_year(now())) %>%
    group_by(across(all_of(c(
      vGrouping_variables
    )))) %>%
    aggregate_as() %>%
    #aggregate_as() %>%
    ungroup()

  ##
  if(pivot & is.null(sAdditional_grouping_variable)) {

    dfStudy_Grouped_pivot <- dfStudy_Grouped %>%
      ## Maak een kolom met variabele naam
      mutate(INS_Splits_variabele = "Alle") %>%
      ## Maak een kolom met de waarde
      mutate(INS_Splits_variabele_waarde = "Alle")

    return(dfStudy_Grouped_pivot)

    #stop("If you want to pivot you have to add an additional grouping variable")
  } else if (pivot & !is.null(sAdditional_grouping_variable)) {

    dfStudy_Grouped_pivot <- dfStudy_Grouped %>%
      ## Maak een kolom met variabele naam
      mutate(INS_Splits_variabele = sAdditional_grouping_variable) %>%
      ## Maak een kolom met de waarde
      rename(INS_Splits_variabele_waarde = sym(sAdditional_grouping_variable))

    return(dfStudy_Grouped_pivot)
  } else {
    return(dfStudy_Grouped)

  }
}


group_EOI_study_as_cohort <- function(dfStudy, sGrouping_variables = "EOI_institution", sAdditional_grouping_variable = NULL, pivot = FALSE) {

  if (sGrouping_variables == "EOI_national") {
    sGrouping_variables <- c(
      "INS_Eerste_jaar_opleiding_en_instelling", ## Aggregatieniveau
      "OPL_CBS_Label_richting", ## Aggregatieniveau
      "OPL_CBS_Richting_code" ## Aggregatieniveau
    )
  } else if(sGrouping_variables == "EOI_institution") {
    sGrouping_variables <- c(
      "INS_Instellingsafkorting", ## Aggregatieniveau
      "INS_Eerste_jaar_opleiding_en_instelling", ## Aggregatieniveau
      "INS_Opleidingscode_actueel", ## Aggregatieniveau
      ## Behoud voor benchmark met landelijk
      "OPL_CBS_Label_richting",
      "OPL_CBS_Richting_code",
      ## Behoud andere variabelen
      "INS_Opleidingsnaam_2002_instroom",
      "INS_Opleiding_CROHO_onderdeel"
    )
  }

  if (!is.null(sAdditional_grouping_variable)) {
    sGrouping_variables <- c(
      sGrouping_variables,
      sAdditional_grouping_variable
    )

  }

  ## Aggregeer obv vooropleiding, geslacht, opleidingsvorm (vol- deeltijd) en Nationaliteit_EER
  dfStudy_Grouped <- dfStudy %>%
    #ungroup() %>%
    #filter(INS_Inschrijvingsjaar < Academic_year(now())) %>%
    #select(vGrouping_variables,
    #       where(is.numeric)) %>%
    group_by(across(all_of(c(
      sGrouping_variables
    )))) %>%
    ## TODO This is very manual, see what can be made dynamic
    summarise(#across((starts_with("SUC_") | matches("INS_Herinschrijver_opleiding_na_jaar_1")) & where(is.numeric),  ~ mean(.x, na.rm = TRUE)),
      SUC_Diploma_binnen_1_jaar = sum(SUC_Diploma_aantal_jaar_cohorten == 1, na.rm = TRUE) / n(),
      SUC_Diploma_binnen_2_jaar = sum(SUC_Diploma_aantal_jaar_cohorten %in% c(1, 2), na.rm = TRUE) / n(),
      SUC_Diploma_binnen_3_jaar = sum(SUC_Diploma_aantal_jaar_cohorten %in% c(1, 2, 3), na.rm = TRUE) / n(),
      SUC_Diploma_binnen_4_jaar = sum(SUC_Diploma_aantal_jaar_cohorten %in% c(1, 2, 3, 4), na.rm = TRUE) / n(),
      SUC_Uitval_tm_jaar_1 = sum(SUC_Uitval_aantal_jaar_cohorten == 1, na.rm = TRUE) / n(),
      SUC_Uitval_tm_jaar_2 = sum(SUC_Uitval_aantal_jaar_cohorten %in% c(1, 2), na.rm = TRUE) / n(),
      SUC_Diploma_nominaal_plus1_jaar_2 = SUC_Diploma_binnen_4_jaar / (1 - SUC_Uitval_tm_jaar_1),
      INS_Aantal_eerstejaars_CHO = n()) %>%
    ungroup()

  ##
  if(pivot & is.null(sAdditional_grouping_variable)) {

    dfStudy_Grouped_pivot <- dfStudy_Grouped %>%
      ## Maak een kolom met variabele naam
      mutate(INS_Splits_variabele = "Alle") %>%
      ## Maak een kolom met de waarde
      mutate(INS_Splits_variabele_waarde = "Alle")

    return(dfStudy_Grouped_pivot)

    #stop("If you want to pivot you have to add an additional grouping variable")
  } else if (pivot & !is.null(sAdditional_grouping_variable)) {

    dfStudy_Grouped_pivot <- dfStudy_Grouped %>%
      ## Maak een kolom met variabele naam
      mutate(INS_Splits_variabele = sAdditional_grouping_variable) %>%
      ## Maak een kolom met de waarde
      rename(INS_Splits_variabele_waarde = sym(sAdditional_grouping_variable))

    return(dfStudy_Grouped_pivot)
  } else {
    return(dfStudy_Grouped)

  }
}
