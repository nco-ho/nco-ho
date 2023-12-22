## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## ui_functions.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: Wrapper functies voor UI-elementen
##


## shinydashboardPlus adaptions ####
##' *INFO* https://stackoverflow.com/questions/43806738/r-shiny-dashboard-custom-dropdown-menu-in-header
## It needs to stay a dropwdown class for lay-out / shinydashboard implementation

## Dropdown that is actually more of a menu with adapted tasks
dropdownTabMenu <- function(..., type = c("messages", "notifications", "tasks"), title = NULL, icon = NULL, .list = NULL, header = NULL) {
  type <- match.arg(type)

  if (is.null(icon)) {
    icon <- switch(type,
                   messages = shiny::icon("envelope"),
                   notifications = shiny::icon("warning"),
                   tasks = shiny::icon("tasks")
    )
  }

  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")

  dropdownClass <- paste0("dropdown ", type, "-menu")
  tags$li(class = dropdownClass, a(href = "#", class = "dropdown-toggle",
                                   `data-toggle` = "dropdown", icon, title), tags$ul(class = "dropdown-menu",
                                                                                     if (!is.null(header)) tags$li(class = "header", header),
                                                                                     tags$li(tags$ul(class = "menu", items))))
}

## Dropdown that is actually a link to a tab
dropdownTabDirect <- function(type = c("messages", "notifications", "tasks"), tab_name, title, icon = NULL, .list = NULL, header = NULL) {
  type <- match.arg(type)

  if (is.null(icon)) {
    icon <- switch(type,
                   messages = shiny::icon("envelope"),
                   notifications = shiny::icon("warning"),
                   tasks = shiny::icon("tasks")
    )
  }

  tabSelect = TRUE
  dropdownClass <- paste0("dropdown ", type, "-menu")

  tags$li(class = dropdownClass,
          a(
            href = "#",
            onclick = paste0("shinyjs.tabSelect('", tab_name, "')"),
            icon,
            title,
            `data-tab-name` = tab_name,
            class = "dropdown-toggle",
            `data-toggle` = "dropdown"
          )
  )
}

## Item for aboven dropdownActionMenu function
##' *INFO* https://stackoverflow.com/questions/43806738/r-shiny-dashboard-custom-dropdown-menu-in-header
taskItemTab <- function(text, tab_name = NULL, href = NULL, tabSelect = FALSE) {
  if (is.null(href)) href <- "#"

  if (tabSelect) {
    tags$li(a(onclick = paste0("shinyjs.tabSelect('", tab_name, "')"), text, `data-tab-name` = tab_name))
  } else {
    tags$li(a(href = href, h3(text)))
  }
}

## Pickers ####

## Function to generate a picker input element based on given id and element
pickerVar <- function(id, element, label = NULL) {

  is_individual <- str_detect(id, "ind")
  is_agg <- str_detect(id, "aggr")

  ## Determine the appropriate data frame and category based on the id
  if (is_individual) {
    df <- dfInstelling_individueel
    dfCategorie_selected <- dfCategorie_ind
  } else if (is_agg) {
    df <- dfCombi_geaggregeerd
    dfCategorie_selected <- dfCategorie_aggr
  }

  id_element <- NS(id, element)

  ## Select desired variables for the selection element based on metadata and presence in df
  lVariables <- dfCategorie_selected %>%
    filter(!is.na(!!sym(id_element))) %>%
    arrange(!!sym(id_element)) %>%
    ## set Categorie based upon the order in the id_element column
    ## Zie: https://stackoverflow.com/a/61503816/6375668
    mutate(Categorie = reorder(factor(Categorie), !!sym(id_element))) %>%
    ## Get variables and put them in named list per category
    group_split(Categorie) %>%
    purrr::set_names(purrr::map_chr(., ~.x$Categorie[1] %>% as.character())) %>%
    map(~ .x %>% pull(Veldnaam) %>% as.list()) %>%
    ## Iterate over elements
    map(
      ~map(
        ## check if element is present and correctly formed
        .x, ~keep(.x, present_and_correct(.x, element, df = df)) %>%
          ## set the display name per element
          set_names(get_display_name(.x, id))
        ## Remove all empty elements
      ) %>% compact() %>% unlist
    )


  ## Set the selected value based on the element
  if (element == "y_rechts") {
    selected <- lVariables[[1]][[2]]
  } else {
    selected <- lVariables[[1]][[1]]
  }

  if (is.null(label)) {
    label <- str_to_title(element)
  }

  pickerInput(
    inputId = id_element,
    label = label,
    choices = lVariables,
    selected = selected,
    options = list(`live-search` = TRUE)
  )
}

## Function to create a picker input for splitting variables
pickerSplitVar <- function(id, variable = "INS_Splits_variabele", name = "color", label = "Kleur", df = dfCombi_geaggregeerd) {

  #browser()

  ## Create a named list with unique values as names and the combination of unique value and column name as value
  choices <-  sort(unique(df[[variable]])) %>%
    discard(~.x == "Alle")  %>%
    map(~paste(.x, variable, sep = ";")) %>%
    set_names(~map_chr(.x, ~get_display_name(.x, id)))

  inputId <- paste(id, name, sep = "-")

  pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    options = list(
      `live-search` = TRUE
    )

  )
}


## Function to create a picker input for filtering value
pickerValues <- function(id, variable = "faculty", role = "left", selected = "All", multiple = TRUE, df = NULL) {

  # if (role == "left") {
  #   role_name <- "links"
  # } else if (role == "right") {
  #   role_name <- "rechts"
  # } else if (role == "both") {
  #   role_name = ""
  # }

  ns <- NS(id)

  ## Set the data frame based on the multiple parameter and provided df
  if (multiple == TRUE && is.null(df)) {
    df = dfInstelling_individueel
  } else {
    df = dfCombi_geaggregeerd
  }

  inputId <- ns(paste(variable, role, sep = "_"))

  ## TODO: variable and label name should be translated / mapped
  ## Convert user-friendly variable names to appropriate column names
  if (variable == "faculty") {
    variable <- "INS_Faculteit"
    variable_name <- "Faculteit"
  } else if (variable == "phase") {
    variable <- "INS_Opleidingsfase_BPM"
    variable_name <- "Fase"
  } else if (variable == "opleiding") {
    variable <- "INS_Opleidingsnaam_2002"
    variable_name <- "Opleiding"
  } else if (variable == "cohort") {
    variable <- "INS_Inschrijvingsjaar_EOI"
    variable_name <- "Cohort"
  } else {
    variable_name <- variable
  }

  label <- variable_name



  ## Create a named list with unique values as names
  choices <-  sort(unique(df[[variable]])) %>%
    purrr::set_names(.) %>%
    map(~paste(.x, variable, sep = ";"))

  #browser()

  if (length(selected) == 1 && selected == "All") {
    selected <- choices
  }

  ## Transform the string all to all choices
  pickerInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    options = list(
      `actions-box` = TRUE,
      `deselect-all-text` = "Geen",
      `select-all-text` = "Alles",
      `none-selected-text` = "-- GEEN FILTER --",
      `live-search` = TRUE
    )
  )
}



### Sankey ####
## TODO Maybe integrate with other pickers
## TODO Expand

## Pick variables for state (left_var of right_var currently)

pickerSankeyVar <- function(id, df_sankey, df_config_sankey, state = "left_var") {

  id_state <- NS(id, state)

  if (state == "left_var") {
    text = "links"
  } else if (state == "right_var") {
    text = "rechts"
  }

  lVariables <- df_config_sankey %>%
    filter(!is.na(!!sym(state))) %>%
    mutate(Categorie = factor(Categorie)) %>%
    group_split(Categorie) %>%
    purrr::set_names(purrr::map_chr(., ~.x$Categorie[1] %>% as.character())) %>%
    map(~ .x %>% pull(target) %>% as.list()) %>%
    ## Iterate over elements
    map(
      ~map(
        ## check if element is present and correctly formed
        .x, ~keep(.x, present_and_correct(.x, df = df_sankey)) %>%
          ## set the display name per element
          set_names(get_display_name(.x, id))
        ## Remove all empty elements
      ) %>% compact() %>% unlist
    )


  pickerInput(
    inputId = id_state,
    label = paste("variabele", text, sep = " "),
    choices = lVariables,
    selected = lVariables[[1]][[1]]
  )
}


## Pick values for transition of the two Sankey states (by default first three)
pickerSankeyValues <- function(id, filter_var, df_sankey, side) {

  inputId_base <- paste0("filter_", side)
  inputId <- NS(id, inputId_base)

  values <- unique(df_sankey[[filter_var]])

  pickerInput(
    inputId = inputId,
    label = paste0("filter ", side),
    choices = values,
    selected = values[1:5],
    multiple = TRUE
  )
}


### Gantt ####
## TODO Maybe integrate with other pickers
## TODO Expand

## Pick a variable to show values in Gantt
pickerGanttVar <- function(id, element, df_config_gantt, input_var_value = NULL) {

  id_element <- NS(id, element)

  if (element == "target_var") {
    basic_choices <- df_config_gantt %>% filter(input_var == input_var_value) %>% pull(target_var) %>% unique()
    label <- "doel variable"
  } else if (element == "input_var") {
    basic_choices <- df_config_gantt[[element]] %>% unique()
    label <- "input variabele"
  }

  ## Set friendly names for choices
  choices <- df_config_gantt %>%
    ## Keep only earlier selected choices
    filter(!!sym(element) %in% basic_choices) %>%
    ## Split per category
    mutate(Categorie = factor(Categorie)) %>%
    group_split(Categorie) %>%
    purrr::set_names(purrr::map_chr(., ~.x$Categorie[1] %>% as.character())) %>%
    map(~ .x %>%
          #filter(!!sym(element) %in% choices) %>%
          pull(!!sym(element)) %>%
          as.list() %>%
          unique()
        ) %>%
    ##' *INFO* present and correct could be added
    map(
      ~set_names(.x, ~map_chr(.x, ~get_display_name(.x, id)))
    )

  pickerInput(
    inputId = id_element,
    label = label,
    choices = choices,
    selected = choices[[1]][[1]]
  )
}


## Filter the values in the gantt
pickerGanttValues <- function(id, filter_var, df_doorstroom_gantt) {

  inputId <- NS(id, "filter")

  pickerInput(
    inputId = inputId,
    label = "input filter",
    choices = unique(dfCHO_doorstroom[[filter_var]])
  )
}

## Picker support ####
present_and_correct <- function(column_name, element = NA, df = dfCombi_geaggregeerd) {

  present <- column_name %in% names(df)

  ## Controleer per type grafiek-element of de kolom voldoet
  correct_form <- switch(
    element,
    ##
    ## TODO: Tijdelijk zo gezet
    #"x" = length(unique(df[[column_name]])) < 15,
    "x" = TRUE,
    "y" = typeof(df[[column_name]]) %in% c("logical", "double", "integer") & class(df[[column_name]]) != "Date",
    "y_links" = typeof(df[[column_name]]) %in% c("logical", "double", "integer") & class(df[[column_name]]) != "Date",
    "y_rechts" = typeof(df[[column_name]]) %in% c("logical", "double", "integer") & class(df[[column_name]]) != "Date",
    ## TODO: Kleur gaat nu via pickerFilter functie
    #"color" = TRUE
    "color" = is.logical(df[[column_name]]) |
      is.integer(df[[column_name]]) & length(unique(df[[column_name]])) < 15 |
      is.character(df[[column_name]]) & length(unique(df[[column_name]])) < 15,
    "sankey" = is.logical(df[[column_name]]) |
      is.integer(df[[column_name]]) & length(unique(df[[column_name]])) < 15 |
      is.character(df[[column_name]]) & length(unique(df[[column_name]])) < 15
  )

  ## Set correct form to TRUE when the element is not defined
  if(is.null(correct_form)) {
    correct_form <- TRUE
  }

  ## Combine the two checks
  result = present & correct_form

  return(result)


}

## Tabellen ####

## Function to define the structure of tables based on the 'type' parameter
tabPanelTables <- function(id, table_one = "tabel", table_two = "tabel_twee") {
  if (str_detect(id, "bench")) {
    ## Call function to create a tab panel with two tables
    tabTableTwo(id, table_one, table_two)
  } else if (str_detect(id, "comp")) {
    ## Call function to create a tab panel with one table
    tabTableOne(id, table_one)
  }
}

## Function to create a tab panel with one table
tabTableOne <- function(id, table_one) {
  tabPanel(span("Tabel",
                title = "info tekst",`data-toggle` = "tooltip"),
           fluidRow(
             column(
               width = 12,
               align = "center",
               withSpinner(DTOutput(NS(id, table_one)))
             )
           )
  )
}

## Function to create a tab panel with two tables
tabTableTwo <- function(id, table_one, table_two) {
  tabPanel(span("Tabel",
                title = "info tekst",`data-toggle` = "tooltip"),
    fluidRow(
      column(
        width = 6,
        align = "center",
        withSpinner(DTOutput(NS(id, table_one)))
      ),
      column(
        width = 6,
        align = "center",
        withSpinner(DTOutput(NS(id, table_two)))
      )
    )
  )
}

## Kan weg: wordt niet meer gebruikt
## Function for the popovers on the tabs of the tables
# tabellenPopover <- function(..., tabblad) {
#
#   tabblad_info <- case_when(tabblad == "Table" ~ "Text table",
#                             tabblad == "Composition percentages" ~ "Text composition %",
#                             TRUE ~ "Test")
#
#   tabblad_tekst <- paste0("<br>", tabblad_info, "</br>")
#
#   spsComps::bsPopover(
#     tag = ...,
#     title = tabblad,
#     content = tabblad_tekst,
#     placement = "left",
#     bgcolor = "#3C8DBC",
#     titlecolor = "white",
#     contentcolor = "#3C8DBC",
#     html = TRUE
#   )
# }



