## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## global.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: Het global.R bestand van de NCO-HO app
##       Laad eenmaal aan het begin
##
## Afhankelijkheden: Geen
##
## Datasets: Geen
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO:
## 1) ___.
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 0. BIBLIOTHEKEN EN FUNCTIES ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## 0.3 Configuratie ####

## TODO: configuratie script aanmaken
Sys.setenv(R_CONFIG_ACTIVE = "synthetische data")
locatie <- config::get("location")

## Voor testing:
# locatie <- "2.Shiny/"

##'*INFO* Fix for error, see: https://stackoverflow.com/questions/3548090/facet-grid-problem-input-string-1-is-invalid-in-this-locale
Sys.setlocale(locale = "C")

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## 0.1 Configuratie: Branch ####

# Branch <- "ontwikkel"
# Stel de standaard taal in bij opstarten
default_language <- "NL"



### Theming plots ####
## Theming plots met thematic/ggpubr packages

## ggplot
#ggplot2::theme_set(ggplot2::theme_minimal())
#theme_update(text = element_text(family = "Helvetica Neue"))
ggplot_instellingen <- list(ggplot2::scale_fill_brewer(palette = "Pastel1"), ggplot2::scale_colour_brewer(palette = "Pastel1"))

#theme_get() %>% View()

## Werkt nog  niet
ggplot2::theme_set(
  ggpubr::theme_pubr(
    #base_size = 10,
    base_family = "Helvetica Neue",
    border = FALSE,
    margin = TRUE,
    # legend = c("top", "bottom", "left", "right", "none"),
    x.text.angle = 0
  )
)

# thematic::thematic_shiny(font = "auto")

# thematic::thematic_off()

## Load all kinds of config
source(paste0(locatie, "libraries.R"))
source(paste0(locatie, "data_model/config.R"))
source(paste0(locatie, "ui_functions.R"))
source(paste0(locatie, "server_functions.R"))
#source(paste0(locatie, "connectie_SQL_database.R"))

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## 0.5 Modules / R ####

## Laad alle modules
for (file in list.files(paste0(locatie, "modules"))) {
  source(file.path(paste0(locatie, "modules"), file))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. LAAD BESTANDEN IN ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Configuratie voor aanwezige variabelen in dashboards (en display namen)
dfCategorie_aggr <- read_delim(paste0(locatie, "data_model/kolom_categorie_geaggregeerd.csv"),
                          delim = ";",
                          show_col_types = FALSE)

dfCategorie_ind <- read_delim(paste0(locatie, "data_model/kolom_categorie_individueel.csv"),
                          delim = ";",
                          show_col_types = FALSE)

df_config_gantt <- read_delim(paste0(locatie, "data_model/doorstroom_config_gantt.csv"),
                                       delim = ";",
                                       show_col_types = FALSE)

df_config_sankey <- read_delim(paste0(locatie, "data_model/doorstroom_config_sankey.csv"),
                                          delim = ";",
                                          show_col_types = FALSE)

##'*INFO* Mapping tables zijn in bovenstaande geaggregeerd.
## Mapping table van variabelnamen in R naar gebruiksvriendelijke namen
# dfDisplay_col_names <- read_delim(paste0(locatie, "data_model/display_columnnames.csv"),
#                                   delim = ";",
#                                   show_col_types = FALSE)

## Turn it to named vector
vDisplay_names_aggr <- set_names(dfCategorie_aggr$Veldnaam_gebruiker,
                            dfCategorie_aggr$Veldnaam)

vDisplay_names_ind <- set_names(dfCategorie_ind$Veldnaam_gebruiker,
                            dfCategorie_ind$Veldnaam)

vDisplay_names_gantt <- set_names(df_config_gantt$Veldnaam_gebruiker,
                                  df_config_gantt$Veldnaam)

vDisplay_names_sankey <- set_names(df_config_sankey$Veldnaam_gebruiker,
                                   df_config_sankey$Veldnaam)

vSplits_variables <- c(
  "INS_Inschrijvingsjaar_EOI", ## Aggregatieniveau
  "INS_Opleidingsnaam_2002"#, ## Aggregatieniveau
  ## TODO: Tijdelijk uitgezet
  #"INS_Opleidingsfase_BPM", ## Noodzakelijk voor onderscheid Bachelor - Premaster
  #"INS_Faculteit" ## Bewaar deze variabele (is hierarchisch)
)
## TODO: Tijdelijk uitgezet, net zoals links naar esquisse / explorer in server.R / ui.R
sCombi_geaggreerd_bestandspad <- paste0(locatie, config::get("dataset_combi_geaggregeerd"))
dfCombi_geaggregeerd <- read_rds(sCombi_geaggreerd_bestandspad) %>%
  mutate(across(where(is.character), ~replace_na(.x, "Onbekend")))

# sCombi_geaggreerd_bestandspad_new <- paste0(locatie, config::get("dataset_combi_geaggregeerd_new"))
# dfCombi_geaggregeerd_new <- read_rds(sCombi_geaggreerd_bestandspad_new)


sInstelling_individueel_bestandspad <- paste0(locatie, config::get("dataset_instelling_individueel"))
dfInstelling_individueel <- read_rds(sInstelling_individueel_bestandspad) %>%
  mutate(across(where(is.character), ~replace_na(.x, "Onbekend")))

sCHO_doorstroom <- paste0(locatie, config::get("dataset_CHO_doorstroom"))
dfCHO_doorstroom <- read_rds(sCHO_doorstroom) %>%
  mutate(across(where(is.character), ~replace_na(.x, "Onbekend"))) %>%
  mutate(INS_Instelling_na_B_opgevuld = INS_Instellingsafkorting_instroom,
         INS_Instelling_voor_M_opgevuld = INS_Instellingsafkorting_examen)

sCHO_doorstroom_gantt <- paste0(locatie, config::get("dataset_CHO_doorstroom_gantt"))
dfCHO_doorstroom_gantt <- read_rds(sCHO_doorstroom_gantt) %>%
  mutate(across(where(is.character), ~replace_na(.x, "Onbekend")))

vSplits_variables <- c(
  "INS_Inschrijvingsjaar_EOI", ## Aggregatieniveau
  "INS_Opleidingsnaam_2002"#, ## Aggregatieniveau
  ## TODO: Tijdelijk uitgezet
  #"INS_Opleidingsfase_BPM", ## Noodzakelijk voor onderscheid Bachelor - Premaster
  #"INS_Faculteit" ## Bewaar deze variabele (is hierarchisch)
)


## TODO: Tijdelijk uitgezet sinds aggregatie
# df <- dfPopulatie %>%
#   ## Groeper per rij om mutate goed te laten gaan
#   rowwise() %>%
#   mutate(across(
#     where(is.numeric),
#     ~ ifelse(
#       ## Check of alle waardes gehele getallen zijn en maak er dan integer van
#       sum(as.double(.x) == as.integer(.x)) == length(.x),
#       as.integer(.x),
#       .x
#     ))) %>%
#   ## Moet na rowwise
#   ungroup()

dfFilters_ind <- dfInstelling_individueel %>%
  dplyr::summarize(INS_Faculteit, INS_Opleidingsfase_BPM, INS_Opleidingsnaam_2002) %>% unique()

dfFilters_agg <- dfCombi_geaggregeerd %>%
  dplyr::summarize(INS_Faculteit, ARB_Opleiding_fase, INS_Opleidingsnaam_2002) %>% unique()

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. LAAD BOOKMARKING ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

enableBookmarking(store = "url")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 4. THEME ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Theming Shiny ####
## Theming with fresh package
## Voorbeeld thema's:
## shiny::runGitHub("dreamRs/fresh", subdir = "inst/examples/create")

NCO_theme <- create_theme(
  adminlte_global(
    content_bg = "#D8DEE9"),
    #box_bg = "#ffffff"),
  adminlte_sidebar(
    ## Beide sidebars
    dark_bg = "#5691cc",
    dark_hover_bg = "#D8DEE9",
    dark_color = "#2E3440"
  ),
  adminlte_color(
    green = "#3fff2d",
    blue = "#2635ff",
    red = "#ff2b2b",
    yellow = "#feff6e",
    fuchsia = "#ff5bf8",
    navy = "#374c92",
    purple = "#615cbf",
    maroon = "#b659c9",
    light_blue = "#5691cc"
  ),
  adminlte_vars()
)


