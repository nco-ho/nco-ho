## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## config.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: Bestand om basis configuratie te maken
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. DATA ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##'*INFO* Currently not in use
## TODO: Voor simpele settings hebben we config.yml. Voor data hebben we /data, dus dit bestand
## kan straks worden uitgefaseerd.

## Bepaal naam en icon per menu item
lMenuInformation <-
  list(
    list("landingspagina", "house"),
    list("toegankelijkheid", "door-open"),
    list("switchgedrag", "shuffle"),
    list("arbeidsmarkt", "briefcase")
  )

## Bepaal id per plotsWithFilters module
lModuleplotsWithFiltersIds <-
  list(
    #"landingspagina", # While this is a tab is not built with a plot module
    "toegankelijkheid",
    "switchgedrag",
    "arbeidsmarkt"
  )

vRelevante_variabelen <-                                                                                                                                                                                vRelevante_variabelen <-
  c(
    ## Data (obv) 1CHO ####

    ## Basis
    #"INS_Studentnummer",
    "INS_Opleidingsnaam_2002",
    "INS_Inschrijvingsjaar",
    "INS_Inschrijvingsjaar_EOI",
    "INS_Studiejaar",
    "INS_Opleidingsfase_BPM",

    ## Inschrijving
    "INS_Hoofdneven",
    #"INS_Opleidingsvorm_naam",
    "INS_September_februari_instroom",
    "INS_Uitschrijving_voor_1_feb",
    "INS_Herinschrijving_jaar_2_na_uitschrijving_voor_1_feb_in_jaar_1",
    "INS_Dubbele_studie_VU",

    ## Demografie
    "DEM_Geslacht",
    "DEM_Leeftijd_peildatum_1_oktober_cat",
    ## Buitenland
    "DEM_Nationaliteit_EER_naam",

    ## Vooropleiding
    "INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO",
    "INS_Hoogste_vooropleiding_soort_cat",
    "VOP_School_reistijd_OV_VU",
    "INS_IO_Herkomst_EER_naam",

    ## Studiesucces
    "SUC_Type_uitstroom",
    "SUC_Type_uitstroom_studiejaar",
    "INS_Datum_uitschrijving",
    "INS_Datum_diploma",
    "INS_Cum_laude",

    ## Doorstroom
    "INS_Direct",
    "INS_Doorstroom_van_bachelor_naar_master",
    "INS_Doorstroom_van_premaster_naar_master",

    ## Tussenjaar
    "INS_Tussenjaar_voor_B",
    "INS_Tussenjaar_voor_M",
    "INS_Tussenjaren_tijdens_B",
    "INS_Tussenjaar_binnen_opleiding",



    ## Data Instelling - HO breed ####

    ## Basis
    "INS_Opleidingsvariant",
    "INS_Faculteit",
    "INS_Dagen_tussen_aanmelding_en_1_september",
    "INS_Status_Zachte_knip_bij_inschrijving",

    ## Opleiding
    "OPL_Studielast_nominaal",
    "OPL_Instructietaal",

    ## Orientatie
    # "ORI_Orientatie_komt_voor",
    # "ORI_Orientatie_vooraf_extensief_aanwezig",
    # "ORI_Orientatie_vooraf_intensief_aanwezig",
    "INT_Introductie_aanwezig",

    ## Introductie
    # "INT_Introductie_aanwezig",
    # "INT_Introductie_type",

    ## Matching
    # "MVR_Studiesituatie_Ouders_universiteit",
    # "MVR_Studiesituatie_Financien_belemmering",
    # "MVR_Studiesituatie_Goede_werkplek",
    # "MVR_Studiesituatie_Omgeving_hulp",
    # "MVR_Studiesituatie_Ondersteuning_nodig",
    # "MVR_Studiesituatie_Uur_per_week_studie_cat",
    # "MVR_Studiesituatie_Uur_per_week_werk_cat",
    # "MVR_Studiesituatie_Uur_per_week_nevenactiviteiten_cat",
    # "MVR_Studiesituatie_Zorgtaak_belemmering",

    ## Studiesuccess
    "BSA_Status_omschrijving_eindejaars",
    "HNP_Honours_programma_begonnen",
    "HNP_Honours_programma_geslaagd",

    "RES_Aantal_EC_tm_jaar_1",
    "RES_Gem_resultaat_tm_jaar_1",

    ## VU Specifieke data ####

    ##
    ## PUC / Better Prepared

    ## Taaltoets
    # "TTS_Totaal_cat",
    # "TTS_Formuleren_cat",
    # "TTS_Grammatica_cat",
    # "TTS_Spelling_en_interpunctie_cat",
    # "TTS_Structuur_cat",
    # "TTS_Woordenschat_en_woordkeuze_cat",

    ## Opleidingsspecifieke kenmerken
    "OPL_Cluster"


  )

