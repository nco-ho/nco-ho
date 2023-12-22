## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## UI.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: UI shiny app NCO-HO project
##
## Afhankelijkheden:
##
## Datasets:
##
## Opmerkingen:
## 1) Geen.
## 2) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## TODO:
## 1) ___.
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Geschiedenis:
## 23-05-2022: PdO: Aanmaak bestand
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. ONDERDELEN VAN HET DASHBOARD ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ui <- function(request) {

  ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ## 1.1 Header ####

  DB_header <- dashboardHeader(
    title = "NCO-HO",
    #enable_rightsidebar = TRUE,
    controlbarIcon = icon("user"),
    leftUi = tagList(

      dropdownTabDirect(
        title = "Home",
        tab_name = "landingspagina",
        type = "tasks"
      ),
      dropdownTabMenu(
        title = "Toegankelijkheid",
        type = "tasks",
        taskItemTab(
          text = "benchmark - ind",
          tab_name = "bench_ind",
          tabSelect = TRUE
        ),
        taskItemTab(
          text = "vergelijk - ind",
          tab_name = "compare_ind",
          tabSelect = TRUE
        ),
        taskItemTab(
          text = "benchmark - aggr",
          tab_name = "bench_aggr",
          tabSelect = TRUE
        ),
        taskItemTab(
          text = "vergelijk - aggr",
          tab_name = "compare_aggr",
          tabSelect = TRUE
        )
      ),
      dropdownTabMenu(
        title = "Doorstroom",
        type = "tasks",
        taskItemTab(
          text = "gantt WO B - M ",
          tab_name = "gantt",
          tabSelect = TRUE
        ),
        taskItemTab(
          text = "sankey WO B - M",
          tab_name = "sankey",
          tabSelect = TRUE
        )
      ),
      dropdownTabMenu(
        title = "Arbeidsmarkt",
        type = "tasks",
        taskItemTab(
          text = "benchmark - ind",
          tab_name = "bench_ind",
          tabSelect = TRUE
        ),
        taskItemTab(
          text = "vergelijk - ind",
          tab_name = "compare_ind",
          tabSelect = TRUE
        ),
        taskItemTab(
          text = "benchmark - aggr",
          tab_name = "bench_aggr",
          tabSelect = TRUE
        ),
        taskItemTab(
          text = "vergelijk - aggr",
          tab_name = "compare_aggr",
          tabSelect = TRUE
        )
      ),
      dropdownTabMenu(
        title = "Explorer",
        type = "tasks",
        taskItemTab(
          text = "explorer",
          tab_name = "explorer",
          tabSelect = TRUE
        )
      )
    )

    ## TODO: Tijdelijk uitgezet omdat we geen messages hebben
    # dropdownMenu(
    #   type = "messages",
    #   messageItem(
    #     from = "CBS data",
    #     message = "Laatste update:",
    #     icon = icon("user"),
    #     time = "2022-01-01"
    #   ),
    #   messageItem(
    #     from = "Vakaanmeldingen",
    #     message = "Laatste update:",
    #     icon = icon("life-ring"),
    #     time = "2022-01-01"
    #   )
    # )
  )

  ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ## 1.2 Sidebar ####

  ## Dit menu wordt overschreven / verborgen, maar de functionaliteit wordt wel gebruikt
  DB_sidebar <- dashboardSidebar(
    sidebarMenuOutput("menu")
  )

  ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ## 1.3 Right sidebar ####

  DB_rightsidebar <- dashboardControlbar(
    skin = "dark",
    id = "personal_menu",
    controlbarMenu(
      id = "menu",
      controlbarItem(
        icon("gears"),
        h3("Bij volgend bezoek:"),
        br(),
        "Laad laatst gekozen doelgroep",
        switchInput(inputId = "keuzes",
                    #label = "Keuzes (variabelen / doelgroep)",
                    value = TRUE,
                    onLabel = "Ja",
                    offLabel = "Nee",
                    onStatus = "primary",
                    offStatus = "warning"),
        "Laad laatst gekozen tabblad",
        switchInput(inputId = "tabblad",
                    #label = "Tabblad",
                    value = TRUE,
                    onLabel = "Ja",
                    offLabel = "Nee",
                    onStatus = "primary",
                    offStatus = "danger")
      ),
      controlbarItem(
        icon("star"),
        "Bookmarks",
        br(),
        "en dan hier een lijst met ",
        messageItem(
          from = "Vakaanmeldingen",
          message = "Laatste update:",
          icon = icon("life-ring"),
          time = "2022-01-01"
        )
      )
    )
  )

  ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ## 1.4 Body ####

  ## Definieer de tabs in het dashboardbody
  DB_body <- dashboardBody(
    ## Add manual JS to make header navigation work
    useShinyjs(),
    extendShinyjs(text = "shinyjs.tabSelect = function(tabName) { $('a[data-value=' + tabName + ']').click();}", functions = c("tabSelect")),
    ## TODO: Dit zoveel mogelijk verplaatsen naar /www map
    ## Laad FontAwesome icons in
    tags$script(src = "https://kit.fontawesome.com/01cb805c1e.js"),

    ## Laad de CSS uit www/custom.css in
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                ## Shinydashboardplus heeft de eigen css custom.css genoemd
                href = "custom2.css")
    ),
    tags$head(tags$script(src = "popover.js")),
    tags$head(tags$script(src = "main.js")),

    tags$head(
      tags$script("
      $( document ).ready(function() {

        // Verberg toggle sidebar links
        document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';
      })

      //waitForElementToDisplay('#introductie',function() {
      /*
      waitForElementToDisplay('#shiny-tab-toegankelijkheid',function() {
        //var element = document.querySelector('#shiny-tab-toegankelijkheid');
        var element = document.querySelector('#shiny-tab-toegankelijkheid');
        console.log(element);
        element.setAttribute('aria-hidden', 'false');
        element.setAttribute('tabindex', '0');
        element.classList.add('active');

        },
      1000,9000);
      */
      ")
    ),
    uiOutput("tabItems")
    ## Bepaal de tabbladen
    # fluidPage(
    #   fluidRow(
    #     uiOutput("tabItems")
    #   )
    # )

  )



  ## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  ## TODO: Op dit moment gebruiken we geen footer
  ## 1.5 Footer ####

  # DB_footer <- dashboardFooter(
  #      left_text = HTML(paste("<b>NCO-HO</b>","<br/>&copy; Vrije Universiteit Amsterdam,",
  #                         format(Sys.Date(), "%Y")),
  #      right_text = HTML(paste("<div id='vu-logo'>.</div>")))
  #     )

  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 2. COMBINEER DE ONDERDELEN ####
  ## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  dashboardPage(DB_header,
                DB_sidebar,
                DB_body,
                DB_rightsidebar,
                freshTheme = NCO_theme,
                preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#367fa9"), # "#3c8dbc"),))
  )
}
