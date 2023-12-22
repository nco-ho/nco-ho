## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## ui_functions.R ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Doel: Wrapper functies voor standaard server functies
##

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Filter functions ####

## TODO: Single list for values naming, potentially multiple options + extended logic
## Prepares a dataframe based on provided filters and naming options.
prep_df <- function(lFilters, lValues_for_naming, df, color_var, facet = "left", facet_var = sym("VIS_Groep"), facet_name_var = sym("VIS_Groep_naam")) {

  ## Collapses values from the naming list into a single string
  sName <- paste(keep_values(lValues_for_naming), collapse = " / ")

  ## Removes null elements from the filter list
  lFilters <- lFilters %>%
    discard(is.null)

  ## Transforms filter list into elements suitable for filtering
  lFilter_elements <- map(lFilters, transform_input)

  ## Applies filters, adds new columns, and casts var used as color to factor
  dfPrepared <- df %>%
    filter_with_lists(lFilter_elements) %>%
    mutate(
      !!facet_var := facet,
      !!facet_name_var := paste("VU",
                                sName,
                                sep = " - ")
    ) %>%
    mutate(!!sym(color_var) := as.factor(!!sym(color_var)))

  return(dfPrepared)
}

## Filters out only relevant values based on the provided filters.
keep_only_relevant_values <- function(lFilters, sVariable, id = NULL) {

  if (is.null(id) || str_detect(id, "ind")) {
    dfFilters <- dfFilters_ind
  } else if (str_detect(id, "aggr")) {
    dfFilters <- dfFilters_agg
  }

  ## Verifies the input variables are set, if not stop execution
  req(lFilters)

  ## Removes null elements from the filter list
  lFilters <- lFilters %>%
    discard(is.null)

  ## Transforms filter list into elements suitable for filtering
  lFilter_elements <- map(lFilters, transform_input)

  ## Retrieves relevant values from the data
  lRelevante_waardes <- dfFilters %>%
    filter_with_lists(lFilter_elements) %>%
    pull(!!sym(sVariable)) %>%
    purrr::set_names(.) %>%
    map(~paste(.x, sVariable, sep = ";"))

  return(lRelevante_waardes)
}

## Supporting functions for filters ####

## Extracts values before the semicolon from a ";"-separated string
keep_values <- function(input) {
  lValues <- map(input, ~str_split(., ";")[[1]][1])

  return(lValues)
}

## Transforms a list of inputs into a column and value for filtering
transform_input <- function(input) {

  ## Splits the string and retrieves the second part as the column name
  sColumn <- str_split(input[1], ";")[[1]][2]

  ## Retrieves the filter values
  lValues <- keep_values(input)

  ## Combines column and values into a filter element
  lFilter_element <- list(sColumn, lValues)

  return(lFilter_element)
}

## Filters a dataframe using a list with column and one or more values
filter_with_lists <- function(df, filters) {

  ##' *INFO* about <<- see: https://adv-r.hadley.nz/environments.html?q=%3C%3C-#super-assignment--
  ## Applies each filter to the dataframe
  walk(filters, function(.x) df <<- df %>% filter(!!sym(.x[[1]]) %in% .x[[2]]))

  return(df)
}

## limit the number of values
limit_n_values_gantt <- function(df, split_var, n_values = 12) {
  if (n_values >= n_distinct(df[[split_var]])) {
    return(df)
  }


  values_to_keep <- df[[split_var]][1:(n_values - 1)]

  ## All variables from manipulation before gantt
  n <- sum(df$n[(n_values):nrow(df)])
  flow_perc <- sum(df$flow_perc[n_values:nrow(df)])
  flow_end_perc <- 1
  flow_start_perc = df$flow_start_perc[n_values]


  df_mutated <- df %>%
    filter(!!sym(split_var) %in% values_to_keep) %>%
    bind_rows(data.frame(#!!sym(split_var) := "other",
      split_var_placeholder = "Anders",
      n = n,
      flow_perc = flow_perc,
      flow_end_perc = flow_end_perc,
      flow_start_perc = flow_start_perc) %>%
        ## Update the dataframe before binding to avoid multiple columns with same names
        rename(!!sym(split_var) := split_var_placeholder)
    ) %>%
    arrange(flow_start_perc)

  return(df_mutated)
}

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Bind and summarize functions ####

## Binds two dataframes row-wise and performs additional manipulations depending on the 'type'
bind_both <- function(dfLeft, dfRight, id = "bench", y_left = NULL, y_right = NULL, facet_var = sym("VIS_Groep"), facet_name_var = sym("VIS_Groep_naam")) {

  ## Binds the left and right dataframes and reorders factor levels
  dfBoth <- bind_rows(dfLeft, dfRight) %>%
    mutate(!!facet_name_var := forcats::fct_reorder(!!facet_name_var, !!facet_var, min))
  #mutate(VIS_Groep_naam = forcats::fct_reorder(VIS_Groep_naam, VIS_Groep, min))

  ## Mutates y for comparison type
  if (str_detect(id, "comp")) {
    dfBoth <- dfBoth %>%
      mutate(y = if_else(!!facet_var == "left",
                         !!sym(y_left),
                         !!sym(y_right))
      )
  }

  return(dfBoth)
}

## Joins two summarized dataframes, relocates y_left before y_right
bind_both_table <- function(dfLeft_summ, dfRight_summ, y_left, y_right) {

  ## Changes VIS_Groep to 'left' for the right summarized dataframe
  dfRight_summ <- dfRight_summ %>% mutate(VIS_Groep = "left")

  ## Joins left and right summarized dataframes, and relocates y_left before y_right
  dfBoth_table <- inner_join(dfLeft_summ, dfRight_summ) %>%
    relocate(!!sym(y_left), .before = !!sym(y_right))

  return(dfBoth_table)
}

## Prepares a summarized dataframe based on provided variables and a y-variable
prep_df_summ <- function(df, variables, y) {

  ## Groups by provided variables, summarizes the y-variable and counts number of observations
  ## per group
  df_summarized <- df %>%
    group_by(across(all_of(variables))) %>%
    dplyr::summarize(
      !!sym(y) := ifelse(y == "Geen",
                         NA,
                         round(mean(!!sym(y), na.rm = TRUE), 3)),
      Aantal = n()
    ) %>%
    ungroup()

  return(df_summarized)
}

## Prepares a summarized dataframe based on provided variables, y-variable, color, and total count
prep_df_summ_aggr <- function (df, variables, y, color, total_n_var = sym("INS_Aantal_eerstejaars"), aggr_split_value_var = sym("INS_Splits_variabele_waarde")) {


  ## Groups by provided variables, calculates weighted mean for y-variable, sums up total count per group and arranges by color
  df_summarized <- df %>%
    group_by(across(all_of(variables))) %>%
    dplyr::summarize(
      ##'*INFO* Code for calculation based on total quantities
      # !!sym(input$y_left) :=  round(
      #   sum(!!sym(input$y_left), na.rm = TRUE) / sum(!!total_n_var, na.rm = TRUE),
      #   3),
      ## Calculates weighted average
      !!sym(y) :=  round(
        sum(
          (!!sym(y) * !!total_n_var), na.rm = TRUE
        ) / sum(!!total_n_var, na.rm = TRUE),
        3),
      Aantal = sum(!!total_n_var)
    ) %>%
    ungroup() %>%
    ## Set splits_variable_value to color, then set color in front and order on it so all the
    ## graphs and tables will be drawn correctly
    mutate(!!sym(color) := !!aggr_split_value_var) %>%
    select(-!!aggr_split_value_var) %>%
    #mutate(!!sym(color) := !!aggr_split_value_var) %>%
    #select(-!!aggr_split_value_var) %>%
    select(!!sym(color), everything()) %>%
    arrange(!!sym(color))

  return(df_summarized)
}


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Plot functions ####

## Wrapped chart function for creating a plot based on the provided dataframe and variables
wrapped_chart <- function(df, x, y, color, id = "bench", y_left = NULL, y_right = NULL, facet_var = sym("VIS_Groep"), facet_name_var = sym("VIS_Groep_naam")) {

  ## Check id
  if (str_detect(id, "ind")) {
    df_source <- dfInstelling_individueel
  } else if ( str_detect(id, "aggr")) {
    df_source <- dfCombi_geaggregeerd
  } else if (str_detect(id, "gantt")) {
    df_source <- dfCHO_doorstroom
  } else if (str_detect(id, "sankey")) {
    df_source <- dfCHO_doorstroom
  }



  ## Depending on the type of plot, set facet wrap and labels settings
  if (str_detect(id, "bench")) {
    facet_wrap_setting <- facet_wrap(vars(!!facet_name_var))
    xlab_setting <- xlab(get_display_name(x, id))
    ylab_setting <- ylab(get_display_name(y, id))

    ## Get boolean vars in order to add formatting %
    if (is.logical(df_source[[y]])) {
      scale_y <- scale_y_continuous(labels = scales::percent)
    } else {
      scale_y <- scale_y_continuous()
    }

  } else if (str_detect(id, "comp")) {
    if (is.logical(df_source[[y_left]]) && is.logical(df_source[[y_right]])) {
      scale_y <- scale_y_continuous(labels = scales::percent)
    } else {
      scale_y <- scale_y_continuous()
    }


    facet_wrap_setting <- facet_wrap(vars(!!facet_var),
                                     scales = "free_y",
                                     labeller = as_labeller(c("left" = get_display_name(y_left, id),
                                                              "right" = get_display_name(y_right, id)))
    )
    xlab_setting <- xlab(NULL)
    ylab_setting <- ylab(NULL)
  }

  ## Create the base plot
  plot <- basic_plot(df, x, y, color, xlab_setting, ylab_setting, ggplot_instellingen, "top", scale_y) +
    facet_wrap_setting

  ## Set plot type based on the nature of x
  if (is.numeric(df[[x]]) && length(unique(df[[x]])) > 1) {
    plot <- plot +
      geom_line(aes(color = !!sym(color))) +
      geom_point(aes(color = !!sym(color))) +
      scale_x_continuous(labels = scales::label_comma(accuracy = 1),
                         n.breaks = length(unique(df[[x]])))
    ## Set breaks instead of n_breaks when there is only value in numeric variable
  } else if (is.numeric(df[[x]])) {
    plot <- plot + geom_bar(position = "dodge", stat = "identity") +
      scale_x_continuous(labels = scales::label_comma(accuracy = 1),
                         breaks = unique(df[[x]]))
  } else {
    plot <- plot + geom_bar(position = "dodge", stat = "identity")
  }





  ## Add legend using ggplotly
  plot <- ggplotly_with_legend(plot, color, id)

  return(plot)
}

## Create a stacked bar chart, with optional settings for percentage (or not) and wrap (or not) modes
stacked_composition_bar_chart <- function(df, x, color, id, facet_name_var = sym("VIS_Groep_naam"), percentage = FALSE, wrap = FALSE) {

  ## Set plot type and y-axis label depending on whether plot is in percentage mode
  if (percentage == TRUE) {
    position <- "fill"
    y <- "Percentage"
    scale_y <- scale_y_continuous(labels = percent)
    df <- df %>%
      rename(Percentage = Aantal)
  } else {
    position <- "stack"
    y <- "Aantal"
    scale_y <- scale_y_continuous()
  }

  xlab_setting <- xlab(get_display_name(x, id))
  ylab_setting <- ylab(y)

  ## Create the base plot
  plot <- basic_plot(df, x, y, color, xlab_setting, ylab_setting, ggplot_instellingen) +
    geom_bar(position = position, stat = "identity") +
    scale_y

  if (is.numeric(df[[x]]) && length(unique(df[[x]])) > 1) {
    plot <- plot  +
      scale_x_continuous(labels = scales::label_comma(accuracy = 1),
                       n.breaks = length(unique(df[[x]])))
  } else if (is.numeric(df[[x]])) {
    plot <- plot +
      scale_x_continuous(labels = scales::label_comma(accuracy = 1),
                         breaks = unique(df[[x]]))
  }

  if (wrap == TRUE) {
    plot <- plot + facet_wrap(vars(!!facet_name_var))
  }

  plot <- ggplotly(plot)

  return(plot)
}


## Function for creating grid boxplots
grid_boxplots <- function(df, x, color, y, id, y_left = NULL, y_right = NULL, facet_var = sym("VIS_Groep"), facet_name_var = sym("VIS_Groep_naam")) {

  df <- df %>% arrange(!!sym(color), !!sym(x)) %>%
    mutate(!!sym(y) := as.numeric(!!sym(y)))

  unique_values_x <- unique(df[[x]])
  n_rows_grid <- length(unique_values_x)

  ## Set the facet grid and labels settings depending on the type of plot
  if (str_detect(id, "bench")) {
    facet_grid_setting <- facet_grid(
      rows = vars(!!sym(x)),
      cols = vars(!!facet_name_var)
    )
    xlab_setting <- xlab(get_display_name(x, id))
    ylab_setting <- ylab(get_display_name(y, id))


    ## Create base plot
    plot <- basic_plot(df, color, y, color, xlab_setting, ylab_setting, ggplot_instellingen) +
      geom_boxplot() +
      facet_grid_setting

    plot <- ggplotly(plot, height = 250 * n_rows_grid)

  } else if (str_detect(id, "comp")) {

    xlab_setting <- xlab(NULL)
    ylab_setting <- ylab(NULL)

    plot_list <- list()
    ## Create a plot for each unique value of x
    for (value_x in (unique_values_x)) {

      cat("x: ", value_x, "\n")

      ## Filter only for this value of x
      df_part <- df %>% filter(!!sym(x) == value_x)


      ## Set labels dynamic as names for facet_var since labeller() function doesn't accept
      ## Non Standard Evaluation (i.e. use of !!)
      labels_facet_var <- setNames(list(c("left" = paste(get_display_name(y_left, id), value_x, sep = " - "),
                                "right" = paste(get_display_name(y_right, id), value_x, sep = " - "))),
                         as.character(facet_var))

      facet_grid_setting <- facet_wrap(
        vars(!!facet_var),
        scales = "free_y",
        labeller = labeller(!!!labels_facet_var)
      )
      scale_x <- scale_x_discrete()

      ## Create base plot
      subplot <- basic_plot(df_part,
                              color,
                              y,
                              color,
                              xlab_setting,
                              ylab_setting,
                              ggplot_instellingen) +
        geom_boxplot() +
        facet_grid_setting +
        scale_x +
        scale_y_continuous(
          labels = label_number(accuracy = 0.1)
        )

      ## Ggplot object is itself a list. Wrap this in a list to get a list with ggplot list objects
      plot_list <- append(plot_list, list(subplot))
    }

    plot_list <- map(plot_list, ~ggplotly(.x, height = 250 * n_rows_grid))

    plot <- subplot(plot_list, nrows = n_rows_grid)

  }



  return(plot)
}


## Function for creating grid histograms
grid_histograms <- function(df, x, color, y, id, y_left = NULL, y_right = NULL, facet_var = sym("VIS_Groep"), facet_name_var = sym("VIS_Groep_naam")) {

  df <- df %>%
    mutate(!!sym(y) := as.numeric(!!sym(y)))

  unique_values_x <- unique(df[[x]])
  n_rows_grid <- length(unique_values_x)
  ylab_setting <- ylab(NULL)
  scale_y <- scale_y_continuous(labels = percent)

  scale_x <- scale_x_continuous() #limits = c(min(df[[y]], na.rm = TRUE), max(df[[y]], na.rm = TRUE)))

  ## Set the facet grid and labels settings depending on the type of plot
  if (str_detect(id, "bench")) {
    xlab_setting <- xlab(get_display_name(y, id))

    facet_grid_setting <- facet_grid(
      rows = vars(!!sym(x)),
      cols = vars(!!facet_name_var)
    )


    ## Create base plot
    plot <- basic_plot(df, y, y, color, xlab_setting, ylab_setting, ggplot_instellingen) +
      geom_histogram(position = "identity",
                     ## The bar overlap so alpha is needed to see through
                     alpha = 0.5,
                     aes(color = !!sym(color), y = stat(width * density))) +
      facet_grid_setting +
      scale_y +
      scale_x

    ## Make plotly with appropriate height and put it in list for further usage
    plot <- ggplotly(plot, height = 250 * n_rows_grid)
    plot_list <- list(plot)

  } else if (str_detect(id, "comp")) {


    xlab_setting <- xlab(NULL)

    plot_list <- list()
    ## Create a plot for each unique value of x
    for (value_x in (unique_values_x)) {

      ## Filter only for this value of x
      df_part <- df %>% filter(!!sym(x) == value_x)


      ## Set labels dynamic as names for facet_var since labeller() function doesn't accept
      ## Non Standard Evaluation (i.e. use of !!)
      labels_facet_var <- setNames(list(c("left" = paste(get_display_name(y_left, id), value_x, sep = " - "),
                                          "right" = paste(get_display_name(y_right, id), value_x, sep = " - "))),
                                   as.character(facet_var))

      facet_grid_setting <- facet_wrap(
        vars(!!facet_var),
        scales = "free_x",
        labeller = labeller(!!!labels_facet_var)
      )

      ## Create base plot
      subplot <- basic_plot(df_part, y, y, color, xlab_setting, ylab_setting, ggplot_instellingen) +
        geom_histogram(position = "identity",
                       ## The bar overlap so alpha is needed to see through
                       alpha = 0.5,
                       aes(color = !!sym(color), y = stat(width * density))) +
        facet_grid_setting +
        scale_y +
        scale_x

      ## Ggplot object is itself a list. Wrap this in a list to get a list with ggplot list objects
      plot_list <- append(plot_list, list(subplot))
    }

    ## Make plotlys of all the ggplots and add total height
    plot_list <- map(plot_list, ~ggplotly(.x, height = 250))

  }

  return(plot_list)
}

gantt_plot <- function(df, x, xend, split_var, title, position_label_y) {

  ggplotly(
    ggplot(df,
           aes(x = !!sym(x),
               xend = !!sym(xend),
               #y = !!sym(split_var),
               #yend = !!sym(split_var),
               y = reorder(!!sym(split_var), !!sym(x), decreasing = TRUE),
               yend = reorder(!!sym(split_var), !!sym(x), decreasing = TRUE),
               color = !!sym(split_var))) +
      ## TODO De grafiek blijft even groot dus bij veel rijen, kunnen de blokken te breed
      ## worden. Dit kan worden getest door de 'plots' pane groter / kleine te maken
      geom_segment(size = 6) +
      theme_pubr() +
      #ggplot_instellingen +
      scale_color_manual(values = brewer.pal(name = "Set3", n = nrow(df)) %>% set_names(df[[split_var]])) +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL) +
      scale_x_continuous(labels = scales::label_percent()) +
      ggtitle(title) +
      ## TODO: Onderstaande regel wordt niet doorvertaald naar plotly.
      ## Daarom extra plotly layout code toegevoegd
      scale_y_discrete(position = position_label_y)
  ) %>%
    layout(
      yaxis = list(side = position_label_y),
      legend =
        list(
          #orientation = "h",
          #xanchor = "center",
          #x = 0.5,
          #y = 1.20,
          title = title)
    )
}

## Create a sankey, ggplotly with alluvial has no flow but only straight (diagonal) lines
sankey_plot <- function(df, left_var, right_var, xlab_setting, ylab_setting, name_left, name_right, title, title_size = 20, title_font = "verdana") {

  ggplot(data = df,
         aes(axis1 = !!sym(left_var),
             axis2 = !!sym(right_var),
             y = n)) +
    scale_x_discrete(limits = c(name_left, name_right), expand = c(.2, .05)) +
    xlab_setting +
    ylab_setting+
    scale_fill_brewer(palette = "Set3") +
    geom_alluvium(aes(fill = c(!!sym(left_var))), alpha = 0.75, width = 1/8) +
    geom_stratum(na.rm = FALSE, alpha = 1, width = 1/8) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    theme_pubr() +
    theme(plot.title = element_text(size = title_size, family = "verdana")) +
    ggtitle(title) +
    guides(fill = "none")
}

### Supporting functions for plots ####

## Create a basic plot from provided data and settings
basic_plot <- function(df, x, y, color, xlab_setting, ylab_setting, ggplot_instellingen, legend_position = "none", scale_y = NULL) {

  plot <- ggplot(df,
                 aes(
                   x = !!sym(x),
                   y = !!sym(y),
                   fill = !!sym(color)
                 )) +
    theme_pubr() +
    ggplot_instellingen +
    xlab_setting +
    ylab_setting +
    theme(legend.position = legend_position)

  if (!is.null(scale_y)) {
    plot <- plot + scale_y
  }

  return(plot)
}

## Get a user-friendly display name for a column if available
get_display_name <- function(col_name, id = NULL, mapping_table = NULL) {

  ## Check if
  if (is.null(mapping_table) && !is.null(id) && str_detect(id, "ind")) {
    mapping_table <- vDisplay_names_ind
  } else if (is.null(mapping_table) && !is.null(id) && str_detect(id, "aggr")) {
    mapping_table <- vDisplay_names_aggr
  } else if (is.null(mapping_table) && !is.null(id) && str_detect(id, "gantt")) {
    mapping_table <- vDisplay_names_gantt
  } else if (is.null(mapping_table) && !is.null(id) && str_detect(id, "sankey")) {
    mapping_table <- vDisplay_names_sankey
  }


  if (col_name %in% names(mapping_table)) {
    col_name <- mapping_table[[col_name]]
  }

  return(col_name)
}

## Updatecolumn names in a dataframe to user-friendly versions if available
display_colnames <- function(dataframe, mappingdf, keep_other_colnames = T)
{

  if (keep_other_colnames == T) {
    index_check <- which(names(dataframe) %in% mappingdf$Veldnaam_export)
    for (col in index_check) {
      colnames(dataframe)[col] <- mappingdf$Veldnaam[match(colnames(dataframe)[col], mappingdf$Veldnaam_export)]
    }
  } else {
    colnames(dataframe) <- mappingdf$Veldnaam[match(colnames(dataframe),
                                                    mappingdf$Veldnaam_export)]
  }
  return(dataframe)
}

## Make ggplotly and add legend with color as title
ggplotly_with_legend <- function(plot, color, id) {
  plot <- ggplotly(plot) %>%
    layout(legend =
             list(
               orientation = "h",
               xanchor = "center",
               x = 0.5,
               y = 1.22,
               title = list(text = get_display_name(color, id)))
    )

  plot <- clean_pltly_legend(plot)

  return(plot)
}

## Wrapper function allowing a function to be run directly without creating a quiet function
quietly_run <- quietly(function(func,...) {
  func(...)
})

## Solve a bug that facet_wrap leads to legend with a number per facet for every value
##' *INFO* Copy from: https://stackoverflow.com/questions/69289623/avoid-legend-duplication-in-plotly-conversion-from-ggplot-with-facet-wrap
clean_pltly_legend <- function(pltly_obj, new_legend = c()) {

  ## Assigns a legend group from the list of possible entries
  assign_leg_grp <- function(legend_group, leg_nms) {

    leg_nms_rem <- leg_nms

    ## Assigns a .leg_name, if possible
    ## leg_options is a 2-element list: 1 = original value; 2 = remaining options
    parse_leg_nms <- function(leg_options) {
      ## No more legend names to assign
      if (is.na(leg_options)) {
        leg_options
      } else if(length(leg_nms_rem) == 0) {
        leg_options
      } else {
        ## Transfer the first element of the remaining options
        leg_nm_new <- leg_nms_rem[[1]]
        leg_nms_rem <<- leg_nms_rem[-1]

        leg_nm_new
      }

    }

    legend_group %>%
      map(~ parse_leg_nms(.))

  }

  ## Simplifies legend groups by removing brackets, position numbers and then de-duplicating
  simplify_leg_grps <- function(legendgroup_vec) {

    leg_grp_cln <-
      map_chr(legendgroup_vec, ~ str_replace_all(., c("^\\(" = "", ",\\d+\\)$" = "")))

    modify_if(leg_grp_cln, duplicated(leg_grp_cln), ~ NA_character_)

  }

  pltly_obj_data <-
    pltly_obj$x$data

  ## pltly_leg_grp is a character vector where each element represents a legend group. Element is NA
  ## if legend group not required or doesn't exist
  pltly_leg_grp <- pltly_obj_data %>%
    map(~ pluck(., "legendgroup")) %>%
    ## Elements where showlegend = FALSE have legendgroup = NULL
    map_chr(~ if (is.null(.)) {NA_character_} else {.}) %>%
    simplify_leg_grps() %>%
    assign_leg_grp(new_legend)

  pltly_obj_data_new <-
    pltly_obj_data %>%
    map2(pltly_leg_grp, ~ list_modify(.x, legendgroup = .y)) %>%
    map2(pltly_leg_grp, ~ list_modify(.x, name = .y)) %>%
    ## Set show legend FALSE when there is no legend
    map2(pltly_leg_grp, ~ list_modify(.x, showlegend = !is.na(.y)))

  ## Update orginal plotly object
  pltly_obj$x$data <- pltly_obj_data_new

  return(pltly_obj)

}

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
## Table functions ####

## Make tabel
## TODO: Basic or advanced
## TODO: Enable more variables for data_table function
prep_table <- function(y, df, df_summmarized, id, y_right = NULL, facet_var = sym("VIS_Groep"), facet_name_var = sym("VIS_Groep_naam"), ...) {


  ## Remove unneeded variables
  dfTabel <- df_summmarized %>%
    select(-!!facet_name_var,
           -!!facet_var)

  ## Set user friendly names
  names(dfTabel) = map_chr(names(dfTabel), ~get_display_name(.x, id))

  ## Get boolean vars in order to add formatting %
  if (is.logical(df[[y]])) {
    sBoolean_vars <- y
  } else {sBoolean_vars = c()}

  if (!is.null(y_right) && is.logical(df[[y_right]])) {
    sBoolean_vars <- c(sBoolean_vars, y_right)
  }

  sBoolean_vars <- sBoolean_vars %>%
    map_chr(~get_display_name(.x, id))

  ## Make datatable object
  dfTabel <- dfTabel %>%
    make_basic_table(
    caption = first(df_summmarized[[facet_name_var]]), ...
  ) %>%
    formatPercentage(sBoolean_vars, 2)

  return(dfTabel)

}

### datatable functions ####

## De r code heeft geen toegang tot het data-object uit de Javascript functie.
## Voeg dit daarom toe als optionele variabele
## TODO: minLength instelbaar maken obv breedte bovenliggend element
## Zie comment bij: https://vustudentanalytics.atlassian.net/browse/VUSASOFT-3541

get_header_callback <- function(data = data) {


  c(
    "function(thead, data, start, end, display){",
    "  var ncols = data[0].length;",
    sprintf("  var shortnames = [%s]",
            paste0(paste0(
              "'", abbreviate(names(data), minlength = 7), "'"
            ), collapse = ",")),
    sprintf("  var tooltips = [%s];",
            paste0(paste0(
              "'", names(data), "'"
            ), collapse = ",")),
    "  for(var i=0; i<ncols; i++){",
    "    $('th:eq('+i+')',thead).attr('title', tooltips[i]).text(shortnames[i]);",
    "  }",
    "}"
  )

}


## TODO: lengte instelbaar maken obv breedte bovenliggend element
## Zie comment bij: https://vustudentanalytics.atlassian.net/browse/VUSASOFT-3541
get_value_callback <- function(data) {

  ## Dit kan misschien ook in code bij initComplete

  # table.columns().every(function() {
  #   $(this.header()).css('min-width',
  #                        493.33 / table.columns().nodes().length + 'px')
  #   $(this.header()).css('max-width',
  #                        493.33 / table.columns().nodes().length + 'px')
  # });
  # "var width = table_box.node().offsetWidth;

  # var table = $('#example').DataTable({
  #   initComplete: function () {
  #     // Apply the search
  #     this.api()
  #     .columns()
  #     .every(function () {
  #       var that = this;
  #
  #       $('input', this.footer()).on('keyup change clear', function () {
  #         if (that.search() !== this.value) {
  #           that.search(this.value).draw();
  #         }
  #       });
  #     });
  #   },
  # });
  # "var width = document.getElementById('table_box').offsetWidth",
  # sprintf("var col_width = width/%d;", ncol(data)),
  # "console.log(col_width);",

  c(
    "function(data, type, row, meta) {",
    "return type === 'display' && data.length > 11 ?",
    "'<span title=\"' + data + '\">' + data.substr(0, 9) + '...</span>' : data;",
    "}"
  )
}


add_with_limit_header_JS <- function(options) {
  headerJS <- list(headerCallback = JS(get_header_callback(data)))

  ## Add header code
  options <- c(options, headerJS)

  return(options)
}


## control table width
add_width_limit_values_JS <- function(options, data) {

  valueJS = list(targets = "_all",
                 render = JS(get_value_callback(data))
  )

  ## Extract current columnDefs internal lists (if set)
  ## Add valueJS to it and set new ColumnDefs
  ##' *INFO* Code is a bit complex, but this method ensures it works also when columnDefs aren't set
  new_columns_options <- append(options["columnDefs"] %>% unname() %>% flatten(), list(valueJS))
  new_columns_options <- setNames(list(new_columns_options), 'columnDefs')

  options["columnDefs"] <- new_columns_options

  return(options)

}


## basic options for table
get_basic_options <- function() {
  list(
    language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Dutch.json"),
    pagingType = "full",
    deferRender = TRUE,
    #dom = 'lfrti<"table-button"B>p',
    #dom = "<'table-button'B><lf><t><'row'<'col-sm-4'i><'col-sm-8'p>>",
    dom = "<'row'<'col-sm-6'l><'col-sm-6'f>><'row'<'col-sm-12'tr>><'row'<'col-sm-4'i><'col-sm-8'p>><'table-button paginate_button'B>",
    buttons = c('excel', 'pdf', 'print'),
    #info = TRUE,
    #processing = TRUE,
    columnDefs = list(
      list(targets = "_all",
           className = 'dt-center')
    )
  )
}


## advanced options for table
get_advanced_options <- function() {
  list(
    pagingType = "full",
    colReorder = TRUE,
    rowReorder = TRUE,
    deferRender = TRUE,
    lengthChange = TRUE,
    dom = 'lfrti<"table-button"B>p',
    scrollX = "362px",
    buttons = c('colvis', 'copy', 'csv'),
    info = TRUE,
    processing = TRUE,
    columnDefs = list(
      list(targets = "_all",
           className = 'dt-center')
    )
  )
}


## make advanced table
make_advanced_table <- function(
    data,
    rownames = FALSE,
    filter = "top",
    extensions = c("Buttons", "ColReorder", "RowReorder"),
    options = get_advanced_options(),
    limit_width = TRUE,
    ...) {

  if (limit_width) {
    ## set JS
    options <- add_width_limit_JS(options, data)
  }

  ## Voeg All als optie lengte tabel toe
  if ("lengthMenu" %in% options) {
    ## do nothing
  } else {
    options <- c(options, list(lengthMenu = list(
      c(10, 25, 50, 100, -1),
      c("10", "25", "50", "100", "All"))))

  }

  if ("pageLength" %in% names(options)) {
    # do nothing
  } else {
    if (nrow(data) <= 25) {
      options <- c(options, paging = FALSE)
    }
  }

  datatable(data,
            rownames = rownames,
            extensions = extensions,
            filter = filter,
            options = options,
            ## Escape is always true for security reasons, see documentation
            escape = TRUE,
            ...)

}

## make advanced table
make_basic_table <- function(data,
                             rownames = FALSE,
                             extensions = c("Buttons"),
                             options = get_basic_options(),
                             limit_width = "values",
                             ...) {



  if (limit_width == "both") {
    ## set JS
    options <- add_width_limit_values_JS(options, data)
    options <- add_with_limit_header_JS(options)

  } else if (limit_width == "values") {
    options <- add_width_limit_values_JS(options, data)
  } else if (limit_width == "headers") {
    options <- add_with_limit_header_JS(options)
  }



  ## Add some basic logic
  if ("pageLength" %in% names(options)) {
    # do nothing
  } else {
    if (nrow(data) <= 15) {
      options <- c(options, paging = FALSE)
      options <- c(options, info = FALSE)
    }
  }

  if("searching" %in% names(options)) {
    # do nothing
  } else {
    if(nrow(data) <= 15) {
      options <- c(options, searching = FALSE)
    }
  }

  if (nrow(data) > 15 & !("lengthChange" %in% options)) {
    options <- c(options, list(lengthMenu = list(
      c(10, -1),
      c("10", "All"))))

  } else {
    options <-  c(options, lengthChange = FALSE)
  }



  datatable(data,
            rownames = rownames,
            extensions = extensions,
            options = options,
            ## Escape is always true for security reasons, see documentation
            escape = TRUE,
            style = "bootstrap",
            ...)
}

## make basic table for html rendering
make_basic_table_html <- function(data, ...) {
  make_basic_table(
    data,
    width = "100%",
    height = "auto",
    options = c(get_basic_options(), scrollX = TRUE),
    limit_width = NULL,
    ...
  )
}

## make advanced table for html rendering
make_advanced_table_html <- function(data, ...) {
  make_advanced_table(
    data,
    width = "100%",
    height = "auto",
    options = c(get_basic_options(), scrollX = TRUE),
    limit_width = NULL,
    ...
  )
}

