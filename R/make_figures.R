

make_figure_1 <- function() {
  knitr::include_graphics(here::here("data/img/fig_survey_screenshot.png"))
}




make_figure_2 <- function(height = NULL) {
  duration_lvls = c(translate(fig_labels$`2 years`$en, fig_labels),
                    translate(fig_labels$`3 years`$en, fig_labels),
                    translate(fig_labels$`4 years`$en, fig_labels))
  
  # setting the order of the different locations and durations of the schemes
  location_lvls = c(translate(fig_labels$`Switzerland only`$en, fig_labels),
                    translate(fig_labels$`Abroad only`$en, fig_labels),
                    translate(fig_labels$`Mixed`$en, fig_labels))
  
  dt_plot <- dt_choice_by_scheme %>%
    # adjust and order location and duration labels for plotting
    mutate(
      choice_pct_lbl = paste0(
        round(choice_pct),
        switch(params$lang, en = "%", de = "%", fr = " %")
      ),
      duration_yr_lbl = paste0(duration/12," ", translate(fig_labels$years$en, fig_labels)),
      location = recode(location,
                        "Switzerland" = translate(fig_labels$`Switzerland only`$en, fig_labels),
                        "Abroad" =  translate(fig_labels$`Abroad only`$en, fig_labels),
                        "Mixed" = translate(fig_labels$`Mixed`$en, fig_labels)),
      # setting categ. levels as integers for building the heatmap
      duration_yr_lbl_int = as.integer(factor(duration_yr_lbl,
                                              levels = duration_lvls)),
      location_int = as.integer(factor(location,
                                       levels = location_lvls)))
  
  
  dt_plot %>%
    ggplot() +
    # set the size of the rectangles
    ggchicklet::geom_rrect(
      aes(xmin = location_int - 0.45,
          xmax = location_int + 0.45,
          ymin = duration_yr_lbl_int - 0.4,
          ymax = duration_yr_lbl_int + 0.4,
          # fill intensity by choice pct
          fill = choice_pct),
      radius = grid::unit(5, "pt"),
      colour = "grey20"
    ) +
    # add label with the choice pct value
    geom_text(aes(location_int,
                  duration_yr_lbl_int,
                  label = choice_pct_lbl),
              size = 7,
              color = if_else(dt_plot$choice_pct > 60,
                              "white",
                              "grey10"),
              fontface = "bold") +
    # properly label the x axis with the location of the scheme
    scale_x_continuous(breaks = seq_along(location_lvls),
                       position = "top",
                       labels = c(
                         translate(fig_labels$`<b>Switzerland only</b>`$en, fig_labels),
                         translate(fig_labels$`<b>Abroad only</b>`$en, fig_labels),
                         paste0(
                           "<b>", translate(fig_labels$Mixed$en, fig_labels), "</b><br>",
                           "<span style='font-weight:400;font-size:8pt;'>",
                           translate(fig_labels$`(Swiss base + optional abroad)`$en, fig_labels),
                           "</span>"
                         ))) +
    # properly label the y axis with the duration of the scheme
    scale_y_continuous(breaks = seq_along(duration_lvls),
                       labels = duration_lvls) +
    scale_fill_gradient2(
      low = "#F2FAFE", 
      mid = "#83D0F5", 
      high = "#04293C", 
      midpoint = 50  # Change this value to shift the center of the gradient
    ) +
    get_datastory_theme(legend_position = "none") +
    # adjust formating
    theme(axis.title.x = element_blank(),
          axis.text.y = element_text(face = "bold",
                                     size = 10.5,
                                     color = "black"),
          axis.text.x.top = ggtext::element_markdown(colour = "black",
                                                     size = 10.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_blank())
  
  
  
}


make_figure_3 <- function() {
  
  knitr::include_graphics(
    here::here(
      switch(
        params$lang,
        en = "data/img/fig_comments_en.png",
        de = "data/img/fig_comments_de.png",
        fr = "data/img/fig_comments_fr.png"
      )
    )
  )
  
}




make_figure_4 <- function() {
  
  knitr::include_graphics(
    here::here(
      switch(
        params$lang,
        en = "data/img/fig_scheme_en.png",
        de = "data/img/fig_scheme_de.png",
        fr = "data/img/fig_scheme_fr.png"
      )
    )
  )
  
}


