#' Plot the catch by area for data d where each area will have a histogram of all
#' years found in the data for that area. Areas are coded in the areas.dat
#'
#' @param d A tibble holding the spatial data, as output by
#' [load_spatial_catch_data()]
#' @param area_lu A lookup table containing the area numbers and names
#' @param rem_areas A vector of areas to remove. Numerical values representing
#' the `code` column in `area_lu` are required
#' @param title Plot title
#' @param inc_years Years to include in the plot. If `NULL`, all years will
#' be included
#' @param scale_factpr The value to divide catches by to get to thousands
#' of tonnes
#'
#' @return
#' @export
#'
#' @examples
plot_area_dist_catch <- function(d,
                                 area_lu = NULL,
                                 rem_areas = NULL,
                                 title = NULL,
                                 inc_years = (year(now()) - 4):year(now()),
                                 scale_factor = 1e6,
                                 view_legend = FALSE,
                                 angle_x_labels = 20,
                                 show_x_axis_labels = TRUE,
                                 ylim = NULL,
                                 x_axis_font_size = 5,
                                 y_axis_font_size = 6,
                                 y_label_font_size = 6,
                                 title_font_size = 6){

  d <- d |>
    rename(areas = PFMC)

  if(!is.null(inc_years)){
    d <- d |>
      filter(years %in% inc_years)
  }

  if(!is.null(rem_areas)){
    d <- d |>
      filter(!areas %in% rem_areas)
  }

  d <- d |>
    mutate(catch = catch / scale_factor)

  # area_lu <- area_lu |>
  #   mutate(code = as.numeric(code))

  if(is.null(area_lu)){
    stop("`area_lu` is `NULL` but is needed", call. =  FALSE)
  }

  # Get all unique included areas
  unq_areas <- unique(d$areas)

  tab <- d |>
    select(years, months, areas, catch) |>
    mutate(years = factor(years)) |>
    mutate(areas = factor(areas)) |>
    arrange(areas) |>
    mutate(areas = recode(areas, !!!area_lu))


  num_yrs <- length(unique(tab$years))

  g <- ggplot(tab, aes(x = areas, y = catch, fill = years)) +
    geom_bar(position = "dodge", stat = "summary", fun = sum, show.legend = view_legend) +
    scale_fill_manual(values = plotcolour(num_yrs))

  num_areas <- length(unq_areas)
  g <- g +
    geom_vline(xintercept = seq(1.5, 1.5 * (num_areas - 2), by = 1),
               linetype = "dashed", color = "grey50")

  g <- g +
    scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
    #xlab("Area") +
    ylab("Catch (thousand tonnes)")

  if(!is.null(angle_x_labels)){
    g <- g +
      theme(axis.text.x = element_text(angle = angle_x_labels, hjust = 1))
  }

  if(!show_x_axis_labels){
    g <- g +
      theme(axis.text.x = element_blank())
  }

  if(!is.null(title)){
    g <- g +
      ggtitle(title)
  }

  if(!is.null(ylim)){
    g <- g +
      coord_cartesian(ylim = ylim)
  }

  if(!is.null(x_axis_font_size)){
    g <- g +
      theme(axis.text.x = element_text(size = x_axis_font_size))
  }

  if(!is.null(y_axis_font_size)){
    g <- g +
      theme(axis.text.y = element_text(size = y_axis_font_size))
  }

  if(!is.null(y_label_font_size)){
    g <- g +
      theme(axis.title.y = element_text(size = y_label_font_size))
  }

  if(!is.null(title_font_size)){
    g <- g +
      theme(plot.title = element_text(size = title_font_size))
  }

  if(view_legend){
    g <- g +
      guides(fill = guide_legend(ncol = 2,
                                 title = "Year"))
  }

  g <- g +
    theme(axis.title.x = element_blank())

  g
}

