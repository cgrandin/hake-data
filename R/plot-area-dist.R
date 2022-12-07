#' Plot the catch by area for data d where each area will have a histogram of all
#' years found in the data for that area. Areas are coded in the areas.dat
#'
#' @param d A tibble holding the spatial data, as output by
#' [load_spatial_catch_data()]
#' @param bymonth If `TRUE`, the plot will be a stacked barplot with
#' month stacked for the year given by `bymonth_year`
#' @param bymonth_year The year to use when making a stacked barplot by month.
#' Only used if `bymonth` is `TRUE`
#' @param fn The filename for the output figure. If `NULL`, no file will be
#' created
#' @param lu A lookup table which is a data frame of the areas with the
#' columns `code` <character>, `short_name` <character>, and
#' `desc` <character>
#' @param rem_areas A vector of areas to remove. Numerical values representing
#' the `code` column in `lu` are required
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
plot_area_dist <- function(d,
                           bymonth = FALSE,
                           bymonth_year = year(now()),
                           fn = "default.png",
                           area_lu = areas_lu,
                           month_lu = months_lu,
                           rem_areas = NULL,
                           title = NULL,
                           inc_years = (year(now()) - 4):year(now()),
                           scale_factor = 1000,
                           view_legend = FALSE,
                           angle_x_labels = FALSE,
                           show_x_axis_labels = TRUE,
                           ylim = NULL,
                           x_axis_font_size = 4,
                           y_axis_font_size = 4,
                           y_label_font_size = 6,
                           title_font_size = 6){

  if(bymonth){
    d <- d |>
      filter(years == bymonth_year)
  }else{
    if(!is.null(inc_years)){
      d <- d |>
        filter(years %in% inc_years)
    }
  }
  unq_months <- as.character(rev(sort(unique(d$months))))

  if(!is.null(rem_areas)){
    d <- d |>
      filter(!PFMC %in% rem_areas)
  }

  d <- d |>
    mutate(catch = catch / scale_factor)

  area_lu <- area_lu |>
    mutate(code = as.numeric(code))

  # Get all unique included areas
  unq_areas <- sort(unique(d$PFMC))
  unq_areas_nms <- area_lu |>
    filter(code %in% unq_areas) |>
    pull(short_name)

  names(area_lu) <- c("area", "short_name", "desc")

  tab <- d |>
    select(years, months, PFMC, catch) |>
    dplyr::rename(area = PFMC) |>
    left_join(area_lu, by = "area") |>
    select(-area, -short_name) |>
    dplyr::rename(area = desc) |>
    mutate(years = as.character(years),
           months = as.factor(months)) |>
    mutate(months = forcats::fct_relevel(months, unq_months))

  if(bymonth){
    tab <- tab |>
      select(-years) |>
      split(~months) |>
      map(~{
        .x |> split(~area) |>
          map(~{
            summarize(.x,
                      months = first(months),
                      catch = sum(catch),
                      area = first(area))
          }) |>
          map_df(~{.x})
      }) |>
      map_df(~{.x})

    num_months <- length(unq_months)

    g <- ggplot(tab, aes(x = area, y = catch, fill = months)) +
      geom_bar(position = "stack", stat = "summary", fun = sum, show.legend = view_legend) +
      scale_fill_manual(values = plotcolour(num_months))

  }else{
    num_yrs <- length(unique(tab$years))

    g <- ggplot(tab, aes(x = area, y = catch, fill = years)) +
      geom_bar(position = "dodge", stat = "summary", fun = sum, show.legend = view_legend) +
      scale_fill_manual(values = plotcolour(num_yrs))

    num_areas <- length(unq_areas)
    g <- g +
      geom_vline(xintercept = seq(1.5, 1.5 * (num_areas - 2), by = 1),
                 linetype = "dashed", color = "grey50")
  }

  g <- g +
    scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
    #xlab("Area") +
    ylab("Catch (tonnes)") +
    guides(fill = guide_legend(title = ifelse(bymonth, "Month", "Year")))

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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

  g <- g +
    theme(axis.title.x = element_blank())

  g
}

