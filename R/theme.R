
#' Epurated Ggplot theme (inspired from hrbrthemes)
#'
#' @param base_family base font family
#' @param base_size base font size
#' @param plot_title_family,plot_title_size,plot_title_face,plot_title_margin plot title family, face, size and margins
#' @param subtitle_family,subtitle_size,subtitle_face,subtitle_margin plot subtitle family, face, size and margins
#' @param strip_text_family,strip_text_size,strip_text_face Facet label family, face, size
#' @param caption_family,caption_size,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_text_size font size of axis text
#' @param axis_title_family,axis_title_size,axis_title_face axis title font family, face and size
#' @param axis_title_just axis title font justification, one of ⁠[blmcrt]⁠
#' @param plot_margin plot margin (specify with ggplot2::margin())
#' @param grid_col,axis_col grid & axis colors; both default to ⁠#cccccc⁠
#' @param grid panel grid (TRUE, FALSE, or a combination of X, x, Y, y)
#' @param axis add x or y axes? TRUE, FALSE, "xy"
#' @param ticks ticks if TRUE add ticks
#'
#' @return
#' @export
#'
#' @examples
#'
#' ## Not run:
#' library(ggplot2)
#' library(dplyr)
#'
#' library(ggplot2)
#' ggplot(data = iris) +
#'   geom_histogram(aes(x = Sepal.Length)) +
#'
#'   epi_ggtheme()
#'

epi_ggtheme <- function (base_family = "sans",
                         base_size = 10,
                         plot_title_family = base_family,
                         plot_title_size = 10,
                         plot_title_face = "bold",
                         plot_title_margin = 10,
                         subtitle_family = base_family,
                         subtitle_size = 9,
                         subtitle_face = "italic",
                         subtitle_margin = 15,
                         strip_text_family = base_family,
                         strip_text_size = 6,
                         strip_text_face = "plain",
                         caption_family = base_family,
                         caption_size = 9,
                         caption_face = "italic",
                         caption_margin = 10,
                         axis_text_size = base_size,
                         axis_title_family = subtitle_family,
                         axis_title_size = 10,
                         axis_title_face = "plain",
                         axis_title_just = "rt",
                         plot_margin = margin(10, 10, 10, 10),
                         grid_col = "#cccccc",
                         grid = TRUE,
                         axis_col = "#cccccc",
                         axis = FALSE,
                         ticks = FALSE) {

  ret <- ggplot2::theme_minimal(base_family = base_family,
                                base_size = base_size)+

    theme(legend.background = element_blank(),
          legend.key = element_blank()
    )

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + theme(panel.grid = element_line(color = grid_col,
                                                 linewidth = 0.2),
                       panel.grid.major = element_line(color = grid_col,
                                                       linewidth = 0.2),
                       panel.grid.minor = element_line(color = grid_col,
                                                       linewidth = 0.15)
    )

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0)
        ret <- ret + theme(panel.grid.major.x = element_blank())
      if (regexpr("Y", grid)[1] < 0)
        ret <- ret + theme(panel.grid.major.y = element_blank())
      if (regexpr("x", grid)[1] < 0)
        ret <- ret + theme(panel.grid.minor.x = element_blank())
      if (regexpr("y", grid)[1] < 0)
        ret <- ret + theme(panel.grid.minor.y = element_blank())
    }
  }
  else {
    ret <- ret + theme(panel.grid = element_blank())
  }
  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line = element_line(color = "#2b2b2b",
                                                size = 0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x = element_blank())
      }
      else {
        ret <- ret + theme(axis.line.x = element_line(color = axis_col,
                                                      linewidth = 0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y = element_blank())
      }
      else {
        ret <- ret + theme(axis.line.y = element_line(color = axis_col,
                                                      linewidth = 0.15))
      }
    }
    else {
      ret <- ret + theme(axis.line.x = element_line(color = axis_col,
                                                    linewidth = 0.15))

      ret <- ret + theme(axis.line.y = element_line(color = axis_col,
                                                    linewidth = 0.15))
    }
  }
  else {
    ret <- ret + theme(axis.line = element_blank())
  }
  if (!ticks) {

    ret <- ret + theme(axis.ticks = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank())
  }
  else {
    ret <- ret + theme(axis.ticks = element_line(size = 0.15),
                       axis.ticks.x = element_line(size = 0.15),
                       axis.ticks.y = element_line(size = 0.15),
                       axis.ticks.length = grid::unit(5, "pt")
    )
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0,
               l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0,
               l = 0, m = 0.5, c = 0.5, r = 1, t = 1)

  ret <- ret + theme(axis.text.x = element_text(size = axis_text_size,
                                                margin = margin(t = 0)),

                     axis.text.y = element_text(size = axis_text_size,
                                                margin = margin(r = 0)),

                     axis.title = element_text(size = axis_title_size,
                                               family = axis_title_family),

                     axis.title.x = element_text(hjust = xj,
                                                 size = axis_title_size, family = axis_title_family, face = axis_title_face),

                     axis.title.y = element_text(hjust = yj,
                                                 size = axis_title_size, family = axis_title_family, face = axis_title_face),

                     axis.title.y.right = element_text(hjust = yj,
                                                       size = axis_title_size, angle = 90, family = axis_title_family,
                                                       face = axis_title_face),

                     strip.text = element_text(hjust = 0, size = strip_text_size,
                                               face = strip_text_face, family = strip_text_family),

                     panel.spacing = grid::unit(2, "lines"),

                     plot.title = element_text(hjust = 0, size = plot_title_size,
                                               margin = margin(b = plot_title_margin), family = plot_title_family,
                                               face = plot_title_face),

                     plot.subtitle = element_text(hjust = 0,
                                                  size = subtitle_size, margin = margin(b = subtitle_margin),
                                                  family = subtitle_family, face = subtitle_face),

                     plot.caption = element_text(hjust = 1,
                                                 size = caption_size, margin = margin(t = caption_margin),
                                                 family = caption_family, face = caption_face),

                     plot.margin = plot_margin)
}
