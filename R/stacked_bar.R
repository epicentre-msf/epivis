#' Stacked barplot
#'
#' @param df un-aggregated dataframe (linelist).
#' @param cols vector of character/factor variables names in `df` to be displayed in the barplot.
#' @param levels_value vector of level values to be used for the plotting.
#' @param keep_na logical, default = `TRUE.` Keep NAs in the graphs and the proportions ?
#' @param use_counts logical, default = `TRUE.` Use counts or proportion in y axis ?
#' @param flip logical, default = `FALSE`. Flip the barplot ?
#' @param x_lab character name for the x axis
#' @param caption logical, default = `TRUE.` Display the plot caption summarising the number of cases ?
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#'
#' #Use fake data from Epidemiologist R handbook
#'
#' suppressMessages(c(library(dplyr), library(readr)))
#' linelist <- read_rds("https://github.com/appliedepi/epirhandbook/raw/master/inst/extdata/case_linelists/linelist_cleaned.rds")
#'
#' plot_stacked_bar(
#'   linelist,
#'   cols = c( "fever", "vomit", "chills", "cough", "aches"),
#'   levels_value = c("yes"),
#'   keep_na = FALSE,
#'   use_counts = TRUE,
#'   flip = FALSE
#' )
#'
#' @import ggplot2
#' @export

plot_stacked_bar <- function(df,
                             cols,
                             levels_value,
                             keep_na = TRUE,
                             use_counts = TRUE,
                             flip = FALSE,
                             x_lab = waiver(),
                             caption = TRUE){

  if (missing(cols)) {

    stop("Please supply colums to use in the barplot") }

  if(missing(levels_value)){

    stop("Please specify the levels to use in the data")

  }

  # are columns character or factor ?
  char_vec <- purrr::map(cols, ~ is.character(df[[.x]]) | is.factor(df[[.x]])) %>% purrr::list_c()

  #rename vector
  names(char_vec) <- cols

  #check columns class and retrun warning or stop
  if(all(char_vec == FALSE)) {

    stop("cols are not character or factor")

  } else if( length(char_vec[char_vec == FALSE]) ) {

    rm_names <- names(char_vec[char_vec == FALSE])

    warning(paste0("cols: ", rm_names, " is not character or factor. It is not used in barplot"))

    df <- df %>% dplyr::select(-all_of(rm_names))

    cols <- cols[cols != rm_names]
  }

  #data prep
  df_long <- df %>%

    tidyr::pivot_longer(cols = all_of(cols),
                 names_to = "group",
                 values_to = "level") %>%

    dplyr::mutate( level = factor(level)

    )

  if(keep_na == FALSE) {

    df_long <- df_long %>% dplyr::filter(!is.na(level)) }


  df_long <- df_long %>%

    dplyr::group_by(group, level) %>%

    dplyr::summarise(n = n(),
              .groups = "drop"
    ) %>%

    dplyr::group_by(group) %>%

    dplyr::mutate(total = sum(n),
           percent = round(digits = 3, n/total),
           group = forcats::fct_reorder(group, percent)
    ) %>%

    dplyr::ungroup()

  #ordering vector
  order_vec <- as.character(dplyr::arrange(dplyr::filter(df_long, level == levels_value[1]), -percent)$group)

  # filter using the levels
  df_final <- df_long %>% dplyr::filter(level %in% levels_value)

  if(nrow(df_final) == 0) {
    stop("Make sure levels are present in columns")
  }

  #plot
  gg <- df_final %>%

    mutate(group = forcats::fct_relevel(group, order_vec)) %>%

    ggplot() +

    geom_col(aes(x = group,
                 y = if(use_counts) {n} else { percent },
                 fill = level)) +

    labs(x = x_lab,
         y = if(use_counts) {"Counts"} else { "Percentages"})

  if(use_counts == FALSE){

    gg <- gg + scale_y_continuous(labels = scales::percent_format() )
  }

  if(flip){

    gg <- gg + coord_flip()
  }

  if(caption){

    gg <- gg +

      labs(caption = glue::glue("Based on {nrow(df)} cases"))
  }

  return(gg)

}
