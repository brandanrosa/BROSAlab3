#' @title ggmytable
#'
#' @param df a data frame
#' @param x a qualitative vector from df
#' @param y a qualitative vector from df
#' @param z a quantitative vector from df
#'
#' @return Returns a bar-plot and summary of the selected vectors from df
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 labs
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{ggmytable(mtbe, "MTBE-Detect", "Aquifier", "MTBE-Level")}
ggmytable <- function(df, x, y, z){# df = dataframe, x/y = cat vars , z = num var
  df1 <- df %>%
    group_by(.data[[y]]) %>%
    summarize(mean = mean(.data[[z]]), n = n())

  gg <- ggplot(data = df, aes(x = .data[[x]])) +
    geom_bar() +
    facet_wrap(~ .data[[y]]) +
    labs(title = "B Rosa", y = "Count")

  list(Summary = df1, Bar_Plot = gg)
}
