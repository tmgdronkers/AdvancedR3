#' Descriptive statistics table
#'
#' @param data
#'
#' @return "a data.frame/tibble"
descriptive_stats <- function(data) {
    data %>%
        dplyr::group_by(metabolite) %>%
        dplyr::summarise(across(
            value,
            list(
                mean = mean,
                sd = sd
            )
        )) %>%
        dplyr::mutate(dplyr::across( # given these columns, do this action
            where(is.numeric),
            ~ round(.x, digits = 1) # the ~    ".x" refers to each column
        ))
}
