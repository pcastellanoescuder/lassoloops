
#' Title
#'
#' @description
#'
#' @param object A LassoLoop object.
#' @param scale f
#' @param scale_by f
#' @param low f
#' @param high f
#'
#' @export
#'
#' @return
#' @author Pol Castellano-Escuder
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select group_by
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom tidyr pivot_longer
coeff_heatmap <- function(object,
                          scale = TRUE,
                          scale_by = "model",
                          low = "steelblue",
                          high = "orange"){

  if(!isTRUE(class(object) == "LassoLoop")){
    stop("Input should be a LassoLoop object")
  }

  merged_selected <- as.data.frame(Reduce(function(...) merge(..., by = "name", all = TRUE), object@coefficients))
  merged_selected[is.na(merged_selected)] <- 0
  merged_selected_wo_intercept <- merged_selected %>%
    column_to_rownames("name") %>%
    t() %>%
    as.data.frame() %>%
    dplyr::select(-1) %>%
    rownames_to_column("coeff") %>%
    pivot_longer(cols = -coeff)

  if(isTRUE(scale)){
    if(scale_by == "model"){
      merged_selected_wo_intercept <- merged_selected_wo_intercept %>%
        group_by(coeff) %>%
        mutate(value = scale(value))
    }
    if(scale_by == "variable"){
      merged_selected_wo_intercept <- merged_selected_wo_intercept %>%
        group_by(name) %>%
        mutate(value = scale(value))
    }
  }

  ggplot(merged_selected_wo_intercept, aes(x = name, y = coeff, fill = value)) +
    geom_tile() +
    ylab("") +
    xlab("") +
    scale_fill_gradient(low = low, high = high) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

}

