
#' Summary of All Models
#'
#' @description This function summarize the information of all models computed and creates some basic plots and tables to explore them.
#'
#' @param object A LassoLoop object.
#'
#' @export
#'
#' @return A list with different summary plots and tables.
#' @author Pol Castellano-Escuder
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom purrr map keep
#' @importFrom dplyr mutate filter arrange desc rename bind_rows
#' @importFrom clisymbols symbol
#' @importFrom crayon blue red green
summary_models <- function(object){

  if(!isTRUE(class(object) == "LassoLoop")){
    stop("Input should be a LassoLoop object")
  }

  ## LENGTH
  num_models <- object@length

  ## NON-ZERO MODELS
  nonzeromodels <- purrr::map(object@coefficients, ~ nrow(.)) %>%
    purrr::keep(. > 1) %>%
    length()

  ## COMMON COEFFICIENTS
  common_coeffs <- bind_rows(object@coefficients) %>%
    filter(duplicated(name)) %>%
    filter(name != "(Intercept)") %>%
    filter(!duplicated(name))

  ## FREQ SELECTED
  freq_coeff <- purrr::map(object@coefficients, 1) %>%
    unlist() %>%
    table() %>%
    as.data.frame() %>%
    rename(feature = 1) %>%
    mutate(feature = as.factor(as.character(feature)),
           Freq = Freq/num_models * 100) %>%
    filter(feature != "(Intercept)") %>%
    arrange(desc(Freq))

  ## PLOT FREQ SELECTED
  freq_plot <- ggplot(freq_coeff, aes(x = reorder(feature, Freq), y = Freq, fill = Freq)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    theme_bw() +
    ylab("Frequency (%)") +
    xlab("") +
    theme(legend.position = "none") +
    scale_fill_continuous(type = "viridis")

  ## COEFF/LOOP
  coeffByloop <- purrr::map(object@coefficients, ~ nrow(.)) %>%
    unlist() %>%
    as.data.frame() %>%
    rename(counts = 1)

  ## PLOT COEFF/LOOP
  count_plot <- ggplot(coeffByloop, aes(reorder(rownames(coeffByloop), counts), counts, fill = counts)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    xlab("Loop Number") +
    ylab("Number of selected variables") +
    theme_bw() +
    theme(legend.position = "none") +
    scale_fill_continuous(type = "viridis")

  ## DENSITY PLOT COEFF/LOOP
  density_count_plot <- ggplot(coeffByloop, aes(counts)) +
    geom_density() +
    ylab("Density") +
    xlab("Number of selected variables") +
    theme_bw() +
    geom_vline(xintercept = mean(coeffByloop$counts), color = "blue") +
    geom_vline(xintercept = median(coeffByloop$counts), linetype = "dashed", color = "orange") +
    geom_text(aes(x = mean(counts), label = paste0("\nmean = ", round(mean(counts),2)), y = quantile(density(counts)$y, 0.5)),
              colour = "blue", angle = 90, check_overlap = T) +
    geom_text(aes(x = median(counts), label = paste0("\nmedian = ", median(counts)), y = quantile(density(counts)$y, 0.5)),
              colour = "orange", angle = 90, check_overlap = T)

  ## SUMMARY MESSAGE
  n_models <- crayon::blue(clisymbols::symbol$bullet, "A total of", num_models, "models have been computed.")
  no_zero <- crayon::blue(clisymbols::symbol$bullet, nonzeromodels, "out of them with no null coefficients (", (nonzeromodels/num_models)*100, "%).")

  if(nrow(common_coeffs) != 0){
    common <- crayon::green(clisymbols::symbol$tick, nrow(common_coeffs), "common selected variables among all models!")
  } else{
    common <- crayon::red(clisymbols::symbol$cross, "No common selected variables among all models...")
  }

  ##

  message(cat(n_models, no_zero, common, sep = "\n"))

  return(list(num_models = num_models, nonzeromodels = nonzeromodels, common_coeffs = common_coeffs,
              freq_coeff = freq_coeff, freq_plot = freq_plot, coeffByloop = coeffByloop, count_plot = count_plot,
              density_count_plot = density_count_plot))

}

