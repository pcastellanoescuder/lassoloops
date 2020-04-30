library(tidyverse)

# cox
x <- survival::cancer[,4:10]
x <- data.frame(apply(x, 2, function(y)ifelse(is.na(y), median(y, na.rm = T), y)))
y <- survival::cancer[,2:3] %>% mutate(status = ifelse(status == 2,1,0))

# cox_mod <- cox_blasso(x, y, loops = 10)
# my_summary <- summary_models(cox_mod)
# coeff_heatmap(cox_mod)
#
# # binom
# x <- MASS::Boston[,c(1:3,5:14)]
# y <- MASS::Boston[, 4]
# y <- as.factor(ifelse(y == 1, "yes", "no"))
#
# binom_mod <- binom_blasso(x, y, loops = 10)
# my_summary <- summary_models(binom_mod)
# coeff_heatmap(binom_mod)
#
# # gaussian
# x <- MASS::Boston[,c(2:14)]
# y <- MASS::Boston[, 1]
#
# gauss_mod <- blasso(x, y, loops = 10)
# my_summary <- summary_models(gauss_mod)
# coeff_heatmap(gauss_mod)


## TEST

# test_list <- list()
# for (i in 1:20){
#   cox_mod <- cox_blasso(x, y, loops = 100, bootstrap = FALSE, ncores = 4)
#   test_list[i] <- pick_best(cox_mod)$cindex[[1]]
# }
#
# test_list2 <- list()
# for (i in 1:20){
#   cox_mod <- cox_blasso(x, y, loops = 100, bootstrap = TRUE, ncores = 4)
#   test_list2[i] <- pick_best(cox_mod)$cindex[[1]]
# }
#
# results <- data.frame(wo_bootstrap = unlist(test_list),
#                       bootstrap = unlist(test_list2))
#
# t.test(results$wo_bootstrap, results$bootstrap)
#
# ggplot(reshape2::melt(results)) +
#   geom_boxplot(aes(variable, value, fill = variable)) +
#   theme_bw()

