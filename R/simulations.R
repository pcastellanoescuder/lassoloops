library(tidyverse)

# cox
x <- survival::cancer[,4:10]
x <- data.frame(apply(x, 2, function(y)ifelse(is.na(y), median(y, na.rm = T), y)))
y <- survival::cancer[,2:3] %>% mutate(status = ifelse(status == 2,1,0))

n <- nrow(y)
new_matrix <- cbind(y, x)
idx_test <- sample(1:(n/3), replace = FALSE)

test <- new_matrix[idx_test ,]
test_x <- test[,-c(1:2)]
test_y <- test[,1:2]

## TRAIN

train <- new_matrix[-idx_test ,]
train_x <- train[,-c(1:2)]
train_y <- train[,1:2]

###########################
###########################
###########################

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


## SIMULATION

# raw cv.glmnet
# test_list <- list()
# for (i in 1:20){
#   model <- cv.glmnet(as.matrix(train_x), as.matrix(train_y), family = "cox")
#   lasso_pred <- predict(model, s = model$lambda.min, newx = as.matrix(test_x), type = "response") # hazards
#   test_list[i] <- survcomp::concordance.index(lasso_pred, surv.time = test_y[,1], surv.event = test_y[,2], method = "noether")$c.index
# }
#
# # without bootstrap
# test_list2 <- list()
# for (i in 1:20){
#   cox_mod <- cox_blasso(train_x, train_y, loops = 100, bootstrap = FALSE, ncores = 4)
#   model <- pick_best(cox_mod)$model
#   lasso_pred <- predict(model, s = model$lambda.min, newx = as.matrix(test_x), type = "response") # hazards
#   test_list2[i] <- survcomp::concordance.index(lasso_pred, surv.time = test_y[,1], surv.event = test_y[,2], method = "noether")$c.index
# }
#
# # bootstrap and internal train/test
# test_list3 <- list()
# for (i in 1:20){
#   cox_mod <- cox_blasso(train_x, train_y, loops = 100, bootstrap = TRUE, ncores = 4)
#   model <- pick_best(cox_mod)$model
#   lasso_pred <- predict(model, s = model$lambda.min, newx = as.matrix(test_x), type = "response") # hazards
#   test_list3[i] <- survcomp::concordance.index(lasso_pred, surv.time = test_y[,1], surv.event = test_y[,2], method = "noether")$c.index
# }
#
# ##
#
# results <- data.frame(raw_glmnet = unlist(test_list),
#                       wo_bootstrap = unlist(test_list2),
#                       bootstrap = unlist(test_list3))
#
# save(results, file = "2_simulation_lassoloops.Rdata")
#
# ##
#
# melt_res <- reshape2::melt(results)
#
# ggplot(melt_res) +
#   geom_boxplot(aes(variable, value, fill = variable)) +
#   geom_hline(yintercept = max(results), linetype = "dashed") +
#   geom_jitter(aes(variable, value, fill = variable)) +
#   theme_bw()

