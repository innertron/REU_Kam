library(rEDM)

#get the first channel only
Ch1 <- neural_data$X1.Chan..1[1:4000]
head(Ch1)

#set library and prediction indecies
lib <- c(1, 100)
pred <- c(201, 500)

#run and plot the simplex algorithm to get the best embedding dimension
simplex_output <- simplex(Ch1, lib, pred, E=1:32)
par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
     ylab = "Forecast Skill (rho)")
bestE <- which.max(simplex_output$rho)

#run and plot the simplex algorithm to see the accuracy of the forcasts
simplex_output <- simplex(Ch1, lib, pred, E = bestE, tp = 1:10)
plot(simplex_output$tp, simplex_output$rho, type = "l", xlab = "Time to Prediction (tp)", 
     ylab = "Forecast Skill (rho)")




block_lnlp_output <- block_lnlp(nd, lib = lib, pred = pred, columns = c(1, 6),
     target_column = 1, stats_only = FALSE, first_column_time = TRUE)

observed <- block_lnlp_output[[1]]$model_output$obs
predicted <- block_lnlp_output[[1]]$model_output$pred

plot_range <- range(c(observed, predicted), na.rm = TRUE)
plot(observed, predicted, xlim = plot_range, ylim = plot_range, xlab = "Observed", 
     ylab = "Predicted")
abline(a = 0, b = 1, lty = 2, col = "blue")


library(rEDM)

nd <- neural_data[1:1000,]
lib <- c(1, length(nd))
pred <- c(1, length(nd))

Ch1 <- nd$X1.Chan..1
#run and plot the simplex algorithm to get the best embedding dimension
simplex_output <- simplex(Ch1, lib, pred, E=1:32)
par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
     ylab = "Forecast Skill (rho)")
bestE <- which.max(simplex_output$rho)

Ch1_xmap_Ch2 <- ccm(nd, E = bestE, lib_column = X1.Chan..1, 
                        target_column = X2.Chan..2, lib_sizes = seq(10, 80, by = 10), random_libs = FALSE)
sst_xmap_anchovy <- ccm(sardine_anchovy_sst, E = 3, lib_column = "np_sst", target_column = "anchovy", 
                        lib_sizes = seq(10, 80, by = 10), random_libs = FALSE)

