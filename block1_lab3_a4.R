library(neuralnet)


get_data <- function(max_value, train_set_count) {
  set.seed(1234567890)
  Var   <- runif(500, 0, max_value)
  Var <- seq(0, max_value, length.out = 500)
  mydata <- data.frame(Var, Sin = sin(Var))
  idx <- sample(1:nrow(mydata))      # shuffle indices
  mydata <- mydata[idx, ]
  tr <- mydata[1:train_set_count, ] 
  te <- mydata[(train_set_count+1):500, ] 
  
  list(tr = tr, te = te)
}

train_network <- function(tr) {
  # Random initialization of the weights in the interval [-1, 1]
  winit <- runif(21, min = -1, max = 1)
  
  # Train neural network: 1 hidden layer with 10 units, regression output
  nn <- neuralnet(
    Sin ~ Var,
    data          = tr,
    hidden        = 10,
    startweights  = winit,
    linear.output = TRUE,
    # threshold = 0.01
  )
  return(nn)
}

mydata <- get_data(max_value=100,train_set_count=200)
tr <- mydata$tr
te <- mydata$te
nn <- train_network(tr)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex = 2)                                  # training data
points(te, col = "blue", cex = 1)                  # test data
points(te[, 1], predict(nn, te), col = "red", cex = 1)  # NN predictions


# New 500 points on [0,50] and true sine
Var_new  <- runif(500, 0, 50)
newdata  <- data.frame(Var = Var_new, Sin = sin(Var_new))

# Predict sine values with the previously trained NN
pred_new <- predict(nn, newdata)   # returns a 500x1 matrix for one output

y_all <- c(newdata$Sin, pred_new)

# Plot: true sine (black) and NN predictions (red)
plot(newdata$Var, newdata$Sin,
     xlab = "x", ylab = "sin(x)",
     main = "NN approximation of sin(x) on [0, 50]",
     ylim = range(y_all))
points(newdata$Var, pred_new,
       col = "red", pch = 16, cex = 0.7)

legend("topright",
       legend = c("True sin(x)", "NN prediction"),
       col    = c("black", "red"),
       pch    = c(1, 16),
       bty    = "n")

x_big  <- seq(1, 50, length.out = 200)
dat_big <- data.frame(Var = x_big)
# collapsing to a value around -2.785
W1 <- nn$weights[[1]][[1]]
W2 <- nn$weights[[1]][[2]]
sigmoid <- function(z) plogis(z)  # logistic activation

# Hidden pre-activations and activations for x > 15
z_hidden <- outer(x_big, W1[1, ], `*`) + matrix(W1[2, ], nrow = length(x_big), ncol = 10, byrow = TRUE)
h_hidden <- sigmoid(z_hidden)

matplot(x_big, h_hidden, type = "l", lty = 1,
        xlab = "x", ylab = "hidden activations",
        main = "Hidden units for x in [1, 50]")
