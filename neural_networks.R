# Load the required library
library(neuralnet)

# Set the seed for reproducibility
set.seed(1234567890)

# Generate 500 random points in the interval [0, 10]
Var <- runif(500, 0, 10)

# Apply the sine function to each point
mydata <- data.frame(Var, Sin=sin(Var))

# Split the data into training and test sets
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test

# Random initialization of the weights in the interval [-1, 1]
winit <- runif(31, min = -1, max = 1)

# Train the neural network
nn <- neuralnet(Sin ~ Var, tr, hidden = 10, startweights = winit)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1], predict(nn, te), col="red", cex=1)

# Predicted values from the neural network
nn_pred <- predict(nn, newdata = te)

# Actual values from the test set
actual_values <- te$Sin

# Calculate the mean squared error (MSE)
mse <- mean((nn_pred - actual_values)^2)

# Display the result
cat("Mean Squared Error (MSE):", mse)

# Define the custom activation functions
h1 <- function(x) {x}
h2 <- function(x) {ifelse(x > 0, x, 0)}
h3 <- function(x) {log(1 + exp(x))}


# Train the neural network with the custom activation functions
nn_h1 <- neuralnet(Sin ~ Var, tr, hidden = 10, startweights = winit, act.fct = h1)
nn_h2 <- neuralnet(Sin ~ Var, tr, hidden = 10, startweights = winit, act.fct = h2)
nn_h3 <- neuralnet(Sin ~ Var, tr, hidden = 10, startweights = winit, act.fct = h3)

# Plot the predictions for each activation function
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1], predict(nn_h1, te), col="red", cex=1)
points(te[,1], predict(nn_h2, te), col="green", cex=1)
points(te[,1], predict(nn_h3, te), col="purple", cex=1)


# Generate 500 random points in the interval [0, 50]
Var_50 <- runif(500, 0, 50)

# Apply the sine function to each point
mydata_50 <- data.frame(Var=Var_50, Sin=sin(Var_50))


# Plot new 500 random data points (green), and predictions (red)
plot(Var, predict(nn, mydata_50), col = "red", pch = 16, cex = 0.5, main = "Predictions using NN from Q1")
points(mydata_50, col = "green", cex=0.5)


# Generate 500 random points in the interval [0, 10]
Var_new <- runif(500, 0, 10)

# Apply the sine function to each point
mydata_new <- data.frame(Var=Var_new, Sin=sin(Var_new))

# Train the neural network that tries to predict x from sin(x)
nn_reverse <- neuralnet(Var ~ Sin, mydata_new, hidden = 10, startweights = winit, threshold = 0.1)


# Plot of the training data (black), predictions (red)
plot(mydata_new$Sin, mydata_new$Var, cex=2)
points(mydata_new$Sin, predict(nn_reverse, newdata = mydata_new), col = "red", cex=1)




