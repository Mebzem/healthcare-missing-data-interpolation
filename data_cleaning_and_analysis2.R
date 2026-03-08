
setwd("C:/Users/zemen/OneDrive/Documents/MATH470/Diabetes_Interpolation_Project2B")


getwd()

library(ggplot2)
library(dplyr)


diabetes <- read.csv("diabetes.csv")


head(diabetes)


summary(diabetes)


sapply(diabetes, function(x) sum(is.na(x)))


diabetes[diabetes == 0] <- NA


sapply(diabetes, function(x) sum(is.na(x)))

save.image(file = "diabetes_workspace2.RData")

# Clear existing variables and reset environment
rm(list = ls())
cat("\014")  # Clear console
gc()  # Trigger garbage collection

install.packages('tidyverse')


library(tidyverse)  # For data manipulation and visualization


setwd("C:/Users/zemen/OneDrive/Documents/MATH470/Diabetes_Interpolation_Project2B")


diabetes <- read.csv("diabetes.csv")  

# Quick look at the structure and summary of the dataset
str(diabetes)
summary(diabetes)


# Check for missing values in each column
missing_values <- sapply(diabetes, function(x) sum(is.na(x)))
print(missing_values)

# Count missing values in each column
missing_values <- sapply(diabetes, function(x) sum(is.na(x)))
print("Missing values in each column:")
print(missing_values)


# Replace 0s in BloodPressure with NA
diabetes$BloodPressure[diabetes$BloodPressure == 0] <- NA


print(paste("Number of missing values in BloodPressure:", sum(is.na(diabetes$BloodPressure))))


# Replace 0s in BloodPressure with NA
diabetes$BloodPressure[diabetes$BloodPressure == 0] <- NA


visualization_data <- data.frame(
  Index = 1:length(diabetes$BloodPressure),
  BloodPressure = ifelse(is.na(diabetes$BloodPressure), -5, diabetes$BloodPressure), # Temporarily assign -5 to missing values
  ValueStatus = ifelse(is.na(diabetes$BloodPressure), "Missing", "Present")          # Mark missing or present
)

library(ggplot2)


ggplot(visualization_data, aes(x = Index, y = BloodPressure, color = ValueStatus)) +
  geom_point() +
  scale_color_manual(values = c("Missing" = "red", "Present" = "blue")) +
  labs(
    title = "BloodPressure Data Before Interpolation",
    x = "Index",
    y = "BloodPressure",
    color = "Value Status"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )

# Step 1: Reload the dataset to start fresh
diabetes <- read.csv("diabetes.csv")  # Replace with your correct file path

# Step 2: Replace 0s with NA in BloodPressure
# Explanation: BloodPressure has 0s that represent missing values in this dataset. We'll replace them with NA to handle them correctly.
diabetes$BloodPressure[diabetes$BloodPressure == 0] <- NA

# Step 3: Verify that NA replacement worked
# Explanation: Checking the number of missing values ensures that we've correctly identified and marked all missing values in BloodPressure.
print(paste("Number of missing values in BloodPressure:", sum(is.na(diabetes$BloodPressure))))



# Step 4: Extract BloodPressure values
# Explanation: We isolate the BloodPressure column for easier manipulation and processing.
blood_pressure <- diabetes$BloodPressure

# Step 5: Identify indices for known and missing values
# Explanation: We separate the indices where values are known and where they are missing.
known_indices <- which(!is.na(blood_pressure))  # Indices of known values
missing_indices <- which(is.na(blood_pressure))  # Indices of missing values

# Step 6: Extract known values
# Explanation: Using the indices of known values, we extract the actual BloodPressure measurements for interpolation.
known_values <- blood_pressure[known_indices]

# Step 7: Set up a system of equations for spline interpolation
# Explanation: In cubic splines, we solve for a set of coefficients that represent smooth polynomials between known points.

# Define the function for manual cubic spline interpolation
manual_cubic_spline <- function(x, y, x_out) {
  # Explanation: This function performs cubic spline interpolation manually.
  # 'x' and 'y' are the known indices and values. 'x_out' are the points where we need interpolated values.
  
  n <- length(x) - 1  # Number of intervals (n = number of points - 1)
  
  # Step 1: Calculate intervals (h) between known x values
  # Explanation: 'h' represents the distance between consecutive known x values.
  h <- diff(x)
  
  # Step 2: Set up the matrix (A) for the system of equations
  # Explanation: This matrix is used to calculate the second derivatives of the spline at each known point.
  A <- matrix(0, n + 1, n + 1)  # Create an empty (n+1)x(n+1) matrix
  
  # Step 3: Populate the matrix A
  # Explanation: Fill in values based on the cubic spline equations. Diagonal, upper, and lower values are calculated here.
  for (i in 2:n) {
    A[i, i - 1] <- h[i - 1]
    A[i, i] <- 2 * (h[i - 1] + h[i])
    A[i, i + 1] <- h[i]
  }
  
  # Boundary conditions (natural spline)
  # Explanation: The first and last rows of the matrix A account for the natural spline boundary conditions.
  A[1, 1] <- 1
  A[n + 1, n + 1] <- 1
  
  # Step 4: Calculate the right-hand side (b) of the system of equations
  # Explanation: 'b' represents the differences between consecutive slopes of the spline.
  b <- numeric(n + 1)
  for (i in 2:n) {
    b[i] <- 6 * ((y[i + 1] - y[i]) / h[i] - (y[i] - y[i - 1]) / h[i - 1])
  }
  
  # Step 5: Solve for second derivatives (M)
  # Explanation: Solve the system of equations A * M = b to find the second derivatives at each known point.
  M <- solve(A, b)
  
  # Step 6: Use second derivatives to calculate spline values
  # Explanation: Using the second derivatives, calculate the interpolated values at the missing indices.
  y_out <- numeric(length(x_out))  # Placeholder for interpolated values
  for (k in seq_along(x_out)) {
    i <- max(which(x <= x_out[k]))  # Find the interval for x_out[k]
    a <- (x[i + 1] - x_out[k]) / h[i]
    b <- (x_out[k] - x[i]) / h[i]
    y_out[k] <- a * y[i] + b * y[i + 1] +
      ((a^3 - a) * M[i] + (b^3 - b) * M[i + 1]) * (h[i]^2) / 6
  }
  
  # Return the interpolated values
  return(y_out)
}

# Step 8: Perform manual spline interpolation
# Explanation: Apply the manual_cubic_spline function to interpolate missing BloodPressure values.
interpolated_bp <- manual_cubic_spline(known_indices, known_values, missing_indices)

# Step 9: Replace missing values in the original dataset
# Explanation: Replace the NA values in BloodPressure with the interpolated values.
diabetes$BloodPressure[missing_indices] <- interpolated_bp

# Step 10: Verify the interpolation
# Explanation: Check that there are no missing values left in BloodPressure after interpolation.
print(paste("Remaining NA values in BloodPressure:", sum(is.na(diabetes$BloodPressure))))




# Prepare data for visualization
visualization_data <- data.frame(
  Index = 1:length(diabetes$BloodPressure),
  BloodPressure = diabetes$BloodPressure,
  ValueType = ifelse(1:length(diabetes$BloodPressure) %in% missing_indices, "Interpolated", "Known")
)

# Visualize the interpolated results
ggplot(visualization_data, aes(x = Index, y = BloodPressure, color = ValueType)) +
  geom_point() +
  scale_color_manual(values = c("Interpolated" = "red", "Known" = "blue")) +
  labs(
    title = "BloodPressure Data After Manual Spline Interpolation",
    x = "Index",
    y = "BloodPressure",
    color = "Value Type"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )

# Reload the original dataset
diabetes <- read.csv("diabetes.csv")

# Replace 0s with NA in BloodPressure
diabetes$BloodPressure[diabetes$BloodPressure == 0] <- NA

# Verify the number of missing values
print(paste("Number of missing values in BloodPressure:", sum(is.na(diabetes$BloodPressure))))


# Replace 0s in BloodPressure with NA
diabetes$BloodPressure[diabetes$BloodPressure == 0] <- NA

# Prepare data for visualization
visualization_data <- data.frame(
  Index = 1:length(diabetes$BloodPressure),
  BloodPressure = ifelse(is.na(diabetes$BloodPressure), -5, diabetes$BloodPressure), # Temporarily assign -5 to missing values
  ValueStatus = ifelse(is.na(diabetes$BloodPressure), "Missing", "Present")          # Mark missing or present
)

# Load ggplot2 library
library(ggplot2)

# Create the plot
ggplot(visualization_data, aes(x = Index, y = BloodPressure, color = ValueStatus)) +
  geom_point() +
  scale_color_manual(values = c("Missing" = "red", "Present" = "blue")) +
  labs(
    title = "BloodPressure Data Before Interpolation",
    x = "Index",
    y = "BloodPressure",
    color = "Value Status"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )

# Replace -5 (used earlier for visualization) with NA
diabetes$BloodPressure[diabetes$BloodPressure == -5] <- NA

# Verify missing values
print(paste("Number of missing values in BloodPressure:", sum(is.na(diabetes$BloodPressure))))
# Step 4: Extract BloodPressure values
# Explanation: We isolate the BloodPressure column for easier manipulation and processing.
blood_pressure <- diabetes$BloodPressure

# Step 5: Identify indices for known and missing values
# Explanation: We separate the indices where values are known and where they are missing.
known_indices <- which(!is.na(blood_pressure))  # Indices of known values
missing_indices <- which(is.na(blood_pressure))  # Indices of missing values

# Step 6: Extract known values
# Explanation: Using the indices of known values, we extract the actual BloodPressure measurements for interpolation.
known_values <- blood_pressure[known_indices]

# Step 7: Set up a system of equations for spline interpolation
# Explanation: In cubic splines, we solve for a set of coefficients that represent smooth polynomials between known points.

# Define the function for manual cubic spline interpolation
manual_cubic_spline <- function(x, y, x_out) {
  # Explanation: This function performs cubic spline interpolation manually.
  # 'x' and 'y' are the known indices and values. 'x_out' are the points where we need interpolated values.
  
  n <- length(x) - 1  # Number of intervals (n = number of points - 1)
  
  # Step 1: Calculate intervals (h) between known x values
  # Explanation: 'h' represents the distance between consecutive known x values.
  h <- diff(x)
  
  # Step 2: Set up the matrix (A) for the system of equations
  # Explanation: This matrix is used to calculate the second derivatives of the spline at each known point.
  A <- matrix(0, n + 1, n + 1)  # Create an empty (n+1)x(n+1) matrix
  
  # Step 3: Populate the matrix A
  # Explanation: Fill in values based on the cubic spline equations. Diagonal, upper, and lower values are calculated here.
  for (i in 2:n) {
    A[i, i - 1] <- h[i - 1]
    A[i, i] <- 2 * (h[i - 1] + h[i])
    A[i, i + 1] <- h[i]
  }
  
  # Boundary conditions (natural spline)
  # Explanation: The first and last rows of the matrix A account for the natural spline boundary conditions.
  A[1, 1] <- 1
  A[n + 1, n + 1] <- 1
  
  # Step 4: Calculate the right-hand side (b) of the system of equations
  # Explanation: 'b' represents the differences between consecutive slopes of the spline.
  b <- numeric(n + 1)
  for (i in 2:n) {
    b[i] <- 6 * ((y[i + 1] - y[i]) / h[i] - (y[i] - y[i - 1]) / h[i - 1])
  }
  
  # Step 5: Solve for second derivatives (M)
  # Explanation: Solve the system of equations A * M = b to find the second derivatives at each known point.
  M <- solve(A, b)
  
  # Step 6: Use second derivatives to calculate spline values
  # Explanation: Using the second derivatives, calculate the interpolated values at the missing indices.
  y_out <- numeric(length(x_out))  # Placeholder for interpolated values
  for (k in seq_along(x_out)) {
    i <- max(which(x <= x_out[k]))  # Find the interval for x_out[k]
    a <- (x[i + 1] - x_out[k]) / h[i]
    b <- (x_out[k] - x[i]) / h[i]
    y_out[k] <- a * y[i] + b * y[i + 1] +
      ((a^3 - a) * M[i] + (b^3 - b) * M[i + 1]) * (h[i]^2) / 6
  }
  
  # Return the interpolated values
  return(y_out)
}

# Step 8: Perform manual spline interpolation
# Explanation: Apply the manual_cubic_spline function to interpolate missing BloodPressure values.
interpolated_bp <- manual_cubic_spline(known_indices, known_values, missing_indices)

# Step 9: Replace missing values in the original dataset
# Explanation: Replace the NA values in BloodPressure with the interpolated values.
diabetes$BloodPressure[missing_indices] <- interpolated_bp

# Step 10: Verify the interpolation
# Explanation: Check that there are no missing values left in BloodPressure after interpolation.
print(paste("Remaining NA values in BloodPressure:", sum(is.na(diabetes$BloodPressure))))

# Check the summary of the BloodPressure column
summary(diabetes$BloodPressure)

# Extract and view interpolated values
interpolated_values <- data.frame(
  Index = missing_indices,
  InterpolatedValues = interpolated_bp
)

print("Interpolated Values:")
print(interpolated_values)





# Step 1: Load the dataset

diabetes <- read.csv("diabetes.csv")  # Replace with actual path

# Step 2: Replace 0s with NA in the SkinThickness column
# Explanation: In the dataset, 0s in SkinThickness are considered missing values.
diabetes$SkinThickness[diabetes$SkinThickness == 0] <- NA

# Step 3: Extract SkinThickness column for easier manipulation
# Explanation: Create a separate variable to work with SkinThickness values.
skin_thickness <- diabetes$SkinThickness

# Step 4: Identify indices of known and missing values
# Explanation: Determine where SkinThickness values are present and missing.
known_indices <- which(!is.na(skin_thickness))  # Indices of known values
missing_indices <- which(is.na(skin_thickness))  # Indices of missing values

# Step 5: Extract known values for interpolation
# Explanation: Use the indices of known values to extract corresponding SkinThickness data.
known_values <- skin_thickness[known_indices]

# Step 6: Define the Lagrange interpolation function
# Explanation: This function calculates interpolated values using the Lagrange polynomial method.
lagrange_interpolation <- function(x, y, x_out) {
  interpolated_values <- numeric(length(x_out))  # Initialize output vector
  for (i in seq_along(x_out)) {
    L <- rep(1, length(x))  # Initialize Lagrange basis polynomial
    for (j in seq_along(x)) {
      for (k in seq_along(x)) {
        if (j != k) {
          L[j] <- L[j] * (x_out[i] - x[k]) / (x[j] - x[k])
        }
      }
    }
    interpolated_values[i] <- sum(L * y)  # Compute the interpolated value
  }
  return(interpolated_values)
}

# Step 7: Apply the Lagrange interpolation function
# Explanation: Use the known indices and values to interpolate the missing values.
interpolated_skin_thickness <- lagrange_interpolation(known_indices, known_values, missing_indices)

# Step 8: Replace missing values with interpolated values
# Explanation: Fill in the missing SkinThickness values with the interpolated results.
diabetes$SkinThickness[missing_indices] <- interpolated_skin_thickness

# Step 9: Verify the interpolation
# Explanation: Check that there are no missing values remaining in SkinThickness.
print(paste("Remaining NA values in SkinThickness:", sum(is.na(diabetes$SkinThickness))))

# Step 10: Visualize the results
# Explanation: Create a scatterplot showing the interpolated (red) and known (blue) values.
library(ggplot2)
visualization_data <- data.frame(
  Index = 1:nrow(diabetes),
  SkinThickness = diabetes$SkinThickness,
  ValueType = ifelse(is.na(skin_thickness), "Interpolated", "Known")
)

ggplot(visualization_data, aes(x = Index, y = SkinThickness, color = ValueType)) +
  geom_point() +
  scale_color_manual(values = c("Interpolated" = "red", "Known" = "blue")) +
  labs(
    title = "SkinThickness Data After Lagrange Interpolation",
    x = "Index",
    y = "SkinThickness",
    color = "Value Type"
  ) +
  theme_minimal()


# Reload the original dataset
diabetes <- read.csv("diabetes.csv")  # Ensure the correct file path is used

# Verify the dataset reload
summary(diabetes)

# Replace zero values in the SkinThickness column with NA
diabetes$SkinThickness[diabetes$SkinThickness == 0] <- NA

# Verify that zeros have been replaced with NA
print(paste("Number of missing values in SkinThickness:", sum(is.na(diabetes$SkinThickness))))


# Load the original dataset
diabetes <- read.csv("diabetes.csv")

# Replace 0s in BloodPressure with NA (as 0 is treated as missing in this dataset)
diabetes$BloodPressure[diabetes$BloodPressure == 0] <- NA

# Check the summary to ensure missing values are correctly identified
summary(diabetes$BloodPressure)


# Count the number of missing values in BloodPressure
missing_bp <- sum(is.na(diabetes$BloodPressure))
print(paste("Number of missing values in BloodPressure:", missing_bp))

# Extract the indices of missing and known values
known_indices <- which(!is.na(diabetes$BloodPressure))  # Indices with known values
missing_indices <- which(is.na(diabetes$BloodPressure))  # Indices with missing values

# Confirm the indices
print(paste("Number of known values in BloodPressure:", length(known_indices)))
print(paste("Number of missing values in BloodPressure:", length(missing_indices)))


# Extract known indices and corresponding BloodPressure values
x_known <- known_indices
y_known <- diabetes$BloodPressure[known_indices]

# Define the function for Lagrange interpolation
lagrange_interpolation <- function(x, y, x_out) {
  n <- length(x)  # Number of known points
  y_out <- numeric(length(x_out))  # Initialize output vector
  
  # Loop through each x_out value
  for (k in seq_along(x_out)) {
    L <- rep(1, n)  # Initialize Lagrange basis polynomial
    for (i in seq_along(x)) {
      for (j in seq_along(x)) {
        if (i != j) {
          L[i] <- L[i] * (x_out[k] - x[j]) / (x[i] - x[j])
        }
      }
    }
    # Compute the interpolated value
    y_out[k] <- sum(y * L)
  }
  return(y_out)
}



# Apply the Lagrange interpolation function to the missing indices
interpolated_bp <- lagrange_interpolation(x_known, y_known, missing_indices)

# Replace the missing values in the original dataset
diabetes$BloodPressure[missing_indices] <- interpolated_bp

# Verify the replacement
summary(diabetes$BloodPressure)


# Reload the original dataset
diabetes <- read.csv("diabetes.csv")

# Replace 0s in BloodPressure with NA
diabetes$BloodPressure[diabetes$BloodPressure == 0] <- NA

# Verify reset
summary(diabetes$BloodPressure)


# Function to find closest points for interpolation
get_closest_points <- function(x, y, x_target, num_points = 5) {
  distances <- abs(x - x_target)
  closest_indices <- order(distances)[1:num_points]
  list(x = x[closest_indices], y = y[closest_indices])
}

# Updated Lagrange Interpolation Function
lagrange_with_subset <- function(x_known, y_known, x_missing, num_points = 5) {
  interpolated_values <- numeric(length(x_missing))
  
  for (k in seq_along(x_missing)) {
    # Get closest points to the current missing index
    closest <- get_closest_points(x_known, y_known, x_missing[k], num_points)
    x <- closest$x
    y <- closest$y
    
    # Perform Lagrange interpolation
    L <- numeric(length(x))
    for (i in seq_along(x)) {
      L[i] <- prod((x_missing[k] - x[-i]) / (x[i] - x[-i]))
    }
    interpolated_values[k] <- sum(L * y)
  }
  
  return(interpolated_values)
}



x_known <- which(!is.na(diabetes$BloodPressure))
y_known <- diabetes$BloodPressure[x_known]


x_missing <- which(is.na(diabetes$BloodPressure))


interpolated_bp <- lagrange_with_subset(x_known, y_known, x_missing, num_points = 5)


diabetes$BloodPressure[x_missing] <- interpolated_bp

summary(diabetes$BloodPressure)





# Prepare data for after interpolation visualization
visualization_data <- data.frame(
  Index = 1:length(diabetes$BloodPressure),
  BloodPressure = diabetes$BloodPressure,
  ValueType = ifelse(
    1:length(diabetes$BloodPressure) %in% x_missing, "Interpolated", "Known"
  )
)

# Plot
library(ggplot2)
ggplot(visualization_data, aes(x = Index, y = BloodPressure, color = ValueType)) +
  geom_point() +
  scale_color_manual(values = c("Interpolated" = "red", "Known" = "blue")) +
  labs(
    title = "BloodPressure Data After Lagrange Interpolation",
    x = "Index",
    y = "Blood Pressure",
    color = "Value Type"
  ) +
  theme_minimal()


# Reload the original dataset
diabetes <- read.csv("diabetes.csv")  # Adjust the file path if necessary

# Replace 0 values with NA in BloodPressure for missing value representation
diabetes$BloodPressure[diabetes$BloodPressure == 0] <- NA

# Verify the reset
summary(diabetes$BloodPressure)
print(paste("Number of missing values in BloodPressure:", sum(is.na(diabetes$BloodPressure))))



# Nearest Neighbor Interpolation
nearest_neighbor_interpolation <- function(x, y, x_out) {
  # 'x' - indices of known values
  # 'y' - known values
  # 'x_out' - indices to interpolate
  
  y_out <- numeric(length(x_out))  # Initialize output vector
  
  for (i in seq_along(x_out)) {
    # Find the closest known index to the missing value index
    closest_index <- which.min(abs(x - x_out[i]))
    y_out[i] <- y[closest_index]  # Assign the nearest value
  }
  
  return(y_out)
}

# Known and missing indices
known_bp_indices <- which(!is.na(diabetes$BloodPressure))
missing_bp_indices <- which(is.na(diabetes$BloodPressure))
known_bp_values <- diabetes$BloodPressure[known_bp_indices]

# Perform nearest neighbor interpolation
interpolated_values <- nearest_neighbor_interpolation(known_bp_indices, known_bp_values, missing_bp_indices)

# Replace missing values with interpolated values
diabetes$BloodPressure[missing_bp_indices] <- interpolated_values

# Verify the interpolation
summary(diabetes$BloodPressure)










# Visualization
library(ggplot2)

# Prepare data for visualization
visualization_data <- data.frame(
  Index = 1:length(diabetes$BloodPressure),
  BloodPressure = diabetes$BloodPressure,
  ValueType = ifelse(1:length(diabetes$BloodPressure) %in% missing_bp_indices, "Interpolated", "Known")
)

# Plot
ggplot(visualization_data, aes(x = Index, y = BloodPressure, color = ValueType)) +
  geom_point(size = 2) +
  labs(
    title = "BloodPressure Data After Nearest Neighbor Interpolation",
    x = "Index",
    y = "Blood Pressure",
    color = "Value Type"
  ) +
  theme_minimal()

