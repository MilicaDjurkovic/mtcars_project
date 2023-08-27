# Load the required libraries
library(tidyverse)
library(dplyr)
library(caret)
library(rpart)
library(corrplot)
library(car)
library(ggplot2)
library(ggrepel)

# Load the mtcars dataset

data_dir <- paste0(getwd(), "/_data/")
mtcars_data <- read.csv(
    paste0(data_dir, "mtcars.csv"),
    header = TRUE,
    check.names = FALSE,
    stringsAsFactors = FALSE
)
glimpse(mtcars_data)

mtcars_data <- mtcars_data %>%
    select("mpg",
           "cyl",
           "disp",
           "hp",
           "drat",
           "wt",
           "qsec",
           "vs",
           "am",
           "gear",
           "carb")

str(mtcars_data)

# Display basic statistics for numeric variables
summary(mtcars_data)

### --- Binomial Logistic Model with two features drat + gear
fit_binomial_linear_model <- function(data) {
    glm(am ~ drat + gear, data = data, family = "binomial")
}

histo_numeric_variables <- function(interactive = TRUE) {
    # Create histograms for numeric variables
    par(mfrow = c(4, 3))  # Arrange plots in a 4x3 grid
    for (col in colnames(mtcars_data)) {
        hist(mtcars_data[[col]], main = col, xlab = "")
    }
}

histo_numeric_variables(interactive = FALSE)

# display correlation matrix
display_correlation_matrix <- function(interactive = TRUE) {
    # Display the correlation matrix
    cor_matrix <- cor(mtcars_data)
    print(cor_matrix)
    corr <- round(cor_matrix, 2)
    correlation_matrix <- corrplot(
        corr,
        type = "upper",
        order = "hclust",
        tl.col = "black",
        tl.srt = 45
    )
}

display_correlation_matrix(interactive = FALSE)

# box plot
box_plot <- function(interactive = TRUE) {
    # Create box plots for each variable against 'am'
    box_plots <- lapply(names(mtcars), function(var_name) {
        ggplot(mtcars, aes(x = factor(am), y = mtcars[[var_name]])) +
            geom_boxplot() +
            ggtitle(paste("Box Plot of", var_name, "vs. am")) +
            xlab("am") +
            ylab(var_name)
    })
    # Display the box plots
    gridExtra::grid.arrange(grobs = box_plots, ncol = 3)  # Adjust ncol as needed
}

box_plot(interactive = FALSE)

# Define a function to calculate accuracy, hit rate, and false alarm rate
calculate_metrics <- function(observation, prediction) {
    accuracy <- sum(observation == prediction) / length(observation)
    hit_rate <-
        sum(observation == 1 & prediction == 1) / sum(observation == 1)
    false_alarm_rate <-
        sum(observation == 0 & prediction == 1) / sum(observation == 0)
    
    return(
        list(
            accuracy = accuracy,
            hit_rate = hit_rate,
            false_alarm_rate = false_alarm_rate
        )
    )
}

generate_predictions <- function(data, model, threshold) {
    predictions <- predict(model, newdata = data, type = "response")
    predictions <- as.numeric(predictions > threshold)
    
    return(data.frame(observation = data$am, prediction = predictions))
}

analyze_model <- function(data, model, threshold = 0.5) {
    vif_values <- vif(model)
    print(paste("VIF values:", vif_values))
    
    model_summary <- summary(model)
    print(model_summary)
    
    coefficients <- exp(model$coefficients)
    print(coefficients)
    
    predictions <- generate_predictions(data, model, threshold)
    
    metrics <-
        calculate_metrics(predictions$observation, predictions$prediction)
    print(metrics)
}

# Define a function to generate ROC analysis frame
generate_roc_frame <- function(data, model) {
    dec_criterion <- seq(0.01, 0.99, by = 0.01)
    
    roc_frame <- lapply(dec_criterion, function(threshold) {
        preds <- generate_predictions(data, model, threshold)
        metrics <-
            calculate_metrics(preds$observation, preds$prediction)
        data.frame(
            hit_rate = metrics$hit_rate,
            false_alarm_rate = metrics$false_alarm_rate,
            accuracy = metrics$accuracy,
            dec = threshold
        )
    })
    
    roc_frame <- do.call(rbind, roc_frame)
    roc_frame$diff <-
        roc_frame$hit_rate - roc_frame$false_alarm_rate
    roc_frame$label <- ""
    roc_frame$label[which.max(roc_frame$diff)] <- "Here!"
    
    return(roc_frame)
}

plot_roc_curve <- function(roc_frame) {
    ggplot(data = roc_frame,
           aes(x = false_alarm_rate,
               y = hit_rate,
               label = label)) +
        geom_path(color = "blue") +
        geom_abline(intercept = 0, slope = 1) +
        geom_text_repel(
            arrow = arrow(
                length = unit(0.06, "inches"),
                ends = "last",
                type = "closed"
            ),
            min.segment.length = unit(0, 'lines'),
            nudge_y = .1
        ) +
        ggtitle("ROC analysis for the Binomial Regression Model") +
        xlab("True Negative (False Alarm) Rate") + ylab("True Positive (Hit) Rate") +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = .5)) +
        theme(panel.border = element_blank())
}

# Fit and analyze the binomial linear model
binomial_model <- fit_binomial_linear_model(mtcars_data)
analyze_model(mtcars_data, binomial_model)

roc_frame <- generate_roc_frame(mtcars_data, binomial_model)

# Plot the ROC curve
plot_roc_curve(roc_frame)

function_names <- c(
    "fit_binomial_linear_model",
    "histo_numeric_variables",
    "display_correlation_matrix",
    "box_plot",
    "calculate_metrics",
    "generate_predictions",
    "generate_roc_frame",
    "fit_binomial_linear_model",
    "analyze_model",
    "plot_roc_curve"
)

dump(function_names, file = "00_scripts_plot_visualization.R")
