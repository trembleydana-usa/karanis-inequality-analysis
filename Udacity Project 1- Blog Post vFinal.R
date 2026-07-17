## ---------------------------------------------
## Udacity Project 1: Blog Post
## Author: Dana Trembley
## ---------------------------------------------

# ---------------------------------------------
# 1. Setup
# ---------------------------------------------

library(tidyverse)
library(ineq)
library(scales)
library(broom)
library(writexl)
library(readxl)

# ---------------------------------------------
# 2. Load Data
# ---------------------------------------------

folder_path <- "C:/Projects/Udacity/Project 1 Blog Post"

df_raw <- read_excel(file.path(folder_path, "karanis-tax-rolls-2024.xlsx"))

# Inspect raw data
glimpse(df_raw)
summary(df_raw)

cat("This analysis uses tax payments as a proxy for economic activity and inequality in Roman Egypt.\n")

# ---------------------------------------------
# 3. Data Cleaning & Preparation
# ---------------------------------------------
# Goals:
# - Clean payment variable
# - Remove unusable columns
# - Standardize key variables
# - Prepare dataset for EDA and modeling
# ---------------------------------------------

df_clean <- df_raw %>%
  
  # Keep only relevant columns
  select(
    transpayer,
    transvalue,
    transpost,
    transobject,
    Sex
  ) %>%
  
  # Clean payment variable
  mutate(
    payment = as.numeric(str_extract(transvalue, "\\d+\\.?\\d*"))
  ) %>%
  
  # Remove missing or invalid values
  filter(!is.na(payment), payment >= 0) %>%
  
  # Standardize variables
  mutate(
    year = as.numeric(transpost),
    tax_type = as.factor(transobject),
    sex = as.factor(Sex)
  ) %>%
  
  # Drop raw/unneeded columns after transformation
  select(
    transpayer,
    payment,
    year,
    tax_type,
    sex
  )

# ---------------------------------------------
# Data Quality Checks
# ---------------------------------------------

cat("\nDATA QUALITY CHECKS:\n")

cat("Total observations:", nrow(df_clean), "\n")
cat("Unique taxpayers:", n_distinct(df_clean$transpayer), "\n")

cat("\nPayment Summary:\n")
summary(df_clean$payment)
# Total observations: 16545
#Unique taxpayers: 1719 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   2.000   4.000   5.587   8.000 302.000 
# ---------------------------------------------
# Aggregate to Individual Level
# ---------------------------------------------

df_individual <- df_clean %>%
  group_by(transpayer) %>%
  summarise(
    total_payment = sum(payment, na.rm = TRUE),
    transactions = n()
  )

cat("\nIndividual-level dataset created.\n")
cat("Total individuals:", nrow(df_individual), "\n")

# ---------------------------------------------
# Section 4: Exploratory Data Analysis (EDA)
# CRISP-DM Phase: Data Understanding

# # Business Questions # --------------------------------------------- 
cat("BUSINESS QUESTIONS:\n\n") 
cat("1. How are tax payments distributed across individuals in Karanis?\n") 
cat("2. How unequal is the distribution of tax burden?\n") 
cat("3. What share of total payments is contributed by the top vs bottom of the population?\n") 
cat("4. To what extent can tax payments be predicted using observable factors?\n\n")


# # Common color palete # --------------------------------------------- 
color_blue <- "#6B8E9B"
color_red <- "#C23B22"
color_gray <- "gray70"

# ---------------------------------------------
# STEP 4.1: Distribution of Tax Payments
# ---------------------------------------------

# Quantify zero payments
num_zero <- sum(df_clean$payment == 0, na.rm = TRUE)
share_zero <- num_zero / nrow(df_clean)

cat("\nZero-payment observations:", num_zero, "\n")
cat("Share of zero payments:", percent(share_zero), "\n")

# Filter for visualization (log requires > 0)
df_plot <- df_clean %>%
  filter(payment > 0)

# Summary statistics (positive values only for log consistency)
median_val <- median(df_plot$payment, na.rm = TRUE)
mean_val <- mean(df_plot$payment, na.rm = TRUE)

cat("1. How are tax payments distributed across individuals in Karanis?\n") 
# ---------------------------------------------
# VISUALIZATION 1: HISTOGRAM (LOG SCALE)
# ---------------------------------------------
df_plot <- df_clean %>%
  filter(payment > 0)

median_val <- median(df_plot$payment, na.rm = TRUE)

p1 <- ggplot(df_plot, aes(x = payment)) +
  geom_histogram(bins = 30, fill = color_blue, color = "white") +
  scale_x_log10(labels = comma) +
  geom_vline(xintercept = median_val,
             linetype = "dashed",
             color = color_red,
             linewidth = 1) +
  labs(
    title = "A Long Tail of Tax Payments",
    subtitle = "Distribution of individual tax payments in Karanis (log scale)",
    x = "Tax Payment (Drachma, log scale)",
    y = "Number of Payments",
    caption = "Source: Karanis Tax Rolls Database, Oxford Roman Economy Project"
  ) +
  annotate("text",
           x = median_val+8,
           y = 3500,
           label = paste0("Median ≈ ", round(median_val, 1)," Drachma"),
           color = color_red,
           hjust = -0.1,
           size = 3) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40")
  )

# ---------------------------------------------
# INTERPRETATION 4.1
# ---------------------------------------------

cat("\nINTERPRETATION (Distribution):\n")

cat("Median (positive payments):", round(median_val,1), "drachma\n")
cat("Mean (positive payments):", round(mean_val,1), "drachma\n")

cat("The mean exceeds the median, indicating a strongly right-skewed distribution.\n")

cat("A non-trivial share of observations are zero-value payments (",
    percent(share_zero), "), which may reflect exemptions,\n",
    "non-payment, or administrative recording practices.\n", sep = "")

cat("Because log transformations require positive values, zero payments are excluded\n")
cat("from the visualization but retained in the broader analysis.\n")

cat("Overall, the distribution suggests that most taxpayers paid small amounts,\n")
cat("while a small number contributed disproportionately large payments.\n\n")

# ---------------------------------------------
# STEP 4.2: Aggregate to Individual Level
# ---------------------------------------------

cat("Aggregating payments to individual level...\n")

df_individual <- df_clean %>%
  group_by(transpayer) %>%
  summarise(
    total_payment = sum(payment, na.rm = TRUE),
    transactions = n()
  )

cat("Total individuals:", nrow(df_individual), "\n")

# ---------------------------------------------
# STEP 4.3: Inequality Analysis (Lorenz Curve)
# ---------------------------------------------


# Zero-payment individuals
num_zero_ind <- sum(df_individual$total_payment == 0, na.rm = TRUE)
share_zero_ind <- num_zero_ind / nrow(df_individual)

# Compute Lorenz + Gini
lc <- Lc(df_individual$total_payment)
gini_val <- ineq(df_individual$total_payment, type = "Gini")


cat("2. How unequal is the distribution of tax burden?\n") 
# ---------------------------------------------
# VISUALIZATION 2: LORENZ CURVE
# ---------------------------------------------
# Convert to dataframe
lorenz_df <- data.frame(
  cum_pop = lc$p,
  cum_share = lc$L
)

# Create plot
p2 <- ggplot(lorenz_df, aes(x = cum_pop, y = cum_share)) +
  
  # Lorenz curve
  geom_line(color = color_red, linewidth = 1.5) +
  
  # Equality line
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed",
              color = color_gray) +
  
  # Labels
  labs(
    title = "Inequality Is Widespread",
    subtitle = paste0("The distribution of tax payments is highly uneven"),
    x = "Sum Share of Taxpayers",
    y = "Sum Share of Tax Payments",
    caption = "Source: Karanis Tax Rolls Database, Oxford Roman Economy Project"
  ) +
  
  # Annotation
  annotate("text",
           x = 0.2, y = 0.7,
           label = "Perfect equality",
           color = "gray50",
           size = 3) +
  annotate( "text", 
            x = 0.6, y = 0.25, 
            label = paste0("Gini = ", round(gini_val, 2)), 
            color = color_red, 
            size = 3, 
            fontface = "bold" ) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40")
  )

# ---------------------------------------------
# INTERPRETATION 4.2
# ---------------------------------------------

cat("INTERPRETATION (Inequality):\n")

cat("Gini coefficient:", round(gini_val, 2), "\n")
cat("Zero-payment individuals:", num_zero_ind, "\n")
cat("Share of zero-payment individuals:", percent(share_zero_ind), "\n")

cat("The Lorenz curve shows a strong deviation from equality,\n")
cat("indicating that tax payments are highly concentrated.\n")

cat("Including zero-payment individuals increases measured inequality\n")
cat("by adding mass at the bottom of the distribution.\n\n")

# ---------------------------------------------
# STEP 4.4: Top Shares Analysis
# ---------------------------------------------

df_sorted <- df_individual %>%
  arrange(desc(total_payment))

n_total <- nrow(df_sorted)

top_10_cutoff <- floor(0.1 * n_total)
bottom_50_cutoff <- floor(0.5 * n_total)

top_10_share <- sum(df_sorted$total_payment[1:top_10_cutoff]) / sum(df_sorted$total_payment)
bottom_50_share <- sum(df_sorted$total_payment[(n_total - bottom_50_cutoff + 1):n_total]) / sum(df_sorted$total_payment)

share_df <- data.frame(
  Group = c("Top 10%", "Bottom 50%"),
  Share = c(top_10_share, bottom_50_share)
)

cat("3. What share of total payments is contributed by the top vs bottom of the population?\n") 
# ---------------------------------------------
# VISUALIZATION 3: TOP SHARES
# ---------------------------------------------
# Sort individuals
df_sorted <- df_individual %>%
  arrange(desc(total_payment))

n_total <- nrow(df_sorted)

# Compute shares
top_10_share <- sum(df_sorted$total_payment[1:floor(0.1 * n_total)]) / sum(df_sorted$total_payment)
bottom_50_share <- sum(df_sorted$total_payment[(n_total - floor(0.5 * n_total) + 1):n_total]) / sum(df_sorted$total_payment)

# Dataframe
share_df <- data.frame(
  Group = c("Top 10%", "Bottom 50%"),
  Share = c(top_10_share, bottom_50_share)
)

# Plot
p3 <- ggplot(share_df, aes(x = Group, y = Share, fill = Group)) +
  
  geom_bar(stat = "identity", width = 0.6) +
  
  geom_text(
    aes(label = paste0(round(Share * 100), "%")),
    vjust = -0.6,
    size = 5
  ) +
  
  scale_fill_manual(values = c(color_red, color_blue)) +
  
  scale_y_continuous(
    labels = percent,
    limits = c(0, 0.5)
  ) +
  
  labs(
    title = "A Small Group Bears Most of the Burden",
    subtitle = "Top taxpayers contribute a disproportionate share of total payments",
    x = "",
    y = "Share of Total Payments",
    caption = "Source: Karanis Tax Rolls Database, Oxford Roman Economy Project"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40")
  )


# ---------------------------------------------
# INTERPRETATION 4.3
# ---------------------------------------------

cat("INTERPRETATION (Top Shares):\n")

cat("Top 10% share:", percent(top_10_share), "\n")
cat("Bottom 50% share:", percent(bottom_50_share), "\n")

cat("The top 10% account for a disproportionate share of total payments,\n")
cat("while the bottom 50% contribute relatively little.\n")

cat("This pattern reflects a highly unequal distribution of economic obligation,\n")
cat("likely tied to differences in asset ownership and economic activity.\n\n")


# ---------------------------------------------
# Section 5: Modeling & Evaluation
# CRISP-DM Phases:
# - Modeling
# - Evaluation
# Purpose:
# - Predict variation in tax payments
# - Assess how much inequality is explainable
# ---------------------------------------------

cat("\nSECTION 5: MODELING & EVALUATION\n")

# ---------------------------------------------
# STEP 5.1: Prepare Modeling Dataset
# ---------------------------------------------

# Create modeling dataset
df_model <- df_clean %>%
  mutate(
    log_payment = log(payment + 1),  # log transform to handle skewness
    year = as.numeric(year),
    tax_type = as.factor(tax_type)
  )

cat("\nModel dataset prepared.\n")
cat("Observations:", nrow(df_model), "\n")

# ---------------------------------------------
# STEP 5.2: Train Model
# ---------------------------------------------

# Linear regression model
model <- lm(log_payment ~ year + tax_type, data = df_model)

# Model summary
model_results <- summary(model)
model_results

cat("4. To what extent can tax payments be predicted using observable factors?\n\n")
# ---------------------------------------------
# STEP 5.3: Evaluate Model Performance
# ---------------------------------------------

model_summary <- glance(model)

r2 <- model_summary$r.squared
adj_r2 <- model_summary$adj.r.squared

# Extract model data
model_df <- augment(model)


# Create plot
p4 <- ggplot(model_df, aes(x = .fitted, y = .resid)) +
  geom_point(color = color_blue, alpha = 0.6, size = 2) +
  geom_smooth(method = "loess",
              se = FALSE,
              color = color_red,
              linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "gray60") +
  labs(
    title = "Model Fit Shows Structured Residual Variation",
    subtitle = "Residuals vs fitted values for log-transformed tax payments",
    x = "Fitted Values",
    y = "Residuals",
    caption = "Source: Karanis Tax Rolls Database, Oxford Roman Economy Project"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(size = 8, color = "gray40")
  )


cat("\nMODEL PERFORMANCE:\n")
cat("R-squared:", round(r2, 2), "\n")
cat("Adjusted R-squared:", round(adj_r2, 2), "\n")

cat("\nThe model explains", percent(r2),
    "of the variation in tax payments.\n")

cat("This indicates that observable factors such as tax type and year\n")
cat("capture part of the variation, but a substantial portion remains unexplained.\n\n")

# ---------------------------------------------
# STEP 5.4: Residual Diagnostics
# ---------------------------------------------


plot(model, which = 1)

cat("Residual plot generated to assess model fit.\n")

# ---------------------------------------------
# INTERPRETATION 5.1
# ---------------------------------------------

cat("\nINTERPRETATION (Modeling):\n")

cat("The model captures systematic differences in tax payments across tax types and time.\n")

cat("However, the relatively modest R-squared suggests that inequality is not fully explained\n")
cat("by observable variables alone.\n")

cat("This implies that unobserved factors—such as wealth, land ownership,\n")
cat("or economic status—likely play a significant role.\n\n")

# ---------------------------------------------
# STEP 5.5: Scenario Simulation
# ---------------------------------------------
# Purpose:
# - Use model to simulate a change in tax structure
# - Interpret economic implications
# ---------------------------------------------

cat("SCENARIO ANALYSIS:\n")

cat("Scenario: Increase in poll tax burden (e.g., fiscal pressure or policy change)\n")

scenario <- df_model %>%
  mutate(
    adjusted_payment = ifelse(tax_type == "Poll Tax",
                              log_payment * 1.1,
                              log_payment)
  )

avg_original <- mean(df_model$log_payment, na.rm = TRUE)
avg_new <- mean(scenario$adjusted_payment, na.rm = TRUE)

cat("Average log payment (original):", round(avg_original, 2), "\n")
cat("Average log payment (scenario):", round(avg_new, 2), "\n\n")

# ---------------------------------------------
# INTERPRETATION 5.2 (Scenario)
# ---------------------------------------------

cat("INTERPRETATION (Scenario):\n")

cat("An increase in poll taxes raises the average tax burden.\n")

cat("Because poll taxes are typically applied broadly,\n")
cat("this type of change may disproportionately affect lower-income individuals.\n")

cat("Depending on the structure of the tax system,\n")
cat("this could either increase or compress inequality.\n")

cat("In this case, the model suggests a modest increase in overall tax burden,\n")
cat("highlighting how fiscal policy can shape economic distribution.\n\n")

# ---------------------------------------------
# Section 6: Final Conclusion
# ---------------------------------------------

cat("\nSECTION 6: FINAL CONCLUSION\n")

cat("This analysis used tax payments from Roman Egypt as a proxy\n")
cat("for economic activity and inequality.\n\n")

cat("The results show a highly unequal distribution of tax burden,\n")
cat("with a small share of taxpayers contributing the majority of payments.\n\n")

cat("While some variation can be explained by tax type and time,\n")
cat("much of the inequality reflects underlying economic structure,\n")
cat("such as differences in wealth and asset ownership.\n\n")

cat("The presence of zero-payment individuals further reinforces\n")
cat("the extent of inequality in the system.\n\n")

cat("Overall, the Karanis tax records reveal a structured and\n")
cat("persistent pattern of economic inequality in the ancient world.\n")


# ---------------------------------------------
# FINAL CONCLUSION & Limitations
# ---------------------------------------------
cat("FINAL CONCLUSION:\n")

cat("The modeling results reinforce the findings from the exploratory analysis.\n")

cat("Tax payments in Karanis are highly unequal, and while some variation\n")
cat("can be explained by observable factors, much of the inequality reflects\n")
cat("underlying economic structure rather than random variation.\n")

cat("\nLIMITATIONS:\n") 
cat("Tax payments are an imperfect proxy for income or wealth.\n") 

cat("Some individuals may be missing or underrepresented.\n") 
cat("Administrative practices may influence recorded values.\n\n") 
cat("These limitations suggest caution in interpreting results\n") 
cat("as exact measures of inequality.\n")

# ---------------------------------------------
# Appenidx: Visual Extracts for Blog
# ---------------------------------------------
# Key numbers 
gini_val 
top_10_share 
bottom_50_share 
r2
median(df_clean$payment, na.rm = TRUE) 
mean(df_clean$payment, na.rm = TRUE)

# Save as PNG
ggsave(filename=file.path(folder_path, "Figure 1.png"), plot=p1, width = 6, height = 5, dpi = 300)
ggsave(filename=file.path(folder_path, "Figure 2.png"), plot=p2, width = 6, height = 5, dpi = 300)
ggsave(filename=file.path(folder_path, "Figure 3.png"), plot=p3, width = 6, height = 5, dpi = 300)
ggsave(filename=file.path(folder_path, "Figure 4.png"), plot=p4, width = 6, height = 5, dpi = 300)
    
