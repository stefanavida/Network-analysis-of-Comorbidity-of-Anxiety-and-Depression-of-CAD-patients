# Load the packages.
library(haven)
library(dplyr)
library(psych)
#library(Hmisc)
library(ggplot2)
library(patchwork)
library(tidyr)
library(BGGM)
library(qgraph)
library(CliquePercolation)

#Check packages version to ensure reproducibility
sessionInfo()

#Data Pre-processing steps
data_t0_raw <- haven::read_sav("D:/TILBURG/network_comorbidity/final_data/THORESCI_T0 NW.sav")
data_t1_raw <- haven::read_sav("D:/TILBURG/network_comorbidity/final_data/THORESCI_T1sNW.sav")
data_t2_raw <- haven::read_sav("D:/TILBURG/network_comorbidity/final_data/THORESCI_T2 NW.sav")
data_t3_raw <- haven::read_sav("D:/TILBURG/network_comorbidity/final_data/THORESCI_T3 NW.sav")
data_t4_raw <- haven::read_sav("D:/TILBURG/network_comorbidity/final_data/THORESCI_T4 NW.sav")

#Combine all time points into one data frame
combined <- dplyr::full_join(data_t0_raw, data_t1_raw, by = "RESEARCH_NUMBER")
combined <- dplyr::full_join(combined, data_t2_raw, by = "RESEARCH_NUMBER")
combined <- dplyr::full_join(combined, data_t3_raw, by = "RESEARCH_NUMBER")
data <- dplyr::full_join(combined, data_t4_raw, by = "RESEARCH_NUMBER")
# Print a glimpse of the data
str(data)


#Remove unnecesary columns
columns_to_remove <- c("REFEREN.x", "REFEREN.y","REFEREN.x.x", "REFEREN.y.y", "date_quest_T2", "date_T3", "date_quest_T4")
data <- data %>% dplyr::select(-all_of(columns_to_remove))

#Add the group differences variables (gender, cardiac history, psychaitric history) measured at baseline
#Load the baseline data with the extra variables
extra_data <- haven::read_sav("D:/TILBURG/network_comorbidity/final_data/THORESCI_2025NW_Comparisons&BaselineCharacteristics.sav")

#Only keep the necessary variables
extra_data <- extra_data %>%
    dplyr::select(RESEARCH_NUMBER, Sex, card_hist, Psychiatric_composite)
summary(extra_data)
#Add the extra variables to the main data frame
full_data <- dplyr::full_join(data, extra_data, by = "RESEARCH_NUMBER")
summary(full_data)


#Remove all unnecessary attributes from the data frame
# Zap the labels.
data <- zap_labels(data)
full_data <- zap_labels(full_data)
# Zap the format attributes.
data <- zap_formats(data)
full_data <- zap_formats(full_data)
# Zap the value label.
data <- zap_label(data)
full_data <- zap_label(full_data)
# Zap the variable widths.
data <- zap_widths(data)
full_data <- zap_widths(full_data)

# Make all variable names lowercase.
data <- data %>% dplyr::rename_all(tolower) %>% dplyr::select(-c(research_number))
full_data <- full_data %>% dplyr::rename_all(tolower) %>% dplyr::select(-c(research_number))

# Print a glimpse of the data
str(data)

#Recode the 9999 missing values to NA
data[data == 9999] <- NA
full_data[full_data == 9999] <- NA

#Shift the scale of the variables by 1 to avoid zeros (useful to avoid errors in BGGM)
data <- data %>%
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ .x + 1)
    )

#Check the data after preprocessing
summary(data)
summary(full_data) #Original scale

#Divide the data again into separate time points
data_t0 <- data %>%
    # Select the variables for the first time point.
    dplyr::select(dplyr::ends_with(match = "t0")) %>%
    # Remove the time suffix.
    dplyr::rename_all(~ gsub(pattern = "_t0", replacement = "", x = .))

data_t1 <- data %>%
    # Select the variables for the second time point.
    dplyr::select(dplyr::ends_with(match = "t1")) %>%
    # Remove the time suffix.
    dplyr::rename_all(~ gsub(pattern = "_t1", replacement = "", x = .))

data_t2 <- data %>%
    # Select the variables for the third time point.
    dplyr::select(dplyr::ends_with(match = "t2")) %>%
    # Remove the time suffix.
    dplyr::rename_all(~ gsub(pattern = "_t2", replacement = "", x = .))

data_t3 <- data %>%
    # Select the variables for the fourth time point.
    dplyr::select(dplyr::ends_with(match = "t3")) %>%
    # Remove the time suffix.
    dplyr::rename_all(~ gsub(pattern = "_t3", replacement = "", x = .))

data_t4 <- data %>%
    # Select the variables for the fifth time point.
    dplyr::select(dplyr::ends_with(match = "t4")) %>%
    # Remove the time suffix.
    dplyr::rename_all(~ gsub(pattern = "_t4", replacement = "", x = .))



#Start of the analysis

# Mean and standard deviation of the questionnaires sum at baseline
PHQ_sum_t0 <- rowSums(data_t0 %>% dplyr::select(starts_with("phq")), na.rm = TRUE)
GAD_sum_t0 <- rowSums(data_t0 %>% dplyr::select(starts_with("gad")), na.rm = TRUE)
mean(PHQ_sum_t0, na.rm = TRUE)
sd(PHQ_sum_t0, na.rm=TRUE)
mean(GAD_sum_t0, na.rm = TRUE)
sd(GAD_sum_t0, na.rm=TRUE)

#Descriptive statistics for each time point
data_t0 %>%
    psych::describe()
data_t1 %>%
    psych::describe()
data_t2 %>%
    psych::describe()
data_t3 %>%
    psych::describe()
data_t4 %>%
    psych::describe()


#Reliability estimates (Chronbach's alpha)
data_t0 %>%
    #Select depression items
    dplyr::select(starts_with("phq")) %>%
       psych::alpha()

data_t0 %>%
    #Select anxiety items
    dplyr::select(starts_with("gad")) %>%
       psych::alpha()

data_t1 %>%
    #Select depression items
    dplyr::select(starts_with("phq")) %>%
       psych::alpha()

data_t1 %>%
    #Select anxiety items
    dplyr::select(starts_with("gad")) %>%
       psych::alpha()

data_t2 %>%
    #Select depression items
    dplyr::select(starts_with("phq")) %>%
       psych::alpha()

data_t2 %>%
    #Select anxiety items
    dplyr::select(starts_with("gad")) %>%
       psych::alpha()

data_t3 %>%
    #Select depression items
    dplyr::select(starts_with("phq")) %>%
       psych::alpha()

data_t3 %>%
    #Select anxiety items
    dplyr::select(starts_with("gad")) %>%
       psych::alpha()

data_t4 %>%
    #Select depression items
    dplyr::select(starts_with("phq")) %>%
       psych::alpha()

data_t4 %>%
    #Select anxiety items
    dplyr::select(starts_with("gad")) %>%
       psych::alpha()


# # Estimate the partial correlations networks via `BGGM::explore`
#Set seed for reproducibility
set.seed(011001)
# Estimation function
estimate_network <- function(data, prior_sd = 0.5, posterior_samples = 5000, ...) {
    # Add one to the ordinal variables to shift the scale and avoid zeros.
    model_fit <- BGGM::explore(
            # Set the data to be used in the model.
            Y = data,

            # Do not include any covariates in the model.
            formula = ~ 1,

            # Set the type of the variables.
            type = "ordinal",

            # Set the prior standard deviation.
            prior_sd = prior_sd,

            # Set the number of posterior samples.
            iter = posterior_samples,

            # Do not impute missing values.
            impute = FALSE,

            # Optional arguments.
            ...
        )

    # Return the model fit object.
    return(model_fit)
}


# Estimate the network for each time point
fit_t0 <- estimate_network(data_t0, posterior_samples = 5000)
fit_t1 <- estimate_network(data_t1, posterior_samples = 5000)
fit_t2 <- estimate_network(data_t2, posterior_samples = 5000)
fit_t3 <- estimate_network(data_t3, posterior_samples = 5000)
fit_t4 <- estimate_network(data_t4, posterior_samples = 5000)

# Select the graph for each time point.
graph_t0 <- BGGM::select(fit_t0, alternative = "two.sided")
graph_t1 <- BGGM::select(fit_t1, alternative = "two.sided")
graph_t2 <- BGGM::select(fit_t2, alternative = "two.sided")
graph_t3 <- BGGM::select(fit_t3, alternative = "two.sided")
graph_t4 <- BGGM::select(fit_t4, alternative = "two.sided")

#EXTRA: Bayesian Model Averaging of the networks based on posterior probabilities mean/median

# Function to perform Bayesian Model Averaging (BMA)
compute_bma_matrix <- function(fit, graph, numdraws = 5000) {

  # Dimensions & Indices
  P <- ncol(fit$Y)
  idx <- which(lower.tri(diag(P), diag = FALSE), arr.ind = TRUE)

  #Extract Bayes Factors and calculate Probabilities
  BF10vec <- graph$BF_10[lower.tri(diag(P))]
  num_pcor <- length(BF10vec) # equal to P*(P-1)/2 as we only extract the lower triangle

  #Stating the 2 hypotheses
  PHP1 <- BF10vec / (BF10vec + 1) # Probability that edge is Non-Zero
  PHP0 <- 1 / (BF10vec + 1)       # Probability that edge is Zero

  # Generate BMA Draws
  BMA_draws <- do.call(cbind,
         lapply(1:num_pcor,function(c){
            #Flip the coin (H0 or H1)
        draws_c <- sample(x=c(0,1),size=numdraws,prob=c(PHP0[c],PHP1[c]),replace = TRUE)
        #Does edge exist?
        welk_H1 <- which(draws_c==1)
        #Fill H1 slots with actual values from the posterior distribution
        draws_c[welk_H1] <- fit$post_samp$pcors[idx[c,1],idx[c,2],1:length(welk_H1)]
        return(draws_c)
  }))
  # Calculate Point Estimates based on median
  bma_means_vec <- apply(BMA_draws, 2, median)

  # Reconstruct the Matrix
  bma_matrix <- matrix(0, P, P)

  # Map the vector of means back to the matrix coordinates
  for (i in 1:nrow(idx)) {
    row <- idx[i, 1]
    col <- idx[i, 2]
    val <- bma_means_vec[i]

    # Fill both triangles to make it symmetric
    bma_matrix[row, col] <- val
    bma_matrix[col, row] <- val
  }

  return(bma_matrix)
}

# Compute BMA matrices for each time point
bma_matrix_t0 <- compute_bma_matrix(fit_t0, graph_t0)
bma_matrix_t1 <- compute_bma_matrix(fit_t1, graph_t1)
bma_matrix_t2 <- compute_bma_matrix(fit_t2, graph_t2)
bma_matrix_t3 <- compute_bma_matrix(fit_t3, graph_t3)
bma_matrix_t4 <- compute_bma_matrix(fit_t4, graph_t4)


# Long variable names.
short_names <- colnames(data_t1)
long_names <- c(
    # The PHQ variables.
    "Loss of Interest",         # PHQ1
    "Depressed Mood",           # PHQ2
    "Disturbed Sleep",          # PHQ3
    "Fatigue",                  # PHQ4
    "Disturbed Appetite",       # PHQ5
    "Worthlessness",            # PHQ6
    "Concentration Problems",   # PHQ7
    "Psychomotor Disturbances", # PHQ8
    "Suicidal Ideation",        # PHQ9

    # The GAD variables.
    "Anxiety",                  # GAD1
    "Uncontrollable Worry",     # GAD2
    "Excessive Worry",          # GAD3
    "Trouble Relaxing",         # GAD4
    "Restlessness",             # GAD5
    "Irritable Mood",           # GAD6
    "Dread"                     # GAD7
)


# Define the conceptual groups names.
groups_names <- list(
    "Depression" = 1:9,
    "Anxiety" = 10:16
)


#Name the columns and rows of the BMA matrices
colnames(bma_matrix_t0) <- short_names
rownames(bma_matrix_t0) <- short_names
colnames(bma_matrix_t1) <- short_names
rownames(bma_matrix_t1) <- short_names
colnames(bma_matrix_t2) <- short_names
rownames(bma_matrix_t2) <- short_names
colnames(bma_matrix_t3) <- short_names
rownames(bma_matrix_t3) <- short_names
colnames(bma_matrix_t4) <- short_names
rownames(bma_matrix_t4) <- short_names

par(mfrow = c(1, 1))


# Define a function to visualize the selected graphs.

# Plotting colors
.__COLORS__ <- list(
    node_bg = "#F9F9F9",
    edge_labels_txt = "#F9F9F9",
    edge_labels_bg_positive = "#3f51b5d6",
    edge_labels_bg_negative = "#b31b0db6",
    edge_positive = "#3F51B5",
    edge_negative = "#b31a0d"
)
#Plotting function
plot_networks <- function(..., file, width = 11.7, height = 8.2) {
    # Extract the models to plot.
    models <- list(...)

    # Extract number of true models provided.
    n_models <- length(models)

    # Extract names.
    names <- sub("model_", "", names(models))

    # Specify the titles for the plots.
    titles <- c("Baseline", "1 month", "6 months", "12 months", "24 months")

    # Set the network graphical arguments.
    network_plot_arguments <- list(
        layout = "circle",
        # labels = TRUE,
        color = .__COLORS__$node_bg,
        posCol = .__COLORS__$edge_positive,
        negCol = .__COLORS__$edge_negative,
        edge.width = .8,
        border.width = 1.5,
        cut = 0,
        vsize = 11
    )

    # Start the printing device.
    pdf(file, width = width, height = height)

    # Outer margins bottom, left, top, right
    par(oma = c(0, 0, 1.5, 0))

    # Inner margins for each panel.
    par(mar = c(5, 5, 5, 5))

    # Specify the layout.
    matrix_layout <- matrix(
        c(
            # Top row with each plot spanning two columns.
            1, 1, 2, 2, 3, 3,

            # Bottom row, blank left and right, plots centered.
            0, 4, 4, 5, 5, 0
        ),
        nrow = 2,
        byrow = TRUE
    )

    # Set the layout with specified widths and heights.
    layout(matrix_layout, widths = rep(1, 6), heights = c(1, 1))

    # Print each true model in turn.
    for (i in 1:n_models) {
        # Not efficient, but extract the model to avoid indexing all over the place.
        model <- models[[i]]

        # Plot model without edge labels.
        do.call(qgraph::qgraph, c(list(input = model), network_plot_arguments))

        # Add title.
        title(
            main = titles[i],
            line = 4,
            cex.main = 1,
            adj = 0.5
        )
    }

    # Reset layout.
    layout(1:1)

    # Turn off.
    dev.off()

    # Remain silent.
    invisible()
}


plot_networks(
  model_t0 = bma_matrix_t0,
  model_t1 = bma_matrix_t1,
  model_t2 = bma_matrix_t2,
  model_t3 = bma_matrix_t3,
  model_t4 = bma_matrix_t4,
  file = "networks_bma.pdf"
)



#SIGNIFICANT TEMPORAL CHANGES IN THE NETWORKS

#Differences in networks between consecutive time points using the function 'ggm_compare_estimate' from BGGM
compare_networks <- function(..., prior_sd = 0.5, posterior_samples = 5000) {
    # Capture the data sets.
    datasets <- list(...)
    # Compare the networks.
    fit <- do.call(
        # The `BGGM` call.
        BGGM::ggm_compare_estimate, c(
            # The datasets.
            datasets,

            # The list of arguments.
            list(
                # Do not include any covariates in the model.
                formula = ~ 1,

                # Set the type of the variables.
                type = "ordinal",

                # Set the prior standard deviation.
                prior_sd = prior_sd,

                # Set the number of posterior samples.
                iter = posterior_samples,

                # Do not impute missing values.
                impute = FALSE
            )
        )
    )

    # Return the fit object.
    return(fit)
}

# Compare the networks.
fit_comparison <- compare_networks(
    # The datasets for comparison.
    data_t4, data_t3, data_t2, data_t1, data_t0,

    # Number of posterior samples.
    posterior_samples = 5000
)

# Select the comparison graphs (the difference between edge weights is significant - posterior credible interval doesn't contain zero).
graph_comparison <- select(fit_comparison, cred = 0.95)

#Significant differences between baseline and 1 month (1 month minus the baseline)
graph_comparison$pcor_adj[[10]] #Positive number means increase in connection, negative number means decrease in connection
#Name the columns and rows of the significant differences matrix
colnames(graph_comparison$pcor_adj[[10]]) <- short_names
rownames(graph_comparison$pcor_adj[[10]]) <- short_names
#Significant differences between 1 month and 6 months (6 months minus 1 month)
graph_comparison$pcor_adj[[8]]
colnames(graph_comparison$pcor_adj[[8]]) <- short_names
rownames(graph_comparison$pcor_adj[[8]]) <- short_names
#Significant differences between 6 months and 12 months (12 months minus 6 months)
graph_comparison$pcor_adj[[5]]
colnames(graph_comparison$pcor_adj[[5]]) <- short_names
rownames(graph_comparison$pcor_adj[[5]]) <- short_names
#Significant differences between 12 months and 24 months (24 months minus 12 months)
graph_comparison$pcor_adj[[1]]
colnames(graph_comparison$pcor_adj[[1]]) <- short_names
rownames(graph_comparison$pcor_adj[[1]]) <- short_names




#GROUP DIFFERENCES ANALYSES

#Function that dividies the full_data dataframe into time points dataframes
divide_by_time_preprocess <- function(data, time_suffix) {
  pattern_string <- paste0("_", time_suffix)

  data_subset <- data %>%
    dplyr::select(dplyr::ends_with(pattern_string)) %>%
        dplyr::rename_with(~ gsub(pattern = pattern_string, replacement = "", x = .)) %>%
            #Shift the scale by 1
             dplyr::mutate(dplyr::across(dplyr::everything(), ~ . + 1))

  return(data_subset)
}


#Group differences section
table(full_data$sex)
table(full_data$card_hist)
table(full_data$psychiatric_composite)

#Function to rename the columns and rows of the BMA matrices for the group differences analyses
rename_col_row <- function(x) {
    colnames(x) <- short_names
    rownames(x) <- short_names
    return(x)
}


#Males only dataframe
males_data <- full_data %>%
    dplyr::filter(sex == 1)

data_t0_males <- divide_by_time_preprocess(males_data, "t0")
data_t1_males <- divide_by_time_preprocess(males_data, "t1")
data_t2_males <- divide_by_time_preprocess(males_data, "t2")
data_t3_males <- divide_by_time_preprocess(males_data, "t3")
data_t4_males <- divide_by_time_preprocess(males_data, "t4")

#Estimate the BMA network for males
fit_males_t0 <- estimate_network(data_t0_males, posterior_samples = 5000)
fit_males_t1 <- estimate_network(data_t1_males, posterior_samples = 5000)
fit_males_t2 <- estimate_network(data_t2_males, posterior_samples = 5000)
fit_males_t3 <- estimate_network(data_t3_males, posterior_samples = 5000)
fit_males_t4 <- estimate_network(data_t4_males, posterior_samples = 5000)
# Select the graph for each time point.
graph_males_t0 <- BGGM::select(fit_males_t0, alternative = "two.sided")
graph_males_t1 <- BGGM::select(fit_males_t1, alternative = "two.sided")
graph_males_t2 <- BGGM::select(fit_males_t2, alternative = "two.sided")
graph_males_t3 <- BGGM::select(fit_males_t3, alternative = "two.sided")
graph_males_t4 <- BGGM::select(fit_males_t4, alternative = "two.sided")
#Bayesian model averaging
bma_males_matrix_t0 <- compute_bma_matrix(fit_males_t0, graph_males_t0)
bma_males_matrix_t1 <- compute_bma_matrix(fit_males_t1, graph_males_t1)
bma_males_matrix_t2 <- compute_bma_matrix(fit_males_t2, graph_males_t2)
bma_males_matrix_t3 <- compute_bma_matrix(fit_males_t3, graph_males_t3)
bma_males_matrix_t4 <- compute_bma_matrix(fit_males_t4, graph_males_t4)
#Rename the columns and rows of the BMA
bma_males_matrix_t0 <- rename_col_row(bma_males_matrix_t0)
bma_males_matrix_t1 <- rename_col_row(bma_males_matrix_t1)
bma_males_matrix_t2 <- rename_col_row(bma_males_matrix_t2)
bma_males_matrix_t3 <- rename_col_row(bma_males_matrix_t3)
bma_males_matrix_t4 <- rename_col_row(bma_males_matrix_t4)
#Plotting
plot_networks(
  model_t0 = bma_males_matrix_t0,
  model_t1 = bma_males_matrix_t1,
  model_t2 = bma_males_matrix_t2,
  model_t3 = bma_males_matrix_t3,
  model_t4 = bma_males_matrix_t4,
  file = "networks_males.pdf"
)


#Females only dataframe
females_data <- full_data %>%
    dplyr::filter(sex == 2)

data_t0_females <- divide_by_time_preprocess(females_data, "t0")
data_t1_females <- divide_by_time_preprocess(females_data, "t1")
data_t2_females <- divide_by_time_preprocess(females_data, "t2")
data_t3_females <- divide_by_time_preprocess(females_data, "t3")
data_t4_females <- divide_by_time_preprocess(females_data, "t4")

#Estimate the BMA network for females
fit_females_t0 <- estimate_network(data_t0_females, posterior_samples = 5000)
fit_females_t1 <- estimate_network(data_t1_females, posterior_samples = 5000)
fit_females_t2 <- estimate_network(data_t2_females, posterior_samples = 5000)
fit_females_t3 <- estimate_network(data_t3_females, posterior_samples = 5000)
fit_females_t4 <- estimate_network(data_t4_females, posterior_samples = 5000)
# Select the graph for each time point.
graph_females_t0 <- BGGM::select(fit_females_t0, alternative = "two.sided")
graph_females_t1 <- BGGM::select(fit_females_t1, alternative = "two.sided")
graph_females_t2 <- BGGM::select(fit_females_t2, alternative = "two.sided")
graph_females_t3 <- BGGM::select(fit_females_t3, alternative = "two.sided")
graph_females_t4 <- BGGM::select(fit_females_t4, alternative = "two.sided")
#Bayesian model averaging
bma_females_matrix_t0 <- compute_bma_matrix(fit_females_t0, graph_females_t0)
bma_females_matrix_t1 <- compute_bma_matrix(fit_females_t1, graph_females_t1)
bma_females_matrix_t2 <- compute_bma_matrix(fit_females_t2, graph_females_t2)
bma_females_matrix_t3 <- compute_bma_matrix(fit_females_t3, graph_females_t3)
bma_females_matrix_t4 <- compute_bma_matrix(fit_females_t4, graph_females_t4)
#Rename the columns and rows of the BMA
bma_females_matrix_t0 <- rename_col_row(bma_females_matrix_t0)
bma_females_matrix_t1 <- rename_col_row(bma_females_matrix_t1)
bma_females_matrix_t2 <- rename_col_row(bma_females_matrix_t2)
bma_females_matrix_t3 <- rename_col_row(bma_females_matrix_t3)
bma_females_matrix_t4 <- rename_col_row(bma_females_matrix_t4)

#Plotting
plot_networks(
  model_t0 = bma_females_matrix_t0,
  model_t1 = bma_females_matrix_t1,
  model_t2 = bma_females_matrix_t2,
  model_t3 = bma_females_matrix_t3,
  model_t4 = bma_females_matrix_t4,
  file = "networks_females.pdf"
)


#Cardiac history: Yes only dataframe
hist_card_yes_data <- full_data %>%
    dplyr::filter(card_hist == 1)

data_t0_hist_card_yes <- divide_by_time_preprocess(hist_card_yes_data, "t0")
data_t1_hist_card_yes <- divide_by_time_preprocess(hist_card_yes_data, "t1")
data_t2_hist_card_yes <- divide_by_time_preprocess(hist_card_yes_data, "t2")
data_t3_hist_card_yes <- divide_by_time_preprocess(hist_card_yes_data, "t3")
data_t4_hist_card_yes <- divide_by_time_preprocess(hist_card_yes_data, "t4")

#Estimate the BMA network for cardiac history yes
fit_hist_card_yes_t0 <- estimate_network(data_t0_hist_card_yes, posterior_samples = 5000)
fit_hist_card_yes_t1 <- estimate_network(data_t1_hist_card_yes, posterior_samples = 5000)
fit_hist_card_yes_t2 <- estimate_network(data_t2_hist_card_yes, posterior_samples = 5000)
fit_hist_card_yes_t3 <- estimate_network(data_t3_hist_card_yes, posterior_samples = 5000)
fit_hist_card_yes_t4 <- estimate_network(data_t4_hist_card_yes, posterior_samples = 5000)
# Select the graph for each time point.
graph_hist_card_yes_t0 <- BGGM::select(fit_hist_card_yes_t0, alternative = "two.sided")
graph_hist_card_yes_t1 <- BGGM::select(fit_hist_card_yes_t1, alternative = "two.sided")
graph_hist_card_yes_t2 <- BGGM::select(fit_hist_card_yes_t2, alternative = "two.sided")
graph_hist_card_yes_t3 <- BGGM::select(fit_hist_card_yes_t3, alternative = "two.sided")
graph_hist_card_yes_t4 <- BGGM::select(fit_hist_card_yes_t4, alternative = "two.sided")
#Bayesian model averaging
bma_hist_card_yes_matrix_t0 <- compute_bma_matrix(fit_hist_card_yes_t0, graph_hist_card_yes_t0)
bma_hist_card_yes_matrix_t1 <- compute_bma_matrix(fit_hist_card_yes_t1, graph_hist_card_yes_t1)
bma_hist_card_yes_matrix_t2 <- compute_bma_matrix(fit_hist_card_yes_t2, graph_hist_card_yes_t2)
bma_hist_card_yes_matrix_t3 <- compute_bma_matrix(fit_hist_card_yes_t3, graph_hist_card_yes_t3)
bma_hist_card_yes_matrix_t4 <- compute_bma_matrix(fit_hist_card_yes_t4, graph_hist_card_yes_t4)
#Rename the columns and rows of the BMA
bma_hist_card_yes_matrix_t0 <- rename_col_row(bma_hist_card_yes_matrix_t0)
bma_hist_card_yes_matrix_t1 <- rename_col_row(bma_hist_card_yes_matrix_t1)
bma_hist_card_yes_matrix_t2 <- rename_col_row(bma_hist_card_yes_matrix_t2)
bma_hist_card_yes_matrix_t3 <- rename_col_row(bma_hist_card_yes_matrix_t3)
bma_hist_card_yes_matrix_t4 <- rename_col_row(bma_hist_card_yes_matrix_t4)
#Plotting
plot_networks(
  model_t0 = bma_hist_card_yes_matrix_t0,
  model_t1 = bma_hist_card_yes_matrix_t1,
  model_t2 = bma_hist_card_yes_matrix_t2,
  model_t3 = bma_hist_card_yes_matrix_t3,
  model_t4 = bma_hist_card_yes_matrix_t4,
  file = "networks_cardiac_history.pdf"
)

#No cardiac history only dataframe
hist_card_no_data <- full_data %>%
    dplyr::filter(card_hist == 0)
data_t0_hist_card_no <- divide_by_time_preprocess(hist_card_no_data, "t0")
data_t1_hist_card_no <- divide_by_time_preprocess(hist_card_no_data, "t1")
data_t2_hist_card_no <- divide_by_time_preprocess(hist_card_no_data, "t2")
data_t3_hist_card_no <- divide_by_time_preprocess(hist_card_no_data, "t3")
data_t4_hist_card_no <- divide_by_time_preprocess(hist_card_no_data, "t4")
#Estimate the BMA network for cardiac history no
fit_hist_card_no_t0 <- estimate_network(data_t0_hist_card_no, posterior_samples = 5000)
fit_hist_card_no_t1 <- estimate_network(data_t1_hist_card_no, posterior_samples = 5000)
fit_hist_card_no_t2 <- estimate_network(data_t2_hist_card_no, posterior_samples = 5000)
fit_hist_card_no_t3 <- estimate_network(data_t3_hist_card_no, posterior_samples = 5000)
fit_hist_card_no_t4 <- estimate_network(data_t4_hist_card_no, posterior_samples = 5000)
# Select the graph for each time point.
graph_hist_card_no_t0 <- BGGM::select(fit_hist_card_no_t0, alternative = "two.sided")
graph_hist_card_no_t1 <- BGGM::select(fit_hist_card_no_t1, alternative = "two.sided")
graph_hist_card_no_t2 <- BGGM::select(fit_hist_card_no_t2, alternative = "two.sided")
graph_hist_card_no_t3 <- BGGM::select(fit_hist_card_no_t3, alternative = "two.sided")
graph_hist_card_no_t4 <- BGGM::select(fit_hist_card_no_t4, alternative = "two.sided")
#Bayesian model averaging
bma_hist_card_no_matrix_t0 <- compute_bma_matrix(fit_hist_card_no_t0, graph_hist_card_no_t0)
bma_hist_card_no_matrix_t1 <- compute_bma_matrix(fit_hist_card_no_t1, graph_hist_card_no_t1)
bma_hist_card_no_matrix_t2 <- compute_bma_matrix(fit_hist_card_no_t2, graph_hist_card_no_t2)
bma_hist_card_no_matrix_t3 <- compute_bma_matrix(fit_hist_card_no_t3, graph_hist_card_no_t3)
bma_hist_card_no_matrix_t4 <- compute_bma_matrix(fit_hist_card_no_t4, graph_hist_card_no_t4)
#Rename the columns and rows of the BMA
bma_hist_card_no_matrix_t0 <- rename_col_row(bma_hist_card_no_matrix_t0)
bma_hist_card_no_matrix_t1 <- rename_col_row(bma_hist_card_no_matrix_t1)
bma_hist_card_no_matrix_t2 <- rename_col_row(bma_hist_card_no_matrix_t2)
bma_hist_card_no_matrix_t3 <- rename_col_row(bma_hist_card_no_matrix_t3)
bma_hist_card_no_matrix_t4 <- rename_col_row(bma_hist_card_no_matrix_t4)
#Plotting
plot_networks(
  model_t0 = bma_hist_card_no_matrix_t0,
  model_t1 = bma_hist_card_no_matrix_t1,
  model_t2 = bma_hist_card_no_matrix_t2,
  model_t3 = bma_hist_card_no_matrix_t3,
  model_t4 = bma_hist_card_no_matrix_t4,
  file = "networks_no_cardiac_history.pdf"
)

#Psychiatric history: Yes only dataframe
psychiatric_hist_yes_data <- full_data %>%
    dplyr::filter(psychiatric_composite == 1)
data_t0_psychiatric_hist_yes <- divide_by_time_preprocess(psychiatric_hist_yes_data, "t0")
data_t1_psychiatric_hist_yes <- divide_by_time_preprocess(psychiatric_hist_yes_data, "t1")
data_t2_psychiatric_hist_yes <- divide_by_time_preprocess(psychiatric_hist_yes_data, "t2")
data_t3_psychiatric_hist_yes <- divide_by_time_preprocess(psychiatric_hist_yes_data, "t3")
data_t4_psychiatric_hist_yes <- divide_by_time_preprocess(psychiatric_hist_yes_data, "t4")
#Estimate the BMA network for psychiatric history yes
fit_psychiatric_hist_yes_t0 <- estimate_network(data_t0_psychiatric_hist_yes, posterior_samples = 5000)
fit_psychiatric_hist_yes_t1 <- estimate_network(data_t1_psychiatric_hist_yes, posterior_samples = 5000)
fit_psychiatric_hist_yes_t2 <- estimate_network(data_t2_psychiatric_hist_yes, posterior_samples = 5000)
fit_psychiatric_hist_yes_t3 <- estimate_network(data_t3_psychiatric_hist_yes, posterior_samples = 5000)
fit_psychiatric_hist_yes_t4 <- estimate_network(data_t4_psychiatric_hist_yes, posterior_samples = 5000)
# Select the graph for each time point.
graph_psychiatric_hist_yes_t0 <- BGGM::select(fit_psychiatric_hist_yes_t0, alternative = "two.sided")
graph_psychiatric_hist_yes_t1 <- BGGM::select(fit_psychiatric_hist_yes_t1, alternative = "two.sided")
graph_psychiatric_hist_yes_t2 <- BGGM::select(fit_psychiatric_hist_yes_t2, alternative = "two.sided")
graph_psychiatric_hist_yes_t3 <- BGGM::select(fit_psychiatric_hist_yes_t3, alternative = "two.sided")
graph_psychiatric_hist_yes_t4 <- BGGM::select(fit_psychiatric_hist_yes_t4, alternative = "two.sided")
#Bayesian model averaging
bma_psychiatric_hist_yes_matrix_t0 <- compute_bma_matrix(fit_psychiatric_hist_yes_t0, graph_psychiatric_hist_yes_t0)
bma_psychiatric_hist_yes_matrix_t1 <- compute_bma_matrix(fit_psychiatric_hist_yes_t1, graph_psychiatric_hist_yes_t1)
bma_psychiatric_hist_yes_matrix_t2 <- compute_bma_matrix(fit_psychiatric_hist_yes_t2, graph_psychiatric_hist_yes_t2)
bma_psychiatric_hist_yes_matrix_t3 <- compute_bma_matrix(fit_psychiatric_hist_yes_t3, graph_psychiatric_hist_yes_t3)
bma_psychiatric_hist_yes_matrix_t4 <- compute_bma_matrix(fit_psychiatric_hist_yes_t4, graph_psychiatric_hist_yes_t4)
#Rename the columns and rows of the BMA
bma_psychiatric_hist_yes_matrix_t0 <- rename_col_row(bma_psychiatric_hist_yes_matrix_t0)
bma_psychiatric_hist_yes_matrix_t1 <- rename_col_row(bma_psychiatric_hist_yes_matrix_t1)
bma_psychiatric_hist_yes_matrix_t2 <- rename_col_row(bma_psychiatric_hist_yes_matrix_t2)
bma_psychiatric_hist_yes_matrix_t3 <- rename_col_row(bma_psychiatric_hist_yes_matrix_t3)
bma_psychiatric_hist_yes_matrix_t4 <- rename_col_row(bma_psychiatric_hist_yes_matrix_t4)
#Plotting
plot_networks(
  model_t0 = bma_psychiatric_hist_yes_matrix_t0,
  model_t1 = bma_psychiatric_hist_yes_matrix_t1,
  model_t2 = bma_psychiatric_hist_yes_matrix_t2,
  model_t3 = bma_psychiatric_hist_yes_matrix_t3,
  model_t4 = bma_psychiatric_hist_yes_matrix_t4,
  file = "networks_psychiatric_history.pdf"
)


#Psychiatric history: No only dataframe
psychiatric_hist_no_data <- full_data %>%
    dplyr::filter(psychiatric_composite == 2)
data_t0_psychiatric_hist_no <- divide_by_time_preprocess(psychiatric_hist_no_data, "t0")
data_t1_psychiatric_hist_no <- divide_by_time_preprocess(psychiatric_hist_no_data, "t1")
data_t2_psychiatric_hist_no <- divide_by_time_preprocess(psychiatric_hist_no_data, "t2")
data_t3_psychiatric_hist_no <- divide_by_time_preprocess(psychiatric_hist_no_data, "t3")
data_t4_psychiatric_hist_no <- divide_by_time_preprocess(psychiatric_hist_no_data, "t4")

#Estimate the BMA network for psychiatric history no
fit_psychiatric_hist_no_t0 <- estimate_network(data_t0_psychiatric_hist_no, posterior_samples = 5000)
fit_psychiatric_hist_no_t1 <- estimate_network(data_t1_psychiatric_hist_no, posterior_samples = 5000)
fit_psychiatric_hist_no_t2 <- estimate_network(data_t2_psychiatric_hist_no, posterior_samples = 5000)
fit_psychiatric_hist_no_t3 <- estimate_network(data_t3_psychiatric_hist_no, posterior_samples = 5000)
#THIS ONE DOESN'T WORK PROPERLY BECAUSE OF LOW SAMPLE SIZE IN THIS GROUP AT T4
fit_psychiatric_hist_no_t4 <- estimate_network(data_t4_psychiatric_hist_no, posterior_samples = 5000)

# Select the graph for each time point.
graph_psychiatric_hist_no_t0 <- BGGM::select(fit_psychiatric_hist_no_t0, alternative = "two.sided")
graph_psychiatric_hist_no_t1 <- BGGM::select(fit_psychiatric_hist_no_t1, alternative = "two.sided")
graph_psychiatric_hist_no_t2 <- BGGM::select(fit_psychiatric_hist_no_t2, alternative = "two.sided")
graph_psychiatric_hist_no_t3 <- BGGM::select(fit_psychiatric_hist_no_t3, alternative = "two.sided")
graph_psychiatric_hist_no_t4 <- BGGM::select(fit_psychiatric_hist_no_t4, alternative = "two.sided")
#Bayesian model averaging
bma_psychiatric_hist_no_matrix_t0 <- compute_bma_matrix(fit_psychiatric_hist_no_t0, graph_psychiatric_hist_no_t0)
bma_psychiatric_hist_no_matrix_t1 <- compute_bma_matrix(fit_psychiatric_hist_no_t1, graph_psychiatric_hist_no_t1)
bma_psychiatric_hist_no_matrix_t2 <- compute_bma_matrix(fit_psychiatric_hist_no_t2, graph_psychiatric_hist_no_t2)
bma_psychiatric_hist_no_matrix_t3 <- compute_bma_matrix(fit_psychiatric_hist_no_t3, graph_psychiatric_hist_no_t3)
bma_psychiatric_hist_no_matrix_t4 <- compute_bma_matrix(fit_psychiatric_hist_no_t4, graph_psychiatric_hist_no_t4)
#Rename the columns and rows of the BMA
bma_psychiatric_hist_no_matrix_t0 <- rename_col_row(bma_psychiatric_hist_no_matrix_t0)
bma_psychiatric_hist_no_matrix_t1 <- rename_col_row(bma_psychiatric_hist_no_matrix_t1)
bma_psychiatric_hist_no_matrix_t2 <- rename_col_row(bma_psychiatric_hist_no_matrix_t2)
bma_psychiatric_hist_no_matrix_t3 <- rename_col_row(bma_psychiatric_hist_no_matrix_t3)
bma_psychiatric_hist_no_matrix_t4 <- rename_col_row(bma_psychiatric_hist_no_matrix_t4)
#Plotting
plot_networks(
  model_t0 = bma_psychiatric_hist_no_matrix_t0,
  model_t1 = bma_psychiatric_hist_no_matrix_t1,
  model_t2 = bma_psychiatric_hist_no_matrix_t2,
  model_t3 = bma_psychiatric_hist_no_matrix_t3,
  file = "networks_no_psychiatric_history.pdf"
)


#Comunity Detection using weighted Clique Percolation Method

#Choose k (clique size) and i (intensity) that maximizes entropy
threshold_t0 <- CliquePercolation::cpThreshold(
    W = bma_matrix_t0,
    method = "weighted",
    k.range = 3:15,
    I.range = c(seq(0.35, 0.01, by = -0.01)),
    threshold = "entropy"
)
#Check results
threshold_t0
#Optimal values at baseline
threshold_t0_optimal_k <- 3
threshold_t0_optimal_i <- 0.15

threshold_t1 <- CliquePercolation::cpThreshold(
    W = bma_matrix_t1,
    method = "weighted",
    k.range = 3:15,
    I.range = c(seq(0.35, 0.01, by = -0.01)),
    threshold = "entropy"
)
threshold_t1
#Optimal values at 1 month
threshold_t1_optimal_k <- 3
threshold_t1_optimal_i <- 0.11

threshold_t2 <- CliquePercolation::cpThreshold(
    W = bma_matrix_t2,
    method = "weighted",
    k.range = 3:15,
    I.range = c(seq(0.35, 0.01, by = -0.01)),
    threshold = "entropy"
)
threshold_t2
threshold_t2_optimal_k <- 3
threshold_t2_optimal_i <- 0.16


threshold_t3 <- CliquePercolation::cpThreshold(
    W = bma_matrix_t3,
    method = "weighted",
    k.range = 3:15,
    I.range = c(seq(0.35, 0.01, by = -0.01)),
    threshold = "entropy"
)
threshold_t3
threshold_t3_optimal_k <- 3
threshold_t3_optimal_i <- 0.14

threshold_t4 <- CliquePercolation::cpThreshold(
    W = bma_matrix_t4,
    method = "weighted",
    k.range = 3:15,
    I.range = c(seq(0.35, 0.01, by = -0.01)),
    threshold = "entropy"
)
threshold_t4
threshold_t4_optimal_k <- 3
threshold_t4_optimal_i <- 0.13


#Permutation test to ensure that the entropy values are not due to chance (number of permutatations can be increased)
permute_threshold_t0 <-cpPermuteEntropy(
    W = bma_matrix_t0,
    cpThreshold.object = threshold_t0,
    n = 200,
    CFinder = FALSE,
    interval = 0.95,
    ncores = 7
)

# Inspect the permutation test results for the first time point.
permute_threshold_t0

permute_threshold_t1 <- CliquePercolation::cpPermuteEntropy(
    W = bma_matrix_t1,
    cpThreshold.object = threshold_t1,
    n = 2000,
    interval = 0.95,
    ncores = 7
)
permute_threshold_t1

permute_threshold_t2 <- CliquePercolation::cpPermuteEntropy(
    W = bma_matrix_t2,
    cpThreshold.object = threshold_t2,
    n = 2000,
    interval = 0.95,
    ncores = 7
)
permute_threshold_t2

permute_threshold_t3 <- CliquePercolation::cpPermuteEntropy(
    W = bma_matrix_t3,
    cpThreshold.object = threshold_t3,
    n = 2000,
    interval = 0.95,
    ncores = 7
)
permute_threshold_t3

permute_threshold_t4 <- CliquePercolation::cpPermuteEntropy(
    W = bma_matrix_t4,
    cpThreshold.object = threshold_t4,
    n = 2000,
    interval = 0.95,
    ncores = 7
)
permute_threshold_t4

# Run the clique percolation algorithm with the optimal k values
cp_t0 <- CliquePercolation::cpAlgorithm(
    W = bma_matrix_t0,
    method = "weighted",
    k = threshold_t0_optimal_k,
    I = threshold_t0_optimal_i
)
summary(cp_t0)

cp_t1 <- CliquePercolation::cpAlgorithm(
    W = bma_matrix_t1,
    method = "weighted",
    k = threshold_t1_optimal_k,
    I = threshold_t1_optimal_i
)
summary(cp_t1)
cp_t2 <- CliquePercolation::cpAlgorithm(
    W = bma_matrix_t2,
    method = "weighted",
    k = threshold_t2_optimal_k,
    I = threshold_t2_optimal_i
)
summary(cp_t2)
cp_t3 <- CliquePercolation::cpAlgorithm(
    W = bma_matrix_t3,
    method = "weighted",
    k = threshold_t3_optimal_k,
    I = threshold_t3_optimal_i
)
summary(cp_t3)
cp_t4 <- CliquePercolation::cpAlgorithm(
    W = bma_matrix_t4,
    method = "weighted",
    k = threshold_t4_optimal_k,
    I = threshold_t4_optimal_i
)
summary(cp_t4)

#Visualize the communities
# Plot the CPA results for a given graph and CPA results.
plot_cpa <- function(graph, cpa_results, title) {
    # Extract the community assignments.
    communities <- lapply(cpa_results$list.of.communities.numbers,
        function(community_nodes) {
            # Extract the node numbers.
            return(community_nodes)
        }
    )

    # Generate names for the communities.
    community_names <- paste("Community", seq_along(communities))

    # If there are no communities.
    if (length(communities) != 0) {
        # Apply the names to the communities.
        names(communities) <- community_names
    }

    # Append the isolated nodes to the communities.
    communities <- c(
        communities,
        list(
            "Isolated Nodes" = cpa_results$isolated.nodes.numbers
        )
    )

    # Generate colours for the communities.
    colors <- grDevices::palette.colors(
        # The number of colors.
        n = length(communities) - 1,

        # The palette to use.
        palette = "Pastel 1"
    )

    # Define the community labels.
    community_list <- cpa_results$list.of.communities.labels

    # If we are dealing only with isolated nodes.
    if (length(communities) == 1) {
        # Set the list of communities to be empty.
        community_list <- NA

        # Remove the colors.
        colors <- NA
    }

    # Plot the graph.
    CliquePercolation::cpColoredGraph(
        # The graph to plot.
        W = graph,

        # The communities to plot.
        list.of.communities = community_list,

        # The specific colors of the nodes.
        own.colors = colors,

        # The layout of the graph.
        layout = "circle",

        # The theme of the graph.
        theme = "colorblind",

        # The node names.
        nodeNames = long_names,

        # The groups of nodes.
        groups = communities,

        # The colors of the nodes.
        color = colors,

        # Graph visualization parameters.
        posCol = .__COLORS__$edge_positive,
        negCol = .__COLORS__$edge_negative,
        edge.width = .8,
        border.width = 1.5,
        cut = 0,
        vsize = 10,

        # Legend sizing when plots are side-by-side.
        legend = TRUE,
        legend.mode = "style1",
        legend.cex = .519,
        GLratio = 1.9
    )

    # Add title.
    title(
        main = title,
        line = 4,
        cex.main = 1.32,
        adj = 0
    )
}

# Plot the CPA networks to a file.
plot_networks_cpa <- function(..., file, width = 15.21, height = 18.2) {
    # Extract the models to plot.
    models <- list(...)

    # Extract number of true models provided.
    n_models <- length(models)

    # Extract names.
    names <- sub("model_", "", names(models))

    # Specify the titles for the plots.
    titles <- c("Baseline", "1 month", "6 months", "12 months", "24 months")

    # Start the printing device.
    pdf(file, width = width, height = height)

    # Outer margins bottom, left, top, right
    par(oma = c(0, 0, 1.5, 0))

    # Inner margins for each panel.
    par(mar = c(6, 5, 6, 5))

    # Specify the layout.
    matrix_layout <- matrix(
        c(
            # Row 1.
            1, 1, 2, 2,

            # Spacer row.
            0, 0, 0, 0,

            # Row 2.
            3, 3, 4, 4,

            # Spacer row.
            0, 0, 0, 0,

            # Row 3, centered.
            0, 5, 5, 0
        ),
        nrow = 5,
        byrow = TRUE
    )

    # Set the layout with specified widths and heights.
    layout(matrix_layout, widths = rep(1, 4), heights = c(1, 0.15, 1, 0.15, 1))

    # Print each true model in turn.
    for (i in 1:n_models) {
        # Not a good idea, but extract the model to avoid indexing.
        model <- models[[i]]

        # Plot CPA.
        plot_cpa(
            graph = model$graph,
            cpa_results = model$cpa_results,
            title = titles[i]
        )
    }

    # Reset layout.
    layout(1:1)

    # Turn off.
    dev.off()

    # Remain silent.
    invisible()
}

# Plot the networks for all time points.
plot_networks_cpa(
    model_t0 = list(graph = bma_matrix_t0, cpa_results = cp_t0),
    model_t1 = list(graph = bma_matrix_t1, cpa_results = cp_t1),
    model_t2 = list(graph = bma_matrix_t2, cpa_results = cp_t2),
    model_t3 = list(graph = bma_matrix_t3, cpa_results = cp_t3),
    model_t4 = list(graph = bma_matrix_t4, cpa_results = cp_t4),
    file = "networks_cp_communities.pdf"
)


#Caculate the partial correlation matrices
library(ppcor)
data_t0_copy <- na.omit(data_t0)
partial_cor_matrix_t0 <- pcor(data_t0_copy, method="pearson")

data_t1_copy <- na.omit(data_t1)
partial_cor_matrix_t1 <- pcor(data_t1_copy, method="pearson")

data_t2_copy <- na.omit(data_t2)
partial_cor_matrix_t2 <- pcor(data_t2_copy, method="pearson")

data_t3_copy <- na.omit(data_t3)
partial_cor_matrix_t3 <- pcor(data_t3_copy, method="pearson")

data_t4_copy <- na.omit(data_t4)
partial_cor_matrix_t4 <- pcor(data_t4_copy, method="pearson")


#Network descriptive statistics
#Keep only the upper triangle of the matrices to avoid double counting edges and exclude the main diagonal (self-connections)
bma_matrix_t0_upper <- bma_matrix_t0[upper.tri(bma_matrix_t0,diag = FALSE)]
bma_matrix_t1_upper <- bma_matrix_t1[upper.tri(bma_matrix_t1,diag = FALSE)]
bma_matrix_t2_upper <- bma_matrix_t2[upper.tri(bma_matrix_t2,diag = FALSE)]
bma_matrix_t3_upper <- bma_matrix_t3[upper.tri(bma_matrix_t3,diag = FALSE)]
bma_matrix_t4_upper <- bma_matrix_t4[upper.tri(bma_matrix_t4,diag = FALSE)]
#Density
mean(bma_matrix_t0_upper!=0)
mean(bma_matrix_t1_upper!=0)
mean(bma_matrix_t2_upper!=0)
mean(bma_matrix_t3_upper!=0)
mean(bma_matrix_t4_upper!=0)
#Number of edges estimated to be zero (120 is the total number of possible edges)
sum(bma_matrix_t0_upper==0)
sum(bma_matrix_t1_upper==0)
sum(bma_matrix_t2_upper==0)
sum(bma_matrix_t3_upper==0)
sum(bma_matrix_t4_upper==0)

#Global strength (sum of absolute edge weights)
sum(abs(bma_matrix_t0_upper))
sum(abs(bma_matrix_t1_upper))
sum(abs(bma_matrix_t2_upper))
sum(abs(bma_matrix_t3_upper))
sum(abs(bma_matrix_t4_upper))

#Centrality measures
library(igraph)

#Create igraph objects from the BMA matrices
bma_graph_t0 <- graph_from_adjacency_matrix(bma_matrix_t0, mode = "undirected", weighted = TRUE)
bma_graph_t1 <- graph_from_adjacency_matrix(bma_matrix_t1, mode = "undirected", weighted = TRUE)
bma_graph_t2 <- graph_from_adjacency_matrix(bma_matrix_t2, mode = "undirected", weighted = TRUE)
bma_graph_t3 <- graph_from_adjacency_matrix(bma_matrix_t3, mode = "undirected", weighted = TRUE)
bma_graph_t4 <- graph_from_adjacency_matrix(bma_matrix_t4, mode = "undirected", weighted = TRUE)


#Degree centrality without considering edge weights (number of connections)
degree.cent_t0 <- igraph::degree(bma_graph_t0)
degree.cent_t1 <- igraph::degree(bma_graph_t1)
degree.cent_t2 <- igraph::degree(bma_graph_t2)
degree.cent_t3 <- igraph::degree(bma_graph_t3)
degree.cent_t4 <- igraph::degree(bma_graph_t4)

#Weighted degree aka strength centrality (sum of edge weights)
weighted.degree.cent_t0 <- strength(bma_graph_t0)
which.max(weighted.degree.cent_t0) #GAD4
weighted.degree.cent_t1 <- strength(bma_graph_t1)
which.max(weighted.degree.cent_t1) #PHQ2
weighted.degree.cent_t2 <- strength(bma_graph_t2)
which.max(weighted.degree.cent_t2) #GAD4
weighted.degree.cent_t3 <- strength(bma_graph_t3)
which.max(weighted.degree.cent_t3) #PHQ4
weighted.degree.cent_t4 <- strength(bma_graph_t4)
which.max(weighted.degree.cent_t4) #PHQ4

#Subgraph centrality
which.max(subgraph_centrality(bma_graph_t0)) #PHQ3
which.max(subgraph_centrality(bma_graph_t1)) #PHQ6
which.max(subgraph_centrality(bma_graph_t2)) #PHQ4
which.max(subgraph_centrality(bma_graph_t3)) #PHQ4
which.max(subgraph_centrality(bma_graph_t4)) #GAD3



#Closeness centrality (cannot use negative edges so firstly we need to convert the BMA graphs in adjancecy graphs)
#Adjacency matrix (keep the matrix form by doing matrix operations)
bma_matrix_t0_adj <- (bma_matrix_t0 != 0) * 1
bma_matrix_t1_adj <- (bma_matrix_t1 != 0) * 1
bma_matrix_t2_adj <- (bma_matrix_t2 != 0) * 1
bma_matrix_t3_adj <- (bma_matrix_t3 != 0) * 1
bma_matrix_t4_adj <- (bma_matrix_t4 != 0) * 1
#Convert to igraph objects
bma_graph_t0_adj <- graph_from_adjacency_matrix(bma_matrix_t0_adj, mode = "undirected", weighted = FALSE)
bma_graph_t1_adj <- graph_from_adjacency_matrix(bma_matrix_t1_adj, mode = "undirected", weighted = FALSE)
bma_graph_t2_adj <- graph_from_adjacency_matrix(bma_matrix_t2_adj, mode = "undirected", weighted = FALSE)
bma_graph_t3_adj <- graph_from_adjacency_matrix(bma_matrix_t3_adj, mode = "undirected", weighted = FALSE)
bma_graph_t4_adj <- graph_from_adjacency_matrix(bma_matrix_t4_adj, mode = "undirected", weighted = FALSE)
#Closeness centrality (number of steps to reach all other nodes)
closeness.cent_t0 <- closeness(bma_graph_t0_adj, mode="all")
which.max(closeness.cent_t0) #PHQ1
closeness.cent_t1 <- closeness(bma_graph_t1_adj, mode="all")
which.max(closeness.cent_t1) #PHQ2
closeness.cent_t2 <- closeness(bma_graph_t2_adj, mode="all")
which.max(closeness.cent_t2) #PHQ4
closeness.cent_t3 <- closeness(bma_graph_t3_adj, mode="all")
which.max(closeness.cent_t3) #PHQ3
closeness.cent_t4 <- closeness(bma_graph_t4_adj, mode="all")
which.max(closeness.cent_t4) #PHQ1

#Betweenness centrality
betweenness.cent_t0 <- betweenness(bma_graph_t0_adj, directed = FALSE)
which.max(betweenness.cent_t0) #GAD5
betweenness.cent_t1 <- betweenness(bma_graph_t1_adj, directed = FALSE)
which.max(betweenness.cent_t1) #PHQ2
betweenness.cent_t2 <- betweenness(bma_graph_t2_adj, directed = FALSE)
which.max(betweenness.cent_t2) #GAD4
betweenness.cent_t3 <- betweenness(bma_graph_t3_adj, directed = FALSE)
which.max(betweenness.cent_t3) #PHQ3
betweenness.cent_t4 <- betweenness(bma_graph_t4_adj, directed = FALSE)
which.max(betweenness.cent_t4) #PHQ1

# Save everything in the current environment
save.image(file = "workspace.RData")


# Save all objects in the current environment to an RDS file
saveRDS(y, file = "y_object.rds")

# Combine all objects in the current environment into a list
all_objects <- mget(ls())

# Save the list as a single RDS file
saveRDS(all_objects, file = "all_objects.rds")
