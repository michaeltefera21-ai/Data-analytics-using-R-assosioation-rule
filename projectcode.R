# Author: MichaelTefera
# Final Project - Association Rule Mining with R
# ID: 79451

# ------------------------------
# 1. LOADing REQUIRED LIBRARIES
# ------------------------------
library(tidyverse)    # Data manipulation
library(arules)       # Association rules
library(arulesViz)    # Rule visualization
library(RColorBrewer) # Color palettes

# ------------------------------
# 2. DATA LOADING & PREPROCESSING
# ------------------------------
# Read and clean data
df <- read_csv("recipe_transactionsEWDS.csv")

# Personalize dataset using my ID
set.seed(79451)  
df_sample <- df %>% 
  distinct(recipe_id, .keep_all = TRUE) %>%  # Ensures unique transactions
  sample_frac(0.9)                           # 90% sampling

# Clean ingredients and transform to transaction list
df_sample <- df_sample %>%
  separate_rows(ingredients, sep = ",") %>%
  mutate(ingredients = tolower(trimws(ingredients))) %>%
  filter(!is.na(ingredients) & ingredients != "")

# Create transaction object
trans_list <- df_sample %>%
  group_by(recipe_id) %>%
  summarise(items = list(ingredients), .groups = "drop") %>%
  pull(items)

trans <- as(trans_list, "transactions")

# ------------------------------
# 3. DATA DESCRIPTION
# ------------------------------
# Basic statistics
data_description <- list(
  n_transactions = length(trans),
  n_items = length(itemLabels(trans)),
  avg_items_per_trans = mean(size(trans)),
  top_3_items = head(sort(itemFrequency(trans, type = "absolute"), decreasing = TRUE), 3)
)

# Print data description
cat("=== Data Description ===\n")
cat("Number of transactions:", data_description$n_transactions, "\n")
cat("Number of unique items:", data_description$n_items, "\n")
cat("Average items per transaction:", round(data_description$avg_items_per_trans, 2), "\n")
cat("Top 3 most frequent items:\n")
print(data_description$top_3_items)

# Plot distribution of items per transaction
hist(size(trans), 
     main = "Items per Transaction Distribution",
     xlab = "Number of Items",
     col = "lightblue")

# Plot top 10 frequent items
itemFrequencyPlot(trans, topN = 10, type = "absolute", col = "steelblue",
                  main = "Top 10 Frequent Items", xlab = "Items", ylab = "Frequency")

# ------------------------------
# 4. FREQUENT ITEMSETS ANALYSIS
# ------------------------------
get_top_itemsets <- function(trans, len, top_n = 3) {
  itemsets <- apriori(trans, 
                      parameter = list(supp = 0.01, 
                                       target = "frequent itemsets",
                                       minlen = len, 
                                       maxlen = len))
  sort(itemsets, by = "support")[1:top_n]
}

# Get top itemsets
top_1itemsets <- get_top_itemsets(trans, 1)
top_2itemsets <- get_top_itemsets(trans, 2)
top_3itemsets <- get_top_itemsets(trans, 3)

# Print results
cat("\n=== Frequent Itemsets ===\n")
cat("Top 3 1-itemsets:\n")
inspect(top_1itemsets)
cat("\nTop 3 2-itemsets:\n")
inspect(top_2itemsets)
cat("\nTop 3 3-itemsets:\n")
inspect(top_3itemsets)

# ------------------------------
# 5. ASSOCIATION RULES
# ------------------------------
generate_rules <- function(trans, supp, conf) {
  apriori(trans, 
          parameter = list(supp = supp, 
                           conf = conf,
                           minlen = 2,  # At least 2 items to form rules
                           target = "rules"))
}

rules_50conf <- generate_rules(trans, 0.01, 0.5)
rules_40conf <- generate_rules(trans, 0.01, 0.4)

# ------------------------------
# 6. VISUALIZATIONS
# ------------------------------
rule_colors <- brewer.pal(8, "Dark2")

# 1. Top 5 rules graph
plot(head(sort(rules_50conf, by = "lift"), 5), 
     method = "graph", 
     control = list(cex = 0.8),
     main = "Top 5 Rules by Lift")

# 2. Support-Confidence scatterplot
plot(rules_50conf, 
     measure = c("support", "confidence"), 
     shading = "lift",
     col = rule_colors,
     main = "Support vs Confidence (Lift as Color)")

# 3. Matrix plot (lift & confidence)
plot(rules_50conf, 
     method = "matrix", 
     measure = c("lift", "confidence"),
     control = list(cex = 0.8),
     main = "Rules Matrix View (Lift & Confidence)")

# ------------------------------
# 7. RESULTS EXPORT
# ------------------------------
# Save workspace for reproducibility
save.image("association_rules_analysis.RData")

# Save top rules to CSV
write.csv(as(rules_50conf, "data.frame"), 
          "top_rules.csv", 
          row.names = FALSE)

# ------------------------------
# 8. SUPPORT/CONFIDENCE IMPACT
# ------------------------------
cat("\n=== Rule Metrics Comparison ===\n")
cat("Rules with 50% Confidence:", length(rules_50conf), "\n")
cat("Rules with 40% Confidence:", length(rules_40conf), "\n")

cat("\nTop 5 Rules by Lift (50% confidence):\n")
inspect(head(sort(rules_50conf, by = "lift"), 5))

# answeer of support impact
cat("\nNote:\n")
cat("Increasing the minimum support reduces the number of rules,\n")
cat("as it filters out itemsets that occur less frequently.\n")
cat("This makes rules more reliable but may hide rare useful patterns.\n")

