library(tercen)
library(dplyr)
library(tidyr)

ctx <- tercenCtx()

# Validate inputs
if (length(ctx$colors) < 1) stop("A color factor is required for grouping.")

# Get parameters
test_stat <- ctx$op.value("test_statistic", type = as.character, default = "Wilks")
method    <- ctx$op.value("method", type = as.character, default = "one.way")

# Get data with color factors using ctx$select()
col_names <- unlist(ctx$colors)
lab_names <- unlist(ctx$labels)
select_cols <- c(".ci", ".ri", ".y", col_names, lab_names)
df <- ctx$select(select_cols) %>% as_tibble()

# Each row in df is one cell: .ci (observation) × .ri (variable) × .y (value)
# Colors annotate observations — each .ci+color combo is a unique data point
# Create a unique observation ID from .ci + all color/label factors
obs_cols <- c(".ci", col_names, lab_names)
df <- df %>%
  mutate(.obs_id = as.integer(factor(interaction(!!!syms(obs_cols)))))

# Pivot to matrix: one row per observation, one column per variable
Y_df <- df %>%
  select(.obs_id, .ri, .y) %>%
  pivot_wider(names_from = .ri, values_from = .y, values_fn = mean) %>%
  arrange(.obs_id)
Y <- Y_df %>% select(-.obs_id) %>% as.matrix()

# Get group factors per observation
group_df <- df %>%
  select(.obs_id, all_of(col_names), all_of(lab_names)) %>%
  distinct() %>%
  arrange(.obs_id)

group1 <- as.factor(group_df[[col_names[1]]])

# Build model formula and fit
if (method == "two.way" && length(lab_names) > 0) {
  group2 <- as.factor(group_df[[lab_names[1]]])
  fit <- manova(Y ~ group1 * group2)
} else {
  fit <- manova(Y ~ group1)
}

# Extract MANOVA summary
manova_summary <- summary(fit, test = test_stat)
manova_table   <- manova_summary$stats

# Build output data frame
effects <- rownames(manova_table)
# Remove "Residuals" row
keep <- effects != "Residuals"

sig_code <- function(p) {
  ifelse(p < 0.001, "***",
    ifelse(p < 0.01, "**",
      ifelse(p < 0.05, "*",
        ifelse(p < 0.1, ".", "ns"))))
}

result <- data.frame(
  Effect           = effects[keep],
  test_statistic   = test_stat,
  statistic_value  = as.double(manova_table[keep, 2]),
  approx_F         = as.double(manova_table[keep, 3]),
  num_df           = as.double(manova_table[keep, 4]),
  den_df           = as.double(manova_table[keep, 5]),
  p_value          = as.double(manova_table[keep, 6]),
  stringsAsFactors = FALSE
)
result$significance <- sig_code(result$p_value)

# Return result as flat summary (empty join keys)
# .ci and .ri are required even for global summaries
result$.ci <- 0L
result$.ri <- 0L

result %>%
  ctx$addNamespace() %>%
  ctx$save()
