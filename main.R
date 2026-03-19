library(tercen)
library(dplyr)
library(tidyr)

ctx <- tercenCtx()

# Validate inputs
if (length(ctx$colors) < 1) stop("A color factor is required for grouping.")

# Get parameters
test_stat <- ctx$op.value("test_statistic", type = as.character, default = "Wilks")
method    <- ctx$op.value("method", type = as.character, default = "one.way")

# Get data with color factors using ctx$select() (not ctx$as.matrix() + ctx$cselect())
col_names <- unlist(ctx$colors)
lab_names <- unlist(ctx$labels)
select_cols <- c(".ci", ".ri", ".y", col_names, lab_names)
df <- ctx$select(select_cols) %>% as_tibble()

# Pivot to matrix: each .ci is an observation, each .ri is a variable
# values_fn = mean handles duplicate .ci/.ri combinations
Y <- df %>%
  select(.ci, .ri, .y) %>%
  pivot_wider(names_from = .ri, values_from = .y, values_fn = mean) %>%
  select(-.ci) %>%
  as.matrix()

# Get group factor(s) — one value per observation (.ci)
group_df <- df %>%
  select(.ci, all_of(col_names)) %>%
  distinct()
group1 <- as.factor(group_df[[col_names[1]]])

# Build model formula and fit
if (method == "two.way" && length(lab_names) > 0) {
  label_df <- df %>%
    select(.ci, all_of(lab_names)) %>%
    distinct()
  group2 <- as.factor(label_df[[lab_names[1]]])
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
result %>%
  ctx$addNamespace() %>%
  ctx$save()
