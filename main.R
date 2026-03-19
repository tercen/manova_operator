library(tercen)
library(dplyr)

ctx <- tercenCtx()

# Validate inputs
if (length(ctx$colors) < 1) stop("A color factor is required for grouping.")

# Get parameters
test_stat <- ctx$op.value("test_statistic", type = as.character, default = "Wilks")
method    <- ctx$op.value("method", type = as.character, default = "one.way")

# Get data as matrix (variables x observations)
mat <- ctx$as.matrix()

# Transpose: rows = observations, columns = variables (what manova() expects)
Y <- t(mat)

# Get group factor(s) from column space
color_df <- ctx$cselect(ctx$colors)
group1 <- as.factor(color_df[[1]])

# Build model formula and fit
if (method == "two.way" && length(ctx$labels) > 0) {
  label_df <- ctx$cselect(ctx$labels)
  group2 <- as.factor(label_df[[1]])
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
