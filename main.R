suppressPackageStartupMessages({
  library(tercen)
  library(dplyr, warn.conflicts = FALSE)
  library(cyCombine)
  library(magrittr)
  library(tidyr)
})

ctx <- tercenCtx()

seed <- ctx$op.value('seed', as.integer, 42)
if(seed < 0) seed <- NULL

norm_method <- ctx$op.value("norm_method", as.character, "scale")

data.all <- ctx$select(unlist(list(".y", ".ri", ".ci", ctx$colors, ctx$labels)))

condition_column <- unlist(ctx$colors)
batch_column <- unlist(ctx$labels)

stopifnot("Labels must contains one factor (batch)" = length(batch_column) == 1)
stopifnot("A single condition factor must be provided as a color." = length(condition_column) < 2)

rnames <- ctx$rselect() %>%
  mutate(.ri = seq_len(nrow(.)) - 1L)

data <- data.all %>%
  rename(batch = all_of(batch_column), condition = all_of(condition_column)) %>%
  left_join(rnames, by = ".ri") %>%
  pivot_wider(
    id_cols = matches(".ci|batch|condition"),
    names_from = ctx$rnames[[1]],
    values_from =".y"
  ) %>%
  drop_na()

# Run batch correction
labels <- data %>%
  normalize(markers = rnames[[1]], norm_method = norm_method) %>%
  create_som(
    markers = rnames[[1]],
    seed = seed,
    rlen = 10,
    xdim = 8,
    ydim = 8
  ) 

if(is.null(condition_column)) {
  covar <- NULL
} else {
  covar <- "condition"
}

corrected <- suppressMessages({
  data %>%
    correct_data(
      label = labels,
      markers = rnames[[1]],
      anchor = NULL,
      covar = covar,
      parametric = TRUE
    )
})

df_out <- corrected %>%
  select(!matches("id|batch|condition|label")) %>%
  pivot_longer(!.ci, names_to = "variable", values_to = "value") %>%
  left_join(rnames, by = c("variable" = ctx$rnames[[1]])) %>%
  select(-variable) %>%
  ctx$addNamespace() %>%
  ctx$save()
