suppressPackageStartupMessages({
  library(tercen)
  library(dplyr, warn.conflicts = FALSE)
  library(cyCombine)
  library(magrittr)
  library(tidyr)
})

ctx <- tercenCtx()

# operator specs
specs <- ctx$query$operatorSettings$operatorRef$operatorSpec

if(length(specs$inputSpecs)) {
  metafactors <- specs$inputSpecs[[1]]$metaFactors
  spec_names <- lapply(metafactors, "[[", "name")
  
  label_factors <- lapply(metafactors[grepl("label", unlist(spec_names))], "[[", "factors")[[1]]
  covar_factors <- lapply(metafactors[grepl("covar", unlist(spec_names))], "[[", "factors")[[1]]
  anchor_factors <- lapply(metafactors[grepl("anchor", unlist(spec_names))], "[[", "factors")[[1]]
  
} else {
  metafactors <- NULL
  spec_names <- NULL
  label_factors <- NULL
  covar_factors <- NULL
  anchor_factors <- NULL
}

has_label <- length(label_factors)
if(has_label) label_factor_names <- lapply(label_factors, "[[", "name") %>% unlist()

has_covar <- length(covar_factors)
if(has_covar) covar_factor_names <- lapply(covar_factors, "[[", "name") %>% unlist()

has_anchor <- length(anchor_factors)
if(has_anchor) anchor_factor_names <- lapply(anchor_factors, "[[", "name") %>% unlist()


seed <- ctx$op.value('seed', as.integer, 42)
if(seed < 0) seed <- NULL

norm_method <- ctx$op.value("norm_method", as.character, "scale")


stopifnot("'label' metafactor must be defined." = has_label)

# select all factors
all_factors <- unlist(list(".y", ".ri", ".ci", label_factor_names, covar_factor_names, anchor_factor_names))

data.all <- ctx$select(all_factors)

rnames <- ctx$rselect() %>% 
  mutate(.ri = seq_len(nrow(.)) - 1L)

# prepare data
data <- data.all %>% 
  rename(batch = all_of(label_factor_names))

if(has_covar) data <- data %>% rename(covar = all_of(covar_factor_names))
if(has_anchor) data <- data %>% rename(anchor = all_of(anchor_factor_names))

data <- data %>% 
  left_join(rnames, by = ".ri") %>% 
  pivot_wider(
    id_cols = matches(c(".ci", "batch", "covar", "anchor")),
    names_from = ctx$rnames[[1]],
    values_from = ".y"
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

if(!has_covar) {
  covar <- NULL
} else {
  covar <- "covar"
}

if(!has_anchor) {
  anchor <- NULL
} else {
  anchor <- "anchor"
}

corrected <- suppressMessages({
  data %>% 
    correct_data(
      label = labels,
      markers = rnames[[1]],
      anchor = anchor,
      covar = covar,
      parametric = TRUE
    )
})

df_out <- corrected %>% 
  select(!matches("id|batch|covar|anchor|label")) %>% 
  pivot_longer(!.ci, names_to = "variable", values_to = "value") %>% 
  left_join(rnames, by = c("variable" = ctx$rnames[[1]])) %>% 
  select(-variable) %>% 
  ctx$addNamespace() %>% 
  ctx$save()
