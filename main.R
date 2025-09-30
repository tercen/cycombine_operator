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

# default to NULL
label_factor_names <- NULL
covar_factor_names <- NULL
anchor_factor_names <- NULL

# try getting factors from specs
if(!is.null(specs$inputSpecs) && length(specs$inputSpecs) > 0) {
  metafactors <- specs$inputSpecs[[1]]$metaFactors
  spec_names <- unlist(lapply(metafactors, "[[", "name"))
  
  if(any(grepl("label", spec_names))) {
    label_factors_list <- lapply(metafactors[grepl("label", spec_names)], "[[", "factors")
    if(length(label_factors_list) > 0 && length(label_factors_list[[1]]) > 0) {
      label_factor_names <- lapply(label_factors_list[[1]], "[[", "name") %>% unlist()
    }
  }
  
  if(any(grepl("covar", spec_names))) {
    covar_factors_list <- lapply(metafactors[grepl("covar", spec_names)], "[[", "factors")
    if(length(covar_factors_list) > 0 && length(covar_factors_list[[1]]) > 0) {
      covar_factor_names <- lapply(covar_factors_list[[1]], "[[", "name") %>% unlist()
    }
  }
  
  if(any(grepl("anchor", spec_names))) {
    anchor_factors_list <- lapply(metafactors[grepl("anchor", spec_names)], "[[", "factors")
    if(length(anchor_factors_list) > 0 && length(anchor_factors_list[[1]]) > 0) {
      anchor_factor_names <- lapply(anchor_factors_list[[1]], "[[", "name") %>% unlist()
    }
  }
}

# backward compatibility: if not defined in specs, get from labels and colors
if(is.null(label_factor_names)) {
  label_factor_names <- unlist(ctx$labels)
}
if(is.null(covar_factor_names)) {
  covar_factor_names <- unlist(ctx$colors)
}

has_label <- !is.null(label_factor_names) && length(label_factor_names) > 0
has_covar <- !is.null(covar_factor_names) && length(covar_factor_names) > 0
has_anchor <- !is.null(anchor_factor_names) && length(anchor_factor_names) > 0


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
