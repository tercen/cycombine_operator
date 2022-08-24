library(tercen)
library(dplyr, warn.conflicts = FALSE)
library(cyCombine)
library(magrittr)
library(tidyverse)

# Directory containing .fcs files
data_dir <- "./FR-FCM-Z52G"
# Markers of interest
markers <- c("CD20", "CD3", "CD27", "CD45RA", "CD279", "CD5", "CD19", "CD14", "CD45RO", "GranzymeA", "GranzymeK", "FCRL6", "CD355", "CD152", "CD69", "CD33", "CD4", "CD337", "CD8", "CD197", "LAG3", "CD56", "CD137", "CD161", "FoxP3", "CD80", "CD270", "CD275", "CD134", "CD278", "CD127", "KLRG1", "CD25", "HLADR", "TBet", "XCL1")

metadata_file <- file.path(data_dir, "Metadata.csv") # Can also be .xlsx

# Prepare a tibble from directory of FCS files
uncorrected <- prepare_data(
  data_dir = data_dir,
  metadata = metadata_file, 
  filename_col = "FCS_name",
  batch_ids = "Batch",
  condition = "Condition",
  down_sample = FALSE,
  markers = markers
)

# Transform data
uncorrected <- df %>% 
  transform_asinh(markers = markers)

options("tercen.workflowId" = "9b27ac887a201c952cd90da2e300e645")
options("tercen.stepId"     = "005df533-f268-4780-a0a7-076e46d9c66d")
ctx = tercenCtx()

data.all<-ctx$select(unlist(list(".y",".ci",".ri",ctx$colors,ctx$labels)))
data.all<-data.all[c(0:10000),]
data <- data.all[0:3] %>% 
  pivot_wider(id_cols=".ci",names_from= ".ri", values_from =".y")

colnames(data) <- c(".ri",ctx$rselect()[[1]])
marker<-colnames(data)
uncorrected.all<-merge(x=data,y=data.all[,3:ncol(data.all)],by = ".ri")

colnames(uncorrected.all)

colnames(uncorrected.all)[grep(pattern = "atch",x = colnames(uncorrected.all))]<-"batch"
colnames(uncorrected.all)[grep(pattern = "ondition",x = colnames(uncorrected.all))]<-"condition"

# Run batch correction
labels <- uncorrected.all %>%
  normalize(markers = markers,
            norm_method = "scale") %>%
  create_som(markers = markers,
             rlen = 10)

corrected <- uncorrected.all %>%
  correct_data(label = labels,
               covar = "condition")
corrected%>%
ctx$addNamespace() %>%
  ctx$save()
