library(tercen)
library(dplyr, warn.conflicts = FALSE)
library(cyCombine)
library(magrittr)
library(tidyverse)

options("tercen.workflowId" = "9b27ac887a201c952cd90da2e300e645")
options("tercen.stepId"     = "005df533-f268-4780-a0a7-076e46d9c66d")

ctx = tercenCtx()

data.all<-ctx$select(unlist(list(".y",".ci",".ri",ctx$colors,ctx$labels)))
#data.all<-data.all[c(0:10000),]
data <- data.all[0:3] %>% 
  pivot_wider(id_cols=".ci",names_from= ".ri", values_from =".y")

colnames(data) <- c(".ri",ctx$rselect()[[1]])
markers<-colnames(data)

uncorrected.all<-merge(x=data,y=data.all[,3:ncol(data.all)],by = ".ri")

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
