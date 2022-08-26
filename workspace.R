library(tercen)
library(dplyr, warn.conflicts = FALSE)
library(cyCombine)
library(magrittr)
library(tidyverse)

options("tercen.workflowId" = "9b27ac887a201c952cd90da2e300e645")
#options("tercen.stepId"     = "005df533-f268-4780-a0a7-076e46d9c66d")
options("tercen.stepId"     = "cd1c0072-588a-4311-99a0-78a526a8c95e")

ctx <- tercenCtx()

data.all<-ctx$select(unlist(list(".y",".ci",".ri",ctx$colors,ctx$labels)))
#data.all<-data.all[c(0:10000),]
data <- data.all[0:3] %>% 
  pivot_wider(id_cols=".ci",names_from= ".ri", values_from =".y")

colnames(data) <- c(".ci",ctx$rselect()[[1]])
markers<-ctx$rselect()[[1]]

uncorrected.all<-dplyr::full_join(x=data,y=data.all[,2:ncol(data.all)],by = ".ci")

#uncorrected.all<-merge(x=data,y=data.all[,3:ncol(data.all)],by = ".ri")

colnames(uncorrected.all)[grep(pattern = "atch",x = colnames(uncorrected.all))]<-"batch"
colnames(uncorrected.all)[grep(pattern = "ondition",x = colnames(uncorrected.all))]<-"condition"

#uncorrected.all<-uncorrected.all %>% drop_na(batch)
uncorrected.all<-uncorrected.all %>% drop_na()

uncorrected<-select(uncorrected.all, -.ri)
uncorrected %<>% mutate(batch = as.integer(batch))
# Run batch correction
labels.ori <- uncorrected %>%
  normalize(markers = markers,
            norm_method = "rank")  #scale or rank

labels<-labels.ori %>%
  create_som(markers = markers,
             rlen = 10,#Higher values are recommended if 10 does not appear to perform well
             xdim = 8,
             ydim = 8) 

corrected <- uncorrected %>%
  correct_data(label = labels,
               markers = markers,
               anchor = NULL,
               covar = "condition",
               parametric = TRUE)

corrected%>%
ctx$addNamespace() %>%
  ctx$save()
