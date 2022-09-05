library(tercen)
library(dplyr, warn.conflicts = FALSE)
library(cyCombine)
library(magrittr)
library(tidyverse)

ctx <- tercenCtx()

seed <- NULL
if(!ctx$op.value('seed') < 0) seed <- as.integer(ctx$op.value('seed'))

norm_method <- ctx$op.value("norm_method", as.character, "scale")

data.all<-ctx$select(unlist(list(".y",".ri",".ci",ctx$colors,ctx$labels)))

### rename the colors and labels colomns to names require by cycombine
colnames(data.all)[ncol(data.all)]<-"condition"
colnames(data.all)[ncol(data.all)-1]<-"batch"

data <- data.all[0:3] %>% 
  pivot_wider(id_cols=".ci",names_from= ".ri", values_from =".y")


markers<-ctx$rselect()[[1]]
colnames(data) <- c(".ci",markers)

uncorrected.all<-dplyr::full_join(x=data,y=unique(data.all[,3:ncol(data.all)]),by = ".ci")

uncorrected<-uncorrected.all %>% drop_na()
uncorrected %<>% mutate(batch = as.integer(batch))

# Run batch correction
labels.ori <- uncorrected %>%
  normalize(markers = markers,
            norm_method = norm_method)

labels<-labels.ori %>%
  create_som(markers = markers,
             seed = seed,
             rlen = 10,#Higher values are recommended if 10 does not appear to perform well
             xdim = 8,
             ydim = 8) 

corrected <- uncorrected %>%
  correct_data(label = labels,
               markers = markers,
               anchor = NULL,
               covar = "condition",
               parametric = TRUE)

corrected.short<-select(corrected, -c(id,batch, condition))

corrected.long <-corrected.short %>%
  select(-label)%>%
  pivot_longer(!.ci, names_to = "variable",values_to = "value")

output <- corrected.long %>% 
  left_join(cbind(unique(data.all[".ri"]),markers),  
            by = c("variable" = "markers"))%>%
  select(-variable)%>%
  ctx$addNamespace()

output %>%
  ctx$save()
