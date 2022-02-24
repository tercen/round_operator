library(tercen)
library(dplyr)

options("tercen.workflowId" = "d330322c43363eb4f9b27738ef0042b9")
options("tercen.stepId"     = "1ae42627-e9ce-4d9f-9797-8700adfd7718")

method <- "round"
if(!is.null(ctx$op.value('method'))) method <- ctx$op.value('method')
digits <- 0
if(!is.null(ctx$op.value('digits'))) digits <- as.numeric(ctx$op.value('digits'))

rnd <- function(y, method, digits) {
  if(method %in% c("round", "signif")) {
    out <- sapply(y, method, digits = digits)
  } else {
    out <- sapply(y, method)
  }  
  return(out)
}

ctx <- tercenCtx()

ctx  %>% 
  select(.y, .ci, .ri) %>% 
  mutate(rounded = rnd(.y, method, digits)) %>%
  ctx$addNamespace() %>%
  ctx$save()
