library(tercen)
library(dplyr)

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

(ctx = tercenCtx())  %>% 
  select(.y, .ci, .ri) %>% 
  mutate(rounded = rnd(.y, method, digits)) %>%
  ctx$addNamespace() %>%
  ctx$save()
