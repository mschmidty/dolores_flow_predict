library(here)
library(readr)
library(tibble)
print(here())

t<-tibble(
  h = c("Hello", "Bad"),
  y = c(3,4),
  date = c(Sys.Date(), Sys.Date())
)
write_csv(t, "predictions/test.csv", append = T)
