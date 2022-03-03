library(tidyverse)
library(lubridate)

data <- read.csv("data.csv")
data$Date <-as.POSIXct(data$Date, tz="UTC")
