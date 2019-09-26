library(dplyr)

mydists <- data.frame(t.hat = c(24,27,30,33,36), prob <- c(1/25,2/25,7/25,6/25,9/25))

summarize(mydists, 
          expected = sum(t.hat*prob), 
          bias = expected - 30, 
          variance = sum((t.hat - expected)^2*prob),
          SD = sqrt(variance),
          MSE = variance + bias^2)
