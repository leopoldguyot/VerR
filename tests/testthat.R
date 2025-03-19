# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.

library(testthat)
library(VerR)

Sys.setenv(TESTTHAT_PARALLEL = "false")
Sys.setenv(TESTTHAT_RSI = "false")

test_check("VerR")
