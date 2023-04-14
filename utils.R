# Libraries --------------------------------------------------------------------

install_fixed_versions <- FALSE

## Init pacman
if (!require("pacman")) install.packages("pacman")
library(pacman)
suppressMessages(p_unload(all))
library(pacman)

## Plotting
p_load(ggplot2)
p_load(ggdark)
p_load(ggpubr)
p_load(gridExtra)
p_load(latex2exp)

## knitr
p_load(knitr)
p_load(kableExtra)

## Essential
p_load(tidyverse)

## Misc
p_load(rtern)
p_load(robustbase)
p_load(Rfast)
p_load(evd)
p_load(EnvStats)
p_load(rootSolve)
p_load(devtools)
p_load(withr)
p_load(fs)

## Weighted quantile functions
load_local <- function(packageName, packageVersion) {
  if (!install_fixed_versions) {
    p_load(char = packageName)
  } else {
    home_r <- file.path(path_home(), ".r")
    if (!dir.exists(home_r))
      dir.create(home_r)
    lib <- file.path(home_r, "paper-wqe-lib")
    if (!dir.exists(lib))
      dir.create(lib)
    if (!require(packageName, lib.loc = lib, character.only = TRUE)) {
      with_libpaths(new = lib, devtools::install_version(packageName, version = packageVersion, upgrade = "never"))
    }
    library(packageName, lib.loc = lib, character.only = TRUE)
  }
}
load_local("modi", "0.1.0")
load_local("DescTools", "0.99.46")
load_local("Hmisc", "4.7-1")
load_local("laeken", "0.5.2")
load_local("MetricsWeighted", "0.5.4")
load_local("reldist", "1.7-1")
load_local("spatstat.geom", "2.4-0")
load_local("matrixStats", "0.62.0")

# Helpers ----------------------------------------------------------------------

## A color palette adopted for color-blind people based on https://jfly.uni-koeln.de/color/
cbp <- list(
  red = "#D55E00", blue = "#56B4E9", green = "#009E73", orange = "#E69F00",
  navy = "#0072B2", pink = "#CC79A7", yellow = "#F0E442", grey = "#999999"
)
cbp$values <- unname(unlist(cbp))