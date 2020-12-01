#tensorflow in r
library(tidyverse)
library(reticulate)
library(tensorflow)

install.packages('tensorflow')



install_tensorflow(
  method               = "conda", 
  version              = "default", # Installs TF 2.0.0 (as of May 15, 2020)
  envname              = "py3.6", 
  conda_python_version = "3.6", 
  extra_packages       = c("matplotlib", "numpy", "pandas", "scikit-learn")
)


conda_list()


use_condaenv("py3.6", required = TRUE)

