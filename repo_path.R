# Set code repository path (e.g., "~/Downloads/TBI Clustering Pipeline Code Repository/)
path = "~/Downloads/TBI Clustering Pipeline Code Repository/"

assign("path", path, envir=.GlobalEnv)

# Required dependencies
require(devtools)
install_version("caret", version = "6.0-78")
install_version("cluster", version = "2.0.6")
install_version("clusteval", version = "0.1")
install_version("crossmatch", version = "1.3-1")
install_version("dplyr", version = "0.8.0.1")
install_version("dummies", version = "1.5.6")
install_version("effsize", version = "0.7.6")
install_version("ggplot2", version = "2.2.1 ")
install_version("ggpubr", version = "0.1.6")
install_version("h2o", version = "3.22.1.1")
install_version("mice", version = "2.46.0")
install_version("modelr", version = "0.1.5")
install_version("nnet", version = "7.3-12")
install_version("plyr", version = "1.8.4")
install_version("purrr", version = "0.3.0")
install_version("reshape", version = "0.8.7")
install_version("reshape2", version = "1.4.3")
install_version("robustHD", version = "0.5.1")
install_version("Rtsne", version = "0.13")
install_version("tidyr", version = "0.8.2")


