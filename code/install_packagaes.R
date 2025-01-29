# installing the packages from wur git repo 
if (!require("remotes")) install.packages("remotes")
remotes::install_git('https://git.wur.nl/isric/dsm-general/dsm.workflows/seedling.git', subdir = "isric.dsm.base",ref="0.3.13")

install.packages("knitr")
install.packages("leaflet")
install.packages("ggspatial")
install.packages("cvTools")
install.packages('sf')
install.packages('dplyr')
install.packages('ggplot2')