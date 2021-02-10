# Knit and render Rmd files with correct related paths, for data, for md output, and for figure plots.

#0-InstallR
rmarkdown::render(
  input = "RmdFiles/0-InstallR.Rmd",
  output_dir = getwd(),
  clean = T
)

#00-RMarkdownReports
rmarkdown::render(
  input = "RmdFiles/00-RMarkdownReports.Rmd",
  knit_root_dir = getwd(),
  output_file = getwd(),
  clean = T
)

#Slides
rmarkdown::render(
  input = "RmdFiles/Slides.Rmd",
  knit_root_dir = getwd(),
  output_dir = getwd(),
  clean = T
)


#run each one separately
name = "1-ExploratoryDataAnalysis"
# name = "2-MultipleLinearRegression"
# name = "3-FactorAnalysis" 
# name = "4-ClusterAnalysis"
# name = "5-GeneralizedLinearModels"
# name = "6-SpatialModels" 
# name = "7-PanelModels" #não tem figs
# name = "9-HazardBasedModels"

rmarkdown::render(
  input = paste0("RmdFiles/", name, ".Rmd"),
  knit_root_dir = getwd(),
)
file.rename(paste0("RmdFiles/", name, ".md"), paste0(name, ".md"))
file.rename(paste0("RmdFiles/RmdFiles/", name, "/"), paste0("RmdFiles/", name, "/"))


#manually delete folder "RmdFiles/RmdFiles/" that is empty.

