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
  output_dir = getwd(),
  clean = T
)

#1-ExploratoryDataAnalysis
rmarkdown::render(
  input = "RmdFiles/1-ExploratoryDataAnalysis.Rmd",
  knit_root_dir = getwd(),
  output_dir = getwd(),
  # clean = T
)

#2-MultipleLinearRegression
rmarkdown::render(
  input = "RmdFiles/2-MultipleLinearRegression.Rmd",
  knit_root_dir = getwd(),
  output_dir = getwd(),
  clean = T
)

#3-FactorAnalysis
rmarkdown::render(
  input = "RmdFiles/3-FactorAnalysis.Rmd",
  knit_root_dir = getwd(),
  output_dir = getwd(),
  clean = T
)

#4-ClusterAnalysis
rmarkdown::render(
  input = "RmdFiles/4-ClusterAnalysis.Rmd",
  knit_root_dir = getwd(),
  output_dir = getwd(),
  clean = T
)

#5-GeneralizedLinearModels
rmarkdown::render(
  input = "RmdFiles/5-GeneralizedLinearModels.Rmd",
  knit_root_dir = getwd(),
  output_dir = getwd(),
  clean = T
)

#6-SpatialModels
rmarkdown::render(
  input = "RmdFiles/6-SpatialModels.Rmd",
  knit_root_dir = getwd(),
  output_dir = getwd(),
  clean = T
)

#7-PanelModels
rmarkdown::render(
  input = "RmdFiles/7-PanelModels.Rmd",
  knit_root_dir = getwd(),
  output_dir = getwd(),
  clean = T
)

#9-HazardBasedModels
rmarkdown::render(
  input = "RmdFiles/9-HazardBasedModels.Rmd",
  knit_root_dir = getwd(),
  output_dir = getwd(),
  clean = T
)

#Slides
rmarkdown::render(
  input = "RmdFiles/Slides.Rmd",
  knit_root_dir = getwd(),
  output_dir = getwd(),
  clean = T
)


