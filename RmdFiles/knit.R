# Knit and render Rmd files with correct related paths, for data, for md output, and for figure plots.


##run each one separately

# name = "1-ExploratoryDataAnalysis"
# name = "2-MultipleLinearRegression"
name = "3-FactorAnalysis"
# name = "4-ClusterAnalysis"
# name = "5-GeneralizedLinearModels"
# name = "6-SpatialModels" 
# name = "7-PanelModels" #não tem figs
# name = "9-HazardBasedModels"
# name = "0-InstallR" #não tem figs
# name = "00-RMarkdownReports" #não tem figs
# name = "Slides" #não tem figs

file.rename(paste0("RmdFiles/", name, ".md"), paste0(name, ".md")) #mover md
file.rename(paste0("RmdFiles/RmdFiles/", name, "/"), paste0("RmdFiles/", name, "/")) #mover figuras


#manually delete folder "RmdFiles/RmdFiles/" that is empty.

