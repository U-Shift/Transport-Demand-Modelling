
#' #### Example exercise: "Residential location satisfaction in the Lisbon metropolitan area"
#' 
#' The aim of this study was to examine the perception of households towards their residential location considering several land use and accessibility factors as well as household socioeconomic and attitudinal characteristics.
#' 
#' _Reference:_ Martinez, L. G., de Abreu e Silva, J., & Viegas, J. M. (2010). Assessment of residential location satisfaction in the Lisbon metropolitan area, TRB (No. 10-1161).
#' 
#' **Your task:** Analyse the data and create meaningful latent factors.  
#' 
#' ## Data
#' #### Variables: 
#' 
#' *1 `DWELCLAS`: Classification of the dwelling;
#' *2 `INCOME`: Income of the household;
#' *3 `CHILD13`: Number of children under 13 years old;
#' *4 `H18`: Number of household members above 18 years old;
#' *5 `HEMPLOY`: Number of household members employed;
#' *6 `HSIZE`: Household size;
#' *7 `IAGE`: Age of the respondent;
#' *8 `ISEX`: Sex of the respondent;
#' *9 `NCARS`: Number of cars in the household;
#' *10 `AREA`: Area of the dwelling;
#' *11 `BEDROOM`: Number of bedrooms in the dwelling;
#' *12 `PARK`: Number of parking spaces in the dwelling; 
#' *13 `BEDSIZE`: BEDROOM/HSIZE;
#' *14 `PARKSIZE`: PARK/NCARS;
#' *15 `RAGE10`: 1 if Dwelling age <= 10;
#' *16 `TCBD`: Private car distance in time to CBD;
#' *17 `DISTHTC`: Euclidean distance to heavy public transport system stops;
#' *18 `TWCBD`: Private car distance in time of workplace to CBD;
#' *19 `TDWWK`: Private car distance in time of dwelling to work place;
#' *20 `HEADH`: 1 if Head of the Household;
#' *21 `POPDENS`: Population density per hectare;
#' *22 `EQUINDEX`: Number of undergraduate students/Population over 20 years old (500m)
#' 
#' 
#' #### Adequate Sample Size: 
#' * At least 10 variables
#' * n < 50 (Unacceptable); n > 200 (recommended); 1:10 or higher
#' * It is recommended to use continuous variables. 
#' * If your data contains categorical variables, you should transform them to dummy variables. 
#' 
#' #### Assumptions: 
#' * Normality; 
#' * linearity; 
#' * Correlations between variables < 0.3 (not appropriate to use Factor Analysis)
#' * Homoscedasticity (some multicollinearity is desirable, but not too much)


#' ## Let's start!
#' #### Import Libraries
library(foreign) # Library used to read SPSS files
library(nFactors) # Library used for factor analysis
library(tidyverse) # Library used in data science to perform exploratory data analysis
library(summarytools) # Library used for checking the summary of the dataset
library(psych) # Library used for factor analysis
library(GPArotation) # Library used for factor analysis
library(corrplot) # Library used for correlation analysis
library(car) #For linear regression and VIF calculation (testing multicollinearity)

#' ### Get to know your dataset
#' ##### Import dataset
#' 
df <- read.spss("Data/example_fact.sav", to.data.frame = T) #transforms a list into a data.frame directly

#' ##### Take a look at the main characteristics of the dataset
#' 
class(df) #type of data
str(df)

#' ##### Select a dataset with the first variables explained above
df = df [,1:24]
#' 
#' ##### Check summary statistics of variables
descriptive_stats <- dfSummary(df)
view(descriptive_stats)

#' > **Note:** I used a different library of the MLR chapter for performing the summary statistics. "R" allows you to do the same or similar tasks with different packages. 
#' 
#' ##### Take a look at the first values of the dataset
#' 
head(df,5)

#' 
#' ##### Make ID as row names or case number
#' 
df<-data.frame(df, row.names = 1)

#' 
#' ### Evaluating the assumptions for factorial analysis
#' * **Adequate Sample Size** #Ideally, you should have at least 5–10 observations per variable for reliable factor analysis results. More is generally better.
n_obs <- nrow(df)
n_vars <- ncol(df)
ratio=n_obs/n_vars
print(n_obs)
print(n_vars)
print(ratio)
#' 
#' * **Normality**
shapiro.test(df$DWELCLAS)  # Test normality of each variable

# Make normality tests to all variables
normality_tests <- sapply(df, function(x) {
  if (is.numeric(x)) {
    shapiro.test(x)
  } else {
    NA  # Skip non-numeric variables
  }
})
# Print the p-values for each variable
normality_tests

# Or Extract p-values from the normality test results ONLY
normality_pvalues <- sapply(df, function(x) {
  if (is.numeric(x)) {
    shapiro.test(x)$p.value
  } else {
    NA  # Skip non-numeric variables
  }
})

# Print the p-values for each variable
normality_pvalues

#If you want to focus on which variables do not follow normality, you can filter the output:
non_normal_vars <- names(normality_pvalues[normality_pvalues < 0.05])
print(non_normal_vars)

#If you want to focus on which variables follow normality, you can filter the output:
normal_vars <- names(normality_pvalues[normality_pvalues >= 0.05])
print(normal_vars)

#Example of histogram
hist(df$H18)

#NOTE: 
#There are no normally distributed variables in the dataset.
#Factor analysis can still work with non-normally distributed data, 
#but for Maximum Likelihood (ML) extraction, normality is preferable.

#' * **Linearity Between Variables**
pairs(df)  # Pairwise scatter plots for all variables
plot(df$BEDROOM, df$H18, 
     main = "Scatterplot of Variable1 vs Variable2", 
     xlab = "Variable1", 
     ylab = "Variable2")
abline(a = 0, b = 1, col = "red", lty = 2)

#Note: Most relationships are non-linear
 
#' * **Correlations between variables** 
#' Correlation matrix
corr_matrix <- cor(df, method = "pearson")
corrplot(corr_matrix, method = "square")

#' The **Bartlett sphericity test** 
#' This test checks if the correlation matrix is significantly different from an identity matrix (where variables are uncorrelated). 
#' If significant, the data is suitable for factor analysis. If the p-value is small (p < 0.05), 
#' you can reject the null hypothesis (which states that the variables are uncorrelated), 
#' meaning that FA is appropriate.
cortest.bartlett(corr_matrix, n = nrow(df))

#' **Note:** The null hypothesis is that there is no correlation between variables. Therefore, in factor analysis you want to reject the null hypothesis.
#' 
#' **Check for sampling adequacy - KMO test** 
#' It assesses whether the correlations 
#' between variables are high enough to justify a factor analysis. 
#' It looks at the proportion of variance that could be common among variables.
KMO(corr_matrix)

#' Note1: We want at least 0.7 of the overall Mean Sample Adequacy (MSA). 
#' If, 0.6 < MSA < 0.7, it is not a good value, but acceptable in some cases.
#' 2.	MSA for each item (variable-specific KMO scores):
#'The KMO test also provides an MSA (Measure of Sampling Adequacy) for each variable, 
#' indicating how well each variable fits with the others in terms of common variance.
#'Here are some important interpretations:
#'  - Good MSA values (≥ 0.70): Variables such as INCOME (0.86), HEMPLOY (0.89), and RAGE10 (0.80) are well-suited for factor analysis and share sufficient common variance with the other variables.
#'  - Mediocre MSA values (0.50 ≤ MSA < 0.70): Variables like DWELCLAS (0.73), HSIZE (0.59), and PARK (0.59) are marginal for factor analysis.
#'      These variables have some shared variance with the other variables but are not as strong contributors.
#'  - Low MSA values (< 0.50): Variables like CHILD13 (0.32), AVADUAGE (0.37), IAGE (0.40), and HEADH (0.46) have very low MSA scores. 
#'      These variables do not share enough common variance with the others and might be poorly suited for factor analysis.

#'Note2: You exclude the low MSA value variables: Code: 
new_df <- df %>% select(-CHILD13, -AVADUAGE, -IAGE, -HEADH, -BEDROOM, -AREA)

corr_matrix <- cor(new_df, method = "pearson")
KMO(corr_matrix)

#' **Check for multicollinearity**. Multicollinearity happens when variables are very highly correlated 
#' (e.g., correlations above 0.9), which can distort factor analysis results.
vif(lm(df$DWELCLAS ~ ., data = new_df))  # Replace Variable1 with a dependent variable



#'Note: If the VIF of any variable is greater than 10, multicollinearity may be an issue. 
#'Consider removing or combining highly correlated variables. May be the problem here is that VIF are too low!?
#' 
#' 
#' ##### Determine the number of factors to extract
#' 
#' **1. Parallel Analysis**
#' 
num_factors = fa.parallel(new_df, fm = "ml", fa = "PAF")

#' 
#' > **Note:** `fm` = factor math; `ml` = maximum likelihood; `fa` = factor analysis
#' 
#' The selection of the number of factors in the Parallel analysis can be threefold:  
#' 
#' * Detect where there is an "elbow" in the graph;
#' * Detect the intersection between the "FA Actual Data" and the "FA Simulated Data";
#' * Consider the number of factors with eigenvalue > 1.
#' 
#' 
#' **2. Kaiser Criterion**
#' 
sum(num_factors$fa.values > 1) #Determines the number of factors with eigenvalue > 1

#' You can also consider factors with eigenvalue > 0.7, since some of the literature indicate that this value does not overestimate the number of factors as much as considering an eigenvalue = 1. 

#'  **3. Principal Component Analysis (PCA)**
#' 
#' * Print variance that explains the components
df_pca <- princomp(new_df, cor=TRUE) #cor = TRUE, standardizes your dataset before running a PCA
summary(df_pca)  

#' 
#' * Scree Plot
plot(df_pca,type="lines", npcs = 17, las = 2) 


#' ## Exploratory Factor Analysis
#' 
#' * **Model 1**: No rotation
#' * **Model 2**: Rotation Varimax
#' * **Model 3**: Rotation Oblimin

# No rotation
df_factor <- factanal(df_new, factors = 4, rotation = "none", scores=c("regression"), fm = "ml")
# Rotation Varimax
df_factor_var <- factanal(df_new, factors = 4, rotation = "varimax", scores=c("regression"), fm = "ml")
# Rotiation Oblimin
df_factor_obl <- factanal(df_new, factors = 4, rotation = "oblimin", scores=c("regression"), fm = "ml")

#' 
#' Let's print out the results of `df_factor_var`, and have a look. 
print(df_factor_var, digits=2, cutoff=0.3, sort=TRUE) #cutoff of 0.3 due to the sample size is higher than 350 observations.

#' 
#' > **Note:** 
#' The variability contained in the factors = Communality + Uniqueness.  
#' Varimax assigns orthogonal rotation, and oblimin assigns oblique rotation.
#' 
#' 
#' Plot factor 1 against factor 2, and compare the results of different rotations
#' 
#' * **No Rotation**
plot(
  df_factor$loadings[, 1],
  df_factor$loadings[, 2],
  xlab = "Factor 1",
  ylab = "Factor 2",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  main = "No rotation"
)
abline(h = 0, v = 0)
load <- df_factor$loadings[, 1:2]
text(
  load,
  names(df),
  cex = .7,
  col = "blue"
)

#' 
#' * **Varimax rotation**
plot(
  df_factor_var$loadings[, 1],
  df_factor_var$loadings[, 2],
  xlab = "Factor 1",
  ylab = "Factor 2",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  main = "Varimax rotation"
)
abline(h = 0, v = 0)
load <- df_factor_var$loadings[, 1:2]
text(
  load,
  labels = names(df),
  cex = .7,
  col = "red"
)

#' 
#' 
#' * **Oblimin Rotation**
plot(
  df_factor_obl$loadings[, 1],
  df_factor_obl$loadings[, 2],
  xlab = "Factor 1",
  ylab = "Factor 2",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  main = "Oblimin rotation"
)
abline(h = 0, v = 0)
load <- df_factor_obl$loadings[, 1:2]
text(
  load,
  labels = names(df),
  cex = .7,
  col = "darkgreen"
)

#' 
#' When you have more than two factors it is difficult to analyse the factors by the plots. Variables that have low explaining variance in the two factors analyzed, could be highly explained by the other factors not present in the graph. However, try comparing the plots with the factor loadings and plot the other graphs to get more familiar with exploratory factor analysis.  
#' 