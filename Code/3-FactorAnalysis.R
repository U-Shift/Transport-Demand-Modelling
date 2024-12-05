
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
#' * n < 50 (Unacceptable); n > 200 (recommended); 10 observations per variable or higher
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
df = df[,1:24]

#' ##### Make ID as row names or case number
#' 
df<-data.frame(df, row.names = 1)

#' 
#' ##### Check summary statistics of variables
descriptive_stats <- dfSummary(df)
view(descriptive_stats)

str(df)

# Remove categorical variables or transform them into dummy (factor)
df = select(df,c(-DWELCLAS, -ISEX, -RAGE10, -HEADH))

#Standardize variables (Zscore = xi - xmean)/sd

mean <- apply(df, 2, mean) # The "2" in the function is used to select the columns. MARGIN: c(1,2)
sd <- apply(df, 2, sd)
df_scaled <- data.frame(scale(df, mean, sd))


#' ##### Take a look at the first values of the dataset
#' 
head(df_scaled,5)

##### Compare the boxplots of the original dataset and standardized one

boxplot(df)
boxplot(df_scaled)
 
#' ### Evaluating the assumptions for factorial analysis
#' * **Adequate Sample Size** #Ideally, you should have at least 10 observations per variable for reliable factor analysis results. More is generally better.

n_obs <- nrow(df_scaled)
n_vars <- ncol(df_scaled)
ratio=n_obs/n_vars

n_obs
n_vars
ratio
#' 
#' * **Normality**
shapiro.test(df_scaled$CHILD13)  # Test normality of each variable

#The null hypothesis of both tests is that the distribution is normal. 
#Therefore, for the distribution to be normal, the pvalue > 0.05 and you should not reject the null hypothesis.

# Make normality tests to all variables
normality_tests <- sapply(df_scaled, function(x) {
  if (is.numeric(x)) {
    shapiro.test(x)
  } else {
    NA  # Skip non-numeric variables
  }
})
# Print the p-values for each variable
normality_tests

# Or Extract p-values from the normality test results ONLY
normality_pvalues <- sapply(df_scaled, function(x) {
  if (is.numeric(x)) {
    shapiro.test(x)$p.value
  } else {
    NA  # Skip non-numeric variables
  }
})

# Print the p-values for each variable
normality_pvalues

# If you want to focus on which variables do not follow normality, you can filter the output:
non_normal_vars <- names(normality_pvalues[normality_pvalues < 0.05])
non_normal_vars

# If you want to focus on which variables follow normality, you can filter the output:
normal_vars <- names(normality_pvalues[normality_pvalues >= 0.05])
normal_vars

# Check histograms to confirm non-normality. Example of histogram
hist(df_scaled$INCOME)

#NOTE: 
#There are no normally distributed variables in the dataset.
#Factor analysis can still work with non-normally distributed data, 
#but for Maximum Likelihood (ML) extraction, normality is preferable.

#' * **Linearity Between Variables**
pairs(df_scaled[,1:10], lower.panel = NULL)  # Pairwise scatter plots for the first 10 variables. Check the others!


plot(df_scaled$BEDROOM, df_scaled$H18, 
     main = "Scatterplot", 
     xlab = "BEDROOM", 
     ylab = "H18")
abline(a = 0, b = 1, col = "red", lty = 2)

#Note: Most relationships are non-linear
 
#' * **Correlations between variables** 

#' Correlation matrix
corr_matrix <- cor(df_scaled, method = "pearson")

corrplot(corr_matrix, method = "circle", type = "upper")

# Check if the correlation is statistically significant
cor.test(df_scaled$H18,df_scaled$HSIZE)

# The null hypothesis is that the correlation is zero. 
# This means that the correlations are only significant when you reject the null hypothesis (pvalue < 0.05).

#' The **Bartlett sphericity test** 
#' This test checks if the correlation matrix is significantly different from an identity matrix (where variables are uncorrelated). 
#' If significant, the data is suitable for factor analysis. If the p-value is small (p < 0.05), 
#' you can reject the null hypothesis (which states that the variables are uncorrelated), 
#' meaning that FA is appropriate.

cortest.bartlett(corr_matrix, n = nrow(df_scaled))

#' **Note:** The null hypothesis is that there is no correlation between variables. 
#' Therefore, in factor analysis you want to reject the null hypothesis (pvalue < 0.05).
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
#'  - Good MSA values (≥ 0.70): Variables such as INCOME (0.86), HEMPLOY (0.87) are well-suited for factor analysis and share sufficient common variance with the other variables.
#'  - Mediocre/Bad MSA values (0.50 ≤ MSA < 0.70): Variables like HSIZE (0.57), and PARK (0.55) are marginal for factor analysis.
#'      These variables have some shared variance with the other variables but are not as strong contributors.
#'  - Low MSA values (< 0.50): Variables like CHILD13 (0.30), and PARKSIZE (0.48) have very low MSA scores. 
#'      These variables do not share enough common variance with the others and might be poorly suited for factor analysis.

#'Note2: You exclude the low MSA value variables: Code: 
new_df <- df_scaled |> select(-CHILD13, -PARKSIZE)

corr_matrix <- cor(new_df, method = "pearson")
KMO(corr_matrix)

#' **Check for multicollinearity**. Multicollinearity happens when variables are very highly correlated 
#' (e.g., correlations above 0.9), which can distort factor analysis results.
vif(lm(df_scaled$INCOME ~ ., data = new_df))  # Replace Variable1 with any other variable


#'Note: If the VIF of any variable is greater than 10, multicollinearity may be an issue. 
#'Consider removing or combining highly correlated variables. May be the problem here is that VIF are too low!?
#' 
#' 
#' ##### Determine the number of factors to extract
#' 
#' **1. Parallel Analysis**
#' 
num_factors = fa.parallel(new_df, fm = "ml", fa = "fa")

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
df_pca <- princomp(new_df, cor=FALSE) #cor = TRUE, standardizes your dataset before running a PCA
summary(df_pca)  

#' 
#' * Scree Plot
plot(df_pca,type="lines", npcs = 17, las = 2) 

#' Check the cummulative variance of the first components and the scree plot, 
#' and see if the PCA is a good approach to detect the number of factors in this case. 
#' 
#' PCA is not the same thing as Factor Analysis! 
#' PCA only considers the common information (variance) of the variables, 
#' while factor analysis takes into account also the unique variance of the variable. 
#' Both approaches are often mixed up. 
#' In this example we use PCA as only a first criteria for choosing the number of factors. 
#' PCA is very used in image recognition and data reduction of big data. 

#' ## Exploratory Factor Analysis
#' 
#' * **Model 1**: No rotation
#' * **Model 2**: Rotation Varimax
#' * **Model 3**: Rotation Oblimin

# No rotation
df_factor <- factanal(new_df, factors = 4, rotation = "none", scores=c("regression"), fm = "ml")
# Rotation Varimax (Ortogonal)
df_factor_var <- factanal(new_df, factors = 4, rotation = "varimax", scores=c("regression"), fm = "ml")
# Rotiation Oblimin
df_factor_obl <- factanal(new_df, factors = 4, rotation = "oblimin", scores=c("regression"), fm = "ml")

#' 
#' Let's print out the results, and have a look. 

print(df_factor, digits=2, cutoff=0.3, sort=TRUE)
print(df_factor_var, digits=2, cutoff=0.3, sort=TRUE) #cutoff of 0.3 due to the sample size is higher than 350 observations.
print(df_factor_obl, digits=2, cutoff=0.3, sort=TRUE)

#' 
#' > **Note:** 
#' The variability contained in the factors = Communality + Uniqueness. 
#' You want uniquenesses below 0.3 (communality above 0.7).  
#' Varimax assigns orthogonal rotation, and oblimin assigns oblique rotation.
#' 
#' Plot factor 3 against factor 4, and compare the results of different rotations
#' 
#' * **No Rotation**
plot(
  df_factor$loadings[, 3],
  df_factor$loadings[, 4],
  xlab = "Factor 1",
  ylab = "Factor 2",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  main = "No rotation"
)
abline(h = 0, v = 0)
load <- df_factor$loadings[, 3:4]
text(
  load,
  names(df),
  cex = .7,
  col = "blue"
)

#' 
#' * **Varimax rotation**
plot(
  df_factor_var$loadings[, 3],
  df_factor_var$loadings[, 4],
  xlab = "Factor 1",
  ylab = "Factor 2",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  main = "Varimax rotation"
)
abline(h = 0, v = 0)
load <- df_factor_var$loadings[, 3:4]
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
  df_factor_obl$loadings[, 3],
  df_factor_obl$loadings[, 4],
  xlab = "Factor 1",
  ylab = "Factor 2",
  ylim = c(-1, 1),
  xlim = c(-1, 1),
  main = "Oblimin rotation"
)
abline(h = 0, v = 0)
load <- df_factor_obl$loadings[, 3:4]
text(
  load,
  labels = names(df),
  cex = .7,
  col = "darkgreen"
)

#' 
#' When you have more than two factors it is difficult to analyse the factors by the plots. 
#' Variables that have low explaining variance in the two factors analyzed, could be highly explained by the other factors not present in the graph. However, try comparing the plots with the factor loadings and plot the other graphs to get more familiar with exploratory factor analysis.  
#' 