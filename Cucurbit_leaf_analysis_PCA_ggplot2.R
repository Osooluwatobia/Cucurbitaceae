# Cucurbits PCA + ggplot2

# Load needed packages from library
# OPTIONAL - If packages not yet installed - install.packages("package_name")
# To install ggbiplot - load library(devtools), then - install_github("vqv/ggbiplot")

# Load excel reader
library(readxl)

#Load ggplot2
library(ggplot2)

# lOAD ggally after ggplot2
library(GGally)

# lOAD tidyverse or magrittr for pipe operator (%>%) after ggplot2
library(tidyverse)

# Load ggbiplot using require
require(ggbiplot)


# Import the Leaf Measures from Excel
Cucurbit_Leaf <- read_excel("Cucurbit_Morphometrics.xlsx", 
                                   sheet = "Leaf_Measures")

# Import the Teeth Measures from Excel
Cucurbit_Teeth <- read_excel("Cucurbit_Morphometrics.xlsx", 
                            sheet = "Teeth_Measures")

# Import the bandWidth Measures from Excel
Cucurbit_BW <- read_excel("Cucurbit_Morphometrics.xlsx", 
                             sheet = "bandWidth")

# OPTIONAL - View the leaves data in window
View(Cucurbit_Leaf)

# OPTIONAL - View the teeth data in window
View(Cucurbit_Teeth)

# View the bandwidth data in window
View(Cucurbit_BW)

# OPTIONAL - View the file in console - head(insert_filename)
# OPTIONAL - For data summary in console - summary(insert_filename)
# OPTIONAL - get column names - colnames(insert_filename)



# PAIRWISE SCATTERPLOT

# Before PCA, plot pairwise scatterplot to check variables correlation
# This is to check and take high dimensional data that is
# Highly correlated and reduce dimensionality
# Check meaning of "[4:9]" below - to select columns 4 to 9
# check plot on the plot window on the right

# ALTERNATIVE - Plot pairwise scatterplot using pairs on base
# (remove hashes below)
#pairs(Cucurbit_Leaf[4:9]), pair =panel.smooth)

# optional - column names
colnames(Cucurbit_Leaf)

# ALTERNATIVE - plot pairwise scatter plot matrix with GGally no color
# (remove hashes below)
#Cucurbit_Leaf %>% ggpairs(columns = c("Blade_Length",
#                                      "Blade_Width_BB", "Blade_Area",
#                                      "Blade_Perimeter", "Blade_Width_IS"),
#                 upper = list(continuous = wrap('cor', size = 8)))

# Pairwise scatterplot matrix with GGally, coloring by species
Leaf_PW_Scatterplot = Cucurbit_Leaf %>% mutate(Species = factor(Species)) %>%
  ggpairs(columns = c("Blade_Length", "Blade_Width_BB", 
                      "Blade_Area", "Blade_Perimeter", "Blade_Width_IS"), 
          aes(color = Species),
          upper = list(continuous = wrap('cor', size = 3)),
          lower = list(combo = wrap("facethist", bins = 30)),
          diag = list(continuous = wrap("densityDiag", alpha = 0.5)))

# View Pairwise scatterplot
Leaf_PW_Scatterplot

# Add title to Leaf PW using labs fxn. Rename it to version 2 of your scatterplot
Leaf_PW_Scatterplot2 = Leaf_PW_Scatterplot+labs (title = "Coefficient Matrix of Cucurbitaceae leaves blade data set",
                               subtitle="scatterplot showing all major leaf blade variables",
                               colour = "Species")

# View titled biplot
Leaf_PW_Scatterplot2

# PLOT PCA 

# Use numerical data not categorical dat
# Use cbind or prcomp then run PCA. I use so I can supply scale
# Select working columns or subset data for PCA
# The filename is the data frame [4:9] represents columns 4 to 9
# [, -5] or [, -1, -2] represent minus column 5 or minus column 1 and 2
# For specific column use prcomp(-column 1 title + column 2 title, data = file.name)
# Ensure the PCA "<-" name is different from the file name. I use "MyPC"
# Try to always scale your data as TRUE - scale = TRUE
# scaling has no disadvantage. It does not change relationship
# Circle = TRUE can only work if scale is set as TRUE

# Run PCA + select PCA working columns (scaled)
MyPC <- prcomp(Cucurbit_Leaf[4:9], scale = TRUE)


# OPTIONAL - View MyPC  for confirmation
MyPC

# OPTIONAL - PCA Summary - importance of components
summary(MyPC)



# Plot screeplot to show variances across each principal components
# Variance is the square of the standard deviation.
# most of the data variability will be accounted for in the 1st and 2nd PC
# Use either bar or lines. (LINE PREFERABLY)

# ALTERNATIVE - plot screeplot in bars (remove hash below)
#plot(MyPC)

# plot screeplot in lines/l
plot(MyPC, type = "l")



# To interpret PC, use biplot or ggbiplot
# It shows how each components and variable agree with each other
# Scale it to zero to make the axis more interpretable

# ALTERNATIVE Plot biplot - biplot(MyPCA, scale = 0)

# ALTERNATIVE - Plot ggbiplot without aesthetics - Leaf_bplot <-  ggbiplot(MyPC)

# Plot ggbiplot without aesthetics
Leaf_bplot =  ggbiplot(pcobj = MyPC, choices = c(1,2),
                        obs.scale =  1, var.scale = 1,
                        labels = row.names(Cucurbit_Leaf),
                        varname.size = 3, varname.abbrev = FALSE,
                        var.axes = TRUE, circle = TRUE,
                        ellipse = TRUE, groups = Cucurbit_Leaf$Species)

# View the biplot
Leaf_bplot

# Add title to biplot using labs fxn. Rename it to version 2 of your biplot
Leaf_bplot2 = Leaf_bplot+labs (title = "PCA of Cucurbitaceae leaves data set",
                               subtitle="PC1 vs PC2",
                               colour = "Species")

# View titled biplot
Leaf_bplot2


# ALTERNATIVE WAY TO VIEW SCATTERPLOT ON GGPLOT2
# biplot may not the best way to represent or look for patterns in data
# after the data has been projected onto the new PC. 
# So, instead, extract PC1 and PC2, attach them to the data frame
# and plot using ggplot


# First examine the structure to see that PCs have values
# for sdev, rotation, center, scale and x

# OPTIONAL - Examine the structure
str(MyPC)

# x values are the PC scores for each observation
# every obs in the dataframe gives score for PC 1 to ....6
# they are coordinates. Where each obs are found

# OPTIONAL - EXTRACT PC SCORES
MyPC$x


# Then take PC score for PC1 and PC2 and attach to the 
# file dataframe so that it can be plotted more nicely
# create a new dataframe (a version 2) for the file using cbind
# a version 2 should be another name (filename 2) - M.charantia2 here
# combine dataframes and plot with ggplot2

# Combine dataframes
Cucurbit_Leaf2 <- cbind(Cucurbit_Leaf, MyPC$x[,1:6])

# View combined dataframe
head(Cucurbit_Leaf2)


# before plotting, supply the version2 dataframe
# load the aesthetic (aes) function
# color the species/slice_name and fill species/slice_name
# Layer (+) stat ellipse polygon geom to show the 95% CI of the data
# with color and line
# Layer (+) geom points with shape and color

# Plot with ggplot2
Leaf_biplot_alternative = ggplot(Cucurbit_Leaf2, aes(x=PC1, y=PC2, col = Species, fill = Species)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") +
  labs(title="PCA of Cucurbitaceae leaves data set",
       subtitle="Space of best fit for each species")

# View Leaf biplot alternative
Leaf_biplot_alternative

# RUN CORRELTATION COEFFICIENT BETWEEN VARIABLES AND PCS
# Instead of biplot to interpret relationship between each variables 
# and PC, statistically, you can look at correlations between
# the original variables and the PCs to interpret how one PC changes,
# whether the variables go up or down and by how much
# it's a pearson correlation coefficient

# Correlation between variables and PCs
cor(Cucurbit_Leaf2[4:9], Cucurbit_Leaf2[14:19])



# PLOT CORRELATION MATRIX BETWEEN VARIABLES

# ALTERNATIVE - Plot correlation matrix
# plot your data scaled if the unit of the variables are different
# to plot matric (unscaled) - plot(Cucurbit$Blade_Length, Cucurbit$Blade_Width_BB)
# to plot matrix (scaled) - plot(scale(Cucurbit$Blade_Length), scale(Cucurbit$Blade_Width_BB))
# Add superscript (square) to cm in label - "label"~(cm^2)

# Plot Correlation Matrix within leaves variables

colnames(Cucurbit_Leaf2)

#Length vs Width, Blade perimeter vs PC1
ggplot(Cucurbit_Leaf2, aes(Blade_Length, Blade_Perimeter,
                           col=Species, fill=Species)) +
  geom_smooth(method = lm) +
  geom_point(shape=21) +
  labs(title="Blade Length vs Blade Perimeter") +
  xlab("Blade Length")+
  ylab("Blade Perimeter")
  

MyPC
