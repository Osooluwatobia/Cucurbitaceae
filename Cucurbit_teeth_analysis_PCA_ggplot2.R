# Cucurbits TEETH Intra-Species ANALYSIS PCA + ggplot2

# Load needed packages from library
# OPTIONAL - If packages not yet installed - install.packages("package_name")


library(readxl)           # Load excel reader

library(tidyverse)              # lOAD tidyverse or magrittr for pipe operator (%>%) after ggplot2

library(ggplot2)                #Load ggplot2

library(GGally)           # lOAD ggally after ggplot2

require(ggbiplot)       # Load ggbiplot using require

library(rgl)            # Load RGL Engine display

library(SciViews)       # Load ScieViews

library(plotrix)

library(corrplot)

library(reshape)

library(gridExtra)

library(ggcorrplot)

library(plotly)


# Set working directory first
setwd("C:/Users/company/Desktop/R_Analysis_Cucurbit_Measures")

# Import the Teeth Measures from Excel
Teeth_Data_NZ <- read_excel("Cucurbit_Morphometrics.xlsx",
                            sheet = "Teeth_Measures_NZ")


# OPTIONAL - View the teeth data in window
View(Teeth_Data_NZ)

# OPTIONAL - View the file in console - head(insert_filename)
# OPTIONAL - For data summary in console - summary(insert_filename)
# OPTIONAL - get column names - colnames(insert_filename)



# PAIRWISE SCATTERPLOT

# optional - column names
colnames(Teeth_Data_NZ)

# Pairwise scatterplot matrix with GGally, coloring by species
Teeth_PW_Scatterplot = Teeth_Data_NZ %>%
  mutate(Tooth_Position = factor(Tooth_Position)) %>%
  ggpairs(columns = c("Tooth_Width", "Tooth_Height_altitude",
                      "Tooth_Height_median", "Tooth_Area",
                      "Tooth_Perimeter", "Position_From_Base",
                      "Position_From_Tip"), 
          aes(color = Species),
          upper = list(continuous = wrap('cor', size = 2.5)),
          lower = list(combo = wrap("facethist", bins = 30)),
          diag = list(continuous = wrap("densityDiag", alpha = 0.5)))

# View Pairwise scatterplot
Teeth_PW_Scatterplot

# Add title to scatterplot using labs fxn. Rename it to version 2 of your biplot
Teeth_PW_Scatterplot2 = Teeth_PW_Scatterplot+labs (title = "Correlation matrix of Teeth Variables",
                               subtitle="Scatterplot of Cucurbitaceae Teeth data set",
                               colour = "Species")

# View titled Pairwise scatterplot
Teeth_PW_Scatterplot2





#PCA

# Use numerical data not categorical data
# Ensure the PCA "<-" name is different from the file name. I use "MyPC"
# Try to always scale your data as TRUE - scale = TRUE

colnames(Teeth_Data_NZ)


# Run 
MyPC <- prcomp(~Blade_Width_BB+Blade_Width_IS+Blade_Length+Blade_Area+
                 Blade_Perimeter+Tooth_Width+Tooth_Width+
                 Tooth_Height_altitude+Tooth_Height_median+Tooth_Height_Width+
                 Tooth_Area+Tooth_Perimeter+Position_From_Tip+
                 Position_From_Base, data=Teeth_Data_NZ, scale = TRUE)


# OPTIONAL - View MyPC  for confirmation
MyPC

# OPTIONAL - PCA Summary - importance of components
summary(MyPC)

str(MyPC)

# Plot screeplot to show variances across each PCs
# Use either bar or lines. (LINE PREFERABLY)

# plot screeplot in lines/l
plot(MyPC, type = "b")




# Plot a CORRELATION PLOT OF ALL VARIABLES VS 3 PCs

My3PCs <- as.data.frame(MyPC$rotation[,1:3])
My3PCs2 = cbind(Leaf = rownames(My3PCs), My3PCs)
My3PCs3 = melt(My3PCs2, id = c("Leaf"))
My3PCs3_Plot = ggplot(My3PCs3, aes(variable, Leaf)) +
  geom_tile(aes(fill = value),
            colour = "white") + scale_fill_gradient(low = "red", high = "green")


# Add title to My3PCs3_Plot and VIEW
My3PCs3_Plot + labs (title = "CORRELATION OF ALL VARIABLES VS 3 PCs") +
  theme(axis.text = element_text(size=20), axis.title = element_text(size = 20))



# Correlation Plot of All PC Variables

# Create a new dataframe
MyPC_Variables = Teeth_Data_NZ[, c("Species", "Blade_Width_BB", "Blade_Width_IS", "Blade_Length",
                         "Blade_Area", "Blade_Perimeter", "Tooth_Width",
                         "Tooth_Width", "Tooth_Height_altitude", "Tooth_Height_median", 
                         "Tooth_Height_Width", "Tooth_Area", "Tooth_Perimeter",
                         "Position_From_Tip", "Position_From_Base")]

MyPC_Variables


# Correlation Matrix
MyPC_Variables_cor<-cor(MyPC_Variables [,-1])

MyPC_Variables_cor

# Plot correlation plot of MyPC_Variables_cor, add title, and VIEW
ggcorrplot(MyPC_Variables_cor, method = "circle") + 
  labs (title = "Correlation Plot of All PC Variables") + 
  theme(axis.text = element_text(size=20), axis.title = element_text(size = 20))











# To interpret PC, use biplot or ggbiplot

# Plot PC1 vs PC2 ggbiplot without aesthetics
Teeth_biplot_PC1_PC2 =  ggbiplot(pcobj = MyPC, scale = TRUE, choices = c(1,2),
                        obs.scale =  1, var.scale = 1,
                        varname.size = 3, varname.abbrev = FALSE,
                        var.axes = TRUE, circle = FALSE,
                        ellipse = TRUE, groups = Teeth_Data_NZ$Species)

# View the biplot
Teeth_biplot_PC1_PC2

# Add title to biplot using labs fxn. Rename it to version 2 of your biplot
Teeth_biplot_PC1_PC2_2 = Teeth_biplot_PC1_PC2+labs (title = "PCA of Cucurbitaceae leaves data set",
                               subtitle="PC1 vs PC2",
                               colour = "Species") + 
  theme(legend.position = "bottom")

# View titled biplot
Teeth_biplot_PC1_PC2_2




# Plot PC3 vs PC4 ggbiplot without aesthetics
Teeth_biplot_PC3_PC4 =  ggbiplot(pcobj = MyPC, scale = TRUE, choices = c(3,4),
                                 obs.scale =  1, var.scale = 1,
                                 varname.size = 3, varname.abbrev = FALSE,
                                 var.axes = TRUE, circle = FALSE,
                                 ellipse = TRUE, groups = Teeth_Data_NZ$Species) +
  theme(legend.position = "bottom")

# View the biplot
Teeth_biplot_PC3_PC4

# Add title to biplot using labs fxn. Rename it to version 2 of your biplot
Teeth_biplot_PC3_PC4_2 = Teeth_biplot_PC3_PC4+labs (title = "PCA of Cucurbitaceae leaves data set",
                                            subtitle="PC3 vs PC4",
                                            colour = "Species") +
  theme(legend.position = "bottom")

# View titled biplot
Teeth_biplot_PC3_PC4_2









# PLOT 3D PCA PLOT
plot3d(My3PCs$PC1, My3PCs$PC2, My3PCs$PC3)




# COMBINE DATASETS - PCA + Variables
# Combine dataframes
All_Dataset <- cbind(Teeth_Data_NZ, MyPC$x[,1:13])

# View combined dataframe
head(All_Dataset)

colnames(All_Dataset)






# A GENERAL BIPLOT DOESN'T GROUP THE TEETH VARIABLES TOGETHER ON PER SPECIES 
# BECAUSE OF THE TEETH POSITION, SO, I'LL PLOT FACET LAYERS PER TEETH POSITION

colnames(Teeth_Data_NZ) 


# Blade Length vs  Tooth Position

ggplot(Teeth_Data_NZ, aes(x=Blade_Length, y=Tooth_Position,
                          col= factor(Total_Teeth_Number),
                          fill= factor(Total_Teeth_Number))) +
  geom_point(shape=21) +
  scale_color_discrete(name="Total Teeth Number")+
  scale_fill_discrete(name="Total Teeth Number")+
  facet_grid(. ~ Species)+
  scale_x_continuous("Blade Length (cm)")+
  scale_y_continuous("Tooth Position")+
  labs(title="Blade Length vs Tooth Position")+
  theme(legend.position = "bottom")+
  coord_equal(ratio=3/1)


  
  #
  scale_x_continuous("Tooth Number", limits = c(2,11), expand = c(0,0))+
  scale_y_continuous("Tooth Position", limits = c(0,6), expand = c(0,0))+
  labs(title="Tooth Number vs Tooth Position")+
  coord_equal(ratio=1/1)+
  theme(legend.position = "bottom")


  
# Blade Length vs Teeth Width

colnames(Teeth_Data_NZ)

ggplot(Teeth_Data_NZ, aes(x=Blade_Length, y=Tooth_Width,
                          col= factor(Tooth_Position),
                          fill= factor(Tooth_Position)))+
  geom_smooth(method=loess, se=T)+
  geom_point(shape=20) +
  scale_color_discrete(name="Tooth Position")+
  scale_fill_discrete(name="Tooth Position")+
  facet_grid(. ~ Species)+
  facet_wrap(. ~ Species, scales = "free")+
  scale_x_continuous("Blade Length (cm)")+
  scale_y_continuous("Teeth Width (cm)")+
  labs(title="Blade Length vs Teeth Width")


# Blade length (cm) vs Tooth height (cm) by Species

colnames(Teeth_Data_NZ)

ggplot(Teeth_Data_NZ, aes(x=Blade_Length, y=Tooth_Height_altitude,
                          col= factor(Tooth_Position),
                          fill= factor(Tooth_Position)))+
  geom_smooth(method=loess, se=T)+
  geom_point(shape=20) +
  scale_color_discrete(name="Tooth Position")+
  scale_fill_discrete(name="Tooth Position")+
  facet_grid(. ~ Species)+
  facet_wrap(. ~ Species, scales = "free")+
scale_x_continuous("Blade Length (cm)")+
  scale_y_continuous("Tooth Height (cm)")+
  labs(title="Blade Length vs Tooth Height")


# Blade length (cm) vs Tooth height (cm) by Tooth

colnames(Teeth_Data_NZ)

ggplot(Teeth_Data_NZ, aes(x=Blade_Length, y=Tooth_Height_altitude,
                          col= Species,
                          fill= Species))+
  geom_smooth(method=loess, se=T)+
  geom_point(shape=20) +
  scale_color_discrete(name="Species")+
  scale_fill_discrete(name="Species")+
  facet_grid(. ~ Tooth_Position)+
  facet_wrap(. ~ Tooth_Position, scales = "free")+
  scale_x_continuous("Blade Length (cm)")+
  scale_y_continuous("Tooth Height (cm)")+
  labs(title="Blade Length vs Tooth Height")


# tooth width (cm) vs Tooth height - 1ST, 2ND, 3RD tooth (cm)

colnames(Teeth_Data_NZ)

ggplot(Teeth_Data_NZ, aes(x=Tooth_Width, y=Tooth_Height_altitude,
                          col= factor(Tooth_Position),
                          fill= factor(Tooth_Position)))+
  geom_smooth(method=loess, se=T)+
  geom_point(shape=20) +
  scale_color_discrete(name="Tooth Position")+
  scale_fill_discrete(name="Tooth Position")+
  facet_grid(. ~ Species)+
  facet_wrap(. ~ Species, scales = "free")+
  scale_y_continuous("Tooth Height (cm)")+
  scale_x_continuous("Tooth Width (cm)")+
  labs(title="Tooth Width vs Tooth Height")


# Blade width (cm) vs Tooth Width (cm)

colnames(Teeth_Data_NZ)

ggplot(Teeth_Data_NZ, aes(x=Blade_Length, y=Tooth_Width,
                          col= factor(Tooth_Position),
                          fill= factor(Tooth_Position)))+
  geom_smooth(method=loess, se=T)+
  geom_point(shape=20) +
  scale_color_discrete(name="Tooth Position")+
  scale_fill_discrete(name="Tooth Position")+
  facet_grid(. ~ Species)+
  facet_wrap(. ~ Species, scales = "free")+
  scale_x_continuous("Blade Length (cm)")+
  scale_y_continuous("Tooth Width (cm)")+
  labs(title="Blade Length vs Tooth Width")




# Blade Perimeter (cm) vs Tooth Perimeter (cm)

colnames(Teeth_Data_NZ)

ggplot(Teeth_Data_NZ, aes(x=Blade_Perimeter, y=Tooth_Perimeter,
                          col= factor(Tooth_Position),
                          fill= factor(Tooth_Position)))+
  geom_smooth(method=loess, se=T)+
  geom_point(shape=20) +
  scale_color_discrete(name="Tooth Position")+
  scale_fill_discrete(name="Tooth Position")+
  facet_grid(. ~ Species)+
  facet_wrap(. ~ Species, scales = "free")+
  scale_x_continuous("Blade Perimeter (cm)")+
  scale_y_continuous("Tooth Perimeter (cm)")+
  labs(title="Blade Perimeter vs Tooth Perimeter")




# Blade Area (cm^2) vs Tooth Area (cm^2)

colnames(Teeth_Data_NZ)

ggplot(Teeth_Data_NZ, aes(x=Blade_Area, y=Tooth_Area,
                          col= factor(Tooth_Position),
                          fill= factor(Tooth_Position)))+
  geom_smooth(method=loess, se=T)+
  geom_point(shape=20) +
  scale_color_discrete(name="Tooth Position")+
  scale_fill_discrete(name="Tooth Position")+
  facet_grid(. ~ Species)+
  facet_wrap(. ~ Species, scales = "free")+
  scale_x_continuous("Blade Area" ~(cm^2))+
  scale_y_continuous("Tooth Area" ~(cm^2))+
  labs(title="Blade Area vs Tooth Area")





# Plot bar chart of parameters
# plot mean + error margin/standard deviation





#Plot heirarchy
install.packages("hclust")

library(hclust)








# OPTIONAL - examine the structure to see that PCs have values
str(MyPC)

# OPTIONAL - EXTRACT PC SCORES
MyPC$x

# Combine dataframes
Teeth_Data_NZ2 <- cbind(Teeth_Data_NZ, MyPC$x[,1:7])

# View combined dataframe
head(Teeth_Data_NZ2)







# RUN CORRELTATION COEFFICIENT BETWEEN VARIABLES AND PCS
# Instead of biplot to interpret relationship between each variables 
# and PC, statistically, you can look at correlations between
# the original variables and the PCs to interpret how one PC changes,
# whether the variables go up or down and by how much
# it's a pearson correlation coefficient

# Correlation between variables and PCs
cor(Teeth_Data_NZ2[15:29])


# PLOT LONGITUDINAL DATA WITH GEOM POINT, GEOM LINE

# ERROR - A continuous variable can not be mapped to linetype
# You are getting this error message because location is 
# continuous variable (numeric variables are treated as 
# continuous in ggplot) and for linetype= only discrete 
# variables can be used. To use it for linetype= 
# convert location to factor.  
# Add superscript (square) to cm in label - "label"~(cm^2)

colnames(Teeth_Data_NZ)

#Length vs Width
ggplot(Teeth_Data_NZ, aes(Blade_Length, Tooth_Position,
                             col=Species, fill=Species)) +
    geom_smooth(method= loess) +
    geom_point(shape=21) +
    labs(title="Blade Length BB vs Tooth Position") +
    xlab("Blade Length (cm)")+
    ylab("Tooth Position")
