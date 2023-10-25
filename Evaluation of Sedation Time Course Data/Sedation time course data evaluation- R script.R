#### Set Parameter ####

# Set folder name containing input data
foldername <- "Example data"

## Set excel file names of input data

# Control name
Contname <- paste("Cherry_Control_Data.xlsx")
# Experimental name
Expname <- "Drgx_Experimental_Data.xlsx"

## Set Name of Control and Experimental
Control <- "Cherry-IR"
Experimental <- "Drgx-IR" 

# Set Observation Time
end_time <- 20

# Choose method for statistical analysis
stat_method <- "t.test"

#### Load Packages ####
library("readxl")
library("ggplot2")
library("tidyr")
library("dplyr")
library ("drc")
library("dplyr")
library("patchwork")
library ("gridExtra")
library("tibble")
library("openxlsx")
library("ggpubr")
library ("svglite")

#### Functions ####

# Calculate Standard error

sem <- function(x) {sd(x)/sqrt(length(x))}


#### Read in data ####
Cherry_data <- as.data.frame(read_excel(paste0(foldername, "/",Contname), sheet = 2))
Drgx_data <- as.data.frame(read_excel(paste0(foldername, "/",Expname),sheet=2))

#### sort data ####

# Set observation time
observation_time <- as.character(seq(0, end_time))

# Remove rows where Number  begins with "#"
Cherry_remove <- subset(Cherry_data, !startsWith(Number, "#"))
Drgx_remove <- subset(Drgx_data, !startsWith(Number, "#"))

# Select only Time Information
Cherry_cut  <- Cherry_remove[, observation_time, ]
Drgx_cut  <- Drgx_remove[, observation_time, ]

# Remove all columns  and rows with only NA values
Cherry_clean <- Cherry_cut[rowSums(!is.na(Cherry_cut)) > 0, colSums(!is.na(Cherry_cut)) > 0]
Drgx_clean <- Drgx_cut[rowSums(!is.na(Drgx_cut)) > 0, colSums(!is.na(Drgx_cut)) > 0]


#### Calculate mean values and sd values ####

# Calculate mean values for each time point
mean_values_cherry <- sapply(Cherry_clean, mean)
mean_values_drgx <- sapply(Drgx_clean, mean)

# Calculate SE values for each time point
se_values_cherry <- sapply(Cherry_clean, sem)
se_values_drgx <- sapply(Drgx_clean, sem)

# Combine in new df
data_cherry <- data.frame(x = as.numeric(colnames(Cherry_clean)), mean = mean_values_cherry, se = se_values_cherry)
data_drgx <- data.frame(x = as.numeric(colnames(Drgx_clean)), mean = mean_values_drgx, se = se_values_drgx)

# Change colnames
colnames(data_cherry)[colnames(data_cherry) == "x"] <- "Time"
colnames(data_drgx)[colnames(data_drgx) == "x"] <- "Time"

colnames(data_cherry)[colnames(data_cherry) == "mean"] <- "Value"
colnames(data_drgx)[colnames(data_drgx) == "mean"] <- "Value"

## Reshape the dataframe 
Cherry_longdat <- pivot_longer(Cherry_clean, cols = everything(), names_to = "Time", values_to = "Value")
Drgx_longdat <- pivot_longer(Drgx_clean, cols = everything(), names_to = "Time", values_to = "Value")

# Time as numeric
Cherry_longdat <- Cherry_longdat %>%
  mutate(Time = as.numeric(Time))

Drgx_longdat <- Drgx_longdat %>%
  mutate(Time = as.numeric(Time))

# Convert into dataframe
Cherry_longdat_df1  <- as.data.frame(Cherry_longdat)
Drgx_longdat_df1  <- as.data.frame(Drgx_longdat)

# remove na values
Cherry_longdat_df2 <- na.omit(Cherry_longdat_df1)
Drgx_longdat_df2 <- na.omit(Drgx_longdat_df1)

#### Fit Sigmoidal model and get summary of fit ####

## Sigmoidal model Cherry data (Control data)

# Plot Data
plot(data=Cherry_longdat_df2, Value ~ Time)
# Fit Model
Cherry_model.LL3<- drm(Value ~ Time, data = Cherry_longdat_df2, fct=LL.3(fixed=c(NA, 1, NA),
                                                                        names = c("Slope", "Upper Limit", "ST50")))
# Summary Model
summary(Cherry_model.LL3)
# Plot Model
plot(Cherry_model.LL3)


## Sigmoidal model Drgx data (Experimental data)

# Plot data
plot(data=Drgx_longdat_df2, Value ~ Time)
# Fit Model
Drgx_model.LL3<- drm(Value ~ Time, data = Drgx_longdat_df2, fct=LL.3(fixed=c(NA, 1, NA), 
                                                                         names = c("Slope", "Upper Limit", "ST50")))
# Summary Model
summary(Drgx_model.LL3)
# Plot Model
plot(Drgx_model.LL3)

#### Plot the inactive Fraction as function of time with Mean Values and SE error bars and the log-logistic model for the total sedation time course data  ####

# Merge data
Cherry_longdat_df2
Drgx_longdat_df2

merged_df <- rbind(Cherry_longdat_df2,Drgx_longdat_df2)

# Set breaks x scale
observation_time
breaks <- as.numeric(observation_time)
breaks

## Set labels x scale
labels <- as.character(breaks)

# Make every second value an empty string
labels[c(FALSE, TRUE)] <- ""
labels

## Generate Plot for total sedation time course data

Combined_plot <- ggplot(data=merged_df, aes(x = Time, y = Value)) +
  # Plot mean values
  geom_point(data=data_cherry, aes(x = Time, y = Value,color=Control))+
  geom_point(data=data_drgx, aes(x = Time, y = Value,color=Experimental))+
  scale_colour_manual(name="legend", values=c("black", "darkred"))+
  # Plot error bars of mean values
  geom_errorbar(data=data_cherry, aes(ymin = Value - se, ymax = Value + se, width = 0.2,color=Control))+
  geom_errorbar(data=data_drgx, aes(ymin = Value - se, ymax = Value + se, width = 0.2,color=Experimental))+
  # Plot sigmoidal model
  geom_smooth(data = Cherry_longdat_df2, method = drm, method.args = list(fct=LL.3(fixed=c(NA, 1, NA))),
              se = FALSE, aes(color="Cherry-IR"))+
  geom_smooth(data = Drgx_longdat_df2, method = drm, method.args = list(fct=LL.3(fixed=c(NA, 1, NA))), 
              se = FALSE, aes(color="Drgx-IR"))+
  
  # Further adjustments of the plot
  theme_bw()+
  labs(x="Time (min)", y="Fraction of inactive flies")+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"),
        #axis.text.x = element_text(size=17),
        legend.text = element_text(size=20),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.7))+
  scale_x_continuous(breaks = breaks, 
                     labels = labels) +
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),labels = c("0","0.25","0.5","0.75","1"))+
  geom_hline(yintercept = 0.5, linetype = "dashed")+
  
  # Change grid 
   theme(panel.grid.minor = element_blank()) # remove small grid

Combined_plot


#### Calculate St50 value (sedation time 50 = time at which half of flies are sedated)  ####


### Fit Sigmoidal model on data and extract St50 values with p values ###

## For Cherry Data (Control data) ##

St50_Cherry_data <- data.frame(File_Count = character(),
                               ST50_Value = numeric(),
                               P_Value = numeric(),
                               stringsAsFactors = FALSE)


# Fit the model and extract xmid values for each row (= n)
for (i in 1:nrow(Cherry_clean)) {
  # Subset the row data
  row_data_cherry <- Cherry_clean[i, ]
  # transform data
  Cherry_longdat <- pivot_longer(row_data_cherry, cols = everything(), names_to = "Time", values_to = "Value")
  # Time as numeric
  Cherry_longdat <- Cherry_longdat %>%
    mutate(Time = as.numeric(Time))
  # as dataframe
  Cherry_longdat_df1  <- as.data.frame(Cherry_longdat)
  # remove nas
  Cherry_longdat_df2 <- na.omit(Cherry_longdat_df1)
  # Fit the model
  Cherry_model.LL3<- drm(Value ~ Time, data =  Cherry_longdat_df2, fct=LL.3(fixed=c(NA, 1, NA), 
                                                                            names = c("Slope", "Upper Limit", "ST50")))
  # Extract xmid value and store in the vector
  xmid_values_cherry <- coef(Cherry_model.LL3)["ST50:(Intercept)"]
  # Extract the p-value
  p_values_cherry <- summary(Cherry_model.LL3)$coefficients["ST50:(Intercept)", "p-value"]
  # Create a temporary dataframe for the current file
  temp_df_cherry <- data.frame(File_Count = paste0("Cherry_", i),
                               ST50_Value =  xmid_values_cherry,
                               P_Value = p_values_cherry)
  
  # Append the temporary dataframe to the result dataframe
  St50_Cherry_data  <- rbind(St50_Cherry_data, temp_df_cherry)
  
}

St50_Cherry_data 


## For Drgx Data (Experimental Data) ##

St50_Drgx_data <- data.frame(File_Count = character(),
                             ST50_Value = numeric(),
                             P_Value = numeric(),
                             stringsAsFactors = FALSE)


# Fit the model and extract xmid values for each row (= n)
for (i in 1:nrow(Drgx_clean)) {
  
  # Subset the row data
  row_data_drgx <- Drgx_clean[i, ]
  # transform data
  Drgx_longdat <- pivot_longer(row_data_drgx, cols = everything(), names_to = "Time", values_to = "Value")
  # Time as numeric
  Drgx_longdat <- Drgx_longdat %>%
    mutate(Time = as.numeric(Time))
  # as dataframe
  Drgx_longdat_df1  <- as.data.frame(Drgx_longdat)
  #remove nas
  Drgx_longdat_df2 <- na.omit(Drgx_longdat_df1)
  # Fit the model
  Drgx_model.LL3<- drm(Value ~ Time, data =  Drgx_longdat_df2, fct=LL.3(fixed=c(NA, 1, NA), 
                                                                        names = c("Slope", "Upper Limit", "ST50")))
  # Extract xmid value and store in the vector
  xmid_values_drgx <- coef(Drgx_model.LL3)["ST50:(Intercept)"]
  # Extract the p-value
  p_values_drgx <- summary(Drgx_model.LL3)$coefficients["ST50:(Intercept)", "p-value"]
  # Create a temporary dataframe for the current file
  temp_df_drgx <- data.frame(File_Count = paste0("Drgx_", i),
                             ST50_Value =  xmid_values_drgx,
                             P_Value = p_values_drgx)
  
  # Append the temporary dataframe to the result dataframe
  St50_Drgx_data  <- rbind(St50_Drgx_data, temp_df_drgx)
  
  
}

St50_Drgx_data

### Calculate the mean and standard error for ST50_Value ###

## Cherry data (Control data)
Cherry_summary <- St50_Cherry_data %>%
  summarise(Mean = mean(ST50_Value), SE = sem(ST50_Value))%>%
  mutate(Group = Control) %>%
  rename(Mean_St50 = Mean)

Cherry_summary

## Drgx data (Experimental data)
 Drgx_summary <- St50_Drgx_data %>%
  summarise(Mean = mean(ST50_Value), SE = sem(ST50_Value))%>%
  mutate(Group = Experimental) %>%
  rename(Mean_St50 = Mean)


Drgx_summary

# Combine the two dataframes
combined_MeanSt50_df <- bind_rows(Cherry_summary, Drgx_summary)
combined_MeanSt50_df


#### Statistical Analysis ####

### check for normal distribution ###
St50_val_Cherry <- St50_Cherry_data$ST50_Value
St50_val_Drgx <- St50_Drgx_data$ST50_Value

values_Drgx <- unlist(Drgx_clean)
values_Cherry <-  unlist(Cherry_clean)

# Perform Shapiro-Wilk test
shapiro.test(St50_val_Cherry)
shapiro.test(St50_val_Drgx)


### Perform Statistical Test (T-Test) ###

# Reorder data
che_stat <- as.data.frame(St50_Cherry_data$ST50_Value)
colnames(che_stat)[1] <- "St50"
che_stat$P_Value  <- St50_Cherry_data$P_Value
che_stat$File  <- St50_Cherry_data$File_Count
che_stat$Group <- Control
che_stat

drg_stat <-as.data.frame(St50_Drgx_data$ST50_Value)
drg_stat$P_Value  <- St50_Drgx_data$P_Value
colnames(drg_stat)[1] <- "St50"
drg_stat$File  <- St50_Drgx_data$File_Count
drg_stat$Group <- Experimental
drg_stat

# Combine in one df
combined_st50_stat <- rbind2(che_stat,drg_stat)
combined_st50_stat

# Perform test and save as df
stats <- as.data.frame(compare_means(St50 ~ Group, data = combined_st50_stat,
              paired = FALSE,method = stat_method))

stats

# Extract the p-value from the t-test result

p_value <- stats$p

# Determine the significance level based on the p-value
if (p_value < 0.001) {
  significance_star <- "***"
} else if (p_value < 0.01) {
  significance_star <- "**"
} else if (p_value < 0.05) {
  significance_star <- "*"
} else {
  significance_star <- "ns"
}

p_value
significance_star


#### Plot St50 values ####

## find y pos value ##

# Find the row with the highest Mean_St50
highest_row <- combined_MeanSt50_df[which.max(combined_MeanSt50_df$Mean_St50), ]

# Extract the y pos value
y_pos <- highest_row$Mean_St50 + highest_row$SE

### ST50 Plot ###

St50_plot2 <- ggplot(combined_st50_stat, aes(x = Group, y =  St50, fill=Group)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge",colour="black", linewidth=0.8, width=0.8, alpha=0.8)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1), geom = "errorbar", 
               position = position_dodge(width = 0.8), width = 0.2)+
  
  # Further adjustements of the plot
  
  scale_fill_manual(values = c("grey10", "grey80")) +
  #scale_x_discrete(labels = names)+
  #scale_y_continuous(limits = c(0, 12.5), expand = c(0, 0))+
  scale_y_continuous(breaks = c(0,2,4,6,8,10,12),labels = c("0","2","4","6","8","10","12"),expand = c(0, 0))+
  theme_bw()+
  labs(x="", y="ST50 (min)")+
  theme(legend.position = "none",
        axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"),
        axis.text.x = element_text(colour = "black", size=20))+
  
  coord_cartesian( xlim = c(1, 2), ylim = c(0, y_pos+1))

#St50_plot2  


## Add significance stars to Plot
p_value
significance_star

St50_plot_sig <- St50_plot2+
           # Add bracket line
           geom_segment(aes(x = 1.5 - 0.5,
                     xend = 1.5 + 0.5,
                     y = y_pos+0.3, yend = y_pos+0.3),
                 linewidth = 1,
                 inherit.aes = FALSE) +
  

          # Add significance stars
           geom_text(aes(x = 1.5, y = y_pos + 0.4, label = significance_star),
                       size = 9, inherit.aes = FALSE,check_overlap = T)


St50_plot_sig

#### Combine Plots ####

All_plots <- grid.arrange(Combined_plot, St50_plot_sig, ncol = 2, widths = c(2, 1)) 


#### Save Evaluation Data #####

###generate Evaluation folder

 # Set name of Evaluation folder
evalfolder <- paste(foldername,"Evaluation")

 # Generate folder
dir.create(file.path(dirname(foldername),evalfolder))

### Save Plot ###

 # Set plot name
plotname <- paste(foldername,"- Ethanol Sedation Assay- Combined Plots - PDF.pdf")

 # Save Plot as pdf
ggsave(filename=plotname,
       path= evalfolder,
       plot = All_plots, 
       device = cairo_pdf, 
       width = 297, 
       height = 210, 
       units = "mm")


 # Save Plot as tif
plotnametif <- paste(foldername,"- Ethanol Sedation Assay- Combined Plots - tiff.tiff")

ggsave(filename=plotnametif,
       path= evalfolder,
       plot = All_plots, 
       device = "tiff", 
       width = 297, 
       height = 210, 
       units = "mm")

#save plot as .svg
plotnamesvg <- paste0(foldername, "- Ethanol Sedation Assay- Combined Plots - svg.svg")

ggsave(filename=plotnamesvg,
       path= evalfolder,
       plot = All_plots, 
       device = "tiff", 
       width = 297, 
       height = 210, 
       units = "mm")

### Save statistical Analysis ###


##save Summary of log logistic Model for total sedation data of each group


## Convert summary of Model to data frame

# Summary of model                                                                                                                                                                                                                        names = c("Slope", "Upper Limit", "ST50")))
Cherry_Mod <-summary(Cherry_model.LL3)
Drgx_Mod <-summary(Drgx_model.LL3)

# Extract Model coeffients and residual standard error with degrees of freedom
Cherry_Mod_df <- as.data.frame (Cherry_Mod$coefficients)
Drgx_Mod_df <- as.data.frame (Drgx_Mod$coefficients)

RSE_Cherry <- as.data.frame(Cherry_Mod$rseMat)
RSE_Drgx <- as.data.frame(Drgx_Mod$rseMat)

# Combine to one df
Cherry_Mod_df[1, c("rse", "df")] <- RSE_Cherry 
Drgx_Mod_df[1, c("rse", "df")] <- RSE_Drgx

# replace nas
Cherry_Mod_df[is.na(Cherry_Mod_df)] = ""
Drgx_Mod_df[is.na(Drgx_Mod_df)] = ""

# Rename to specify Model data
Cherry_Mod_df<- rownames_to_column(Cherry_Mod_df, var="Cherry Data:") %>% head
Drgx_Mod_df<- rownames_to_column(Drgx_Mod_df, var="Drgx Data:") %>% head

# print
Cherry_Mod_df
Drgx_Mod_df

## Combine in one df
# Create an empty column
empty_column <- rep("")

# Combine
Combined_models <- cbind(Cherry_Mod_df,empty_column,Drgx_Mod_df)
Combined_models

# Rename the empty column
names(Combined_models)[names(Combined_models) == 'empty_column'] <- ''

Combined_models


## Save all in one excel file

# Create Excel file and add data frames as sheets
wb <- createWorkbook()

# Add Cherry Model Summary
addWorksheet(wb, "Model Summary")
writeData(wb, sheet = 1, Combined_models, startRow = 1, startCol = 1)

## Add St50 values
addWorksheet(wb, "St50 values")
writeData(wb, sheet = 2, combined_st50_stat, startRow = 1, startCol = 1)

## Add Statistical test results
# Set name of test
stat_test_name <- stats$method[1]
addWorksheet(wb, stat_test_name)
writeData(wb, sheet = 3, stats, startRow = 1, startCol = 1)

# Set name of file
evalname  <- paste(evalfolder,"/",foldername,"_Evaluation.xlsx",sep="")

# Save Excel file
saveWorkbook(wb,evalname, overwrite = TRUE)

