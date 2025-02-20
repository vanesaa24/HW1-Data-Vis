# Part 3.

library(dplyr)
library(ggplot2)
library(ggthemes)

Lung_cancer <- read.csv("lung_cancer_prediction_dataset.csv")
Air_pollution <- read.csv("global_air_pollution_dataset.csv")

#1.Create a Boxplot of Lung Cancer Deaths Distribution.

ggplot(data = Lung_cancer, aes(x = Annual_Lung_Cancer_Deaths)) +
  geom_boxplot() +
  scale_x_continuous(labels = function(x) {
      paste0(x / 1000, "K")}) +
  scale_y_continuous(breaks = NULL) +
  labs(title = "Lung Cancer Deaths Distribution", x = "Death counts") +
  theme_minimal()

#2.Create a Histogram of PM2.5 AQI Values.

ggplot(data = Air_pollution, aes(x = PM2.5_AQI_Value)) +
  geom_histogram() +
  labs(x = "PM2.5 AQI Value", y = "Count", 
       title = "Histogram of PM2.5 AQI Values") +
  theme_minimal()

#3.Create a Density Plot of the Lung Cancer Mortality Rate.

ggplot(data = Lung_cancer, aes(x = Mortality_Rate)) +
  geom_density() +
  labs(title = "Density Plot of the Lung Cancer Mortality Rate",
       x = "Mortality Rate", y =  "Density") +
  theme_minimal()

#4.Create a Scatter Plot by generating 100 random values 
# from both the normal and logistic distributions. 
# The points should be brown and use theme_solarized 
# with argument light set to false.

set.seed(123)
Normal <- rnorm(100)
Logistic <- rlogis(100)
Log_Norm <- data.frame(Normal = Normal, Logistic = Logistic)
  
ggplot(data = Log_Norm, aes(x = Normal, y = Logistic)) +
  geom_point(color = "brown") +
  labs(title = "Scatter Plot") +
  theme_solarized(light = FALSE)

# Part 4.

# Task 2.
Air2 <- Air_pollution %>%
  group_by(Country) %>%
  summarise(PM2.5_AQI_Value = round(mean(PM2.5_AQI_Value),2))

Cancer2 <- Lung_cancer %>%
  group_by(Country) %>%
  summarise(Annual_Lung_Cancer_Deaths = sum(Annual_Lung_Cancer_Deaths))
  
AQIvsDeaths <- inner_join(Air2, Cancer2, by = "Country")

Specific <- c("China", "Japan", "India")
AQIvsDeaths1 <- AQIvsDeaths %>%
  filter(Country %in% Specific)
AQIvsDeaths2 <- AQIvsDeaths %>%
  filter(!Country %in% Specific)

ggplot(data = AQIvsDeaths1, aes(x = PM2.5_AQI_Value,
                                y = Annual_Lung_Cancer_Deaths,
                                colour = Country,
                                size = Annual_Lung_Cancer_Deaths,
                                label = Country)) +
  geom_point() +
  geom_point(data = AQIvsDeaths2, aes(x = PM2.5_AQI_Value,
                                      y = Annual_Lung_Cancer_Deaths,
                                      colour = Country,
                                      size = Annual_Lung_Cancer_Deaths)) +
  geom_text(hjust = 0.5, vjust = 0.5, 
            color = "black", fontface = "bold") +
  labs(title = "PM2.5 AQI Value vs. Annual Lung Cancer Deaths",
       x = "PM2.5 AQI Value",
       y = "Annual Lung Cancer Deaths") +
  theme(panel.background = element_rect(colour = "grey90", fill = NA),
        panel.grid.major = element_line(linetype = 2, size = 0.5, colour =
                                           "grey"),
        panel.grid.minor = element_line(linetype = 2, 
                                        size = 0.2, color = "grey"),
        panel.border = element_rect(color = "black", fill = NA))

# Task 3.
Cancer3 <- Lung_cancer %>%
  filter(Cancer_Stage != "None") %>%
  filter(Years_of_Smoking != 0)

ggplot(data = Cancer3, aes(x = Years_of_Smoking, 
                           y = Cancer_Stage, colour = Gender,
                           shape = Gender)) +
  geom_jitter(alpha = 0.5, size = 1.2) +
  facet_wrap(~Gender) +
  scale_color_manual(values = c("Male" = "#5469f1", "Female" = "#d554f1")) +
  scale_shape_manual(values = c("Male" = "circle", "Female" = "triangle")) +
  labs(title = "Lung Cancer Stage vs. Smoking Years", 
       subtitle = "Comparison by Gender",
       y = "Cancer Stage",
       x = "Years of Smoking") +
  theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(face = "italic"),
          strip.text.x = element_text(face = "bold"),
          legend.position = "bottom")

# Task 4.
Countries <- c("Brazil", "Germany", "India", "Italy",
               "Russian Federation",
               "United States of America")
Air3 <- Air_pollution %>%
  filter(Country == all_of(Countries))

ggplot(data = Air3, aes(x = PM2.5_AQI_Value, fill = Country)) +
  geom_histogram(colour = "black", bins = 100) +
  facet_wrap(~Country, scales = "free_y") +
  scale_fill_viridis_d(option = "plasma") +
  labs(title = "PM2.5 AQI Distribution Across Countries", 
       subtitle = "Comparison of Air Pollution Levels",
       y = "Frequency",
       x = "PM2.5 AQI Value") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic"),
        strip.text.x = element_text(face = "bold"),
        legend.position = "bottom")



