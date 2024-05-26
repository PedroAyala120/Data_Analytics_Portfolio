# Shop Customer Data Analytics Case Study

author: "Pedro Ayala"
       
date: "2023-09-18"
                                                          
## Table of Contents 
                                                                                                                                         
1. [Introduction](#introduction)
2. [Business Objectives](#business_objectives)
3. [Dataset Overview](#dataset_overview)
4. [Data Cleaning and Preprocessing](#data_cleaning_and_preprocessing)
5. [Exploratory Data Analysis (EDA)](#exploratory_data_analysis (eda))
6. [Customer Segmentation](#customer_segmentation)
7. [Customer Profiling](#customer_profiling)
8. [Spending Behavior Analysis](#spending_behavior_analysis)
9. [Age and Income Insights](#age_and_income_insights)
10. [Profession and Experience Impact](#profession_and_experience_impact)
11. [Family Size Dynamics](#damily_size_dynamics)
12. [Conclusion](#conclusion)
13. [Recommendations](#recommendations)

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
### Install Packages
```{r warning=FALSE}
install.packages("tidyverse")
install.packages("dplyr")
```
### Load Libraries
```{r warning=FALSE}
library(tidyverse)
library(dplyr)
library(ggplot2)
# Load CSV file
Customers <- read.csv("CSV files/Customers.csv") 
```
## R Markdown

# Shop Customer Data Analytics Case Study

## 1. Introduction

In this case study, we will perform data analytics on the "Shop Customer Data" dataset to gain valuable insights into customer behavior and demographics. The dataset contains information about shop customers, including their gender, age, annual income, spending score, profession, work experience, and family size. We will follow a structured approach to meet the business objectives.

The analysis seeks to uncover patterns, insights, and trends that can aid the shop's owner in better understanding their customers.

## 2. Business Objectives

### The primary objectives of this analysis are as follows:

- **Customer Segmentation:** Identify distinct customer segments based on demographic and behavioral attributes.
- **Customer Profiling:** Create profiles of different customer segments to understand their characteristics and preferences.
- **Spending Behavior Analysis:** Investigate spending patterns and behaviors to tailor marketing strategies.
- **Age and Income Insights:** Analyze the relationship between age, annual income, and spending score.
- **Profession and Experience Impact:** Explore how profession and work experience influence spending habits.
- **Family Size Dynamics:** Examine the effect of family size on spending patterns.

## 3. Dataset Overview

### The data set consists of 2000 records with 8 columns:

1. **Customer ID:** A unique identifier for each customer.
2. **Gender:** The gender of the customer (e.g., Male, Female, Other).
3. **Age:** The age of the customer.
4. **Annual Income:** The annual income of the customer.
5. **Spending Score:** A score assigned by the shop based on customer behavior and spending nature.
6. **Profession:** The profession of the customer.
7. **Work Experience:** The customer's work experience in years.
8. **Family Size:** The size of the customer's family.

## 4. Data Cleaning and Preprocessing

### Handle missing values and outliers in the dataset.

```{r}
# Handle missing values
# Remove rows with blank or empty "Profession" values
Customers <- Customers %>%
  filter(!is.na(Profession) & Profession != "")
# Remove rows where Annual Income equals 0
Customers <- Customers[Customers$Annual.Income.... != 0, ]
```

In this code chunk, we remove rows with missing data to keep the data accurate and clean because we can’t assume that rows with no profession listed are unemployed or not so we must assume the data is incomplete and remove this inconsistency. Also, we removed rows with an annual income of zero because logically a customer must have an income to buy products. It is safe to remove these rows because the data set is still big enough to be used for analysis.

```{r}
# Handle inconsistent data
# Remove rows with Age less than 14 or Work Experience larger than Age
Customers <- Customers %>%
  filter(Age >= 14, Work.Experience <= Age)
```

In this code chunk, we handle further inconsistencies with the data such as customers with professions but are too young to be legally working, so we removed all the rows with ages that are below the legal working age and any customer that has more years of work experience than their age.

```{r}
# Detect and handle outliers
# Remove rows that are outside the 99th percentile in Annual Income
Customers <- Customers %>%
  filter(Annual.Income.... <= quantile(Annual.Income...., 0.99))

# Remove rows that are outside the 99th percentile in Age
Customers <- Customers %>%
  filter(Age <= quantile(Age, 0.99))

# Remove rows that are outside the 99th percentile in Spending Score
Customers <- Customers %>%
  filter(Spending.Score..1.100. <= quantile(Spending.Score..1.100., 0.99))
```

This code chunk filters the dataset to keep only those rows where the "AnnualIncome" is not an extreme outlier, as defined by values below or equal to the 99th percentile of "AnnualIncome." I chose 0.99 because the 99th percentile is a common threshold for removing extreme outliers.

### Convert categorical variables like Gender and Profession into numerical representations.

```{r}
# Convert Categorical Variables to Numerical
Customers <- Customers %>%
  mutate(Gender = as.factor(Gender),
         Profession = as.factor(Profession))
```

This code chunk is a crucial preprocessing step when working with machine learning and data analysis. It's necessary because many machine learning algorithms require numerical input data.

### Normalize or scale relevant numerical columns for analysis.

```{r}
# Normalize or Scale Numerical Columns
# Using Min-Max scaling for Age, Annual Income, and Work Experience.
# The formula used here for Min-Max scaling is (x - min(x)) / (max(x) - min(x)), where x represents the original column values.
Scaled_Customers <- Customers %>%
  mutate(Age = (Age - min(Age)) / (max(Age) - min(Age)),
         Annual.Income.... = (Annual.Income.... - min(Annual.Income....)) / (max(Annual.Income....) - min(Annual.Income....)),
         Work.Experience = (Work.Experience - min(Work.Experience)) / (max(Work.Experience) - min(Work.Experience)))
```

Normalization or scaling is important when you have numerical features with different scales or ranges. Scaling ensures that all numerical features have a consistent scale, which can be beneficial for various machine-learning algorithms and data analyses. In this code, Min-Max scaling is applied to three numerical columns: "Age," "AnnualIncome," and "WorkExperience."

After all the data processing we are left with a sample size of 1662 of the original 2000, this gives us a confidence level of 95% with a 0.99% margin of error.

## 5. Exploratory Data Analysis (EDA)

### Generate summary statistics.

```{r}
# Quick preview of the Customers data set and the Customers data set with scaled values
head(Customers)
head(Scaled_Customers)

# Calculates the average of the "Age", "Annual Income", and "Spending Score" columns
result <- Customers %>%
  summarise(
    avg_age = mean(Age),
    avg_income = mean(Annual.Income....),
    avg_score = mean(Spending.Score..1.100.),
    total_customers = n()
  )

# Print the calculated averages
print(result)
```
![case 1](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/27f84dec-b2b3-41ce-af03-7905e75ec2fa)
![case 2](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/a736c8c1-347e-4ef8-b5ee-c874cd5f1d19)
![case 3](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/61310261-0818-45e8-b0ba-e69e2d1c444e)

Here is a quick preview of the data we have to work with before analysis.

### Create distribution plots and histograms to visualize the distribution of Age, Annual Income, and Spending Score.

```{r}
# Calculate binwidth using Scott's rule
n <- length(Customers$Age)
estimated_binwidth_age <- 3.5 * sd(Customers$Age) / (n^(1/3))
# Plot the histogram for age
ggplot(Customers, aes(x = Age)) + geom_histogram(binwidth = estimated_binwidth_age, fill = "blue", color = "black") + labs(title = "Age Distribution", x = "Age", y = "Customers") +
  theme_minimal()

# Calculate binwidth using Scott's rule
n <- length(Customers$Annual.Income....)
estimated_binwidth_income <- 3.5 * sd(Customers$Annual.Income....) / (n^(1/3))
# Plot the histogram for annual income
ggplot(Customers, aes(x = Annual.Income....)) + geom_histogram(binwidth = estimated_binwidth_income, fill = "green", color = "black") + labs(title = "Annual Income Distribution", x = "Annual Income", y = "Customers") +
  theme_minimal()

# Calculate binwidth using Scott's rule
n <- length(Customers$Spending.Score..1.100.)
estimated_binwidth_spending_score <- 3.5 * sd(Customers$Spending.Score..1.100.) / (n^(1/3))
# Plot the histogram for Spending Score
ggplot(Customers, aes(x = Spending.Score..1.100.)) + geom_histogram(binwidth = estimated_binwidth_spending_score, fill = "red", color = "black") + labs(title = "Spending Score Distribution", x = "Spending Score", y = "Customers") +
  theme_minimal()
```
![case 4](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/ddd66e2b-2959-4ed1-810e-680a3754be66)
![case 5](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/cb9c0e4a-490c-4af9-ae97-4e0064beae71)
![case 6](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/5b7ac8e1-e656-4d94-9aa4-5080400ae177)

I use Scott's rule to calculate the binwidth of the histograms (binwidth = 3.5 * standard deviation(data) / (n^(1/3))). Scott's rule is commonly used when you assume that your data follows a relatively normal distribution without significant outliers. It tends to create narrower bins and more detail in the histogram.

We create three of these distribution plots and histograms provide a visual representation of the data distribution for each variable. They help us understand the shape of the data, whether it's skewed, and where its central tendency lies. Histograms also make it easier to spot potential patterns or anomalies in the data, such as multiple modes or unusual spikes.

### Explore gender distribution and its relationship with other attributes.

```{r}
# Creates a bar graph of the number of customers who are male and female
gender_counts <- Customers %>% group_by(Gender) %>% summarise(count = n())
ggplot(gender_counts, aes(x = Gender, y = count)) + geom_bar(stat = "identity", fill = "purple") + labs(title = "Gender Distribution")
```
![case 7](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/39c60b47-c69e-4de9-b017-1cdbccf681ef)

Here using a bar graph to visualize the gender distribution of the data, we can identify that a majority of the customers are female.

### Analyze the correlation between variables to identify potential insights.

```{r}
correlation_matrix <- cor(Scaled_Customers[, c("Age", "Annual.Income....", "Spending.Score..1.100.", "Work.Experience", "Family.Size")])
```

The correlation_matrix now contains a table that displays the pairwise correlations between the selected variables. The values in the matrix range from -1 to 1.

A value of 1 indicates a perfect positive linear correlation, meaning that as one variable increases, the other also increases linearly.
A value of -1 indicates a perfect negative linear correlation, meaning that as one variable increases, the other decreases linearly.
A value close to 0 indicates little to no linear correlation between the variables.

```{r}
# Convert the correlation matrix to a data frame for plotting
correlation_df <- as.data.frame(correlation_matrix)

# Melt the data frame for ggplot2
correlation_df <- correlation_df %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value")

# Visualize the correlation matrix as a heatmap
ggplot(data = correlation_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "lightblue", mid = "white", high = "darkblue", midpoint = 0, limits = c(-1, 1)) +
  geom_text(aes(label = round(value, 2)), vjust = 1) +  # Add labels with rounded correlation values
  theme_minimal() +
  labs(title = "Variable Correlation Heatmap",
       x = "Variables",
       y = "Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
```
![case 8](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/ea6d38ba-ecc5-4af6-ae82-00c00bafead2)

The resulting plot is a heat map where each cell represents the correlation between two variables. The color intensity of each cell indicates the strength and direction of the correlation. The correlation matrix can provide valuable insights into the relationships between these variables. Here we see:
The correlation between "Family Size" and "Annual Income" is positive and significant, it suggests that customers with bigger families tend to have higher annual incomes.
The correlation between "Spending Score" and "Annual Income" is positive, it implies that customers with higher incomes tend to have higher spending scores.
The correlation between "Spending Score" and "Work Experience" is negative, it implies that customers with less work experience tend to have higher spending scores.

This visualization allows us to quickly identify patterns and relationships between variables, which can be essential for making informed decisions in data analysis and interpretation. 

## 6. Customer Segmentation

### Apply clustering techniques (e.g., K-means) to segment customers based on Age, Annual Income, and Spending Score.

```{r}
# Select the columns for clustering
data_for_clustering <- Customers[, c("Age", "Annual.Income....", "Spending.Score..1.100.")]

# Determine the range of 'k' values to test
k_values <- 1:10

# Calculate within-cluster variances (WCSS) for each 'k'
wcss <- numeric(length(k_values))
for (i in 1:length(k_values)) {
  kmeans_result <- kmeans(data_for_clustering, centers = k_values[i])
  wcss[i] <- kmeans_result$tot.withinss
}

# Create an elbow method plot to determine the optimal 'k'
elbow_plot <- data.frame(K = k_values, WCSS = wcss)

ggplot(elbow_plot, aes(x = K, y = WCSS)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method for Optimal K", x = "Number of Clusters (K)", y = "Within-Cluster Sum of Squares (WCSS)")
```
![case 9](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/2f825ce0-680e-4780-8043-38f260b5b7ad)

The elbow method is used to determine the optimal number of clusters in a dataset for K-means clustering or other clustering algorithms. It helps identify the point at which adding more clusters does not significantly improve the model's performance. The method is called "elbow" because the plot of the number of clusters against the within-cluster variance resembles an elbow, and the "elbow point" represents the optimal number of clusters, in this case being 3.

```{r}
# K-means clustering
optimal_k <- 3  # Adjusted based on plot

# Perform K-means clustering with the optimal K value
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(data_for_clustering, centers = optimal_k)

# Add cluster labels to the original data set
Customers$Cluster <- as.factor(kmeans_result$cluster)

# Visualize customer segments using scatter plots for different combinations of variables
ggplot(Customers, aes(x = Annual.Income...., y = Spending.Score..1.100., color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "Customer Segmentation", x = "Annual Income", y = "Spending Score") +
  theme_minimal()

ggplot(Customers, aes(x = Annual.Income...., y = Age, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "Customer Segmentation", x = "Annual Income", y = "Age") +
  theme_minimal()

# Describe each customer segment's characteristics
segment_summary <- Customers %>%
  group_by(Cluster) %>%
  summarize(
    Avg_Age = mean(Age),
    Avg_AnnualIncome = mean(Annual.Income....),
    Avg_SpendingScore = mean(Spending.Score..1.100.),
    Count = n()
  )

print(segment_summary)
```
![case 10](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/3d695742-865e-4e05-a585-42fcd1fc9cf1)
![case 11](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/8ae837e0-b397-4b1a-8957-abaa9955f20e)
![case 12](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/4701bccc-ed8d-4077-9fa1-14d5a8fa53c7)

After performing K-means clustering and assigning customers to different segments, we can describe the characteristics and behaviors of each customer segment based on the provided data set attributes, which include "Age," "Annual Income," and "Spending Score."

## 7. Customer Profiling

The following descriptions provide a high-level overview of each segment's characteristics and behaviors based on "Annual Income" and "Spending Score."

**Segment 1 (Cluster 1) - "Mature, Moderate Spenders" Customers:**

Average Age: Around 57 years old, with a mature customer base.
Average Annual Income: Moderate annual income of approximately $103,130.
Average Spending Score: A moderate spending score of about 49.23, indicates balanced spending habits.
Number of Customers: This segment comprises 596 individuals.

**Segment 2 (Cluster 2) - "Affluent Shoppers" Customers:**

Average Age: Typically around 56 years old, with mature customers.
Average Annual Income: High annual income averaging about $159,817.
Average Spending Score: A relatively high spending score of approximately 51.84, indicating a willingness to spend generously.
Number of Customers: This segment includes 574 individuals.

**Segment 3 (Cluster 3) - "Economical Shoppers" Customers:**

Average Age: Around 51 years old, with a mix of younger and older individuals.
Average Annual Income: Lower annual income, averaging about $54,322.
Average Spending Score: A low spending score of around 49.16, reflecting budget-conscious behavior.
Number of Customers: This segment encompasses 492 individuals.

## 8. Spending Behavior Analysis

These customer profiles provide a general understanding of the preferences, behaviors, and traits of each customer segment based on the data set attributes.

**Segment 1 (Cluster 1) -** "Mature, Moderate Spenders" Customers: Characteristics: Customers in this segment are typically mature, with an average age of around 57 years. They have moderate annual incomes, averaging about $103,130. Their spending scores are at a moderate level, approximately 49.23.

  - Behaviors: These customers are neither high-spenders nor tight-budget shoppers, striking a balance in their     spending habits. They may make purchases based on their needs and preferences. Marketing efforts can focus on     maintaining customer satisfaction and encouraging slightly higher spending among this segment.

**Segment 2 (Cluster 2) -** "Affluent Shoppers" Customers: Characteristics: Customers in this segment are typically mature, with an average age of around 56 years. They have high annual incomes, averaging about $159,817. Their spending scores are relatively high, approximately 51.84.

  - Behaviors: These customers are likely to be high-value clients who can contribute significantly to the shop's revenue. They may prefer premium products and personalized services, making them prime targets for up selling and cross-selling. Marketing strategies can focus on retaining and further enhancing the shopping experience for this segment.

**Segment 3 (Cluster 3) -** "Economical Shoppers" Customers: Characteristics: Customers in this segment have an average age of around 51 years, with a mix of younger and older individuals. They have relatively low annual incomes, averaging about $54,322. Their spending scores are low, around 49.16, indicating budget-conscious behavior.
  
  - Behaviors: These customers are budget-conscious and price-sensitive, prioritizing savings in their shopping decisions. They are less likely to make extravagant purchases and prefer value-based offerings. Marketing strategies can aim at value-based products, discounts, and promotions to attract and retain these customers.

## 9. Age and Income Insights

### Plot visualizations (e.g., scatter plots) to explore the relationship between Age, Annual Income, and Spending Score.

```{r}
# Scatter plot of Age vs. Annual Income
ggplot(Customers, aes(x = Age, y = Annual.Income...., color = Spending.Score..1.100.)) +
  geom_point(size = 3) +
  labs(title = "Age vs. Annual Income vs. Spending Score", x = "Age", y = "Annual Income") +
  theme_minimal()

# Scatter plot of Age vs. Spending Score
ggplot(Customers, aes(x = Age, y = Spending.Score..1.100., color = Annual.Income....)) +
  geom_point(size = 3) +
  labs(title = "Age vs. Spending Score vs. Annual Income", x = "Age", y = "Spending Score") +
  theme_minimal()

# Scatter plot of Annual Income vs. Spending Score
ggplot(Customers, aes(x = Annual.Income...., y = Spending.Score..1.100., color = Age)) +
  geom_point(size = 3) +
  labs(title = "Annual Income vs. Spending Score vs. Age", x = "Annual Income", y = "Spending Score") +
  theme_minimal()
```
![case 13](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/4863bbac-cf66-44d8-8c5f-aa65a19f2cd3)
![case 14](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/02989349-8321-4c98-ae2a-b2c485820187)
![case 15](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/a866a989-51f5-432b-84d2-7dd6659954aa)

We create three scatter plots to explore the relationships:
"Age vs. Annual Income vs. Spending Score"
"Age vs. Spending Score vs. Annual Income"
"Annual Income vs. Spending Score vs. Age"

Each scatter plot uses different combinations of attributes as x and y axes and assigns a color based on the third attribute. This helps visualize the relationships between the variables while considering the impact of the third variable (color).

### Analyze any trends or patterns that emerge from the visualizations:

It appears that there is a trend of increasing annual income after $50,000, with more customers in that income range. Additionally, it's interesting to note that spending scores and age seem relatively even across the board, with no significant correlation between these variables.

**Summarize the key patterns and observations:**

- In the "Age vs. Annual Income vs. Spending Score" plot, there is a pronounced cluster of customers with moderate to high-income levels and spending scores. Customers with lower income levels are less densely populated.

- In the "Age vs. Spending Score vs. Annual Income" plot, neither age nor income appears to have a direct relationship with spending scores. Spending scores are evenly distributed across age groups, and income levels are scattered without a clear correlation.

- In the "Annual Income vs. Spending Score vs. Age" plot, there is a notable increase in customers with annual incomes exceeding $50,000, which can also be observed in the first plot. Below $50,000 in annual income, the number of customers is relatively low. However, spending scores and age remain evenly distributed, suggesting that income might be a more influential factor in spending behavior.

Overall, these visualizations indicate that income levels play a significant role in determining spending behavior, with higher incomes potentially leading to higher spending scores. Age and spending scores do not seem to have a strong correlation in these plots. These insights can guide marketing and customer segmentation strategies to target customers based on their income levels to maximize revenue and customer satisfaction.

## 10. Profession and Experience Impact

### Visualize the spending score distribution for different professions and work experience groups.

```{r}
profession_spending <- Customers %>%
  group_by(Profession) %>%
  summarize(Avg_SpendingScore = mean(Spending.Score..1.100.))

# Create "WorkExpGroup" column
Customers <- Customers %>%
  mutate(WorkExpGroup = cut(Work.Experience, breaks = c(-1, 5, 10, 15, max(Work.Experience)), labels = c("0-5", "6-10", "11-15", "16+")))

work_experience_spending <- Customers %>%
  mutate(WorkExpGroup = cut(Work.Experience, breaks = c(-1, 5, 10, 15, max(Work.Experience)), labels = c("0-5", "6-10", "11-15", "16+"))) %>%
  group_by(WorkExpGroup) %>%
  summarize(
    Avg_SpendingScore = mean(Spending.Score..1.100.),
    Num_of_Cutomers = n()  # Add a count column to work_experience_spending
  )

head(work_experience_spending)
```
![case 16](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/1091a8f8-197c-4718-a993-51395d2b9610)

The code chunk above creates a table for categorizing the customers based on years of experience for easier analysis as well as be used in the following code chunk for creating bar graphs and histogram visualizations.

```{r}
# Bar plot of average spending score by profession
ggplot(profession_spending, aes(x = reorder(Profession, -Avg_SpendingScore), y = Avg_SpendingScore)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Spending Score by Profession", x = "Profession", y = "Average Spending Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(work_experience_spending, aes(x = WorkExpGroup, y = Avg_SpendingScore)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Average Spending Score by Work Experience Group", x = "Work Experience Group", y = "Average Spending Score") +
  theme_minimal()

# Histogram of spending score distribution by profession
ggplot(Customers, aes(x = Spending.Score..1.100., fill = Profession)) +
  geom_histogram(binwidth = estimated_binwidth_spending_score, position = "dodge") +
  labs(title = "Spending Score Distribution by Profession", x = "Spending Score") +
  theme_minimal()

# Histogram of spending score distribution by work experience group
ggplot(Customers, aes(x = Spending.Score..1.100., fill = WorkExpGroup)) +
  geom_histogram(binwidth = estimated_binwidth_spending_score, position = "dodge") +
  labs(title = "Spending Score Distribution by Work Experience Group", x = "Spending Score") +
  theme_minimal()
```
![case 17](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/f4a9f393-6472-404f-b74a-2857401961a7)
![case 18](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/e96aff33-5203-4462-85aa-2b0622b151cf)
![case 19](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/50f78469-0cac-4882-b08a-6874648b2969)
![case 20](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/fc23f648-7126-449e-aabe-cff3edeb20d2)

We create bar plots to visualize the average spending score for each profession and work experience group.
We also create histograms to visualize the distribution of spending scores for different professions and work experience groups.

### Analyze how different professions and work experience levels influence spending behavior.

**Analysis of Profession and Work Experience Influence:**

1. **Profession Influence on Spending:**

    - The bar plot of average spending scores by profession shows how different professions correlate with spending behavior.
    - We can observe that the Artist profession has the highest average spending while the Homemaker profession has the lowest.

2. **Work Experience Influence on Spending:**

    - The bar plot of average spending scores by work experience group helps understand how work experience levels impact spending behavior.
    - We can identify that the less amount of work experience a customer has the higher the average spending score they have.

3. **Spending Score Distribution:**

    - The two histograms show the distribution of spending scores within each profession and work experience group.
    - This provides a visual representation of the spread and concentration of spending scores for different segments. 
    - We can observe more clearly the difference in spending patterns for each profession as well as the difference in spending scores for different years of work experience.

## 11. Family Size Dynamics

### Analyze whether larger families tend to have different spending patterns compared to smaller families.

```{r}
# Create a scatter plot to visualize the relationship between Family Size and Spending Score
ggplot(data = Customers, aes(x = Family.Size, y = Spending.Score..1.100.)) +
  geom_point() +
  labs(title = "Scatter Plot of Family Size vs. Spending Score", x = "Family Size", y = "Spending Score") +
  theme_minimal()

# Perform one-way ANOVA
anova_result <- aov(Spending.Score..1.100. ~ Family.Size, data = Customers)

# Print the result
summary(anova_result)
```
![case 21](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/257f2481-fb53-418e-b99e-6fe7c1f969bb)
![case 22](https://github.com/PedroAyala120/Data_Analytics_Portfolio/assets/58533253/8abe5e96-8f17-4942-9090-1b438b23cdfe)

We can observe from the scatter plot that family size is evenly distributed in terms of spending scores, but to define it more clearly we will use a technique called ANOVA to compare spending scores.

The result will include F-statistic, degrees of freedom, and p-value. A small F value and a large p-value (as seen in the printed output) suggest that there are no statistically significant differences in spending scores among different family-size groups. In this case, family size does not appear to have a significant impact on spending habits.

If the p-value were less than a chosen significance level (e.g., 0.05), we could conclude that there are significant differences between the groups. However, in the output, the high p-value suggests that family size does not have a significant effect on spending scores in this data set.

## 12. Conclusion

**Customer Segmentation:**

- Three distinct customer segments were identified: "Mature, Moderate Spenders," "Affluent Shoppers," and "Economical Shoppers," based on Age, Annual Income, and Spending Score.
- Each segment exhibits unique characteristics and spending behaviors.

**Customer Profiling:**

- "Mature, Moderate Spenders" are balanced in spending, "Affluent Shoppers" are high-value customers, and "Economical Shoppers" prioritize budget-conscious shopping.
- Tailoring marketing strategies to each segment can maximize engagement and sales.

**Spending Behavior Analysis:**

- Income levels significantly influence spending behavior, with higher incomes leading to higher spending scores.
- Age and spending scores do not exhibit a strong correlation.
- Marketing efforts can focus on income-based segmentation for more effective targeting.

**Profession and Experience Impact:**

- Artists tend to have the highest average spending scores, while Homemakers have the lowest.
- Less work experience correlates with higher average spending scores.
- Marketing strategies can be tailored to cater to the preferences and behaviors of different professions and experience levels.

**Family Size Dynamics:**

- Family size does not appear to have a significant impact on spending habits, as indicated by a high p-value in ANOVA analysis.
- Marketing strategies should not be heavily influenced by family size in this data set.


## 13. Recommendations

### Leveraging Insights for Enhancing Business Performance:

1. **Segmented Marketing Strategies:**

  - Develop personalized marketing campaigns for each customer segment, addressing their unique preferences and behaviors.
  - For "Affluent Shoppers," focus on premium products and personalized services.
  - For "Mature, Moderate Spenders," encourage slightly higher spending while maintaining customer satisfaction.
  - For "Economical Shoppers," offer value-based products, discounts, and promotions.

2. **Income-Based Targeting:**

  - Prioritize targeting customers with higher incomes, as they tend to have higher spending scores.
  - Create income-specific promotions and loyalty programs to attract and retain high-value customers.

3. **Profession and Experience-Based Strategies:**

  - Tailor marketing messages and product offerings to align with the preferences of different professions.
  - Recognize the potential of customers with less work experience and devise strategies to engage them effectively.

4. **Continuous Monitoring:**

  - Regularly analyze customer data to identify changing trends and adapt marketing strategies accordingly.
  - Explore additional data sources to gain deeper insights into customer behavior.
  
5. **Customer Experience Enhancement:**

  - Improve the in-store and online shopping experience for each customer segment.
  - Gather feedback and use it to refine services and product offerings.
  
6. **Data-Driven Decision-Making:**

  - Continue to use data analytics to inform marketing decisions and measure the impact of strategies.
  - Consider implementing predictive modeling to anticipate future customer behavior.

By applying these insights and strategies, the shop can optimize its marketing efforts, enhance customer engagement, and ultimately improve overall business performance. Regularly revisiting and refining these strategies based on ongoing data analysis will contribute to long-term success.

### How to enhance the data

To enhance the analysis and gain deeper insights into customer behavior and preferences, we can consider incorporating additional data sources and variables. Here are some potential data sources and variables that could be valuable for further analysis:

**Transaction Data:**

  - Detailed transaction records, including items purchased, purchase frequency, and transaction amounts.
  - Purchase history data, such as the types of products or categories customers prefer.
  - Time and date of purchases to identify seasonal trends and peak shopping hours.
  
**Customer Feedback and Surveys:**

  - Customer feedback and survey responses to understand customer satisfaction, preferences, and areas for improvement.
  - Net Promoter Score (NPS) or Customer Satisfaction Score (CSAT) data to gauge overall customer sentiment.
  
**Online Behavior Data:**

  - Website or mobile app analytics data, including click stream data, session duration, and bounce rates.
  - Data on products viewed, added to cart, and purchased online.
  - Conversion rates and drop-off points in the online sales funnel.
  
**Geographic Data:**

  - Customer location data to analyze regional preferences and tailor marketing campaigns to specific geographic areas.
  - Demographic data for different regions to understand how location affects customer behavior.
  
**Competitor Data:**

  - Data on competitors' pricing, promotions, and customer reviews.
  - Market share and competitive positioning data.

**Social Media Data:**

  - Social media mentions, likes, shares, and comments related to the shop or its products.
  - Sentiment analysis of social media conversations to gauge brand perception.
  
**Customer Loyalty Program Data:**

  - Data on customers enrolled in loyalty programs, including points earned, rewards redeemed, and program engagement.
  - Analysis of the impact of loyalty programs on customer retention and spending.

**Economic Indicators:**

  - Economic data such as inflation rates, unemployment rates, and consumer confidence indices for the shop's region.
  - These indicators can help contextualize customer spending behavior in relation to broader economic trends.
  
**Product Inventory Data:**

  - Information on product availability, stock levels, and product life cycle stages.
  - Analysis of product popularity and seasonality.
  
**Customer Segmentation Data:**

  - Behavioral segmentation data based on customer interactions, such as email opens, click-through rates, and response to marketing campaigns.
  - Psycho graphic data, including lifestyle and personality traits.
  
**Customer Support Data:**

  - Customer support interactions and resolution times.
  - Analysis of customer support tickets to identify common issues and areas for improvement.
  
**External Data Sources:**

  - Data from third-party sources, such as demographic data providers, market research reports, and industry benchmarks.
  - Economic forecasts and industry-specific data.

By incorporating these additional data sources and variables, we can create a more comprehensive view of customer behavior, preferences, and market dynamics. This enriched data set will enable us to perform more sophisticated analyses, develop targeted marketing strategies, and make data-driven decisions to enhance the shop's performance and customer engagement.
