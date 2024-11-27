# === use case start ======= 
# Exploratory Data Analysis using R
# Objective:
# From this assignment we learnt how to apply our learning in R to analyze a dataset effectively. 
# We used key techniques such as creating and manipulating data frames, slicing, sampling, utilizing # apply-family functions (apply, lapply, sapply, mapply), performing summary operations (mean, # sum etc), and visualizing data to derive insights.# Steps: 
# 1. Importing a Dataset from a pre-existing dataset (iris,) from R’s built-in datasets 
# 2. Manipulate data frames 
# 3. Slicing 
# 4. Sampling 
# 5. Utilize apply-family functions (apply, lapply, sapply, mapply) 
# 6. performing summary operations (mean, sum, median, max, min, frequwncy,...) 
# 7. Visualizing data to derive insights 
# Instructions 1.You are required to work on at least one dataset
#               2. Ensure your dataset has at least 4 columns and 50 rows, including numerical and categorical data.
# === use case end ======= 


# ===================== Implememtation start =========================================
# Load the necessary libraries need by the entire script
library(dplyr)  # For data manipulation
library(ggplot2)  # For visualization

# Task 1 create/Import a Dataset
df <- read.csv("/Users/edison-mel/Desktop/Edison/UR-ACED/Programming For Data Scientits/Learn-R/employee_data.csv")
# check the number of rows and column of a dataframe and confirm if it meeting the criteti
cat("This dataframe has",nrow(df),"rows and",ncol(df),"columns") # 6 columns and 914 rows
## print the top 5 rows of the dataframe and check if has columns required
head(df,n=3)

# Task 2 Slicing
## Extract specific rows and columns based on conditions. we have applied the following filter for the 
## purpose of exploring the dataframe in deep

## Filter rows where salary is greater than 50000
high_salary_df <- df[df$salary > 600000, ]
high_salary_df
cat('From this dataframe we have',nrow(high_salary_df),'with salary above 600,000')

## Filter rows where age is between 16 and 35 ( this is considered as youth in context of Rwanda)
youth_people_df <- df[df['age'] >= 18 & df['age'] <= 35,]
youth_people_df
cat('From this dataframe we have',nrow(youth_people_df),'youth peaple(age between 16 and 35')

## Find employees with high scores in a specific year 
top_performers_df <- df[df$score >= 80 & df$year == 2013, ]
top_performers_df
## Display a custom message to the console
cat('In 2013 we have',nrow(top_performers_df),'exceeding score of 80')

# Task 3 Sampling:
## Randomly select a subset of rows from the dataset.
## Select a random sample of 10 rows.

random_10_rows <-  sample(1:nrow(df), size=10) # sample 10 rows
random_10_rows_df <- df[random_10_rows,]  # generate a dataframe of the corresponding sample
random_10_rows_df
## Display a custom message to the console
cat('The sampled dataset has',nrow(random_10_rows_df))

# Task 3 Apply-family Functions
## 3.1 apply: Apply a function across rows or columns.

# Let slice all numerical columns in our data apply-family functions
df_numeric <- df[, sapply(df, is.numeric) & names(df) != "year"] # this return age salary score columns only
df_numeric

## compute column wise mean
cols_mean <- apply(df_numeric, 2, mean) # Sum of each column, 2 represent row margin
cols_mean
## Display a custom message to the console
cat('The average is',cols_mean[['age']],'Average salary is',cols_mean['salary'],'Finally average score is',cols_mean['score'])


# 3.2 lapply(): Compute range for each numeric column
# use range function returns a list with min and max for each column
## this is crucial to understand the variation of values within a column
col_min_max <- lapply(df_numeric, range)
col_min_max

# 3.3 sapply(): Summarize numeric columns with multiple statistics
# Transforms list output to matrix/vector for easier reading

# Define a function to calculate summary statistics for a column
calculate_summary <- function(column) {
  c(
    mean = mean(column, na.rm = TRUE),    # Calculate mean, ignoring NA values
    median = median(column, na.rm = TRUE),
    min = min(column, na.rm = TRUE),      
    max = max(column, na.rm = TRUE)
  )
}

# Apply the function to each column of the numeric dataframe
column_summaries <- sapply(df_numeric, calculate_summary)
column_summaries


# 3.4 mapply(): Combine multiple columns using custom function
# Example: Create a new column combining age and salary
# Define a function to calculate the combined metric
calculate_combined_metric <- function(age, salary) {
  # Compute the product of age and salary, divided by 1000
  age * salary / 1000
}

# Use mapply to apply the function element-wise to two columns (age and salary)
combined_metric <- mapply(
  calculate_combined_metric,  # The function to apply
  df$age,                     # First argument: age column
  df$salary                   # Second argument: salary column
)
# Display the result to the console
combined_metric

# Task 4 Summary Operations:
# Load library to allow readable and Concise Syntax,Specialized Functions for Common Operations, 
# and leverage pipe operator(%>%) for easy chaining of the operation

## 5.1 Numeric columns statistics excluding the 'year' column
## Calculate summary statistics for numeric columns, excluding the 'year' column
view_stats <- df %>%
  summarize(
    across(
      .cols = where(is.numeric) & !c(year),  # Select numeric columns excluding 'year'
      .fns = list(
        mean = ~mean(., na.rm = TRUE),       # Calculate mean on all columns, ignoring NA values
        sum = ~sum(., na.rm = TRUE)         # Calculate sum on all columns, ignoring NA values
      )
    )
  )

# View the summarized statistics
view_stats


# Get total count by departments using builtin table function
count_by_departnemt <- table(df$Department)
count_by_departnemt

# Get total count by gender
count_by_gender <- table(df$gender)
count_by_gender

# Task 6 Visualization

# 1. Create a scatter plot to visualize the relationship between Age and Score
age_ver_score <- ggplot(df, aes(x = age, y = score)) +  # Define the plot and aesthetics
  geom_point(alpha = 0.7) +                                            # Add points with transparency for better visualization
  labs(                                                                # Add plot labels
    title = "Relationship Between Age and Performance Score",                                 # Set the plot title
    x = "Age",                                                         # Set the label for the x-axis
    y = "Score"                                                        # Set the label for the y-axis
  )

#Print the histogram to the  plotting window
print(age_ver_score) 

# 2 Bar Plot: Count by Department

# Create a bar chart to visualize the number of employees per department
department_count <- ggplot(df, aes(x = Department)) +  # Define the dataset and map 'Department' to the x-axis
  geom_bar(fill = "darkgreen") +                       # Add bars to the plot with a green fill color
  geom_text(stat = "count",                            # Add text for each bar using 'count' statistics
            aes(label = ..count..),                   # Display the count value above each bar
            vjust = -0.5,                             # Adjust the vertical position of the text
            color = "black",                          # Set the color of the text
            size = 3.5) +                             # Set the size of the text
  labs(                                                # Add labels to the plot
    title = "Number of Employees by Department",       # Set the title for the plot
    x = "Department",                                  # Label for the x-axis
    y = "Total Number of Employees"                   # Label for the y-axis
  )

# Print the bar chart to the plotting window
print(department_count)


# 3. Create a histogram to visualize the distribution of salary values
salary_hist <- ggplot(df, aes(x = salary)) +          # Define the dataset and map 'salary' to the x-axis
  geom_histogram(                                     # Add a histogram layer to show the distribution of salaries
    bins = 20,                                        # Set the number of bins (intervals) to divide salary ranges
    fill = "yellow",                                   # Use yellow color to fill the bars
    color = "black"                                   # Set black borders for the bars for better contrast
  ) +
  labs(                                               # Add descriptive labels to the plot
    title = "Salary Distribution",                   # Set the title for the histogram
    x = "Salary",                                     # Label the x-axis as 'Salary' (the variable being analyzed)
    y = "Frequency"                                   # Label the y-axis as 'Frequency' (count of salaries in each bin)
  )

# Print the histogram to the  plotting window
print(salary_hist)

# Get more insight from the data

# pie chart presenting gender 
gender_pie <- ggplot(df, aes(x = "", fill = gender)) +
  geom_bar(width = 1, stat = "count") +                     # Create bar segments for the pie chart
  coord_polar("y", start = 0) +                             # Transform to pie chart
  geom_text(aes(label = scales::percent(..count../sum(..count..))), # Add percentage labels
            stat = "count", position = position_stack(vjust = 0.5)) +
  labs(
    title = "Employee Count by Gender",                     # Add title
    fill = "Gender"                                         # Add legend label
  ) +
  theme_void()                                              # Remove unnecessary axes and gridlines

# Print the pie chart
print(gender_pie)


# Line plot of average score over years
# Calculate average score by year
avg_score_by_year <- df %>%
  group_by(year) %>%
  summarize(mean_score = mean(score, na.rm = TRUE))

# Line plot of average score over years with values
line_plot <- ggplot(avg_score_by_year, aes(x = year, y = mean_score)) +
  geom_line(color = "blue", size = 1) +                # Add the line connecting the points
  geom_point(color = "red", size = 2) +               # Add red points on the line
  geom_text(aes(label = round(mean_score, 2)),        # Add labels displaying the mean_score values
            vjust = -0.5,                             # Adjust the vertical position of the text
            size = 3.5,                               # Set the size of the text
            color = "black") +                        # Set the color of the text
  labs(title = "Average Score Variation by Year",      # Title for the plot
       x = "Year",                                    # Label for the x-axis
       y = "Average Score")                           # Label for the y-axis

print(line_plot)


# ===================== Implememtation END =========================================