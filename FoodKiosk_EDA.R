#' ---
#' title: "Food Kiosk EDA"
#' author: "Yash Khare"
#' date: "2024-03-16"
#' output:
#'   pdf_document: default
#'   html_document: default
#' ---
#' 
## -------------------------------------------------------------------
options(knitr.purl.inline = TRUE)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(modelr)

#' 
## -------------------------------------------------------------------

#Loading datasets
sold <- read.csv("/Users/yashkhare/Documents/IDMP/PriceOptimization-main/Cafe+-+Sell+Meta+Data.csv",
                 na = "NULL")

transaction <- read_csv("/Users/yashkhare/Documents/IDMP/PriceOptimization-main/Cafe+-+Transaction+-+Store.csv", na = "NULL")

dates <- read_csv("/Users/yashkhare/Documents/IDMP/PriceOptimization-main/Cafe+-+DateInfo.csv",
                  na = "NULL")


#' 
#' ## SOLD Dataset
#' 
## -------------------------------------------------------------------
#SOLD
print(sold)
summary(sold)
#Checking for null values
any(is.na(sold))

#' 
#' Information about Sold df: SELL_ID: a categorical variable, identifier of the combination of items that is contained in the product.
#' 
#' SELL_CATEGORY: “0” identifies single products; the category “2” identifies the combo ones.
#' 
#' ITEM_ID: a categorical variable, identifier of the item that is contained in the product 1-to-1 relation with item_name.
#' 
#' ITEM_NAME: a categorical variable, identifying the name of the item
#' 
#' ## TRANSACTION Dataset
#' 
## -------------------------------------------------------------------
#TRANSACTION

transaction <- transaction |>
  mutate(
    # Convert CALENDAR_DATE to a date object, add 7 years, and then format it back to a character string if necessary
    CALENDAR_DATE = as.character(as.Date(CALENDAR_DATE, format = "%m/%d/%y") %m+% years(7))
  )

head(transaction)
summary(transaction)
#Checking for null values
any(is.na(transaction))

#' 
#' Information about Transaction df: Important: It’s supposed the PRICE for that product in that day will not vary.
#' 
#' In details: CALENDAR_DATE: a date/time variable, having the time always set to 00:00 AM.
#' 
#' PRICE: a numeric variable, associated with the price of the product identified by the SELL_ID.
#' 
#' QUANTITY: a numeric variable, associated with the quantity of the product sold, identified by the SELL_ID.
#' 
#' SELL_ID: a categorical variable, identifier of the product sold.
#' 
#' SELL_CATEGORY: a categorical variable, category of the product sold.
#' 
#' ## DATES Dataset
#' 
## -------------------------------------------------------------------
#DATE INFO

dates <- dates %>%
  mutate(
    # Add 7 years to the YEAR column
    YEAR = YEAR + 7,
    CALENDAR_DATE = as.character(as.Date(CALENDAR_DATE, format = "%m/%d/%y") %m+% years(7))
  )

dates
summary(dates)
#Checking for null values
any(is.na(dates))

#' 
#' ## Since NA values exist in Holiday, we have to deal with them
#' 
## -------------------------------------------------------------------
dates[!complete.cases(dates), ]
#replacing na values with "NO holiday"
dates <- dates %>%
  mutate(HOLIDAY = replace_na(HOLIDAY, "No Holiday"))
dates
summary(dates)
#checking for null data again
any(is.na(dates))

#' 
## -------------------------------------------------------------------
sold_wide <- sold %>%
  mutate(is_filled = 1) %>%
  pivot_wider(names_from = ITEM_NAME, values_from = is_filled, values_fill = list(is_filled = 0)) %>%
  group_by(SELL_ID, SELL_CATEGORY) %>%
  summarise(
    BURGER = sum(BURGER, na.rm = TRUE), 
    COFFEE = sum(COFFEE, na.rm = TRUE), 
    COKE = sum(COKE, na.rm = TRUE), 
    LEMONADE = sum(LEMONADE, na.rm = TRUE),
    .groups = 'drop'  # Automatically ungroup after summarise
  ) %>%
  dplyr::select(-SELL_CATEGORY)


head(sold_wide)

#' 
## -------------------------------------------------------------------
## SOLD + TRANSACTION => MERGED_DATA
merged_data <- inner_join(transaction, sold_wide, by = "SELL_ID")

# View the result
head(merged_data)

#' 
#' Here, I inner joined sold and transaction on sell_id. This is crucial for a comprehensive analysis where we might need to study transaction details alongside the sales data. By selecting to remove SELL_ID from 'sold' and SELL_CATEGORY from 'transaction', it simplifies the dataset by eliminating irrelevant information for the subsequent analysis steps.
#' 
## -------------------------------------------------------------------
kiosk_data <- inner_join(merged_data, dates, by = "CALENDAR_DATE") |>
            dplyr::select(CALENDAR_DATE, PRICE, QUANTITY, SELL_ID, SELL_CATEGORY, BURGER, COFFEE, COKE, LEMONADE, YEAR, HOLIDAY, IS_WEEKEND, IS_SCHOOLBREAK,  AVERAGE_TEMPERATURE, IS_OUTDOOR)

kiosk_data

#' 
## -------------------------------------------------------------------
#csv
#write_csv(kiosk_data,"Kiosk.csv")

#' 
#' ## Univariate Analysis
#' 
#' ## Numerical Variables
#' 
#' ## QUANTITY
#' 
## -------------------------------------------------------------------
# Histogram of QUANTITY
ggplot(kiosk_data, aes(x = QUANTITY)) +
  geom_histogram(binwidth = 5, fill = "grey", color = "black") +
  labs(title = "Histogram of Target QUANTITY", x = "QUANTITY", y = "Frequency")

# Density Plot of QUANTITY
ggplot(kiosk_data, aes(x = QUANTITY)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of QUANTITY", x = "QUANTITY", y = "Density")

# Q-Q Plot of QUANTITY
qqnorm(kiosk_data$QUANTITY)
qqline(kiosk_data$QUANTITY, col = "red")

# Boxplot of QUANTITY
ggplot(kiosk_data, aes(y = QUANTITY)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Target QUANTITY", y = "QUANTITY")

#' 
#' ## PRICE
#' 
## -------------------------------------------------------------------
# Histogram of QUANTITY
ggplot(kiosk_data, aes(x = PRICE)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Histogram of Target PRICE", x = "PRICE", y = "Frequency")

# Density Plot of QUANTITY
ggplot(kiosk_data, aes(x = PRICE)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of PRICE", x = "PRICE", y = "Density")

# Q-Q Plot of QUANTITY
qqnorm(kiosk_data$PRICE)
qqline(kiosk_data$PRICE, col = "red")

# Boxplot of QUANTITY
ggplot(kiosk_data, aes(y = PRICE)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Target PRICE", y = "PRICE")


#' 
#' The PRICE variable seems to be bimodal too and just a little bit right-skewed. The boxplot shows no outliers.
#' 
#' ## AVERAGE TEMPERATURE
#' 
## -------------------------------------------------------------------
# Histogram of AVERAGE_TEMPERATURE
ggplot(kiosk_data, aes(x = AVERAGE_TEMPERATURE)) +
  geom_histogram(binwidth = 5, fill = "grey", color = "black") +
  labs(title = "Histogram of Target AVERAGE_TEMPERATURE", x = "AVERAGE_TEMPERATURE", y = "Frequency")

# Density Plot of AVERAGE_TEMPERATURE
ggplot(kiosk_data, aes(x = AVERAGE_TEMPERATURE)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of AVERAGE_TEMPERATURE", x = "AVERAGE_TEMPERATURE", y = "Density")

# Q-Q Plot of AVERAGE_TEMPERATURE
qqnorm(kiosk_data$AVERAGE_TEMPERATURE)
qqline(kiosk_data$AVERAGE_TEMPERATURE, col = "red")

# Boxplot of AVERAGE_TEMPERATURE
ggplot(kiosk_data, aes(y = AVERAGE_TEMPERATURE)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Target AVERAGE_TEMPERATURE", y = "AVERAGE_TEMPERATURE")

#' 
#' The AVERAGE_TEMPERATURE is bimodal too. This time the distribution is a little bit left-skewed.
#' 
#' ## Categorical variables
#' 
## -------------------------------------------------------------------

kiosk_data %>%
  mutate(MONTH = month(CALENDAR_DATE)) %>%
ggplot(aes(x = as.factor(MONTH), fill = as.factor(MONTH))) +
  geom_bar() +
  labs(title = "Bar Plot of Transactions by MONTH", x = "MONTH", y = "Number of Transactions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() + 
  theme(legend.position = "none")

#' 
#' # Multi-variate Analysis
#' 
#' ## Numerical variables
#' 
#' 1.  PRICE
#' 2.  TEMPERATURE
#' 3.  AVERAGE_TEMPERATURE
#' 4.  YEAR
#' 
#' ## Categorical variables
#' 
#' 1.  SELL_ID
#' 2.  SELL_CATEGORY
#' 3.  BURGER
#' 4.  COFFEE
#' 5.  LEMONADE
#' 6.  COKE
#' 7.  CALENDAR_DATE
#' 8.  IS_WEEKEND
#' 9.  IS_OUTDOOR
#' 10. IS_SCHOOLBREAK
#' 11. CALENDAR_DATE
#' 
## -------------------------------------------------------------------
numerical_r_squared <- sapply(kiosk_data[, sapply(kiosk_data, is.numeric)], function(x) {
  summary(lm(kiosk_data$QUANTITY ~ x, data = kiosk_data))$r.squared
})

top_numerical <- sort(numerical_r_squared, decreasing = TRUE)[1:2]


eta_squared <- function(model) {
  model_sum <- summary(model)
  ss_total <- sum(model_sum[[1]]$SumSq)
  ss_model <- model_sum[[1]]$SumSq[1]
  return(ss_model / ss_total)
}

numerical_var <- "PRICE"

categorical_vars <- c("SELL_ID", "BURGER", "COFFEE", "COKE", "LEMONADE", 
                      "YEAR", "HOLIDAY", "IS_WEEKEND", "IS_OUTDOOR")


for(cat_var in categorical_vars) {
  kiosk_data[[cat_var]] <- as.factor(kiosk_data[[cat_var]])
}


categorical_eta_squared <- list()

for(cat_var in categorical_vars) {
  # Ensure the variable is a factor and drop unused levels
  kiosk_data[[cat_var]] <- droplevels(as.factor(kiosk_data[[cat_var]]))
  
  # Skip the variable if it has less than two levels
  if (length(levels(kiosk_data[[cat_var]])) < 2) {
    next
  }
  
  formula <- as.formula(paste(numerical_var, cat_var, sep = " ~ "))
  categorical_eta_squared[[cat_var]] <- aov(formula, data = kiosk_data)
}


eta_squared <- sapply(categorical_eta_squared, function(model) {
  # Extract the Sum of Squares for the Model (Effect) and Residuals (Error)
  anova_table <- summary(model)
  ss_model <- anova_table[[1]]$"Sum Sq"[1]  # Sum of Squares for the effect
  ss_total <- sum(anova_table[[1]]$"Sum Sq")  # Total Sum of Squares
  eta_sq <- ss_model / ss_total
  eta_sq
})

top_eta_squared <- sort(eta_squared, decreasing = TRUE)[1:6]

head(top_eta_squared)

#' 
#' ## The associations between categorical and numerical variables are computed using the eta-squared metric.
#' 
## -------------------------------------------------------------------
df_for_plot <- data.frame(
  Variable = names(top_eta_squared),
  Eta_squared = top_eta_squared
)

ggplot(df_for_plot, aes(x = Variable, y = Eta_squared, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 6 Associated Categorical Variables with Price", x = "Variable", y = "Eta-squared") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

#' 
#' ## PRICE VS QUANTITY
#' 
## -------------------------------------------------------------------
ggplot(kiosk_data, aes(x = PRICE, y = QUANTITY)) +
  geom_point() +  # Plot the raw data as points
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Add LOESS line
  labs(title = "Scatter Plot of PRICE vs QUANTITY with LOESS",
       x = "Price",
       y = "Quantity") +
  theme_minimal()

#' 
#' ## PRICE VS AVERAGE_TEMPERATURE
#' 
## -------------------------------------------------------------------
ggplot(kiosk_data, aes(x = PRICE, y = AVERAGE_TEMPERATURE)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue")+
  labs(title = "Scatter Plot of PRICE vs Average Temperature with LOESS",x = "Price",y = "Average Temperature")

#' 
#' ## QUANTITY VS AVERAGE_TEMPERATURE
#' 
## -------------------------------------------------------------------
ggplot(kiosk_data, aes(x = AVERAGE_TEMPERATURE, y = QUANTITY)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue")+
  labs(title = "Scatter Plot of AVERAGE_TEMPERATURE vs QUANTITY with LOWESS",x = "Quantity",y = "Average Temperature")

#' 
#' ## SELL_ID VS PRICE
#' 
## -------------------------------------------------------------------
ggplot(kiosk_data, aes(x = factor(SELL_ID), y = PRICE)) +
  geom_boxplot() +
  labs(x = "SELL_ID", y = "PRICE", title = "Interaction between PRICE and SELL_ID") +
  theme_minimal()

#' 
#' ## SELL_ID VS QUANTITY
#' 
## -------------------------------------------------------------------
ggplot(kiosk_data, aes(x = factor(SELL_ID), y = QUANTITY)) +
  geom_boxplot() +
  labs(x = "SELL_ID", y = "QUANTITY", title = "Interaction between QUANTITY and SELL_ID") +
  theme_minimal()

#' 
#' ## SELL_CATEGORY VS PRICE
#' 
## -------------------------------------------------------------------
ggplot(kiosk_data, aes(x = factor(SELL_CATEGORY), y = PRICE)) +
  geom_boxplot() +
  labs(x = "SELL_CATEGORY", y = "PRICE", title = "Interaction between PRICE and SELL_CATEGORY") +
  theme_minimal()

#' 
#' ## COKE VS PRICE
#' 
## -------------------------------------------------------------------
ggplot(kiosk_data, aes(x = factor(COKE), y = PRICE)) +
  geom_boxplot() +
  labs(x = "COKE", y = "PRICE", title = "Interaction between COKE and PRICE") +
  theme_minimal()

#' 
#' # Outlier treatment
#' 
## -------------------------------------------------------------------
cat_cols <- c("COFFEE", "COKE", "LEMONADE", "HOLIDAY", "IS_WEEKEND", "IS_SCHOOLBREAK", "IS_OUTDOOR")
df1 <- kiosk_data %>% mutate_at(cat_cols, factor)

df1 %>%
  filter(SELL_ID == "1070" &
         PRICE < 14)

#' 
## -------------------------------------------------------------------
df1 %>%
  filter(SELL_ID == "1070",
           CALENDAR_DATE >= as.Date("2020-02-27"),
           CALENDAR_DATE <= as.Date("2020-03-02"))

#' 
#' ### There are more than one row for the date 2020-03-01, so there've been a mistake in
#' 
#' ### registering aggregated transactions. Looking at the prices of the neighboring lines,
#' 
#' ### the price of \$15.5 seems the right one, the right quantity seems to be 90 (value present
#' 
#' ### into the 2020-03-01 rows) and the average temperature is 32, due to the same reason.
#' 
#' ### So, let's get the "true/cleaned" row
#' 
## -------------------------------------------------------------------
cleaned_row_20200301_1070 <- df1 %>%
  filter(SELL_ID == "1070" &
         CALENDAR_DATE == "2020-03-01" &
         PRICE == 15.5 &
         AVERAGE_TEMPERATURE == 32) %>%
  distinct()

print(cleaned_row_20200301_1070)

#' 
#' # Now let's filter out all the "old" 2020-03-01 rows and let's add the cleaned row into the data frame
#' 
## -------------------------------------------------------------------
kiosk_data <- rbind(
  kiosk_data %>%
    filter( !(SELL_ID == "1070" &
              CALENDAR_DATE == "2020-03-01") ),
  cleaned_row_20200301_1070 )


#' 
#' # Let's check that now there is only one row for the key (CALENDAR_DATE="2020-03-01", SELL_ID="1070")
#' 
## -------------------------------------------------------------------
kiosk_data %>%
  filter(SELL_ID == "1070" &
           CALENDAR_DATE == "2020-03-01")

#' 
#' # Now let's check if there are other cases of multiple aggregate transaction rows for the same date
#' 
#' # and the same SELL_ID
#' 
## -------------------------------------------------------------------
kiosk_data %>%
  group_by(CALENDAR_DATE, SELL_ID) %>%
  summarise(n = n()) %>%
  filter(n > 1)

#' 
#' # Wow! It seems there had been some issues on 2020-03-01 for each SELL_ID.
#' 
#' # Let's keep only the rows having SELL_ID = "2051" and PRICE \> 15
#' 
## -------------------------------------------------------------------
kiosk_data %>%
  filter(SELL_ID == "2051" &
           PRICE > 15)

#' 
#' # let's check what's happen to the transactions for the day before and the day
#' 
#' # after the 2020-03-01 for SELL_ID="2051"
#' 
## -------------------------------------------------------------------
kiosk_data %>%
  filter(SELL_ID == "2051" &
         CALENDAR_DATE > as.Date("2020-02-26") &
         CALENDAR_DATE <= as.Date("2020-03-03"))

#' 
## -------------------------------------------------------------------
cleaned_row_20200301_2051 <- kiosk_data %>%
  filter(SELL_ID == "2051" &
           CALENDAR_DATE == "2020-03-01" &
           PRICE < 15 &
           QUANTITY == 22 &
           AVERAGE_TEMPERATURE == 32) %>%
  distinct()

print(cleaned_row_20200301_2051)

#' 
#' # Now let's filter out all the "old" 2020-03-01 rows for SELL_ID = "2051" and let's add the cleaned row
#' 
#' # into the data frame
#' 
## -------------------------------------------------------------------
kiosk_data <- rbind(
  kiosk_data %>%
    filter( !(SELL_ID == "2051" &
                CALENDAR_DATE == "2020-03-01") ),
  cleaned_row_20200301_2051 )

#' 
#' # Let's check that now there is only one row for the key (CALENDAR_DATE="2020-03-01", SELL_ID="2051")
#' 
## -------------------------------------------------------------------
kiosk_data %>%
  filter(SELL_ID == "2051" &
           CALENDAR_DATE == "2020-03-01")

#' 
#' # Let's check what's happen to the transactions for the day before and the day
#' 
#' # after the 2020-03-01 for SELL_ID="2052"
#' 
## -------------------------------------------------------------------
kiosk_data %>%
  filter(SELL_ID == "2052" &
           CALENDAR_DATE > as.Date("2020-02-26") &
           CALENDAR_DATE <= as.Date("2020-03-03"))

#' 
## -------------------------------------------------------------------
cleaned_row_20200301_2052 <- kiosk_data %>%
  filter(SELL_ID == "2052" &
           CALENDAR_DATE == "2020-03-01" &
           QUANTITY == 26 &
           AVERAGE_TEMPERATURE == 32) %>%
  distinct()

print(cleaned_row_20200301_2052)

#' 
## -------------------------------------------------------------------
kiosk_data <- rbind(
  kiosk_data %>%
    filter( !(SELL_ID == "2052" &
                CALENDAR_DATE == "2020-03-01") ),
  cleaned_row_20200301_2052 )

#' 
#' # Let's check that now there is only one row for the key (CALENDAR_DATE="2020-03-01", SELL_ID="2052")
#' 
## -------------------------------------------------------------------
kiosk_data %>%
  filter(SELL_ID == "2052" &
           CALENDAR_DATE == "2020-03-01")

#' 
#' # Let's check what's happen to the transactions for the day before and the day
#' 
#' # after the 2020-03-01 for SELL_ID="2053"
#' 
## -------------------------------------------------------------------
kiosk_data %>%
  filter(SELL_ID == "2053" &
           CALENDAR_DATE > as.Date("2020-02-26") &
           CALENDAR_DATE <= as.Date("2020-03-03"))

#' 
## -------------------------------------------------------------------
cleaned_row_20200301_2053 <- kiosk_data %>%
  filter(SELL_ID == "2053" &
           CALENDAR_DATE == "2020-03-01" &
           QUANTITY == 40 &
           AVERAGE_TEMPERATURE == 32) %>%
  distinct()

print(cleaned_row_20200301_2053)

#' 
## -------------------------------------------------------------------
kiosk_data <- rbind(
  kiosk_data %>%
    filter( !(SELL_ID == "2053" &
                CALENDAR_DATE == "2020-03-01") ),
  cleaned_row_20200301_2053 )

#' 
#' # Let's check that now there is only one row for the key (CALENDAR_DATE="2020-03-01", SELL_ID="2053")
#' 
## -------------------------------------------------------------------
kiosk_data %>%
  filter(SELL_ID == "2053" &
           CALENDAR_DATE == "2020-03-01")

#' 
#' # Now we expect to have one row for each SELL_ID in the date 2020-03-01
#' 
## -------------------------------------------------------------------
kiosk_data %>%
  filter( CALENDAR_DATE == "2020-03-01" )

#' 
#' ### We were lucky to find a data issue just checking for outliers. Removing the wrong transaction rows we have also removed all the extreme outliers showed before. So, we have just killed multiple birds with one stone!
#' 
#' # Variable transformation
#' 
#' Sometimes outliers in a variable distribution aren’t wrong measures, they may be inherent in the nature of the variable itself, especially if it’s skewed. Cutting away these values from a distribution can remove useful information for a successful predictive modeling. So, it’s preferable to transform the variable to mitigate the skewness, trying to make its distribution much similar to a normal one. Reducing non-normality often reduces non-linearity as well and even if the transformed distribution is not exactly normal, it will be usually symmetric.
#' 
#' When a distribution is right-skewed, a log transformation is often used. But what if it’s left-skewed? Fortunately there is a generic way to transform non-normal distribution: the Box-Cox transformation. It’ll be applied to PRICE and QUANTITY only, because AVERAGE_TEMPERATURE is already symmetric (a Box-Cox transformation upon it doesn’t change its distribution, since it cannot be transformed in a normal one).
#' 
## -------------------------------------------------------------------
var_distribution <- function(data, var_name){
  par(mfrow=c(2,2))
  if(length(data) >= 5000){
    sampled_data = data[sample(1:length(data), 5000, replace=FALSE)]
    normtest <- shapiro.test(sampled_data)
  } else{
    normtest <- shapiro.test(data)
  }
  
  
  p.value <- round(normtest$p.value,4)
  if (p.value < 0.05) {
    h0 <- 'rejected.'
    color <- 'red'
  } else {
    h0 <- 'accepted.'
    color <- 'blue'
  }
  hist(data, xlab = var_name, main = paste('Histogram of', var_name))
  
  d <- density(data) 
  plot(d, main = paste('Density Plot of', var_name))
  qqnorm(data, main = paste('QQ Plot of', var_name))
  qqline(data)
  boxplot(data, main = paste('Boxplot of', var_name))
  mtext(paste('Normality test of', var_name, h0, '( p-value=', p.value, ')'),
        side = 3, line = -1, outer = TRUE, col=color)
  par(mfrow=c(1,1))
}

boxcox_transf <- function(data){
  require(MASS)
  box <- boxcox( data ~ 1,                   # Transform data as a single vector
                 lambda = seq(-6,6,0.1),     # Try values from -6 to 6 by 0.1
                 plotit = FALSE )
  
  cox <- data.frame( box$x, box$y )          # Create a data frame with the results
  cox2 <- cox[with(cox, order(-cox$box.y)),] # Order the new data frame by decreasing y
  
  lambda <- cox2[1, "box.x"]                 # Extract that lambda
  
  list( "data"=(data ^ lambda - 1)/lambda,
        "lambda"=lambda )                    # Return a list containing the transformed
                                             # data and lambda value
}


var_distribution(kiosk_data[['PRICE']], 'PRICE')
t_price <- boxcox_transf(kiosk_data[['PRICE']])
var_distribution(t_price$data, 'T-PRICE')

print(t_price$lambda)

#' 
## -------------------------------------------------------------------
var_distribution(kiosk_data[['QUANTITY']], 'QUANTITY')
t_quantity <- boxcox_transf(kiosk_data[['QUANTITY']])
var_distribution(t_quantity$data, 'T-QUANTITY')

print(t_quantity$lambda)

#' 
#' The var_distribution function gives us four plots.
#' 
#' The boxcox_transf function returns a list containing the transformed data and the value of lambda calculated by the Box-Cox transformation.
#' 
#' As you can see, the transformed density plot shows a more symmetric curve and the transformed Q-Q plot is closer to the normal line. The lambda value used to transform the PRICE variable is -1.1.
#' 
#' The lambda value used to transform the QUANTITY variable is -0.1. In this case, as you can see from the T-QUANTITY boxplot, the outliers have disappeared without removing them thanks to the transformation.
#' 
#' # Variable creation
#' 
#' ## New Date/Time variables
#' 
#' It’s also useful to extract a new variable representing the counter of days since the first day we can find in the data set. In general, date/time variable are cyclical (e.g. number of month goes from 1 to 12; number of day goes from 1 to 31). A machine learning algorithm doesn’t take into account the cyclicity of variables. So, a counter of days that represents the passing of the time is a good variable to add, because it can help the algorithm to catch any sales growth since the beginning of the activity.
#' 
## -------------------------------------------------------------------
library(lubridate)

# Ensure CALENDAR_DATE is a Date object
kiosk_data$CALENDAR_DATE <- as.Date(kiosk_data$CALENDAR_DATE)

# MONTH and DAY variables will be extracted from CALENDAR_DATE
# A new variable counting the days from the beginning in the data frame will be added

min_date <- min( kiosk_data$CALENDAR_DATE )

kiosk_data <- kiosk_data %>%
        mutate( DAYS_FROM_BEGINNING = as.integer(CALENDAR_DATE - min_date) ) %>%
        mutate( MONTH = month(CALENDAR_DATE) ) %>%
        mutate( DAY = day(CALENDAR_DATE) ) %>%
        mutate( WDAY = wday(CALENDAR_DATE) )

head(kiosk_data)

#' 
#' ## HOLIDAY VS QUANTITY
#' 
## -------------------------------------------------------------------
ggplot(kiosk_data, aes(x = HOLIDAY, y = QUANTITY, fill = HOLIDAY)) +
  geom_boxplot() +
  labs(x = "HOLIDAY", y = "QUANTITY", title = "Interaction between HOLIDAY and QUANTITY") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") 

#' 
#' It seems that in business days (HOLIDAY = “No Holiday”) the quantities sold are significantly greater then during the holidays. So, a new dummy variable called IS_HOLIDAY (1 if the transactions happen in holidays; 0 otherwise) might help machine learning algorithms to perform in a better way.
#' 
## -------------------------------------------------------------------
library(dplyr)

kiosk_data <- kiosk_data %>%
        mutate( IS_HOLIDAY = as.factor(ifelse(HOLIDAY == "No Holiday", 0, 1)) )

head(kiosk_data)

#' 
#' There are also some factor columns that have to be converted to one-hot encoded variables (as you can see, we already have some of them in our data set: COFFEE, LEMONADE, …). One-hot encoded variables (or dummy variables) are numeric variables, since they represent a characteristic that exists (1) or not exists (0). In our analysis they’re often transformed in categorical variables to facilitate the graphical representations. In R one-hot encoded variables can be achieved thanks to the model.matix function.
#' 
## -------------------------------------------------------------------
kiosk_data <- cbind( kiosk_data,
       model.matrix( ~ HOLIDAY - 1, data = kiosk_data ))

# We'll keep the HOLDAY variable for a subsequent analysis.
# After that, it can be removed.
# kiosk_data$HOLIDAY <- NULL

kiosk_data <- cbind( kiosk_data,
             model.matrix( ~ SELL_ID - 1, data = kiosk_data ) )

#kiosk_data$SELL_ID <- NULL

head(kiosk_data)

#' 
#' # New Numerical variables
#' 
#' Having a variable with the number of items in a combo product could be an important feature to determine a price variation. Here it’s possible to add the variable NO_ITEMS defined as:
#' 
## -------------------------------------------------------------------
kiosk_data$NO_ITEMS <- as.integer(kiosk_data$BURGER) + as.integer(kiosk_data$COFFEE)
+ as.integer(kiosk_data$COKE) + as.integer(kiosk_data$LEMONADE)
head(kiosk_data)

#' 
#' # Further analysis after cleaning data
#' 
#' ## Holidays analysis
#' 
#' Before dropping the HOLIDAY column, as commented in the previous code, it’s interesting to check how each holiday is represented in our data set for each year. How to do that? The pirateplot comes to the rescue. Before using this plot, for a better visual representation, we’ll add a new variable (HOLIDAY_ABBR) with abbreviated labels for each holiday.
#' 
## ----fig.height=10, fig.width=15------------------------------------
library(yarrr)

kiosk_data <- kiosk_data %>%
        mutate(HOL = case_when(
                                .$HOLIDAY == "Luner New Year" ~ "LY",
                                .$HOLIDAY == "No Holiday" ~ "NH",
                                .$HOLIDAY == "Labor Day" ~ "LD",
                                .$HOLIDAY == "Dragon Boat Festivel" ~ "DB",
                                .$HOLIDAY == "Mid-Autumn Day" ~ "MAD",
                                .$HOLIDAY == "New Year" ~ "NY",
                                .$HOLIDAY == "WWII Celebration" ~ "WW2",
                                .$HOLIDAY == "National Day" ~ "ND",
                                .$HOLIDAY == "Qing Ming Festival" ~ "QM"
        ))

par(mfrow=c(1,1))
pirateplot(formula = QUANTITY ~ HOL + YEAR,
           data = kiosk_data, theme = 2)  


#' 
#' As you can see, a few holidays are missing for some year:
#' 
#' Mid-Autumn Day is missing in 2022 National Day is missing in 2022 WWII Celebration (the end of the Second World War) is missing for years 2019, 2020 and 2021.
#' 
#' It’s easy to justify the lack of the first two holidays in the year 2022. The registered transactions we have in the data set have a maximum date of September the 10th for the year 2022. So these holidays fell later than the maximum date.
#' 
## -------------------------------------------------------------------
max( kiosk_data$CALENDAR_DATE )

#' 
#' The WWII Celebration is missing in years before the 2022 Since the data set is made by transactions of a Burger Cafè in Microsoft China, it seems China decided to celebrate the end of WWII just since 2022. So, the reason of lack in years before 2022 is explained.
#' 
#' ## Impact of SELL_ID to the demand curve
#' 
#' We’d like to know if the burger sales follows a linear demand. We supposed there were another variable that broke what could be a linear behavior between QUANTITY and PRICE. Let’s go deeper in the analysis to answer this question.
#' 
## ----fig.height=8, fig.width=10-------------------------------------
kiosk_data <- kiosk_data %>%
  mutate(IS_PRICE_HIGH = as.factor(ifelse(PRICE >= 14, 1, 0)) )

pirateplot(formula = QUANTITY ~ SELL_ID + IS_PRICE_HIGH,
           data = kiosk_data,
           main = "Quantity VS SELL_ID and IS_PRICE_HIGH")

#' 
#' Looking at the plot, the presence of only the SELL_ID 1070 into the IS_PRICE_HIGH area corresponding to prices equal of greater of \$14, and the inference bands around the means that aren’t overlapping, it seems confirmed that each product identified by SELL_ID is sold at their mean quantity with 9% of confidence and that high prices identify specific SELL_ID.
#' 
## ----fig.height=8, fig.width=10-------------------------------------
pirateplot(formula = PRICE ~ SELL_ID ,
           data = kiosk_data,
           main = "Price VS SELL_ID")

#' 
#' Mean confidence bars for SELL_IDs 2052 and 2053 are overlapping, so they are hardly distinguishable by price.
#' 
## ----fig.height=8, fig.width=10-------------------------------------

library(gridExtra)

plot1 <- kiosk_data %>% 
  filter(SELL_ID == 1070) %>%
  ggplot(aes(x=PRICE, y=QUANTITY)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method="lm", color="red") +
  labs(title="BURGER: PRICE vs QUANTITY",x="Price (US$)", y="Quantity") +
  theme_minimal()

plot2 <- kiosk_data %>% 
  filter(SELL_ID == 2051) %>%
  ggplot(aes(x=PRICE, y=QUANTITY)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method="lm", color="red") +
  labs(title="BURGER+COKE: PRICE vs QUANTITY",x="Price (US$)", y="Quantity") +
  theme_minimal()

plot3 <- kiosk_data %>% 
  filter(SELL_ID == 2052) %>%
  ggplot(aes(x=PRICE, y=QUANTITY)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method="lm", color="red") +
  labs(title="BURGER+LEMONADE: PRICE vs QUANTITY",x="Price (US$)", y="Quantity") +
  theme_minimal()

plot4 <- kiosk_data %>% 
  filter(SELL_ID == 2053) %>%
  ggplot(aes(x=PRICE, y=QUANTITY)) +
  geom_point() +
  geom_smooth() +
  geom_smooth(method="lm", color="red") +
  labs(title="BURGER+COKE+COFFEE: PRICE vs QUANTITY",x="Price (US$)", y="Quantity") +
  theme_minimal()

grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

#' 
## ----fig.height=6, fig.width=10-------------------------------------
scatterplot <- function(dfrm, var1, var2, sell_id){
  correlation <- round(cor(data[[var1]], data[[var2]]),4)
  plot(data[[var1]], data[[var2]], xlab = var1, ylab=var2, main = paste(var1, 'VS', var2, 'for', sell_id, ":", 'correlation =', correlation))
  # regression line (y~x)
  abline(lm(as.formula(paste(var1,'~',var2)), data =data), col='red')  
  # lowess line (x,y) 
  lines(lowess(data[[var1]], data[[var2]]), col='blue')
}

library(dplyr)

par(mfrow=c(2,2))

data <- kiosk_data %>% filter(SELL_ID == "1070")
scatterplot(data, 'PRICE', 'QUANTITY', '1070')


data <- kiosk_data %>% filter(SELL_ID == "2051")
scatterplot(data, 'PRICE', 'QUANTITY', '2051')


data <- kiosk_data %>% filter(SELL_ID == "2052")
scatterplot(data, 'PRICE', 'QUANTITY', '2052')


data <- kiosk_data %>% filter(SELL_ID == "2053")
scatterplot(data, 'PRICE', 'QUANTITY', '2053')

par(mfrow=c(1,1))

#' 
#' # Exporting cleaned csv file
#' 
## -------------------------------------------------------------------
#csv
write_csv(kiosk_data,"Food_Kiosk.csv")

#' 
#' ## Building a linear regression model
#' 
#' ### Plotting a relationship
#' 
## -------------------------------------------------------------------

ggplot(kiosk_data, aes(x=PRICE, y=QUANTITY)) +
  geom_point() +
  geom_smooth(aes(color = "Default Smooth")) +
  geom_smooth(method="lm", aes(color = "Linear Model")) +
  labs(x="Price (US$)", y="Quantity",  color = "Method") +
  theme_minimal() +
  theme(legend.position = "bottom") 


#' 
## -------------------------------------------------------------------

# Fit a linear model (lm) 
cafe_model <- lm(QUANTITY ~ PRICE + SELL_ID +  HOLIDAY + IS_WEEKEND + 
                 + IS_OUTDOOR 
                , data = kiosk_data)
summary(cafe_model)


#' 
## -------------------------------------------------------------------
kiosk_data %>%
  add_residuals(cafe_model, "resid") %>%
  ggplot(aes(x=resid)) +
  geom_histogram(bins=50) +
  labs(title = "Fit Residuals of Quantity", x="Residuals") +
  theme_minimal()

#' 
## -------------------------------------------------------------------
# Diagnostic plots
par(mfrow=c(2,2))
plot(cafe_model)

#' 
#' ## PREDICTING OPTIMAL PRICE
#' 
## -------------------------------------------------------------------
# Create a new data frame with the same structure as 'kiosk_data' for the predictors
new_data <- data.frame(
  PRICE = c(9,15),
  SELL_ID = c("2051","2051"),
  HOLIDAY = c("No Holiday","No Holiday"),
  IS_WEEKEND = c("0","1"),
  IS_OUTDOOR = c("1","1")
)

# Use the model to predict quantity
predicted_quantity <- predict(cafe_model, newdata = new_data)

predicted_quantity
# The 'predicted_quantity' object now contains the predicted quantities based on your model


