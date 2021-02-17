# Packages Used
install.packages("readxl")
library("readxl")
install.packages("psych")
library("psych")
install.packages("pastecs")
library("pastecs")

# Question 1
ProjectData4_Q1 = read_excel(file.choose(), sheet = "Question1")
attach(ProjectData4_Q1)

# Question 1.a
# Descriptive Summary Using Packages
stat.desc(Age) # Calculated by package "pastecs"
describe(Age) # Calculated by package "psych"
describe(Age [Gender == "Male"])
describe(Age [Gender == "Female"])

# Descriptive Summary Using Individual Functions
sampleSize = length(Age)
sampleMean = mean(Age)
median(Age)
range(Age) # Only the min and max values are displayed.
sampleSD = sd(Age)
PopulationMode = table(Age) # Creating a table to calculate the mode
names(PopulationMode)[which(PopulationMode==max(PopulationMode))] # Results in the mode of the data set
MaleMode = table(Age [Gender == "Male"])
names(MaleMode)[which(MaleMode==max(MaleMode))]
FemaleMode = table(Age [Gender == "Female"])
names(FemaleMode)[which(FemaleMode==max(FemaleMode))]

# Question 1.b
hist(Age [Gender == "Male"],
     main = "Figure 1 - Typical Age for Male Customers",
     xlab = "Age Limit",
     ylab = "Number of Customers",
     freq = TRUE,
     col = "purple",
     nclass = 5)
hist(Age [Gender == "Female"],
     breaks = seq(min(Age [Gender == "Female"]), max(Age [Gender == "Female"]), l = 6),
     main = "Figure 2 - Typical Age for Female Customers",
     xlim = c(18, 70),
     ylim = c(0, 15),
     xlab = "Age Limit",
     ylab = "Number of Customers",
     freq = TRUE,
     col = "gold")

# Question 1.c
# The data set represents the age and gender of a sample of 60 customers (21 male, 39 female) who ordered an advertised product on TV during the last month. The descriptive summary calculated measures including mean, median, mode, range, and standard deviation. According to the sample mean, the average age of a customer ordering the product is 36.23. Furthermore, as the median age (34) of customer is lower than the mean, it can be deduced that the data set is skewed to the right. The youngest and oldest customers are 18 and 65 respectively, which indicates a range of 47 that shows a great dispersion. According to the standard deviation, on average, data variability is within 13.79 years around the mean. The data set also shows that the median and mode (34) are equal, which shows the most frequent value is centered in the midpoint of the sample population. If the data set is analyzed from the gender perspective, the descriptive statistics would be different. For instance, the average age of the male customers slightly increases to 36.33, while for female customers it slightly decreases to 36.18. The sample size for the male customers is also less than the female customers. Although the median remains the same, the mode for the male customers becomes 20 and 34, and the mode for the female customers becomes 27. The range for male and female customers is 44 and 47 respectively. Additionally, The standard deviation for male and female customers is 14.66 and 13.5 respectively. Finally, by looking at the histograms and descriptive statistics of both genders, it can be observed that their shape of distribution is also skewed to the right.

# Question 1.d
t.test(Age, conf.level = 0.90)
# Assuming a normal distribution, we can be 90% confident that the average (population mean) age of customers ordering a product advertised on television is between 33.25 and 39.20 years old.

# Question 1.d using the formula instead of the function.
confidenceInterval = 0.9
sigLvl = 0.1
sampleSize = 60
df = sampleSize - 1
marginOfError = qt(confidenceInterval+sigLvl/2, df)*(sampleSD/sqrt(sampleSize))
lowerInterval = sampleMean - marginOfError
upperInterval = sampleMean + marginOfError

# Question 2
ProjectData4_Q2 = read_excel(file.choose(), sheet = "Question2")
attach(ProjectData4_Q2)

# Question 2.a
sampleMean = mean(Transport)
populationMean =  23.15
sampleSD = sd(Transport)
sampleSize = length(Transport)
hypothesisTest1 = t.test(Transport,
                         alternative = "two.sided",
                         mu = populationMean,
                         conf.level = 0.99) # since alpha is 0.01
hypothesisTest1

# Question 2.b
# H0: mu = 23.15, Ha: mu != 23.15
# Decision Rule: Reject H0, if p-value is less than 0.01
tObserved1 = (sampleMean - populationMean) / (sampleSD / sqrt(sampleSize))
# t-value_Observed = - 2.2153
# Decision: Since the p-value (0.02962) is greater than the level of significance (0.01), do not reject null hypothesis.
# Conclusion: We do not have enough statistical evidence at 1% level of significance to conclude that the population mean annual total transport costs per household in Sydney is different $23.15 (in $'000s).

# Question 3
ProjectData4_Q3 = read_excel(file.choose(), sheet = "Question3")
attach(ProjectData4_Q3)

# Question 3.a
Difference = Online - Blended
tObserved2 = t.test(Difference,
                    alternative = "two.sided",
                    conf.level = 0.95)

# Question 3.b
# H0: u1-u2=0, Ha: u1-u2!=0.
# two-tailed t-test
# Decision Rule: Reject H0, if p-value is less than 0.05
# t-value_Observed = 0.87431
tObserved2 = mean(Difference) / (sd(Difference)/sqrt(39))
# Decision: Since the p-value (0.3874) is greater than the level of significance (0.05), do not reject the null hypothesis.
# Conclusion: We do not have enough statistical evidence at 5% level of significance to conclude that there is a significant difference between the average final marks received by the students who studied under the fully online learning method and the students who studied under the blended learning method.

# Question 4
ProjectData4_Q4 = read_excel(file.choose(), sheet = "Question4")
attach(ProjectData4_Q4)

# Question 4.a
# Rating is the independent variable (x)
# Amount Spent is the dependent variable (y)
plot(Rating, `Amount Spent`,
     main = "Figure 3 - Scatterplot of Rating vs Amount Spent",
     xlim = c(0, 10),
     xlab = "Rating",
     ylab = "Amount Spent",
     col = "#cc0033",
     pch = 19,
     frame = FALSE)
model = data.frame(Rating, "Amount Spent")
reg.model = lm(`Amount Spent` ~ Rating, data = model)
summary(reg.model)
abline(reg.model, col = "#ff6633")

# Question 4.b
# y-intercept (b0) = 0.6943
# slope = 14.0253
# y-hat = 0.69 + 14.03x

# Question 4.c
# An increase of 1 point in Rating, would increase the Amount Spent by $14.03.

# Question 4.d
# r^2 = 0.6659 (Coefficient of Determination)
# 1-r^2 = 0.3341 (Coefficient of Non-Determination)
# r = sqrt(0.6659) = 0.8160 (Coefficient of Correlation)
# 66.59% of the variability in the amount spent is explained by the variability in rating, and the remaining 33.41% cannot be explained by the regression model.