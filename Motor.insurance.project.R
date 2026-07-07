# =====================================================
# Motor Insurance Pricing using Generalized Linear Models
# Author: Vardaan Bahl
# =====================================================

library(readr)
library(dplyr)
library(ggplot2)

freq <- read_csv("Data/freMTPL2freq.csv")

sev <- read_csv("Data/freMTPL2sev.csv")

head(freq)

head(sev)

names(freq)
str(freq)

table(freq$ClaimNb)


# =====================================================
# Exploratory Data Analysis (EDA)
# =====================================================

# Observation 1:
# Approximately 95% of policies made zero claims.

# Observation 2:
# Around 4.7% of policies made exactly one claim.

# Observation 3:
# Policies with multiple claims are very rare.

# Observation 4:
# There are a few extreme observations (up to 16 claims).


prop.table(table(freq$ClaimNb))


mean(freq$ClaimNb)

var(freq$ClaimNb)
# Observation 5:
# Mean Claim Frequency = 0.0532

# Observation 6:
# Variance = 0.0577

# Observation 7:
# Variance is only slightly greater than the mean,
# indicating that a Poisson GLM is a reasonable
# starting point for frequency modelling.

ggplot(freq, aes(x = ClaimNb)) +
  geom_bar()

# ===============================
# Key Business Insights
# ===============================

# Insight 1:
# Approximately 95% of policies did not report any claim,
# indicating that claim occurrence is a rare event.

# Insight 2:
# The claim count distribution is highly right-skewed,
# supporting the use of count-based models such as the Poisson GLM
# as an initial modelling approach.

# -----------------------------------------------------
# Note:
# The above bar chart correctly shows the distribution of
# claim counts. However, since approximately 95% of policies
# have zero claims, the first bar dominates the graph and
# makes policies with multiple claims difficult to visualize.
# Therefore, we will create additional visualizations to
# better examine the tail of the claim count distribution.
# -----------------------------------------------------


claim_prop <- as.data.frame(prop.table(table(freq$ClaimNb)))

names(claim_prop) <- c("ClaimNb", "Proportion")

claim_prop


ggplot(claim_prop, aes(x = ClaimNb, y = Proportion)) +
  geom_col()
# =====================================================
# Technical Observation
# =====================================================
# The y-axis now represents proportions instead of
# raw counts. The overall shape of the distribution
# remains unchanged.

# =====================================================
# Business Insight
# =====================================================
# Approximately 95% of policies did not report any
# claim during the observation period, indicating
# that claim occurrence is a rare event. Since the
# zero-claim policies dominate the portfolio, further
# visualizations will be created to better analyse
# the tail of the claim count distribution.



# =====================================================
# Explanatory Variable 1: Driver Age (DrivAge)
# =====================================================

# Business Hypothesis:
# Driver age is expected to influence claim frequency,
# as driving experience and risk-taking behaviour vary
# across different age groups.

# Step 1: Summary Statistics
summary(freq$DrivAge)
# =====================================================
# Technical Observations
# =====================================================
# Minimum Driver Age = 18 years
# Maximum Driver Age = 100 years
# Mean Driver Age = 45.5 years
# Median Driver Age = 44 years
# Mean and median are close, suggesting that the
# distribution is not heavily skewed.

# =====================================================
# Business Insights
# =====================================================
# The portfolio mainly consists of middle-aged drivers.
# No impossible driver ages are observed, indicating
# good data quality. Although the maximum age of
# 100 years is unusual, it is still plausible and
# should be investigated further rather than removed.


# =====================================================
# Distribution of Driver Age
# =====================================================

# Business Question:
# What is the distribution of driver ages in the portfolio?
# Are there any unusual ages or signs of data quality issues?

ggplot(freq, aes(x = DrivAge)) +
  geom_histogram()
# =====================================================
# Technical Observations
# =====================================================
# The distribution of driver age is moderately
# right-skewed with a longer tail towards older ages.
# Most drivers are between approximately 30 and 60 years.
# Very few drivers are younger than 20 or older than 80.

# =====================================================
# Business Insights
# =====================================================
# The portfolio is primarily composed of middle-aged
# drivers. The small number of very elderly drivers
# appears plausible and does not immediately suggest
# data quality issues.


# =====================================================
# Creating Driver Age Groups
# =====================================================
# Objective:
# Create driver age groups to facilitate business
# interpretation and exploratory analysis.
freq$AgeGroup <- cut(
  freq$DrivAge,
  breaks = c(18, 25, 35, 45, 55, 65, 100),
  labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "66+"),
  include.lowest = TRUE
)
table(freq$AgeGroup)
# Business Insight:
# Most policyholders fall between 26 and 55 years of age,
# indicating that the portfolio is primarily composed of
# middle-aged drivers.

age_claim_freq <- freq %>%
  group_by(AgeGroup) %>%
  summarise(
    Average_Claim_Frequency = mean(ClaimNb),
    Number_of_Policies = n()
  )
age_claim_freq
# =====================================================
# Business Insight
# =====================================================
# Drivers aged 18–25 have the highest average claim
# frequency, supporting the hypothesis that younger
# and less experienced drivers are more likely to
# make claims. Claim frequency decreases for middle-
# aged drivers but rises again for older drivers,
# suggesting a non-linear relationship between
# driver age and claim frequency.

# =====================================================
# Relationship between Driver Age and Claim Frequency
# =====================================================

ggplot(age_claim_freq,
       aes(x = AgeGroup,
           y = Average_Claim_Frequency)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Claim Frequency by Driver Age Group",
    x = "Driver Age Group",
    y = "Average Claim Frequency"
  ) +
  theme_minimal()
# =====================================================
# Business Insights
# =====================================================
# Driver Age appears to be an important predictor of
# claim frequency. Young drivers (18-25) exhibit the
# highest average claim frequency, while drivers aged
# 26-35 have the lowest. Claim frequency generally
# increases with age after 35, although the relationship
# is not perfectly linear.

# =====================================================
# Explanatory Variable 2: Vehicle Age (VehAge)
# =====================================================

# Business Hypothesis:
# Older vehicles may exhibit higher claim frequency due
# to wear and tear, mechanical deterioration and older
# safety technology.

summary(freq$VehAge)
# =====================================================
# Business Insight
# =====================================================
# The average insured vehicle is approximately 7 years
# old. Vehicle ages range from 0 to 100 years, with
# a few extremely old vehicles that should be examined
# further during exploratory analysis.



# =====================================================
# Distribution of Vehicle Age
# =====================================================

# Business Questions:
# What is the distribution of vehicle ages?
# Are there any unusually old vehicles?
# Is the distribution skewed?

ggplot(freq, aes(x = VehAge)) +
  geom_histogram(binwidth = 1) +
  labs(
    title = "Distribution of Vehicle Age",
    x = "Vehicle Age (Years)",
    y = "Number of Policies"
  ) +
  theme_minimal()
# =====================================================
# Technical Observations
# =====================================================
# Vehicle Age is highly right-skewed. Most insured
# vehicles are relatively new, with the majority being
# less than 15 years old. Only a very small number of
# vehicles are extremely old.

# =====================================================
# Business Insights
# =====================================================
# The portfolio is dominated by newer vehicles.
# Very old vehicles exist but represent only a tiny
# proportion of the portfolio and should be investigated
# further if necessary.


# =====================================================
# Data Preparation: Vehicle Age Groups
# =====================================================

# Objective:
# Create vehicle age groups based on the distribution
# of Vehicle Age observed in the histogram.

freq$VehAgeGroup <- cut(
  freq$VehAge,
  breaks = c(0, 1, 2, 3, 4, 5, 7, 10, 15, 100),
  labels = c("0-1", "1-2", "2-3", "3-4", "4-5",
             "5-7", "7-10", "10-15", "15+"),
  include.lowest = TRUE
) 
table(freq$VehAgeGroup)

# =====================================================
# Business Insight
# =====================================================
# Vehicle Age groups were created using narrower
# intervals for newer vehicles and wider intervals
# for older vehicles due to the highly right-skewed
# distribution. Each group contains a sufficiently
# large number of policies for meaningful analysis.

veh_claim_freq <- freq %>%
  group_by(VehAgeGroup) %>%
  summarise(
    Average_Claim_Frequency = mean(ClaimNb),
    Number_of_Policies = n()
  )

veh_claim_freq
# =====================================================
# Business Insight
# =====================================================
# Initial Hypothesis:
# We expected older vehicles to exhibit higher average
# claim frequency than newer vehicles due to wear and
# tear, mechanical deterioration and older safety
# technology.

# Observation:
# Contrary to our initial hypothesis, the newest
# vehicles (0-1 years) exhibit the highest average
# claim frequency, while vehicles older than 15 years
# exhibit the lowest average claim frequency.

# Conclusion:
# Vehicle Age alone does not fully explain claim
# frequency. The observed relationship suggests that
# other explanatory variables may also influence claim
# frequency, highlighting the need for a multivariable
# GLM to understand the combined effects of different
# risk factors.

# =====================================================
# Relationship between Vehicle Age and Claim Frequency
# =====================================================

ggplot(veh_claim_freq,
       aes(x = VehAgeGroup,
           y = Average_Claim_Frequency)) +
  geom_col(fill = "steelblue", width = 0.7) +
  labs(
    title = "Average Claim Frequency by Vehicle Age Group",
    x = "Vehicle Age Group",
    y = "Average Claim Frequency"
  ) +
  theme_minimal()

# =====================================================
# Graph Interpretation
# =====================================================
# The relationship between Vehicle Age and Average Claim
# Frequency is non-linear. Contrary to our initial
# hypothesis, the newest vehicles (0-1 years) have the
# highest average claim frequency, while vehicles older
# than 15 years have the lowest. This suggests that
# Vehicle Age alone does not explain claim frequency and
# that other explanatory variables should also be
# considered in the GLM.



# =====================================================
# Explanatory Variable 3: BonusMalus
# =====================================================

# Definition:
# BonusMalus is an insurance risk score that reflects a
# driver's previous claims history. Drivers who have
# remained claim-free receive a bonus (lower BonusMalus
# value), while drivers who have made claims receive a
# malus (higher BonusMalus value). Insurance companies
# use this score to adjust future premiums based on the
# policyholder's past claim experience.

# Business Hypothesis:
# We expect drivers with higher BonusMalus values to
# exhibit higher average claim frequency because their
# previous claims history suggests a higher underlying
# insurance risk.
#A careful driver might have BonusMalus = 50.
#An average driver might have BonusMalus = 100.
#A risky driver might have BonusMalus = 150 or 200.
#So higher numbers indicate higher risk, but they are not negative.

summary(freq$BonusMalus)
# =====================================================
# Business Insight
# =====================================================
# BonusMalus ranges from 50 to 230, where lower values
# represent safer drivers and higher values represent
# drivers with a poorer claims history. More than half
# of the policyholders have a BonusMalus score of 50,
# indicating that the portfolio is dominated by
# claim-free or low-risk drivers. The distribution is
# expected to be positively skewed due to a relatively
# small number of high-risk policyholders.


# =====================================================
# Distribution of BonusMalus
# =====================================================

# Business Questions:
# What is the distribution of BonusMalus?
# Is the portfolio dominated by low-risk drivers?
# Are there relatively few high-risk drivers?

ggplot(freq, aes(x = BonusMalus)) +
  geom_histogram(binwidth = 5) +
  labs(
    title = "Distribution of BonusMalus",
    x = "BonusMalus",
    y = "Number of Policies"
  ) +
  theme_minimal()
# =====================================================
# Graph Interpretation
# =====================================================
# The histogram confirms that the BonusMalus
# distribution is highly right-skewed. A large
# proportion of policyholders have the minimum
# BonusMalus score of 50, while relatively few
# policyholders have high BonusMalus values. This
# indicates that the portfolio is dominated by
# low-risk drivers, with only a small proportion
# of high-risk policyholders.


bonus_claim_freq <- freq %>%
  group_by(BonusMalus) %>%
  summarise(
    Average_Claim_Frequency = mean(ClaimNb),
    Number_of_Policies = n()
  )

bonus_claim_freq

# =====================================================
# Business Insight
# =====================================================
# The average claim frequency does not increase
# monotonically with BonusMalus. Individual BonusMalus
# values show noticeable fluctuations, partly because
# some BonusMalus levels contain relatively few
# policyholders. Therefore, the overall trend should
# be examined graphically rather than relying solely
# on the summary table.


# =====================================================
# Data Preparation: BonusMalus Groups
# =====================================================

# Objective:
# Create BonusMalus groups for exploratory data analysis.
# The variable will still be treated as continuous when
# fitting the GLM.

freq$BonusMalusGroup <- cut(
  freq$BonusMalus,
  breaks = c(50, 60, 70, 80, 90, 100, 230),
  labels = c("50-59", "60-69", "70-79",
             "80-89", "90-99", "100+"),
  include.lowest = TRUE
)
table(freq$BonusMalusGroup)

bonus_claim_freq_group <- freq %>%
  group_by(BonusMalusGroup) %>%
  summarise(
    Average_Claim_Frequency = mean(ClaimNb),
    Number_of_Policies = n()
  )

bonus_claim_freq_group

ggplot(bonus_claim_freq_group,
       aes(x = BonusMalusGroup,
           y = Average_Claim_Frequency)) +
  geom_col(fill = "steelblue", width = 0.7) +
  labs(
    title = "Average Claim Frequency by BonusMalus Group",
    x = "BonusMalus Group",
    y = "Average Claim Frequency"
  ) +
  theme_minimal()

# =====================================================
# Graph Interpretation
# =====================================================

# Initial Hypothesis:
# Drivers with higher BonusMalus scores were expected
# to exhibit higher average claim frequency because
# BonusMalus reflects previous claims history.

# Observation:
# The graph largely supports the initial hypothesis.
# Average claim frequency generally increases as
# BonusMalus increases, although small fluctuations
# are observed across some intermediate groups.

# Conclusion:
# Drivers with BonusMalus scores of 100 or more have
# substantially higher average claim frequency than
# drivers with lower BonusMalus scores. This suggests
# that BonusMalus is a strong predictor of claim
# frequency and is likely to be an important variable
# in the Poisson GLM.


# =====================================================
# Explanatory Variable 4: Vehicle Power
# =====================================================

# Definition:
# VehPower represents the engine power category of the
# insured vehicle. Vehicles with greater engine power
# generally have higher performance and are capable of
# travelling at higher speeds.

# Business Hypothesis:
# We expect vehicles with higher engine power to exhibit
# higher average claim frequency because they may be
# driven at higher speeds and may be involved in more
# severe or frequent accidents.

summary(freq$VehPower)
# =====================================================
# Business Insight
# =====================================================
# Vehicle Power ranges from 4 to 15, indicating a
# relatively small number of power categories. The
# median power category is 6, and the mean (6.455) is
# close to the median, suggesting that the distribution
# is not expected to be highly skewed. Since there are
# only 12 possible power levels, each level can be
# analysed individually without creating broader groups
# for exploratory data analysis.

table(freq$VehPower)
# =====================================================
# Business Insight
# =====================================================
# The portfolio is dominated by vehicles with moderate
# engine power (categories 6 and 7). The number of
# policies decreases substantially for higher Vehicle
# Power categories, indicating that high-powered
# vehicles form only a small proportion of the insured
# portfolio. However, each power category still contains
# enough observations to be analysed individually.

vehpower_claim_freq <- freq %>%
  group_by(VehPower) %>%
  summarise(
    Average_Claim_Frequency = mean(ClaimNb),
    Number_of_Policies = n()
  )

vehpower_claim_freq

# =====================================================
# Business Insight
# =====================================================

# Initial Hypothesis:
# Vehicles with higher engine power were expected to
# exhibit higher average claim frequency because they
# are capable of travelling at higher speeds and may
# therefore be associated with greater accident risk.

# Observation:
# The summary table does not show a clear increasing
# relationship between Vehicle Power and average claim
# frequency. Instead, the claim frequency fluctuates
# across different Vehicle Power categories.

# Conclusion:
# Vehicle Power alone does not appear to have a strong
# univariate relationship with claim frequency.
# However, it may still be an important predictor when
# considered alongside other explanatory variables in
# the multivariable Poisson GLM.

ggplot(vehpower_claim_freq,
       aes(x = factor(VehPower),
           y = Average_Claim_Frequency)) +
  geom_col(fill = "steelblue", width = 0.7) +
  labs(
    title = "Average Claim Frequency by Vehicle Power",
    x = "Vehicle Power",
    y = "Average Claim Frequency"
  ) +
  theme_minimal()
# =====================================================
# Graph Interpretation
# =====================================================

# Initial Hypothesis:
# We expected average claim frequency to increase with
# Vehicle Power because more powerful vehicles may be
# associated with higher speeds and greater accident
# risk.

# Observation:
# The graph does not show a clear increasing trend.
# Instead, average claim frequency fluctuates across
# different Vehicle Power categories without a
# consistent pattern.

# Conclusion:
# Vehicle Power alone does not appear to strongly
# explain claim frequency. However, it may still
# provide useful predictive information when combined
# with other explanatory variables in the Poisson GLM.



# =====================================================
# Explanatory Variable 5: Vehicle Fuel Type (VehGas)
# =====================================================

# Definition:
# VehGas represents the type of fuel used by the insured
# vehicle. The dataset contains two fuel types:
# Diesel and Regular (Petrol).

# Business Hypothesis:
# We expect claim frequency to differ between Diesel and
# Regular vehicles because the fuel type may reflect
# differences in vehicle usage, annual mileage and
# driving patterns. However, we do not make a strong
# prediction regarding which fuel type will have the
# higher claim frequency before analysing the data.

summary(freq$VehGas)
table(freq$VehGas)
# =====================================================
# Business Insight
# =====================================================
# The portfolio is almost evenly divided between
# Diesel and Regular vehicles, with Regular vehicles
# representing a slightly larger proportion of the
# insured policies. Since both fuel types contain a
# large number of observations, comparisons between
# them are expected to be statistically reliable.

vehgas_claim_freq <- freq %>%
  group_by(VehGas) %>%
  summarise(
    Average_Claim_Frequency = mean(ClaimNb),
    Number_of_Policies = n()
  )

vehgas_claim_freq

# =====================================================
# Business Insight
# =====================================================

# Initial Hypothesis:
# We did not make a strong prediction regarding the
# relationship between fuel type and claim frequency,
# as fuel type alone does not directly determine
# accident risk.

# Observation:
# Regular fuel vehicles exhibit a slightly higher
# average claim frequency than Diesel vehicles.
# However, the difference between the two fuel types
# is relatively small.

# Conclusion:
# VehGas appears to have only a modest univariate
# relationship with claim frequency. Any observed
# difference may also reflect the influence of other
# explanatory variables, which will be accounted for
# in the multivariable Poisson GLM.

ggplot(vehgas_claim_freq,
       aes(x = VehGas,
           y = Average_Claim_Frequency)) +
  geom_col(fill = "steelblue", width = 0.7) +
  labs(
    title = "Average Claim Frequency by Vehicle Fuel Type",
    x = "Fuel Type",
    y = "Average Claim Frequency"
  ) +
  theme_minimal()
# =====================================================
# Graph Interpretation
# =====================================================

# Initial Hypothesis:
# We did not make a strong prediction regarding which
# fuel type would exhibit a higher claim frequency
# because fuel type alone does not directly determine
# accident risk.

# Observation:
# Regular fuel vehicles exhibit a slightly higher
# average claim frequency than Diesel vehicles.
# However, the difference between the two fuel types
# is relatively small.

# Conclusion:
# Vehicle Fuel Type (VehGas) appears to have only a
# modest univariate relationship with claim frequency.
# Although the observed difference is small, VehGas
# may still contribute useful predictive information
# when analysed together with other explanatory
# variables in the multivariable Poisson GLM.


# =====================================================
# Explanatory Variable 6: Vehicle Brand (VehBrand)
# =====================================================

# Definition:
# VehBrand represents the manufacturer or brand of the
# insured vehicle. Different vehicle brands may vary in
# terms of performance, safety features, repair costs
# and the characteristics of the drivers who own them.

# Business Hypothesis:
# We expect claim frequency to differ across vehicle
# brands because different brands are associated with
# different vehicle types, driving behaviour and
# customer profiles. However, we do not make a strong
# prediction regarding which specific brand will have
# the highest or lowest claim frequency before
# analysing the data.

summary(freq$VehBrand)

table(freq$VehBrand)
# =====================================================
# Business Insight
# =====================================================
# The portfolio consists of 11 vehicle brands.
# Brands B12, B1 and B2 account for the majority of
# insured vehicles, while the remaining brands have
# comparatively fewer policies. Since each brand still
# contains a sufficient number of observations, all
# brands can be analysed individually without creating
# broader groups.



vehbrand_claim_freq <- freq %>%
  group_by(VehBrand) %>%
  summarise(
    Average_Claim_Frequency = mean(ClaimNb),
    Number_of_Policies = n()
  )

vehbrand_claim_freq
# =====================================================
# Business Insight
# =====================================================

# Initial Hypothesis:
# We expected claim frequency to differ across vehicle
# brands because different brands may be associated
# with different vehicle characteristics and customer
# profiles. However, no prediction was made regarding
# which specific brand would exhibit the highest or
# lowest claim frequency.

# Observation:
# Most vehicle brands exhibit very similar average
# claim frequencies. Only small differences are
# observed across brands, with Brand B5 showing the
# highest average claim frequency and Brand B14 the
# lowest.

# Conclusion:
# Vehicle Brand appears to have only a modest
# univariate relationship with claim frequency.
# Nevertheless, it may still improve the predictive
# performance of the multivariable Poisson GLM when
# analysed together with other explanatory variables.


ggplot(vehbrand_claim_freq,
       aes(x = VehBrand,
           y = Average_Claim_Frequency)) +
  geom_col(fill = "steelblue", width = 0.7) +
  labs(
    title = "Average Claim Frequency by Vehicle Brand",
    x = "Vehicle Brand",
    y = "Average Claim Frequency"
  ) +
  theme_minimal()
# =====================================================
# Graph Interpretation
# =====================================================

# Initial Hypothesis:
# We expected claim frequency to differ across vehicle
# brands because different brands may represent
# different vehicle characteristics and customer
# profiles. However, no prediction was made regarding
# which specific brand would have the highest or
# lowest claim frequency.

# Observation:
# The graph shows only small differences in average
# claim frequency across most vehicle brands. Brand
# B5 exhibits the highest average claim frequency,
# while Brand B14 has the lowest. Most of the
# remaining brands have very similar claim
# frequencies.

# Conclusion:
# Vehicle Brand does not appear to have a strong
# univariate relationship with claim frequency.
# However, it may still contribute useful predictive
# information when analysed together with other
# explanatory variables in the multivariable
# Poisson GLM.


# =====================================================
# Explanatory Variable 7: Area
# =====================================================

# Definition:
# Area represents the geographical area in which the
# insured vehicle is located. Different areas may vary
# in terms of traffic density, road infrastructure,
# driving conditions and accident risk.

# Business Hypothesis:
# We expect claim frequency to differ across areas
# because geographical location influences driving
# conditions, traffic congestion and exposure to road
# accidents. However, we do not make a strong
# prediction regarding which specific area will have
# the highest or lowest claim frequency before
# analysing the data.

summary(freq$Area)

table(freq$Area)
# =====================================================
# Business Insight
# =====================================================
# The portfolio is distributed across six geographical
# areas. Area C contains the largest number of insured
# policies, while Area F has the smallest. Despite the
# variation in portfolio size, each area contains a
# sufficiently large number of observations to be
# analysed individually without combining categories.

area_claim_freq <- freq %>%
  group_by(Area) %>%
  summarise(
    Average_Claim_Frequency = mean(ClaimNb),
    Number_of_Policies = n()
  )

area_claim_freq
# =====================================================
# Business Insight
# =====================================================

# Initial Hypothesis:
# We expected claim frequency to differ across
# geographical areas because location influences
# traffic conditions, road infrastructure and driving
# environment. However, no prediction was made
# regarding which specific area would exhibit the
# highest claim frequency.

# Observation:
# Average claim frequency generally increases from
# Area A to Area F. Area F exhibits the highest claim
# frequency, while Area A has the lowest.

# Conclusion:
# Area appears to have a noticeable univariate
# relationship with claim frequency. This suggests
# that geographical location is an important
# explanatory variable and is likely to contribute
# useful predictive information in the multivariable
# Poisson GLM.

ggplot(area_claim_freq,
       aes(x = Area,
           y = Average_Claim_Frequency)) +
  geom_col(fill = "steelblue", width = 0.7) +
  labs(
    title = "Average Claim Frequency by Area",
    x = "Area",
    y = "Average Claim Frequency"
  ) +
  theme_minimal()

# =====================================================
# Graph Interpretation
# =====================================================

# Initial Hypothesis:
# We expected claim frequency to differ across
# geographical areas because location influences
# traffic conditions, road infrastructure and driving
# environment. However, no prediction was made
# regarding which specific area would exhibit the
# highest claim frequency.

# Observation:
# The graph shows a clear increasing trend in average
# claim frequency from Area A to Area F. Area F
# exhibits the highest average claim frequency, while
# Area A exhibits the lowest.

# Conclusion:
# Area appears to have a relatively strong univariate
# relationship with claim frequency. This suggests
# that geographical area is an important explanatory
# variable and is likely to improve the predictive
# performance of the multivariable Poisson GLM.




# =====================================================
# Explanatory Variable 8: Region
# =====================================================

# Definition:
# Region represents the geographical region in which
# the insured vehicle is located. Different regions may
# differ in terms of climate, traffic conditions, road
# infrastructure, population characteristics and
# driving behaviour.

# Business Hypothesis:
# We expect claim frequency to vary across regions
# because different geographical regions may have
# different driving environments and accident risks.
# However, we do not make a strong prediction regarding
# which specific region will have the highest or lowest
# claim frequency before analysing the data.

summary(freq$Region)

table(freq$Region)
# =====================================================
# Business Insight
# =====================================================

# The portfolio is distributed across 22 geographical
# regions. The number of insured policies varies
# considerably between regions, with Region R24
# containing the largest number of policies, while
# Region R43 contains the fewest. Despite this
# variation, every region contains a sufficient number
# of observations to be analysed individually without
# combining categories.



region_claim_freq <- freq %>%
  group_by(Region) %>%
  summarise(
    Average_Claim_Frequency = mean(ClaimNb),
    Number_of_Policies = n()
  )

region_claim_freq
print(region_claim_freq, n = 22) #to see all 22 regions 



ggplot(region_claim_freq,
       aes(x = Region,
           y = Average_Claim_Frequency)) +
  geom_col(fill = "steelblue", width = 0.7) +
  labs(
    title = "Average Claim Frequency by Region",
    x = "Region",
    y = "Average Claim Frequency"
  ) +
  theme_minimal()
# =====================================================
# Business Insight
# =====================================================

# Initial Hypothesis:
# We expected claim frequency to differ across
# geographical regions because regions may vary in
# terms of weather conditions, road infrastructure,
# traffic patterns and driving behaviour. However, no
# prediction was made regarding which specific region
# would exhibit the highest or lowest claim frequency.

# Observation:
# The table and graph show noticeable variation in
# average claim frequency across the 22 regions.
# Region R53 exhibits the highest average claim
# frequency, while Region R23 has the lowest.
# Unlike the Area variable, no clear increasing or
# decreasing trend is observed because Region is a
# nominal categorical variable.

# Conclusion:
# Region appears to have a moderate univariate
# relationship with claim frequency. Although the
# differences are not systematic, geographical region
# captures important location-specific characteristics
# and is therefore expected to contribute useful
# predictive information in the multivariable Poisson
# GLM.


# =====================================================
# Explanatory Variable 9: Population Density (Density)
# =====================================================

# Definition:
# Density represents the population density of the
# area in which the insured vehicle is located. Higher
# values indicate more densely populated areas, while
# lower values correspond to less populated regions.

# Business Hypothesis:
# We expect claim frequency to increase with population
# density because densely populated areas generally
# experience heavier traffic, greater vehicle
# interaction and a higher probability of road
# accidents.

summary(freq$Density)
# =====================================================
# Business Insight
# =====================================================

# Population density ranges from 1 to 27,000,
# indicating substantial variation across locations.
# The mean (1,792) is considerably higher than the
# median (393), suggesting that the distribution is
# highly right-skewed. This indicates that while most
# policies are located in low- to moderately-populated
# areas, a relatively small number of policies are
# associated with very densely populated locations.
#
# Since Density is a continuous variable with a wide
# range of values, we will first visualise its
# distribution before creating appropriate density
# groups for further analysis.

ggplot(freq, aes(x = Density)) +
  geom_histogram(binwidth = 250) +
  labs(
    title = "Distribution of Population Density",
    x = "Population Density",
    y = "Number of Policies"
  ) +
  theme_minimal()

# =====================================================
# Graph Interpretation
# =====================================================

# Initial Hypothesis:
# We expected population density to be positively
# associated with claim frequency because densely
# populated areas generally experience heavier traffic
# and greater exposure to road accidents.

# Observation:
# The histogram confirms that Population Density is
# highly right-skewed. Most policies are concentrated
# in areas with relatively low population density,
# while only a small proportion of policies are
# located in extremely densely populated areas.
# These high-density observations create a long right
# tail in the distribution.

# Conclusion:
# Since Density exhibits a highly skewed distribution
# with a wide range of values, analysing every unique
# value individually would not be informative.
# Therefore, we will create meaningful density groups
# before investigating its relationship with claim
# frequency.


quantile(freq$Density,
         probs = c(0, 0.25, 0.50, 0.75, 1))

freq$DensityGroup <- cut(
  freq$Density,
  breaks = c(1, 92, 393, 1658, 27000),
  labels = c("Low", "Medium-Low", "Medium-High", "High"),
  include.lowest = TRUE
)
table(freq$DensityGroup)

density_claim_freq <- freq %>%
  group_by(DensityGroup) %>%
  summarise(
    Average_Claim_Frequency = mean(ClaimNb),
    Number_of_Policies = n()
  )

density_claim_freq

ggplot(density_claim_freq,
       aes(x = DensityGroup,
           y = Average_Claim_Frequency)) +
  geom_col(fill = "steelblue", width = 0.7) +
  labs(
    title = "Average Claim Frequency by Population Density",
    x = "Population Density Group",
    y = "Average Claim Frequency"
  ) +
  theme_minimal()

# =====================================================
# Business Insight
# =====================================================

# Initial Hypothesis:
# We expected claim frequency to increase with
# population density because densely populated areas
# generally experience heavier traffic, greater
# vehicle interaction and higher accident exposure.

# Observation:
# The density groups contain approximately equal
# numbers of policies, providing a fair basis for
# comparison. Average claim frequency increases
# steadily from the Low density group to the High
# density group.

# Conclusion:
# The observed results support our initial hypothesis.
# Population Density exhibits a clear positive
# relationship with claim frequency and appears to be
# an important explanatory variable for the
# multivariable Poisson GLM.




# =====================================================
# Poisson Generalized Linear Model
# =====================================================

# Objective:
# Fit a multivariable Poisson Generalized Linear Model
# (GLM) to examine the relationship between the number
# of claims (ClaimNb) and the selected explanatory
# variables.


poisson_model <- glm(
  ClaimNb ~ DrivAge +
    VehAge +
    BonusMalus +
    VehPower +
    VehGas +
    VehBrand +
    Area +
    Region +
    Density,
  family = poisson(link = "log"),
  data = freq
)

summary(poisson_model)

# =====================================================
# Model Interpretation
# =====================================================

# The model estimates the effect of each explanatory
# variable on the expected claim frequency while
# simultaneously adjusting for all other variables in
# the model.

# Positive coefficients indicate an increase in the
# expected claim frequency, whereas negative
# coefficients indicate a decrease, relative to the
# reference category (for categorical variables) or
# for a one-unit increase (for continuous variables).

# Variables with small p-values provide strong
# statistical evidence of an association with claim
# frequency after controlling for the remaining
# explanatory variables.

# The AIC and residual deviance provide measures of
# model fit and will be used later for model
# comparison and refinement.



# =====================================================
# Model Diagnostics: Dispersion Check
# =====================================================

# One of the key assumptions of the Poisson GLM is
# that the conditional mean and variance of the
# response variable are approximately equal.
#
# To assess this assumption, we calculate the Pearson
# dispersion statistic, which is obtained by dividing
# the Pearson Chi-square statistic by the residual
# degrees of freedom.
#
# A dispersion value close to 1 indicates that the
# Poisson assumption is appropriate. Values
# substantially greater than 1 indicate
# overdispersion, whereas values substantially less
# than 1 indicate underdispersion.

pearson_dispersion <- sum(residuals(poisson_model,
                                    type = "pearson")^2) /
  poisson_model$df.residual

pearson_dispersion

# =====================================================
# Business Insight
# =====================================================

# The Pearson dispersion statistic is approximately
# 1.086, which is very close to 1. This indicates
# that there is no evidence of substantial
# overdispersion or underdispersion in the fitted
# Poisson GLM.
#
# Therefore, the assumption that the conditional
# variance is approximately equal to the conditional
# mean appears to be reasonable, suggesting that the
# Poisson model provides an appropriate fit for the
# claim frequency data.




# =====================================================
# Model Diagnostics: Residual Analysis
# =====================================================

# Residual analysis is performed to assess whether
# the fitted Poisson GLM adequately captures the
# relationship between the explanatory variables and
# the response variable.
#
# A well-fitting model should produce residuals that
# are randomly scattered around zero without any
# systematic pattern. Any visible trend or structure
# may indicate model misspecification.

plot(poisson_model, which = 1,main="Residuals vs Fitted Plot")
#The residuals are randomly scattered around zero without any clear systematic pattern.
#Although a few observations exhibit relatively large positive residuals, 
#these appear to be isolated outliers rather than evidence of model misspecification. 
#Overall, the residual plot suggests that the fitted Poisson GLM provides 
#an adequate representation of the claim frequency data.



# =====================================================
# Model Diagnostics: Goodness of Fit
# =====================================================

# The goodness of fit of the Poisson GLM is assessed
# by comparing the fitted model with the null model
# using an Analysis of Deviance. This evaluates
# whether the explanatory variables collectively
# provide a statistically significant improvement
# in explaining the variation in claim frequency.
anova(poisson_model, test = "Chisq")


# =====================================================
# Business Insight
# =====================================================

# The Analysis of Deviance indicates that Driver Age,
# Vehicle Age, BonusMalus, Vehicle Fuel Type, Vehicle
# Brand, Area and Region each provide a statistically
# significant improvement in the model fit.

# Among all explanatory variables, BonusMalus and
# Region contribute the largest reductions in
# deviance, indicating that they are the strongest
# predictors of claim frequency.

# In contrast, Vehicle Power and Population Density
# do not significantly improve the model after
# accounting for the remaining variables. This
# suggests that their effects are largely explained
# by other variables already included in the model.


# =====================================================
# Model Refinement
# =====================================================

# Based on the coefficient table and the Analysis of
# Deviance, Vehicle Power and Population Density do
# not significantly improve the model.
#
# Therefore, a reduced Poisson GLM is fitted by
# excluding these variables. The reduced model is
# subsequently compared with the full model to assess
# whether the simpler model provides a comparable fit.

reduced_poisson_model <- glm(
  ClaimNb ~ DrivAge +
    VehAge +
    BonusMalus +
    VehGas +
    VehBrand +
    Area +
    Region,
  family = poisson(link = "log"),
  data = freq
)
summary(reduced_poisson_model)
summary(poisson_model)
AIC(poisson_model, reduced_poisson_model)
anova(reduced_poisson_model,
      poisson_model,
      test = "Chisq")

# =====================================================
# Model Comparison and Refinement
# =====================================================

# A reduced Poisson GLM was fitted by excluding
# Vehicle Power and Population Density, as both
# variables were found to be statistically
# insignificant in the full model.

# The reduced model was compared with the full model
# using both the Akaike Information Criterion (AIC)
# and a Likelihood Ratio Test.


# =====================================================
# Business Insight
# =====================================================

# The reduced model produced a slightly lower AIC,
# indicating a marginal improvement in model
# simplicity without sacrificing predictive
# performance.

# Furthermore, the Likelihood Ratio Test yielded a
# p-value of 0.2904, suggesting that adding Vehicle
# Power and Population Density does not significantly
# improve the model fit.

# Although the reduced model is statistically more
# parsimonious, the full model is retained for the
# final analysis. This allows the effects of all
# relevant policyholder, vehicle and geographical
# characteristics to be evaluated, while also
# demonstrating that Vehicle Power and Population
# Density do not contribute significantly after
# adjusting for the remaining explanatory variables.



# =====================================================
# Final Interpretation of the Poisson GLM
# =====================================================

# The Poisson GLM indicates that Driver Age, Vehicle Age,
# Bonus-Malus Score, Vehicle Fuel Type, Vehicle Brand,
# Area, and Region have a statistically significant
# effect on motor insurance claim frequency.

# Among these variables, Bonus-Malus Score is one of the
# strongest predictors, highlighting the importance of a
# driver's claim history in estimating future claims.

# Vehicle Power and Population Density were found to be
# statistically insignificant after adjusting for the
# remaining explanatory variables and therefore do not
# contribute significantly to the model.

# Overall, the fitted Poisson GLM provides a suitable
# framework for modelling claim frequency and can assist
# insurers in risk assessment, premium pricing,
# underwriting and portfolio management.





# The Poisson GLM assumes that the mean and variance of the
# response variable are equal. Although the dispersion test
# indicated no significant overdispersion, this assumption
# may not always hold for real-world insurance claim data.

# The model considers only claim frequency and does not
# account for claim severity or claim amount. Therefore,
# it cannot be used to estimate the total cost of claims.

# Interaction effects between explanatory variables were
# not included in the model. Certain combinations of
# variables may influence claim frequency differently.

# The analysis is based only on the variables available in
# the dataset. Other factors such as driving behaviour,
# annual mileage, weather conditions and road quality were
# not available for modelling.




# Future Scope

# The predictive performance of the model can be further
# improved by fitting a Negative Binomial GLM if the data
# exhibits overdispersion in future studies.

# Interaction effects between explanatory variables can be
# included to capture more complex relationships affecting
# claim frequency.

# Additional variables such as annual mileage, driving
# behaviour, weather conditions and road characteristics
# can be incorporated to improve prediction accuracy.

# Machine learning techniques such as Random Forest,
# Gradient Boosting or XGBoost can also be explored and
# compared with the Poisson GLM.

# Future studies may jointly model both claim frequency
# and claim severity to estimate the overall insurance
# risk and premium more accurately.



# Conclusion

# This project applied Exploratory Data Analysis (EDA)
# and a Poisson Generalized Linear Model (GLM) to
# investigate the factors influencing motor insurance
# claim frequency.

# The analysis identified Driver Age, Vehicle Age,
# Bonus-Malus Score, Vehicle Fuel Type, Vehicle Brand,
# Area and Region as significant predictors of claim
# frequency, while Vehicle Power and Population Density
# were not found to significantly improve the model
# after adjusting for the remaining variables.

# Model diagnostics, including the Pearson dispersion
# statistic, residual analysis and goodness-of-fit
# assessment, indicated that the fitted Poisson GLM
# provides an appropriate representation of the claim
# frequency data.

# Overall, the project demonstrates how statistical
# modelling can be used to understand claim behaviour
# and support insurance companies in risk assessment,
# underwriting and premium pricing decisions.