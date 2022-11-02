set.seed(1) 
vo2max <- rnorm(100000, 70, 5)
samp <- sample(vo2max, 20, replace = FALSE)
summary(samp)

library(tidyverse) # Needed for making the plot!
df <- data.frame(samp = samp)

##Dotplot
df %>%
  # Plots our samples on the x axis, and sets all y to 1.
  ggplot(aes(samp, y = 1)) + 
  # Adds points and "jitter" on the y axis
  geom_point(position = position_jitter(height = 0.2)) + 
  # Scales the y axis to better plot the data
  scale_y_continuous(limits = c(0,2)) +
  # Set the name of the x axis
  labs(x = "VO2max") +
  # The code below modifies the theme of the plot, removing 
  # the y axis
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank())

##Boxplot
df %>%
  # Plots our samples on the y axis, and sets all x to 1.
  ggplot(aes(x = 1, samp)) + 
  # Adds the boxplot
  geom_boxplot(width = 0.5) +
  # Scales the x axis to better plot the data
  scale_x_continuous(limits = c(0,2)) +
  # Set the name of the y axis
  labs(y = "VO2max") +
  # The code below modifies the theme of the plot, removing 
  # the x axis
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.line.x = element_blank())

##Histogram
df %>%
  # Plots our samples on the y axis, and sets all x to 1.
  ggplot(aes(samp)) + 
  # Adds the histogram
  geom_histogram(binwidth = 3, color = "blue", fill = "lightblue") +
  # Set the name of the y axis
  labs(x = "VO2max", y = "Count") 


###Inference about the population
set.seed(2)
samp2 <- sample(vo2max, 20, replace = FALSE)

# Calculate the mean
m2 <- mean(samp2)

s2 <- sd(samp2)

# Print the results from calculation of SD
c(m2, s2) ## funka ikke med s, s?? byttet ut med m2

# set the seed
set.seed(123)

# create the data frame
results <- data.frame(mean = rep(NA, 10000)) 
# The rep function creates a vector full of NA 
# each NA can be replaced with a mean

# Second we build the for loop
for(i in 1:10000){
  
  results[i, 1] <- mean(sample(x = vo2max, size = 20, replace = FALSE))
  
}
ggplot(data = results, aes(mean)) +
  geom_histogram(fill = "lightblue", color = "gray40", binwidth = 1) + 
  theme_minimal()

lower <- mean(results[,1]) - 1.96 * sd(results[,1])
upper <- mean(results[,1]) + 1.96 * sd(results[,1])

mean(vo2max)
mean(results[,1])


### Estimation of the sampling distribution
## The standard error of a sample is an approximation 
##of the standard deviation of the sampling distribution

# Copy and paste the code if you want the results

# set the seed
set.seed(123)

# create the data frame
results <- data.frame(mean = rep(NA, 1000), 
                      sd = rep(NA, 1000)) 
# The rep function creates a vector full of NA 
# each NA can be replaced with a mean

# Second we build the for loop
for(i in 1:10000){
  
  samp <- sample(x = vo2max, size = 20, replace = FALSE)
  
  results[i, 1] <- mean(samp)
  results[i, 2] <- sd(samp)
}


results %>%
  
  # Calculate the standard error of each sample
  mutate(se = sd/sqrt(20)) %>%
###Make a graph containing estimates and empirical values
ggplot(aes(mean)) + geom_histogram(binwidth = 0.5) + 
  
  # Add a line representing the standard deviation of the distribution of means
  geom_segment(aes(y = 350, 
                   yend = 350, 
                   x = mean(mean), 
                   xend = mean(mean) + sd(mean)), 
               lty = 1, color = "green", size = 2) +
  # Add text to discribe the line
  geom_text(aes(x = mean(mean),
                y = 300, 
                label = paste("Empirical: Mean ", 
                              round(mean(mean), 0), 
                              " + SD: ", 
                              round(sd(mean), 1) )), 
            color = "green") +
  geom_segment(aes(y = 160, 
                   yend = 160, 
                   x = mean(mean), 
                   xend = mean(mean) + mean(se)), 
               lty = 1, color = "blue", size = 2) +
  # Add text to describe the above
  geom_text(aes(x = mean(mean),
                y = 220, 
                label = paste("Estimate: Mean ", 
                              round(mean(mean), 0), 
                              " + average SE: ", 
                              round(mean(se), 1) )), 
            color = "blue")  
 
###A confidence interval for the mean
# Creat new variables with upper and lower limits of the confidence interval  
cis <- results %>%
  # Using the normal distribution
  mutate(lower.limit = mean - 1.96 * sd/sqrt(20), 
         upper.limit = mean + 1.96 * sd/sqrt(20)) %>%
  # Test if the true mean is within the limits
  # If the true mean is above the lower limit and below the upper limit then TRUE
  # otherwise FALSE
  mutate(true.mean = if_else(mean(vo2max) > lower.limit & mean(vo2max) < upper.limit, 
                             TRUE, FALSE)) 

# Plot the data, only plotting 200 data points to make it suitable for every computer

cis[1:200, ] %>%
  ggplot(aes(seq(1:nrow(.)), 
             y = mean, 
             color = true.mean, # set a specific color 
             alpha = true.mean)) + # and transparency to 
  # intervals containing and not containing the true mean
  
  # add a line showing the true mean
  geom_hline(yintercept = mean(vo2max)) +
  # add errorbars showing each interval
  geom_errorbar(aes(ymin = lower.limit, ymax = upper.limit)) + 
  # scale the colors and transparency. Intervals not containing 
  # the true mean are red.
  scale_color_manual(values = c("red", "black")) + 
  scale_alpha_manual(values = c(1, 0.2)) + 
  # Set label texts
  labs(x = "Sample", 
       y = "Mean", 
       alpha = "Contains\nthe true mean", 
       color = "Contains\nthe true mean") + 
  # Change the theme
  theme_minimal()


# Calculate the proportions of intervals not containing the true mean
sum(cis$true.mean == FALSE) / sum(cis$true.mean == TRUE)

# Almost 5%!   

###Why isn't it 5%
# Creat new variables with upper and lower limits of the confidence interval  
cis <- results %>%
  # Using the t-distribution
  mutate(lower.limit = mean - qt(0.975, 20-1) * sd/sqrt(20), 
         upper.limit = mean + qt(0.975, 20-1) * sd/sqrt(20)) %>%
  # Test if the true mean is within the limits
  # If the true mean is above the lower limit and below the upper limit then TRUE
  # otherwise FALSE
  mutate(true.mean = if_else(mean(vo2max) > lower.limit & mean(vo2max) < upper.limit, 
                             TRUE, FALSE)) 

# Plot the data, only plotting 100 data points to make it suitable for every computer

cis[1:200, ] %>%
  ggplot(aes(seq(1:nrow(.)), 
             y = mean, 
             color = true.mean, # set a specific color 
             alpha = true.mean)) + # and transparency to 
  # intervals containing and not containing the true mean
  
  # add a line showing the true mean
  geom_hline(yintercept = mean(vo2max)) +
  # add errorbars showing each interval
  geom_errorbar(aes(ymin = lower.limit, ymax = upper.limit)) + 
  # scale the colors and transparency. Intervals not containing 
  # the true mean are red.
  scale_color_manual(values = c("red", "black")) + 
  scale_alpha_manual(values = c(1, 0.2)) + 
  # Set label texts
  labs(x = "Sample", 
       y = "Mean", 
       alpha = "Contains\nthe true mean", 
       color = "Contains\nthe true mean") + 
  # Change the theme
  theme_minimal()


# Calculate the proportions of intervals not containing the true mean
sum(cis$true.mean == FALSE) / sum(cis$true.mean == TRUE)
# Almost 5%!

###Sampling distribution of IQ
# set the seed
set.seed(1)

# A population of numbers
pop <- rnorm(100000, mean = 100, sd = 15)

# Sampling from the distribution
n5 <- sample(pop, 5, replace = FALSE)
n25 <- sample(pop, 25, replace = FALSE)
n100 <- sample(pop, 100, replace = FALSE)


# n = 10
mean_n5 <- mean(n5)
s_n5 <- sd(n5)

error_n5 <- qnorm(0.975) * s_n5/sqrt(5)

# n = 25
mean_n25 <- mean(n25)
s_n25 <- sd(n25)

error_n25 <- qnorm(0.975) * s_n25/sqrt(25)

# n = 100
mean_n100 <- mean(n100)
s_n100 <- sd(n100)

error_n100 <- qnorm(0.975) * s_n100/sqrt(100)

df <- data.frame(sample.size = c(5, 25, 100),
                 mean = c(mean_n5, mean_n25, mean_n100), 
                 error = c(error_n5, error_n25, error_n100))

df %>%
  ggplot(aes(as.factor(sample.size), mean)) +
  geom_errorbar(aes(ymin = mean-error, ymax = mean + error), width = 0.2) +
  geom_point(size = 3) + 
  theme_minimal()

# set the seed

set.seed(123)


n5 <- sample(pop, 5, replace = FALSE)
n25 <- sample(pop, 25, replace = FALSE)
n100 <- sample(pop, 100, replace = FALSE)
# n = 5
mean_n5 <- mean(n5)
s_n5 <- sd(n5)

error_n5 <- qnorm(0.975) * s_n5/sqrt(5)
errort_n5 <- qt(0.975, df = 5 - 1) * s_n5/sqrt(5)


# n = 25
mean_n25 <- mean(n25)
s_n25 <- sd(n25)

error_n25 <- qnorm(0.975) * s_n25/sqrt(25)
errort_n25 <- qt(0.975, df = 25-1) * s_n25/sqrt(25)

# n = 100
mean_n100 <- mean(n100)
s_n100 <- sd(n100)

error_n100 <- qnorm(0.975) * s_n100/sqrt(100)
errort_n100 <- qt(0.975, df = 100-1) * s_n100/sqrt(100)

df <- data.frame(sample.size = c(5, 25, 100,  5, 25, 100),
                 mean = c(mean_n5, mean_n25, mean_n100, mean_n5, mean_n25, mean_n100), 
                 error = c(error_n5, error_n25, error_n100, errort_n5, errort_n25, errort_n100), 
                 error.type = c("Normal", "Normal", "Normal", "t", "t", "t"))

df %>%
  ggplot(aes(as.factor(sample.size), mean, color = error.type)) +
  geom_errorbar(aes(ymin = mean-error, ymax = mean + error), 
                width = 0.2, 
                position = position_dodge(width = 0.2)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) + 
  labs(x = "Sample size", 
       y = "Estimated mean (95% CI)", 
       color = "Type of distribution") +
  theme_minimal()


###A hypothesis test
chess.players <- c(129, 101,98 ,89 ,103,107,123,117,114,
                   109,110,99 ,101,102,130,121,129,115,
                   107,109,107,96 ,98,102)


chess.mean <- mean(chess.players)

chess.error <- qt(0.975, df = 24-1) * sd(chess.players)/sqrt(24)


c(chess.mean - chess.error, chess.mean + chess.error)


###Using a confidence interval when planning a study
library(tidyverse); library(exscidata)
data("cyclingstudy")

cyclingstudy %>%
  select(subject, group, timepoint, VO2.max) %>%
  filter(timepoint %in%  c("pre", "meso3")) %>%
  pivot_wider(names_from = timepoint, values_from = VO2.max) %>%
  mutate(change = 100 * (meso3-pre)/pre) %>%
  group_by() %>%
  summarise(m = mean(change, na.rm = TRUE), 
            s = sd(change, na.rm = TRUE), 
            n = sum(!is.na(change)), 
            error = qt(0.975, df = n -1) * s/sqrt(n), 
            lower = m - error, 
            upper = m + error) %>%
  print()

error <- qt(0.975, df = seq(from = 10, to = 100, by = 2) - 1) * 4.79 / sqrt(seq(from = 10,
                                                                                to = 100, 
                                                                                by = 2))

ggplot(data.frame(n = seq(from = 10, to = 100, by = 2), 
                  lower.bound = 2-error), aes(n, lower.bound)) + geom_point() +
  geom_hline(yintercept = 0)
