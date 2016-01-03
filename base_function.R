
# Function should fulfill the following requirements
# {attempt to limit to approx 5 parameters}
# 1. Moments
# 2. Basic Summary Statistics [mean, median, mode, IQR]
# 3. outlier info, with ability to remove normal and extreme outliers, if desired
# 4. distribution evaluation >> kernel pdf, empirical cdf, empirical quantile function, histogram
# 5. ability to perform basic hypothesis test

x<-c(
  0.6 ,0.7 ,1.1 ,1.3 ,1.8 ,2.0 ,2.3 ,2.7
  ,2.9 ,3.1 ,3.9 ,4.3 ,4.4 ,4.9 ,5.2 ,5.4
  ,6.1 ,6.8 ,7.1 ,8.0 ,9.4 ,10.3 ,12.9 ,15.9 
  ,16.0 ,22.0 ,22.2 ,22.5 ,23.0 ,23.1 ,23.9 ,26.5
  ,26.7 ,28.4 ,28.5 ,32.2 ,40.2 ,42.5 ,47.2 ,48.3
  ,55.8 ,57.0 ,57.2 ,64.9 ,67.6 ,71.3 ,79.5 ,114.5 ,128.6 ,293.5
)

require("moments") # for skewness and kurtosis
install.packages("BSDA"); require("BSDA")
install.packages("stargazer"); library("stargazer")

auto_eda <- function(x) # x should just be a vector with elements corresponding to one observed variable
{
    
  # MOMENT INFO
  
  n <- length(x)
  mu <- mean(x)
  std_dev <- sd(x)
  variance <- var(x)
  skew <- skewness(x)
  kurt <- kurtosis(x)
  
  moments <- data.frame(n, mu, std_dev, variance, skew, kurt)
  names(moments) <- c("N", "Mean", "Standard Deviation", "Variance", "Skewness", "Kurtosis")
  
  
  
  # BASIC STATISTICAL MEASURES
  loc_mean <- round(mean(x), 3)
  loc_median <- round(median(x), 3)
  ux <- unique(x)
  loc_mode <- ux[which.max(tabulate(match(z,ux)))]
  range <- max(x)-min(x)
  IQR <- quantile(x, probs=0.75) - quantile(x, probs=0.25)
  
  stat_measures <- data.frame(loc_mean, loc_median, loc_mode, std_dev, variance, range, IQR)
  names(stat_measures) <- c("Mean", "Median", "Mode", "Std Deviation", "Variance", "Range", "Interquartile Range")
  # TEST FOR LOCATION >> T-TEST, SIGN TEST, WILCOXON SIGNED RANK TEST
  
  # Test for location where mu=0
  T_TEST <- t.test(x, alternative = "two.sided", mu=0, conf.level=0.95)
  #binom.test(length(x[which(x  0)]) , length(x)) 
  #wilcox.test(x, alternative = "two.sided", mu = 0, conf.level = 0.95)

  
  # DESCRIPTION OF QUANTILES
  quants <- data.frame(rbind((quantile(x, probs = c(1.0, 0.99, 0.95, 0.90
                                ,0.75, 0.5, 0.25, 0.1, 0.5, 0.01, 0)))))
  names(quants) <- c("100% Max", "99%", "95%", "90%", "75% Q3", "50% Median"
                    , "25% Q1", "10%", "5%", "1%", "0% Min")
  
  # PRINTING OF OUTLIERS
  ordered_vec <- sort(x)
  if (length(x) > 10) {
    min_values <- ordered_vec[1:5]
    max_values <- ordered_vec[(length(ordered_vec)-4):length(ordered_vec)]
    outliers <- data.frame(min_values, max_values)  
    names(outliers) <- c("Lowest Observations", "Highest Observations")
  }   
  

  
  # BEGIN TO OUTPUT THINGS
  stargazer(moments, summary=FALSE, type="text")
  stargazer(stat_measures, summary=FALSE, type="text")
  stargazer(quants, summary=FALSE, type="text")
  stargazer(outliers, summary=FALSE, type="text")
    
}

auto_eda(x)

