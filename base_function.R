
# Function should fulfill the following requirements
# {attempt to limit to approx 5 parameters}
# 1. Moments
# 2. Basic Summary Statistics [mean, median, mode, IQR]
# 3. outlier info, with ability to remove normal and extreme outliers, if desired
# 4. distribution evaluation >> kernel pdf, empirical cdf, empirical quantile function, histogram
# 5. ability to perform basic hypothesis test

x <- c(10, 15, 23, 18)

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
  
  # TEST FOR LOCATION >> T-TEST, SIGN TEST, WILCOXON SIGNED RANK TEST
  
  # Test for location where mu=0
  t.test(x, alternative = "two.sided", mu=0, conf.level=0.95)
  #binom.test(length(x[which(x  0)]) , length(x)) 
  #wilcox.test(x, alternative = "two.sided", mu = 0, conf.level = 0.95)

  
  # DESCRIPTION OF QUANTILES
  quants <- quantile(c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
  
  # DESCRIPTION OF OUTLIERS
  outlier_norm <-
    
  
  # BEGIN TO OUTPUT THINGS
  stargazer(moments, summary=FALSE, type="text")
  
    
}


