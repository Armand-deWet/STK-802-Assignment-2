#install.packages("flexmix")
#install.packages("psych")
library(flexmix)
library(ggplot2)

claims_data <- read.csv("tdata.csv")
head(claims_data)

fitted_model_3c <- flexmix(Claims ~ Beneficiaries + Female_Ratio + Age + 
                             Pens_ratio + Depen_ratio, k = 3, data = claims_data)

fitted_model_3c
summary(fitted_model_3c)
summary(refit(fitted_model_3c))
parameters(fitted_model_3c)

plot(fitted_model_3c)

# add clusters to claims data
data_with_clusters <- claims_data
data_with_clusters$cluster <- factor(clusters(fitted_model_3c))

# looking at the clusters
library(psych)
describeBy(data_with_clusters, data_with_clusters$cluster)

# regression plot for each predictor
# Beneficiaries
plot_beneficiaries <- qplot(x = Beneficiaries, y = Claims, data = data_with_clusters, 
      col = data_with_clusters$cluster)
plot_beneficiaries + geom_abline(intercept = 0.37693984, slope = 0.04314243, col = 'red') + geom_abline(intercept = -0.51815256, slope = 0.03442894, col = 'green') + geom_abline(intercept = 0.04472873, slope = 0.01070234, col = 'blue')
ggsave('beneficiaries.png', plot = last_plot())

# Female_Ratio
plot_Female_Ratio <- qplot(x = Female_Ratio, y = Claims, data = data_with_clusters, 
                            col = cluster)
plot_Female_Ratio + geom_abline(intercept = 0.37693984, slope = 0.16527869, col = 'red') + geom_abline(intercept = -0.51815256, slope = 0.23914002, col = 'green') + geom_abline(intercept = 0.04472873, slope = 0.13606925, col = 'blue')
ggsave('female_ratio.png', plot = last_plot())

# Age
plot_Age <- qplot(x = Age, y = Claims, data = data_with_clusters, 
                           col = cluster)
plot_Age + geom_abline(intercept = 0.37693984, slope = 1.29683180, col = 'red') + geom_abline(intercept = -0.51815256, slope = 0.36963841, col = 'green') + geom_abline(intercept = 0.04472873, slope = 0.95054676, col = 'blue')
ggsave('age.png', plot = last_plot())

# Pens_ratio
plot_Pens_ratio <- qplot(x = Pens_ratio, y = Claims, data = data_with_clusters, 
                  col = cluster)
plot_Pens_ratio + geom_abline(intercept = 0.37693984, slope = -0.04810246, col = 'red') + geom_abline(intercept = -0.51815256, slope = -0.19718065, col = 'green') + geom_abline(intercept = 0.04472873, slope = -0.15067925, col = 'blue')
ggsave('pens_ratio.png', plot = last_plot()) 

# Pens_ratio
plot_Depen_ratio <- qplot(x = Depen_ratio, y = Claims, data = data_with_clusters, 
                         col = cluster)
plot_Depen_ratio + geom_abline(intercept = 0.37693984, slope = 0.04432812, col = 'red') + geom_abline(intercept = -0.51815256, slope = 0.04689732, col = 'green') + geom_abline(intercept = 0.04472873, slope = 0.17274676, col = 'blue')
ggsave('depen_ratio.png', plot = last_plot())
