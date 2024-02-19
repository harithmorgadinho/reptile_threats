library(dplyr)
library(reshape2)

simulation_func = function(x = df$value){
  
  
  
  generate_random_probabilities <- function(n) {
    probs <- runif(n) # Generate n random numbers
    probs / sum(probs) # Normalize so they sum to 1
  }
  
  list_b = list()
  list_b_percentages = list()
  list_b_CR = list()
  list_b_EN = list()
  list_b_VU = list()
  
  for (i in 10:100){
    print(i)
    list_s = list()
    list_s_percentages = list()
    list_s_CR = list()
    list_s_EN = list()
    list_s_VU = list()
    
    for(m in 1:1000){
      
      probs = generate_random_probabilities(5)
      value_temp = sample(x = df$value[-1],size = i,replace = T,prob = probs)
      
      list_zeros = ifelse(value_temp %in% df$value[4:6],1,0)
      
      percentage = length(list_zeros[list_zeros == 1])/length(list_zeros)
      
      CR_percentage = length(value_temp[value_temp == df$value[6]])/length(value_temp)
      EN_percentage = length(value_temp[value_temp == df$value[5]])/length(value_temp)
      VU_percentage = length(value_temp[value_temp == df$value[4]])/length(value_temp)
      
      
      value =  predict(glm(list_zeros ~ 1, weights = value_temp,
                           family = binomial(link = 'logit')),type = 'response')[1]
      list_s[m] = value
      list_s_percentages[m] = percentage
      list_s_CR[m] = CR_percentage
      list_s_EN[m] = EN_percentage
      list_s_VU[m] = VU_percentage
      
      
    }
    list_b[[i]] = unlist(list_s)
    list_b_percentages[[i]] = unlist(list_s_percentages)
    list_b_CR[[i]] = unlist(list_s_CR)
    list_b_EN[[i]] = unlist(list_s_EN)
    list_b_VU[[i]] = unlist(list_s_VU)
    
  }
  
  list_final = unlist(list_b)
  list_final_percentage = unlist(list_b_percentages)
  list_final_CR = unlist(list_b_CR)
  list_final_EN = unlist(list_b_EN)
  list_final_VU = unlist(list_b_VU)
  
  return(list(list_final,list_final_percentage,list_final_CR,list_final_EN,list_final_VU))
}

df = melt(cbind.data.frame( DD = 0.0513,
                            LC = 0.0009,
                            NT = 0.0071, 
                            VU = 0.0513, 
                            EN = 0.4276, 
                            CR = 0.9688))

plot(df$value)

df$value = exp(df$value + df$value^2)

plot(df$value)



binomial_2 =simulation_func(df$value)


df_plot_binomial_2 = cbind.data.frame(richness = rep(10:100,1000), 
                                      ER = binomial_2[[1]], 
                                      Perc = binomial_2[[2]],
                                      CR_percentage = binomial_2[[3]],
                                      EN_percentage = binomial_2[[4]],
                                      VU_percentage = binomial_2[[5]])
p1 = ggplot()+
  geom_point(data = df_plot_binomial_2, aes(x = Perc, y = ER, col = CR_percentage),size = 0.1)+
  scale_color_gradientn(colours = pal(100))

p2 = ggplot()+
  geom_point(data = df_plot_binomial_2, aes(x = Perc, y = ER, col = EN_percentage),size = 0.1)+
  scale_color_gradientn(colours = pal(100))

p3 = ggplot()+
  geom_point(data = df_plot_binomial_2, aes(x = Perc, y = ER, col = VU_percentage),size = 0.1)+
  scale_color_gradientn(colours = pal(100))

library(gridExtra)

grid1 = arrangeGrob(p1,p2,p3,ncol = 3)

ggsave(filename = 'grid_binomial_2.png',width = 15,height = 5,plot = grid1)

  
df = melt(cbind.data.frame( DD = 0.0513,
                            LC = 0.0009,
                            NT = 0.0071, 
                            VU = 0.0513, 
                            EN = 0.4276, 
                            CR = 0.9688))

plot(df$value)

df$value = exp(df$value)

binomial_2 =simulation_func(df$value)



df_plot_exp = cbind.data.frame(richness = rep(10:100,1000), 
                                      ER = binomial_2[[1]], 
                                      Perc = binomial_2[[2]],
                                      CR_percentage = binomial_2[[3]],
                                      EN_percentage = binomial_2[[4]],
                                      VU_percentage = binomial_2[[5]])
p1 = ggplot()+
  geom_point(data = df_plot_exp, aes(x = Perc, y = ER, col = CR_percentage),size = 0.1)+
  scale_color_gradientn(colours = pal(100))

p2 = ggplot()+
  geom_point(data = df_plot_exp, aes(x = Perc, y = ER, col = EN_percentage),size = 0.1)+
  scale_color_gradientn(colours = pal(100))

p3 = ggplot()+
  geom_point(data = df_plot_exp, aes(x = Perc, y = ER, col = VU_percentage),size = 0.1)+
  scale_color_gradientn(colours = pal(100))

library(gridExtra)

grid1 = arrangeGrob(p1,p2,p3,ncol = 3)

ggsave(filename = 'grid_df_plot_exp.png',width = 15,height = 5,plot = grid1)


df = melt(cbind.data.frame( DD = 0.0513,
                            LC = 0.0009,
                            NT = 0.0071, 
                            VU = 0.0513, 
                            EN = 0.4276, 
                            CR = 0.9688))

plot(df$value)

df$value = exp(df$value + df$value^2 + df$value^3)

binomial_2 =simulation_func(df$value)



df_plot_binomial_3 = cbind.data.frame(richness = rep(10:100,1000), 
                               ER = binomial_2[[1]], 
                               Perc = binomial_2[[2]],
                               CR_percentage = binomial_2[[3]],
                               EN_percentage = binomial_2[[4]],
                               VU_percentage = binomial_2[[5]])
p1 = ggplot()+
  geom_point(data = df_plot_binomial_3, aes(x = Perc, y = ER, col = CR_percentage),size = 0.1)+
  scale_color_gradientn(colours = pal(100))

p2 = ggplot()+
  geom_point(data = df_plot_binomial_3, aes(x = Perc, y = ER, col = EN_percentage),size = 0.1)+
  scale_color_gradientn(colours = pal(100))

p3 = ggplot()+
  geom_point(data = df_plot_binomial_3, aes(x = Perc, y = ER, col = VU_percentage),size = 0.1)+
  scale_color_gradientn(colours = pal(100))

library(gridExtra)

grid1 = arrangeGrob(p1,p2,p3,ncol = 3)

ggsave(filename = 'grid_df_binomial_3.png',width = 15,height = 5,plot = grid1)


summary(lm(data = df_plot_binomial_3,ER ~ Perc))

summary(lm(data = df_plot_binomial_2,ER ~ Perc))

summary(lm(data = df_plot_exp,ER ~ Perc))


  
  #old
hist(list_final)
hist(list_final_percentage)

df_plot = cbind.data.frame(richness = rep(10:100,1000), ER = list_final, Perc = list_final_percentage)

library(ggplot2)

df_plot_melt = melt(df_plot,id = 'richness')
colnames(df_plot_melt)

pal = colorRampPalette(hcl.colors(10,'viridis'))

ggplot()+
  geom_point(data = df_plot, aes(x = Perc, y = ER, col = ER),size = 0.1)+
  scale_color_gradientn(colours = pal(100))
  

df_plot1 = df_plot


df = melt(cbind.data.frame( DD = 0.0513,
                            LC = 0.0009,
                            NT = 0.0071, 
                            VU = 0.0513, 
                            EN = 0.4276, 
                            CR = 0.9688))

df$value = exp(df$value + df$value^2 + df$value^3)

plot(df$value)

generate_random_probabilities <- function(n) {
  probs <- runif(n) # Generate n random numbers
  probs / sum(probs) # Normalize so they sum to 1
}

list_b = list()
list_b_percentages = list()

for (i in 10:100){
  print(i)
  list_s = list()
  list_s_percentages = list()
  
  for(m in 1:1000){
    
    probs = generate_random_probabilities(5)
    value_temp = sample(x = df$value[-1],size = i,replace = T,prob = probs)
    
    list_zeros = ifelse(value_temp %in% df$value[4:6],1,0)
    
    percentage = length(list_zeros[list_zeros == 1])/length(list_zeros)
    
    value =  predict(glm(list_zeros ~ 1, weights = value_temp,
                         family = binomial(link = 'logit')),type = 'response')[1]
    list_s[m] = value
    list_s_percentages[m] = percentage
    
  }
  list_b[[i]] = unlist(list_s)
  list_b_percentages[[i]] = unlist(list_s_percentages)
  
}

list_final = unlist(list_b)
list_final_percentage = unlist(list_b_percentages)

hist(list_final)
hist(list_final_percentage)

df_plot = cbind.data.frame(richness = rep(10:100,1000), ER = list_final, Perc = list_final_percentage)

library(ggplot2)

df_plot_melt = melt(df_plot,id = 'richness')
colnames(df_plot_melt)

pal = colorRampPalette(hcl.colors(10,'viridis'))

ggplot()+
  geom_point(data = df_plot, aes(x = Perc, y = ER, col = ER),size = 0.1)+
  scale_color_gradientn(colours = pal(100))

df_plot2 = df_plot


df = melt(cbind.data.frame( DD = 0.0513,
                            LC = 0.0009,
                            NT = 0.0071, 
                            VU = 0.0513, 
                            EN = 0.4276, 
                            CR = 0.9688))

df$value = exp(df$value + df$value^2 + df$value^3 + df$value^4)

plot(df$value)

generate_random_probabilities <- function(n) {
  probs <- runif(n) # Generate n random numbers
  probs / sum(probs) # Normalize so they sum to 1
}

list_b = list()
list_b_percentages = list()

for (i in 10:100){
  print(i)
  list_s = list()
  list_s_percentages = list()
  
  for(m in 1:1000){
    
    probs = generate_random_probabilities(5)
    value_temp = sample(x = df$value[-1],size = i,replace = T,prob = probs)
    
    list_zeros = ifelse(value_temp %in% df$value[4:6],1,0)
    
    percentage = length(list_zeros[list_zeros == 1])/length(list_zeros)
    
    value =  predict(glm(list_zeros ~ 1, weights = value_temp,
                         family = binomial(link = 'logit')),type = 'response')[1]
    list_s[m] = value
    list_s_percentages[m] = percentage
    
  }
  list_b[[i]] = unlist(list_s)
  list_b_percentages[[i]] = unlist(list_s_percentages)
  
}

list_final = unlist(list_b)
list_final_percentage = unlist(list_b_percentages)

hist(list_final)
hist(list_final_percentage)

df_plot = cbind.data.frame(richness = rep(10:100,1000), ER = list_final, Perc = list_final_percentage)

library(ggplot2)

df_plot_melt = melt(df_plot,id = 'richness')
colnames(df_plot_melt)

pal = colorRampPalette(hcl.colors(10,'viridis'))

ggplot()+
  geom_point(data = df_plot, aes(x = Perc, y = ER, col = ER),size = 0.1)+
  scale_color_gradientn(colours = pal(100))

df_plot3 = df_plot

df = melt(cbind.data.frame( DD = 0.0513,
                            LC = 0.0009,
                            NT = 0.0071, 
                            VU = 0.0513, 
                            EN = 0.4276, 
                            CR = 0.9688))

df$value = exp(df$value)

plot(df$value)

generate_random_probabilities <- function(n) {
  probs <- runif(n) # Generate n random numbers
  probs / sum(probs) # Normalize so they sum to 1
}

list_b = list()
list_b_percentages = list()

for (i in 10:100){
  print(i)
  list_s = list()
  list_s_percentages = list()
  
  for(m in 1:1000){
    
    probs = generate_random_probabilities(5)
    value_temp = sample(x = df$value[-1],size = i,replace = T,prob = probs)
    
    list_zeros = ifelse(value_temp %in% df$value[4:6],1,0)
    
    percentage = length(list_zeros[list_zeros == 1])/length(list_zeros)
    
    value =  predict(glm(list_zeros ~ 1, weights = value_temp,
                         family = binomial(link = 'logit')),type = 'response')[1]
    list_s[m] = value
    list_s_percentages[m] = percentage
    
  }
  list_b[[i]] = unlist(list_s)
  list_b_percentages[[i]] = unlist(list_s_percentages)
  
}

list_final = unlist(list_b)
list_final_percentage = unlist(list_b_percentages)

hist(list_final)
hist(list_final_percentage)

df_plot = cbind.data.frame(richness = rep(10:100,1000), ER = list_final, Perc = list_final_percentage)

library(ggplot2)

df_plot_melt = melt(df_plot,id = 'richness')
colnames(df_plot_melt)

pal = colorRampPalette(hcl.colors(10,'viridis'))

ggplot()+
  geom_point(data = df_plot1, aes(x = Perc, y = ER, col = ER),size = 0.1)+
  scale_color_gradientn(colours = pal(100))

df_plot4 = df_plot


df = melt(cbind.data.frame( DD = 0.0513,
                            LC = 0.0009,
                            NT = 0.0071, 
                            VU = 0.0513, 
                            EN = 0.4276, 
                            CR = 0.9688))

df$value = exp(df$value + df$value^2)

list_zeros = c(0,0,0,0,0,1,1,1,1,1)
value_temp = c(df$value[2],df$value[2],df$value[2],df$value[2],df$value[2],
               df$value[6],df$value[6],df$value[6],df$value[6],df$value[6])
predict(glm(list_zeros ~ 1, weights = value_temp,
            family = binomial(link = 'logit')),type = 'response')[1]



list_zeros = c(0,0,0,0,0,1,1,1,1,1)
value_temp = c(df$value[2],df$value[2],df$value[2],df$value[2],df$value[2],
               df$value[5],df$value[5],df$value[5],df$value[5],df$value[5])
predict(glm(list_zeros ~ 1, weights = value_temp,
            family = binomial(link = 'logit')),type = 'response')[1]

list_zeros = c(0,0,0,0,0,1,1,1,1,1)
value_temp = c(df$value[2],df$value[2],df$value[2],df$value[2],df$value[2],
               df$value[4],df$value[4],df$value[4],df$value[4],df$value[4])
predict(glm(list_zeros ~ 1, weights = value_temp,
            family = binomial(link = 'logit')),type = 'response')[1]



df = melt(cbind.data.frame( DD = 0.0513,
                            LC = 0.0009,
                            NT = 0.0071, 
                            VU = 0.0513, 
                            EN = 0.4276, 
                            CR = 0.9688))

df$value = exp(df$value)

list_zeros = c(0,0,0,0,0,1,1,1,1,1)
value_temp = c(df$value[2],df$value[2],df$value[2],df$value[2],df$value[2],
               df$value[6],df$value[6],df$value[6],df$value[6],df$value[6])
predict(glm(list_zeros ~ 1, weights = value_temp,
            family = binomial(link = 'logit')),type = 'response')[1]



list_zeros = c(0,0,0,0,0,1,1,1,1,1)
value_temp = c(df$value[2],df$value[2],df$value[2],df$value[2],df$value[2],
               df$value[5],df$value[5],df$value[5],df$value[5],df$value[5])
predict(glm(list_zeros ~ 1, weights = value_temp,
            family = binomial(link = 'logit')),type = 'response')[1]

list_zeros = c(0,0,0,0,0,1,1,1,1,1)
value_temp = c(df$value[2],df$value[2],df$value[2],df$value[2],df$value[2],
               df$value[4],df$value[4],df$value[4],df$value[4],df$value[4])
predict(glm(list_zeros ~ 1, weights = value_temp,
            family = binomial(link = 'logit')),type = 'response')[1]





# Assuming df$ER is your vector of interest


df_plot4 == '0'
df_plot3 == '4'
df_plot2 == '3'
df_plot1 == '2'



###

# Assuming df$ER is your vector of interest

# Range
range_value <- range(df_plot1$ER) # Returns a vector with min and max values
range_diff <- diff(range(df_plot1$ER)) # Difference between max and min

# Interquartile Range (IQR)
iqr_value <- IQR(df_plot1$ER)

# Variance
variance_value <- var(df_plot1$ER)

# Standard Deviation
std_deviation <- sd(df_plot1$ER)

# Coefficient of Variation (CV)
# CV = standard deviation / mean
cv_value <- std_deviation / mean(df_plot1$ER)

# Print the results
cat("Range: ", range_value, "\n")
cat("Range (Difference): ", range_diff, "\n")
cat("Interquartile Range (IQR): ", iqr_value, "\n")
cat("Variance: ", variance_value, "\n")
cat("Standard Deviation: ", std_deviation, "\n")
cat("Coefficient of Variation (CV): ", cv_value, "\n")

###

# Assuming df$ER is your vector of interest

# Range
range_value <- range(df_plot2$ER) # Returns a vector with min and max values
range_diff <- diff(range(df_plot2$ER)) # Difference between max and min

# Interquartile Range (IQR)
iqr_value <- IQR(df_plot2$ER)

# Variance
variance_value <- var(df_plot2$ER)

# Standard Deviation
std_deviation <- sd(df_plot2$ER)

# Coefficient of Variation (CV)
# CV = standard deviation / mean
cv_value <- std_deviation / mean(df_plot2$ER)

# Print the results
cat("Range: ", range_value, "\n")
cat("Range (Difference): ", range_diff, "\n")
cat("Interquartile Range (IQR): ", iqr_value, "\n")
cat("Variance: ", variance_value, "\n")
cat("Standard Deviation: ", std_deviation, "\n")
cat("Coefficient of Variation (CV): ", cv_value, "\n")

# Assuming df$ER is your vector of interest

# Range
range_value <- range(df_plot3$ER) # Returns a vector with min and max values
range_diff <- diff(range(df_plot3$ER)) # Difference between max and min

# Interquartile Range (IQR)
iqr_value <- IQR(df_plot3$ER)

# Variance
variance_value <- var(df_plot3$ER)

# Standard Deviation
std_deviation <- sd(df_plot3$ER)

# Coefficient of Variation (CV)
# CV = standard deviation / mean
cv_value <- std_deviation / mean(df_plot3$ER)

# Print the results
cat("Range: ", range_value, "\n")
cat("Range (Difference): ", range_diff, "\n")
cat("Interquartile Range (IQR): ", iqr_value, "\n")
cat("Variance: ", variance_value, "\n")
cat("Standard Deviation: ", std_deviation, "\n")
cat("Coefficient of Variation (CV): ", cv_value, "\n")


# Assuming df$ER is your vector of interest

# Range
range_value <- range(df_plot4$ER) # Returns a vector with min and max values
range_diff <- diff(range(df_plot4$ER)) # Difference between max and min

# Interquartile Range (IQR)
iqr_value <- IQR(df_plot4$ER)

# Variance
variance_value <- var(df_plot4$ER)

# Standard Deviation
std_deviation <- sd(df_plot4$ER)

# Coefficient of Variation (CV)
# CV = standard deviation / mean
cv_value <- std_deviation / mean(df_plot4$ER)

# Print the results
cat("Range: ", range_value, "\n")
cat("Range (Difference): ", range_diff, "\n")
cat("Interquartile Range (IQR): ", iqr_value, "\n")
cat("Variance: ", variance_value, "\n")
cat("Standard Deviation: ", std_deviation, "\n")
cat("Coefficient of Variation (CV): ", cv_value, "\n")

