#Imagine we have sinusoidal data and we want to find its periodicity.
#We could do this by measuring the distance between maxima.
#We can plot the sine wave using geom_smooth, but how do we find the maxima?

library(tidyverse)
library(mgcv)

#Generate and plot noisy sinusoidal sample data
set.seed(404)
df <- data.frame(x = seq(0,4*pi,length.out=1000),
                 y = sin(seq(0,4*pi,length.out=1000))+rnorm(100,0,1))
df %>% ggplot(aes(x=x,y=y)) +
  geom_point() +
  geom_smooth()

#Use the function underlying geom_smooth to get y values of the curve
df <- df %>% 
  mutate(smooth_y = predict(gam(y ~ s(x,bs="cs"),data=df)))

#To find a single maximum in this data is easy; let's plot it as a vertical line
maximum <- df$x[which.max(df$smooth_y)]
df %>% ggplot() +
  geom_point(aes(x=x,y=y)) +
  geom_smooth(aes(x=x,y=y)) +
  geom_line(aes(x=x,y=smooth_y),size = 1.5, linetype = 2, col = "red")  +
  geom_vline(xintercept = maximum,color="green")

#To find other local maxima, however, we use a Monte Carlo method to find
#maxima within random ranges of x (discarding them if they occur at end of range)
maxima <- replicate(100,{
  x_range <- sample(df$x,size=2,replace=FALSE) %>% sort()
  max_loc <- df %>%
    filter(x >= x_range[1] & x <= x_range[2]) %>%
    filter(smooth_y == max(smooth_y)) %>%
    pull(x)
  if(max_loc == min(x_range)|max_loc == max(x_range)){NA}else{max_loc}
})
unique_maxima <- unique(maxima[!is.na(maxima)])

#We can plot the result to verify that it worked
df %>% ggplot() +
  geom_point(aes(x=x,y=y)) +
  geom_smooth(aes(x=x,y=y)) +
  geom_line(aes(x=x,y=smooth_y),size = 1.5, linetype = 2, col = "red")  +
  geom_vline(xintercept = unique_maxima,color="green")
