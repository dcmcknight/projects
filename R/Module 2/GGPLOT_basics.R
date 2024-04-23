library(ggplot2)
library(dplyr)
set.seed(123)
ddf = diamonds %>% dplyr::sample_frac(0.1)
ggplot(ddf, aes(x = carat, y = price)) 


###open a connection that routes all plot output
###to a file called "myname.pdf" which will be
###created in your current working directory.
#pdf("GGPLOT_Basics_Quiz_Drew.pdf") ## route output to file instead of Plots pane

##create your ten plots (two sample plots are created below)
ggplot(ddf,aes(x=clarity)) + geom_bar()
ggplot(ddf,aes(x=clarity, y = price)) +
  geom_jitter(alpha = 0.5, aes(color = cut))

ggplot(data = ddf) +
 aes(x = carat, y = price) + geom_point() + geom_smooth()

ggplot(data = ddf) +
  aes(x = carat, y = price, color = clarity) + geom_point() + geom_smooth()

ggplot(data = ddf) +
  aes(x = carat, y = price) + geom_point(alpha = 0.1)

###Define the data and aesthetics layers:
###map carat on the x and price on the y axis.
###Assign it to an object: diaPlot.
diaPlot = ggplot(ddf, aes(x = carat, y = price))

##Using +, add a layer of points
diaPlot + geom_point(aes(color = cut)) + ggtitle("Ideal Diamonds Become more Frequent as Diamonds
Get Bigger") 

diaPlot + geom_smooth(aes(color = cut),se = F)

ggplot(ddf,aes(carat,price)) + geom_point(color="darkblue")
ggplot(ddf,aes(carat,price)) + geom_point(color="darkorange")

ggplot(ddf,aes(x=clarity, fill = cut))+geom_bar(position = 'dodge')
###close the connection to the file so that your
###plots are saved
#dev.off() ## if you are having issues, run this line repeatedly
