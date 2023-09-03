# Purpose
# Use ggbreak to effectively utilize plotting space to deal with large datasets and outliers


# Gap plot

#Feature 1: Breaking the plot

library(ggplot2)
library(ggbreak) 
library(patchwork)

set.seed(2023-08-30)
data <- data.frame(x = 1:20,
                y = c(rnorm(5) + 4, 
                      rnorm(5) + 20, 
                      rnorm(5) + 5, 
                      rnorm(5) + 22))

#data = write.csv(data,"data.csv",row.names = F)

data2 <- data.frame(x = c(2, 18), 
                 y = c(18, 26), 
                 label = c("hello", "world"))

p1 <- ggplot(data, aes(y, x)) + 
  geom_col(orientation="y",fill=rainbow(20),alpha=.7,colour="gray22")+
  theme_minimal()

p2 <- p1 + scale_x_break(c(7, 17)) + 
  geom_text(aes(y, x, label=label), 
            data=data2, hjust=1, 
            colour = 'firebrick')  + 
  xlab(NULL) + ylab(NULL) + 
  theme_minimal()

p1 + p2

# Feature 2: Multiple break-points are supported

p2 + scale_x_break(c(2, 4))

# Feature 3: Zoom in or zoom out of subplots

p1 + scale_x_break(c(7, 17), scales = 1.5) + scale_x_break(c(18, 21), scales=2)

#Feature 4: Support reverse scale
g <- ggplot(data, aes(x, y)) + geom_col(fill=rainbow(20),colour="gray22")
g2 <- g + scale_y_break(c(7, 17), scales = 1.5) + 
  scale_y_break(c(18, 21), scale=2) + scale_y_reverse()
g + g2

#Feature 5: Compatible with scale transform functions
#Users can apply scale transform functions, such as scale_x_log10 and scale_x_sqrt, to axis break plot.

p2 <- p1 + scale_x_break(c(7, 17)) 
p3 <- p1 + scale_x_break(c(7, 17)) +
  scale_x_log10()
p2 + p3

#Feature 6: Compatible with coord_flip
g + coord_flip() + scale_y_break(c(7, 18))

#Feature 7: Compatible with facet_grid and facet_wrap
set.seed(2023-08-30)
d <- data.frame(
  x = 1:20,
  y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22),
  group = c(rep("A", 10), rep("B", 10)),
  face=c(rep("C", 5), rep("D", 5), rep("E", 5), rep("F", 5))
)

p <- ggplot(d, aes(x=x, y=y)) +
  geom_col(orientation="x",
           colour="gray22",
           fill=rainbow(20)) +
  scale_y_reverse() +
  facet_wrap(group~.,
             scales="free_y",
             strip.position="right",
             nrow=2
  ) +
  coord_flip()
pg <- p +
  scale_y_break(c(7, 17), scales="free") +
  scale_y_break(c(19, 21), scales="free")
print(pg)

#Feature 8: Compatible with legends
pg <- pg + aes(fill=group) + theme(legend.position = "bottom")
print(pg)

# Feature 9: Supports all plot labels
pg + labs(title="My Graph", subtitle="Made by random data", 
          tag="A tag", 
          caption="A caption") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.placement = "outside",
    axis.title.x=element_text(size=10),
    plot.title = element_text(size = 22),
    plot.subtitle = element_text(size = 16),
    plot.tag = element_text(size = 10),
    plot.title.position = "plot",
    plot.tag.position = "topright",
    plot.caption = element_text(face="bold.italic"),
    
  )

# Feature 10: Allows setting tick labels for subplots
require(ggplot2)
library(ggbreak)
set.seed(2023-08-30)
d <- data.frame(
  x = 1:20,
  y =  c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22),
  group = c(rep("A", 10), rep("B", 10))
)

p <- ggplot(d, aes(x=x, y=y)) +
  scale_y_reverse() +
  scale_x_reverse() +
  geom_col(aes(fill=group)) +
  scale_fill_manual(values=c("#00AED7", "#009E73")) +
  facet_wrap(
    group~.,
    scales="free_y",
    strip.position="right",
    nrow=2
  ) +
  coord_flip()                                                                                                                                                                                                  

p +
  scale_y_break(c(7, 10), scales=0.5, ticklabels=c(10, 11.5, 13)) +
  scale_y_break(c(13, 17), scales=0.5, ticklabels=c(17, 18, 19)) +
  scale_y_break(c(19,21), scales=1, ticklabels=c(21, 22, 23))

#Feature 11: Compatible with dual axis
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(
    "mpg (US)",
    sec.axis = sec_axis(~ . * 1.20, name = "mpg (UK)")
  ) +
  theme(
    axis.title.y.left = element_text(color="deepskyblue"),
    axis.title.y.right = element_text(color = "orangered")
  )
p1 <- p + scale_y_break(breaks = c(20, 30))
p2 <- p + scale_x_break(breaks = c(3, 4))
p1 + p2

#Feature 12: Compatible with patchwork
library(patchwork)

set.seed(2023-08-30)
d <- data.frame(
  x = 1:20,
  y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22)
)

p <- ggplot(d, aes(x, y)) + 
  geom_col(colour="gray22",
           fill=rainbow(20),alpha=.5)
x <- p+scale_y_break(c(7, 17 ))

x + p

# Wrap plot
#The scale_wrap() function wraps a ‘gg’ plot over multiple rows to make plots with long x-axes easier to read.

p <- ggplot(economics, 
            aes(x=date, y = unemploy, 
                colour = uempmed)) +
  geom_line()

p + scale_wrap(n=4)

#Both categorical and numerical variables are supported.

ggplot(mpg, aes(class, hwy)) + 
  geom_boxplot() +
  scale_wrap(n = 2)

# Cut plot
The scale_x_cut or scale_y_cut cuts a ‘gg’ plot to several slices with the ability to specify which subplots to zoom in or zoom out.

library(ggplot2)
library(ggbreak)
set.seed(2023-08-30)
d <- data.frame(
  x = 1:20,
  y = c(rnorm(5) + 4, rnorm(5) + 20, rnorm(5) + 5, rnorm(5) + 22)
)
p <- ggplot(d, aes(x, y)) + 
  geom_col(fill=topo.colors(20),colour="gray22")
p + scale_y_cut(breaks=c(7, 18), 
                which=c(1, 3), scales=c(3, 0.5))

#Adjust the amount of space between subplots
#The space parameter in scale_x_break(), scale_y_break(), scale_x_cut() and scale_y_cut() allows user to control the space between subplots.

p + scale_y_cut(breaks=c(7, 18), which=c(1, 3), scales=c(3, 0.5), space=.5)

# Place legend at any position
## original plot
p1 <- ggplot(mpg, 
             aes(displ, hwy, 
                      color=factor(cyl))) +
       geom_point(size=5,shape=18)

## ggbreak plot without legend
p2 <- p1 + scale_x_break(c(3, 4)) +
  theme(legend.position="none") 

## extract legend from original plot
leg = cowplot::get_legend(p1)

## redraw the figure
p3 <- ggplotify::as.ggplot(print(p2))

## place the legend 
p3 + ggimage::geom_subview(x=.9, y=.8, subview=leg)

# References
#https://cran.r-project.org/web/packages/ggbreak/vignettes/ggbreak.html?fbclid=IwAR04S-Dk8Sr5vCf1mioAJs-XhY1q21n0ocjsICRqEwG46xa0nfjW_i2SDJ8#:~:text=Axis%20break%20or%20a%20so,distributed

# colours
# rainbow(n)
# heat.colors(n)
# terrain.colors(n)
# topo.colors(n)
# cm.colors(n)