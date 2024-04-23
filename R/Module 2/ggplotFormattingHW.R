## A large portion of thoughts and concepts in this exercise are inspired by Harvard's tutorial _Introduction to R Graphics with_ `ggplot2`.  Available at http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html.

## ----ggplotCodeStructure, eval=FALSE-------------------------------------
## ggplot(data = <default data set>,
##        aes(x = <default x axis variable>,
##            y = <default y axis variable>,
##            ... <other default aesthetic mappings>),
##        ... <other plot defaults>) +
##
##         geom_<geom type>(aes(size = <size variable for this geom>,
##                       ... <other aesthetic mappings>),
##                   data = <data for this point geom>,
##                   stat = <statistic string or function>,
##                   position = <position string or function>,
##                   color = <"fixed color specification">,
##                   <other arguments, possibly passed to the _stat_ function) +
##
##         scale_<aesthetic>_<type>(name = <"scale label">,
##                      breaks = <where to put tick marks>,
##                      labels = <labels for tick marks>,
##                      ... <other options for the scale>) +
##
##         theme(plot.background = element_rect(fill = "gray"),
##         ... <other theme elements>)

## ------------------------------------------------------------------------
library("dplyr")
library("ggplot2")
library("causact")
# retrieve the data
data("corruptDF")

# define the coordinate system
ggplot(corruptDF, aes(x = CPI2017, y = HDI2017))

## ----basicGG, fig.cap = "A basic plot of the corruption data."-----------
plot1 = ggplot(corruptDF, aes(x = CPI2017, y = HDI2017)) + geom_point()
plot1

## ----basic2GG, fig.cap = "A basic plot of the corruption data."----------
ggplot(corruptDF, aes(x = CPI2017, y = HDI2017)) +
  geom_point(aes(color=region), size = 4)

## ----themetest, fig.cap = "(ref:themetest)", out.width = "95%"-----------
library(gridExtra)
plotA = plot1 + theme_classic()
plotB = plot1 + theme_minimal()
grid.arrange(plotA,plotB, nrow = 1)

## ----themeNew, fig.cap = "A custom theme example."-----------------------
plot1 = ggplot(corruptDF, aes(x = CPI2017, y = HDI2017)) +
  geom_point(aes(color=region))

theme_new <- theme_classic() +
  theme(plot.background = element_rect(size = 1, color = "blue", fill = "black"),
        text=element_text(size = 16, color = "darkblue"),
        axis.text.y = element_text(colour = "purple"),
        axis.text.x = element_text(colour = "red"),
        panel.background = element_rect(fill = "pink"),
        strip.background = element_rect(fill = "orange"))

plot1 + theme_new

## ----basicPlot, fig.cap = "The basic plot from which we build."----------
plot2 = ggplot(corruptDF, aes(x = CPI2017, y = HDI2017)) +
  theme_minimal()
plot2 + geom_point(aes(color=region))

## ---- out.width = "95%"--------------------------------------------------
plot2 + geom_point(aes(color=region), shape = 21, size = 4, stroke = 2)
###and after a few adjustments
plot3 = plot2 +
  geom_point(aes(color=region), shape = 21, size = 5, stroke = 2, fill = "white")
plot3

## See https://ggplot2.tidyverse.org/reference/geom_smooth.html for more information on the `geom_smooth` arguments.

## ---- out.width = "95%"--------------------------------------------------
plot3 + geom_smooth()

###with a few adjustments
plot3 + geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "red")

##placing the line layer behind the points layer
plot4 = plot2 +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "red") +
  geom_point(aes(color=region), shape = 21, size = 5, stroke = 2, fill = "white")
plot4

## ------------------------------------------------------------------------
countriesToLabel = c("Russia", "Venezuela", "China", "Kenya", "Syria",
                   "Afghanistan", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "Spain","Botswana", "France",
                   "United States", "Germany", "Britain", "Barbados",
                   "Norway", "Japan","New Zealand", "Singapore")

corruptDF2 = corruptDF %>%
  mutate(cLabel =ifelse(country %in% countriesToLabel, country, NA))
corruptDF2

## ---- out.width = "95%"--------------------------------------------------
plot5 = ggplot(corruptDF2, aes(x = CPI2017, y = HDI2017)) +
  theme_minimal(14) +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "red") +
  geom_point(aes(color=region), shape = 21, size = 4, stroke = 2, fill = "white")
plot5

## ---- out.width = "95%", fig.fullwidth = TRUE, fig.cap = "Plot with labels for selected countries.", fig.width = 12----
plot6 = plot5 +
  geom_text(aes(label = cLabel),
            nudge_x = -0.05,
            nudge_y = 0.01,
            fontface = "bold")
plot6

### use install.packages("ggrepel") as needed
library(ggrepel)
plot7 = plot5 +
  geom_text_repel(aes(label = cLabel),
                  force = 2,
                  point.padding = 0.75,
                  fontface = "bold")
plot7

## While beyond the scope of this exercise, you can make the above plots interactive using the `plotly` package.  This can allow for things like mouseover tooltips that display information for each point as you hover over them with your mouse. For more info, see: https://plot.ly/ggplot2/user-guide/.

## ------------------------------------------------------------------------
valuesNeedingColors = unique(corruptDF2$region)
valuesNeedingColors

## ------------------------------------------------------------------------
## create list of colors using hexadecimal code - your list can be different
colorList = c("#466aff",
"#936796",
"#95000c",
"#02d48e",
"#002500",
"#b5ae64")
names(colorList) = valuesNeedingColors

##
plot7 + scale_color_manual(values = colorList)

## ------------------------------------------------------------------------
corruptDF %>%
  group_by(region) %>%
  top_n(n = 1, wt = population)

##
colorList2 = c("Asia Pacific" = "#DE2910", # based on China Red
             "East EU Cemt Asia" = "#D52B1E",  # based on Russia Red
             "MENA" = "#C09300", # based on Egypt Gold
             "SSA" = "#008751", # based on Nigeria Green
             "Americas" = "#002868", # based on United States Blue
             "EU W. Europe" = "#000000" # based on germany Black
)

plot7 + scale_color_manual(values = colorList2)

##
plot8 = plot7 + scale_color_manual(values = colorList)

## $^{**}$ A list of scale functions can be found at https://ggplot2.tidyverse.org/reference/#section-scales.

## ----themeAdjustedEc, fig.fullwidth = TRUE, out.width = "85%", fig.width = 12----
plot8 = plot7 +
  guides(col = guide_legend(nrow = 1, title = NULL)) +
  ggtitle("Corruption and Human Development") +
  theme(text = element_text(color = "gray20"),
        legend.position = c("top"), # position the legend in the upper left
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()
        ) +
  scale_x_continuous(name = "Corruption Perceptions Index, 2017 (100=least corrupt)",
                     limits = c(0, 100),
                     breaks = seq(0, 100, by = 5)) +
  scale_y_continuous(name = "Human Development Index, 2017 (1=Best)",
                     limits = c(0.2, 1.0),
                     breaks = seq(0.2, 1.0, by = 0.1)) +
  scale_color_manual(name = "",
                     values = c("#24576D",
                                "#099DD7",
                                "#28AADC",
                                "#248E84",
                                "#F2583F",
                                "#96503F"))
plot8

### save the last made plot to myEconomistPlot.pdf
### in your working directory
ggsave("myEconomistPlot.pdf")

## $^{**}$ A list of theme elements can be found at https://ggplot2.tidyverse.org/reference/theme.html.

