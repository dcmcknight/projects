library("dplyr")
library("ggplot2")
library("causact")
library(ggrepel)
# retrieve the data
data("corruptDF")

# define the coordinate system
ggplot(corruptDF, aes(x = CPI2017, y = HDI2017))

corruptDF2 = corruptDF %>%
  group_by(region) %>%
  top_n(n = 10, wt = population) %>%
  arrange(region, desc(population))
corruptDF2

## ---- out.width = "95%"--------------------------------------------------
plot1 = ggplot(corruptDF2, aes(x = CPI2017, y = HDI2017, label = country)) +
  theme_minimal(14) +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "red") +
  geom_point(aes(color=region), shape = 21, size = 4, stroke = 2, fill = "white") +
  geom_text_repel(force = 2,
                  point.padding = 0.85,
                  fontface = "bold",
                  data = corruptDF2[corruptDF2$country != "Mexico",], color = "black") + 
  geom_text_repel(force = 2,
                  point.padding = 0.75,
                  fontface = "bold",
                  data = corruptDF2[corruptDF2$country == "Mexico",], color = "red")
plot1

plot2 = plot1 +
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
                                "orange")) 
plot2


### save the last made plot to myEconomistPlot.pdf
### in your working directory
ggsave("McKnightEconomistPlot.pdf")


