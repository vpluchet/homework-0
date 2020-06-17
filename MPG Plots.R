library(tidyverse)
data(mpg)
head(mpg)

?mpg

# plot miles per gallon on highways (hwy) vs car engine size in litres (displ)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))

# mapping a continuous class to shape (see the warning)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# mapping a continuous class to size (see the warning)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class))

# mapping a continuous class to two esthetics (see the warning)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class, color = class))

# mapping color to a test
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = displ < 5))

# using facet_wrap
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~class, nrow = 2)

# using facet_grid
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class)) + facet_grid(drv~cyl)

# using facet_grid or facet_wrap
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv~.)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~drv)

# scatterplot of hwy vs cyl
ggplot(data = mpg) + geom_point(mapping = aes(x = cyl, y = hwy))

# scatterplot of class vs drv
ggplot(data = mpg) + geom_point(mapping = aes(x = drv, y = class))

# Using ggsmooth
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + geom_smooth()
ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) + geom_point() + geom_smooth()
ggplot(data = mpg, aes(x = displ, y = hwy, linetype = drv)) + geom_smooth()
ggplot(data = mpg, aes(x = displ, y = hwy, color = drv)) + geom_smooth(show.legend = FALSE)
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point() + geom_smooth(aes(color = drv), se = FALSE)

ggplot(data = mpg, aes(x = displ, y = hwy, color = drv)) + geom_point(aes(color = class)) + geom_smooth()

# applying ggsmooth only to subcompact
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point(aes(color = class)) + geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

# ggsmooth with and without se
ggplot(data = mpg, aes(x = displ, y = hwy, color = drv)) + geom_point() + geom_smooth()
ggplot(data = mpg, aes(x = displ, y = hwy, color = drv)) + geom_point() + geom_smooth(se = FALSE)

# placing the mapping in or out of ggplot
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point() + geom_smooth()
ggplot() + geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))


data(mtcars)
?mtcars

# bar charts displaying counts
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))
ggplot(data = diamonds) + stat_count(mapping = aes(x = cut))
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = cut))

# stacked bars and other positions
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + geom_bar()
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + geom_bar(position = "fill")
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + geom_bar(position = "dodge")
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + geom_bar(position = "identity") # not recommended without alpha
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, color = clarity)) + geom_bar(fill = NA, position = "identity")


# bar chart displaying proportions
ggplot(data = diamonds) + stat_count(mapping = aes(x = cut, y = ..prop.., group = 1))

demo <- tribble(
  ~a,     ~b,
  "bar_1", 20,
  "bar_2", 30,
  "bar_3", 40
)
ggplot(data = demo) + geom_bar(mapping = aes(x = a, y = b), stat = "identity")
ggplot(data = demo) + geom_col(mapping = aes(x = a, y = b))

# See the difference with group = 1
ggplot(data = demo) + geom_bar(mapping = aes(x = a, y = ..prop..))
ggplot(data = demo) + geom_bar(mapping = aes(x = a, y = ..prop.., group = 1))

# Summaries
ggplot(data = diamonds) + stat_summary(mapping = aes(x = cut, y = depth),
                                      fun.min = min,
                                      fun.max = max,
                                      fun = median)

# Jitter
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point() # without jitter
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point(position = "jitter") # with jitter
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_jitter() # with jitter
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_count() # compare count with jitter

ggplot(data = mpg, mapping = aes(x = cty, y = hwy, color = class)) + geom_point(position = "jitter", alpha = 0.5)

# Box plot and coord flip
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot() + coord_flip()


# maps and quickmap
fr <- map_data("france")
ggplot(fr, aes(long, lat, group = group)) + geom_polygon(fill = "white", color = "black") #distorted
ggplot(fr, aes(long, lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_quickmap()

# interactive charts: SCATTER AND BUBBLE PLOTS: USE PLOTLY.
library(plotly)
p <- ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))
p
ggplotly(p)

# Themes (make sure you load ggthemes)
murders %>% ggplot(aes(x = population/10^6, y = total, label = abb, color = region)) +
  geom_point() + geom_text_repel() + scale_x_log10()

murders %>% ggplot(aes(x = population/10^6, y = total, label = abb, color = region)) +
  geom_point() + geom_text_repel() + scale_x_log10() + theme_economist()

murders %>% ggplot(aes(x = population/10^6, y = total, label = abb, color = region)) +
  geom_point() + geom_label() + scale_x_log10() + theme_fivethirtyeight()

g <- murders %>% ggplot(aes(x = population/10^6, y = total, label = abb, color = region)) +
  geom_point() + scale_x_log10()
ggplotly(g)

# define the intercept
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate

# make the plot, combining all elements
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()


