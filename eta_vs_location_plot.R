library(ggplot2)

# compute eta if loc is prior mode
eta_mode <- function(loc, K) (K/2 - 1  - loc * (K/2) + loc * 2) / loc

N <- 500
location <- rep(seq(0, 1, length.out = N), 4)
K <- rep(c(10,50,100,500), each = N)
eta <- eta_mode(location, K)

df <- data.frame(x = location, y = eta, K = factor(K))
ggplot(df, aes(x=x, y=y, color=K)) + 
  geom_line(size = 1) + 
  xlab(expression(paste("Prior mode of ", R^2))) + 
  ylab(expression(paste("Implied value of ", eta))) + 
  scale_color_discrete(name = "K (predictors)") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 2000), expand = FALSE) +
  theme_classic() +
  theme(legend.position = c(.7, 0.8),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        axis.line.y = element_line(size = 0.5),
        axis.text.x = element_text(size = 14), 
        axis.line.x = element_line(size = 1), 
        axis.title = element_text(size = 15))

ggsave("fig/eta_vs_location_plot.pdf", w = 7, h = 3.5)
