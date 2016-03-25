library(reshape2)
library(rstan)

code <- "
functions {
  vector std_beta_rng(int K, real eta) {
    matrix[K+1, K+1] Lambda;
    Lambda <- lkj_corr_rng(K + 1, eta);
   return inverse_spd(Lambda[1:K, 1:K]) * Lambda[1:K, K+1];
  }
}
model {}
"
expose_stan_functions(stanc(model_code = code))

N <- 1e4
K <- 100
draws1 <- replicate(N, std_beta_rng(K, 1))[1, ]
draws2 <- replicate(N, std_beta_rng(K, 5))[1, ]
draws3 <- replicate(N, std_beta_rng(K, 10))[1, ]
draws4 <- replicate(N, std_beta_rng(K, 50))[1, ]

df <- melt(data.frame(a = draws4, b = draws3, c = draws2, d = draws1))
ggplot(df, aes(x = value, color = variable)) +
  stat_density(geom = "line", size = 1, position = "identity") +
  scale_color_discrete(name = bquote(eta), labels = c("50", "10", "5", "1")) +
  coord_cartesian(xlim = c(-5, 5), expand = FALSE) +
  labs(x=NULL,y=NULL) +
  theme_classic() +
  theme(legend.position = c(.7, 0.8),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        axis.line.y = element_line(size = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14), 
        axis.line.x = element_line(size = 1))

ggsave("fig/betaplot.pdf", w = 7, h = 3.5)


# 
# # With K=10
# N <- 1e5
# K <- 10
# draws1 <- replicate(N, std_beta_rng(K, 1))[1, ]
# draws2 <- replicate(N, std_beta_rng(K, 2.5))[1, ]
# draws3 <- replicate(N, std_beta_rng(K, 5))[1, ]
# 
# df <- data.frame(x = c(draws3, draws2, draws1),
#                  eta = rep(c("5", "2.5", "1"), each = N))
# ggplot(df, aes(x = x, color = eta)) +
#   stat_density(size = 1, geom = "line") +
#   scale_color_discrete(name = bquote(eta)) +
#   coord_cartesian(xlim = c(-10, 10)) +
#   labs(x=NULL,y=NULL) +
#   theme_classic() +
#   theme(legend.position = c(.7, 0.8),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 15),
#         axis.line.y = element_line(size = 0.5),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.x = element_text(size = 14), 
#         axis.line.x = element_line(size = 1))
# ggsave("fig/betaplot.pdf", w = 7, h = 3.5)
