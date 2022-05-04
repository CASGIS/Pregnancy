setwd("~/Document/Github/Pregancy/figure1/")
devtools::source_gist("https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f")


# ---------------------
# figure 1
#----------------------
df <- read.csv("data for Figure 1.csv")

theme.color <- "#2E9FDF"
p <- ggplot(df, aes(x = week, y = mean)) + 
  geom_hline(yintercept = mean(df$mean), color="grey50", linetype = "dashed") +
  geom_vline(xintercept = 0, color="grey50", linetype = 1) +
  geom_line(size = 1.0, colour = theme.color) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.30, fill = theme.color) +
  geom_point(colour = theme.color, size = 3, shape = 20, show.legend = FALSE) +
  theme_Publication()+
  ylab("Sentiment") +
  xlab("Week") +
  scale_fill_manual(values = c(theme.color, "#FFFFFF"))

ggplot2::ggsave("sentiment_change.pdf", p, width = 9, height = 4.5)


# ---------------------
# figure 2
#----------------------
df <- read.csv("data for Figure 2.csv")

# Specify the width of your confidence intervals
interval1 <- 1
# interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

p <- ggplot(df, aes(x = week, y = b, colour = group, ymin = LB, ymax = UB)) + 
  geom_hline(yintercept = 0, color="grey50", linetype = "dashed") +
  geom_vline(xintercept = 0, color="grey50", linetype = 1) +
  geom_line(size = 1.0) +
  geom_linerange(aes(ymin = b - interval1*se, ymax = b + interval1*se), lwd = 2) +
  geom_linerange(aes(ymin = b - interval2*se, ymax = b + interval2*se), lwd = 0.5) +
  # geom_ribbon(aes(fill = group), alpha = 0.30) +
  # geom_point(aes(shape = sig.lev), size = 3,  show.legend = FALSE) +
  geom_point(fill = "white", size = 3, shape = 21, show.legend = FALSE) +
  theme_Publication()+
  ylab("Sentiment change") +
  xlab("Week") +
  theme(
    legend.position = "none"
  )

ggplot2::ggsave("sentiment_change_regression.pdf", p, width = 9, height = 4.5)
