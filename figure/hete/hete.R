setwd("~/Document/Github/Pregancy/hete/")
library(reshape2)
library(tidyverse)
library(cowplot)
devtools::source_gist("https://gist.github.com/Jianghao/d806dcc220a49df21024e0516f102b2f")


# ---------------------
# heterogeneity: Heterogeneous effects 
#----------------------
df <- read.csv("hete.csv") %>%
  mutate(semax = coef + 1.96 * se, semin = coef - 1.96 * se)
df$group <- factor(df$group, levels = unique(df$group))


Barplot_dodge <- function(data, xlab = NULL, ylab = NULL, axis.y.hide = FALSE, 
                          mycols = c("#E64B35", "#357EBD", "#00A087", "#9632B8", "#4DBBD5", "#3C5488"),
                          position = position_dodge(0.6),
                          legend.position = c(0.25, 0.85),
                          vline = 0, text.angle = 0) {
  data$star_f <- ifelse(nchar(as.character(data$star)) > 0, "sig", "ns") 
  p <- ggplot(data, aes(x = id, y = coef, fill = group, colour = group, 
                        ymin = coef - 1.96*se, ymax = coef + 1.96*se)) +
    geom_hline(yintercept = c(0), linetype = 1, colour = "grey50") +
    geom_col(stat = "identity", width = 0.6, alpha = 0.3, size = 0.8,
             position = position) +
    geom_text(aes(label = star), vjust = 1.4, hjust = -0.1, size = 7,
              position=position, show.legend = FALSE) +
    geom_errorbar(position=position, width = 0.1, size = 0.8, alpha = 1) +
    # geom_point(data = subset(data, star_f == "ns"), aes(id, coef), 
    #            shape = 21, position = position, size = 2.0, fill = "white") +
    # geom_point(data = subset(data, star_f == "sig"), aes(id, coef, fill = group2, colour = group2), 
    #            shape = 21, position = position, size = 2.0) +
    geom_point(shape = 21, size = 2, position=position) +
    geom_vline(xintercept = c(vline), linetype = 2, colour = "grey50") +
    xlab(xlab) +
    ylab(ylab) +
    ylim(df.range) +
    # scale_fill_npg() +
    # scale_color_npg() +
    scale_color_manual(values = mycols) +
    scale_fill_manual(values = mycols) +
    theme_Publication() +
    guides(fill = guide_legend(title = NULL),
           colour = guide_legend(title = NULL)) +
    theme(
      legend.text = element_text(size = 16, colour = c("grey25")),
      axis.text.x = element_text(angle = text.angle, vjust = 0.6),
      legend.direction = "vertical",
      legend.spacing.y = unit(2, 'cm'),
      legend.key.size = unit(0.6, "cm"),
      legend.position = legend.position,
      plot.margin = unit(c(0.2,0.2,1,0.2), "cm")
    )
  if(axis.y.hide) {
    p <- p + 
      theme(axis.line.y = element_line(colour = NA),
            axis.text.y = element_text(colour = NA),
            axis.ticks.y = element_line(colour = NA)
      )
  }
  return(p)
}

data <- dplyr::filter(df, group %in% c("Season", "Weight", "Age"))
df.range <- range(c(data$semax, data$semin), na.rm = TRUE)

data <- dplyr::filter(df, group == "Season")
p.season <- Barplot_dodge(data = data,  mycols = c("#E64B35"),
                          ylab = "Percentage change", axis.y.hide = FALSE, legend.position = "none")

data <- dplyr::filter(df, group == "Weight") %>%
  mutate(id = factor(id, levels = c("<5", "[5, 6)", "[6, 7)", "[7, 8)", ">8")))
p.weight <- Barplot_dodge(data = data,  mycols = c("#357EBD"),
                         ylab = NULL, axis.y.hide = TRUE, legend.position = "none")

data <- dplyr::filter(df, group == "Age") %>%
  mutate(id = factor(id, levels = c("<20", "[20, 25)", "[25, 30)", ">30")))
p.age <- Barplot_dodge(data = data,  mycols = c("#00A087"), 
                       ylab = NULL, axis.y.hide = TRUE, legend.position = "none")


p <- plot_grid(p.season, p.weight, p.age,
               ncol = 3, nrow = 1, 
               rel_widths = c(1,1.1,1), 
               labels=c("a", "b", "c"),
               align="vh", label_size = 16)
save_plot("hete-barplot.pdf", p, base_width = 14, base_height = 6)


data <- dplyr::filter(df, group %in% c("GDP", "Dialect", "Education", "Delivery", "College", "Elderly-maternal"))
df.range <- range(c(data$semax, data$semin), na.rm = TRUE)

data <- dplyr::filter(df, group == "GDP")
p.gdp <- Barplot_dodge(data = data,  mycols = c("#E64B35"), 
                       ylab = "Percentage change", axis.y.hide = FALSE, legend.position = "none")

data <- dplyr::filter(df, group == "Dialect")
p.dialect <- Barplot_dodge(data = data,  mycols = c("#357EBD"),
                          ylab = NULL, axis.y.hide = TRUE, legend.position = "none")

data <- dplyr::filter(df, group == "Education") 
p.education <- Barplot_dodge(data = data,  mycols = c("#00A087"), text.angle = 20, 
                       ylab = NULL, axis.y.hide = TRUE, legend.position = "none")

data <- dplyr::filter(df, group == "Delivery") 
p.delivery <- Barplot_dodge(data = data,  mycols = c("#9632B8"), text.angle = 20, 
                             ylab = "Percentage change", axis.y.hide = FALSE, legend.position = "none")

data <- dplyr::filter(df, group == "College") 
p.college <- Barplot_dodge(data = data,  mycols = c("#4DBBD5"), text.angle = 20,
                            ylab = NULL, axis.y.hide = TRUE, legend.position = "none")

data <- dplyr::filter(df, group == "Elderly-maternal") 
p.elderly.maternal <- Barplot_dodge(data = data,  mycols = c("#3C5488"), text.angle = 20,
                           ylab = NULL, axis.y.hide = TRUE, legend.position = "none")

p <- plot_grid(p.gdp, p.dialect, p.education, p.delivery, p.college, p.elderly.maternal,
               ncol = 3, nrow = 2, 
               # labels=c("a", "b", "c", "d", "e"),
               # label_size = 16,
               align="vh")
save_plot("hete-barplot_v2.pdf", p, base_width = 10, base_height = 8)

