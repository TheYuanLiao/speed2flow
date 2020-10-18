# Title     : Visualize the time history of flow
# Objective : Ground truth vs. Estimated
# Created by: Yuan Liao
# Created on: 2020-10-18

library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(latticeExtra)
library(viridisLite)
library(latex2exp)

df_para <- read.csv('results/params.csv')
df <- read.csv('dbs/flow2m_estimated.csv')
df <- df %>% mutate(time = ymd_hms(Time))

df_day <- df %>%
  group_by(hour, Here_segmentID) %>%
  summarise(flow.min = min(flow),
            flow.max = max(flow),
            flow.ave = median(flow),
            Flow.min = min(Flow),
            Flow.max = max(Flow),
            Flow.ave = median(Flow))

g1 <- ggplot(data = df_day, aes(x=as.factor(hour), y=flow.ave, group = Here_segmentID, color=Here_segmentID)) +
  theme(legend.position = "none") +
  theme_minimal() +
  ylim(0, 8000) +
  labs(x = "Time of day", y = "Estimated flow") +
  geom_ribbon(aes(x = as.factor(hour), ymin = flow.min, ymax = flow.max, fill=Here_segmentID), color=NA, alpha = 0.1) +
  geom_line(size=0.5)


g2 <- ggplot(data = df_day, aes(x=as.factor(hour), y=Flow.ave, group = Here_segmentID, color=Here_segmentID)) +
  theme_minimal() +
  ylim(0, 8000) +
  labs(x = "Time of day", y = "Flow by sensors") +
  geom_line(size=0.5, alpha=0.3)

g3 <- ggplot(data = df, aes(x=flow, y=Flow, group = Here_segmentID, color=Here_segmentID)) +
  theme(legend.position = "none") +
  theme_minimal() +
  coord_fixed(xlim = c(0, 8000), ylim=c(0, 8000)) +
  labs(x = "Estimated flow", y = "Flow by sensors") +
  geom_point(size=0.5, alpha=0.3) +
  geom_abline(intercept =0 , slope = 1)

g4 <- levelplot(mse ~ alpha * beta, df_para, xlab = TeX("$\\alpha$"), ylab = TeX("$\\beta$"),
                main = "MSE between sensor flow and BPR-based flow",
                pretty = TRUE,
                par.settings = list(axis.line = list(col = "gray"),
                                    strip.background = list(col = 'transparent'),
                                    strip.border = list(col = 'transparent')),
                panel = panel.levelplot.points, cex = 1.2,
                col.regions = viridis(100)
) + layer_(panel.2dsmoother(..., n = 200))

h <- 7
G <- ggarrange(g1, g2, g3,
               labels = c("BPR with calibrated parameters", "Flow sensors", "Sources comparison"),
               ncol = 3, nrow = 1, common.legend = TRUE, legend="right")
G_f <- ggarrange(G, g4, widths = c(4, 1))
ggsave(filename = "figures/flow_time.png", plot=G_f,
       width = 4 * h, height = h, unit = "in", dpi = 300)
