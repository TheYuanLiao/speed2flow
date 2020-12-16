# Title     : Visualize the time history of flow
# Objective : Empirical vs fitted
# Created by: Yuan Liao
# Created on: 2020-11-20

library(dplyr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(latticeExtra)
library(viridisLite)
library(latex2exp)

df_para <- read.csv('results/istanbul_params.csv')
df <- read.csv('dbs/flow_istanbul_m_estimated.csv')
# df <- df %>%
#   mutate(time = ymd_hms(time)) %>%
#   mutate(hour = hour(time))

df_fit <- df[,c("HERE_segID", "time", "speed_gt", "direction", "flow_fit")]
names(df_fit) <- c("HERE_segID", "hour", "speed", "direction", "flow")
df_fit$src <- 'BPR'

df_em <- df[,c("HERE_segID", "time", "speed_gt", "direction", "flow")]
names(df_em) <- c("HERE_segID", "hour", "speed", "direction", "flow")
df_em$src <- 'Sensor'

df <- rbind(df_fit, df_em)

df_day <- df %>%
  group_by(hour, HERE_segID, direction, src) %>%
  summarise(flow.min = min(flow),
            flow.max = max(flow),
            flow.ave = median(flow))

roads <- unique(df_para$HERE_segID)

rdplot <- function(rd) {
  df_day_r <- df_day[df_day$HERE_segID == rd,]
  df_para_r <- df_para[df_para$HERE_segID == rd,]
  df_r <- df[df$HERE_segID == rd, ]

  # line names and optimal parameters
  para0 <- TeX(sprintf("$Direction{ }0:{ }\\alpha = %.2f,{ }\\beta = %.2f,{ }R^2 = %.2f$",
                       df_para_r[df_para_r$direction==0,]$alpha,
                       df_para_r[df_para_r$direction==0,]$beta,
                       df_para_r[df_para_r$direction==0,]$r2
  ))
  para1 <- TeX(sprintf("$Direction{ }1:{ }\\alpha = %.2f,{ }\\beta = %.2f,{ }R^2 = %.2f$",
                       df_para_r[df_para_r$direction==1,]$alpha,
                       df_para_r[df_para_r$direction==1,]$beta,
                       df_para_r[df_para_r$direction==1,]$r2
  ))

  g1 <- ggplot(data = df_day_r) +
    theme_minimal() +
    scale_x_discrete() +
    labs(title = paste('HERE road', rd), x = "Time of day", y = "Flow") +
    annotate("text", x=-Inf, y = Inf, label = para0, vjust=1, hjust=0) +
    annotate("text", x=-Inf, y = Inf, label = para1, vjust=2.5, hjust=0) +
    geom_ribbon(data = df_day_r, aes(x = as.factor(hour),
                  ymin = flow.min,
                  ymax = flow.max,
                  group = interaction(as.factor(direction), src),
                  fill=as.factor(direction)), color=NA, alpha = 0.05) +
    geom_line(aes(x=as.factor(hour),
                  y=flow.ave,
                  group = interaction(as.factor(direction), src),
                  color = as.factor(direction),
                  linetype=src), size=0.7)

  g2 <- ggplot(data = df_r) +
    theme_minimal() +
    labs(x = "Speed", y = "Flow", subtitle = 'Direction') +
    geom_point(aes(x=speed,
                  y=flow,
                  color = src), size=0.3) +
    facet_grid(.~direction)

  G <- ggarrange(g1, g2, ncol = 1, nrow = 2)

  h <- 8
  ggsave(filename = paste0("figures/", "flow_road_", rd, ".png"), plot=G,
         width = h, height = h, unit = "in", dpi = 300)
}
#rd <- roads[1]
#rdplot(rd)
lapply(roads, rdplot)