
############################################################################################################
############################################################################################################
# Figure: Prediction accuracy for clumping/pruning polygenic scores by P-value threshold and ancestry.
# Polygenic score predict usual walking pace.
############################################################################################################
############################################################################################################

library(readr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)


figure.df <- read_excel("Figure1_prune_clump_plot.xlsx")
figure.df$Thresh <- as.factor(figure.df$Thresh)
figure.df$Cat <- factor(figure.df$Cat, labels = c("Pruning", "Clumping"), level = c(0,1))
figure.df$Target <- as.factor(figure.df$Target)
figure.df$new.target <- as.factor(figure.df$new.target)


x.lab.names <-  c(substitute(paste("5x", 10^{-8})),substitute(paste("1x", 10^{-6})),substitute(paste("1x", 10^{-4})),substitute(paste("1x", 10^{-3})),substitute(paste("1x", 10^{-2})),substitute(paste("1x", 10^{-1})),substitute(paste("5x", 10^{-1})), "1")




p1 <-   ggplot(data = figure.df, aes(x = Thresh, y = R2_obs, ymin = R2_obs.95.low, ymax = R2_obs.95.high, color =  new.target)) +
  theme_classic() +
  geom_point( position=position_dodge2(width = 0.45, padding = 0.0, ))+
  geom_pointrange( position=position_dodge2(width = 0.45, padding = 0.0, )) +
  theme(legend.background = element_rect(size=0.5, linetype="solid", color = "black"))   +
  theme(strip.text = element_text(size = 12)  ,
        axis.title.y=element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x=element_blank(),
        axis.text.x= element_text(angle = 90, vjust = 0.5, hjust=1,size = 11),
        legend.text=element_text(size=12),
        legend.title=element_text(size=8),
        legend.position = "right",
        legend.background = element_rect(color = "lightgrey"),
        legend.key.width =  unit(0.05, "cm"),
        legend.key.height = unit(0.3, "cm")) +
  scale_color_manual(name = NULL, values = c("#969696", "#fc8d59" ,"#3690c0", "#238b45")  , labels = c("EUR", "SAS", "EAS", "AFR"))+
  scale_y_continuous(name = substitute(paste("Prediction accuracy ( ", R^{2}, ")")))+
  scale_x_discrete(labels=x.lab.names)+
  facet_wrap(~ Cat)

# extract legend

leg1 <- gtable_filter(ggplot_gtable(ggplot_build(p1)), "guide-box") 

plot1 <- ggplot(data = figure.df, aes(x = Thresh, y = R2_obs, ymin = R2_obs.95.low, ymax = R2_obs.95.high, color =  new.target)) +
  theme_classic() +
  geom_point( position=position_dodge2(width = 0.45, padding = 0.0, ))+
  geom_pointrange( position=position_dodge2(width = 0.45, padding = 0.0, )) +
  theme(legend.background = element_rect(size=0.5, linetype="solid", color = "black"))   +
  theme(strip.text = element_text(size = 12)  ,
        axis.title.y=element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x=element_blank(),
        axis.text.x= element_text(angle = 90, vjust = 0.5, hjust=1,size = 11),
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        legend.position = "null",
        legend.background = element_rect(color = "lightgrey"),
        legend.key.width =  unit(0.05, "cm"),
        legend.key.height = unit(0.3, "cm")) +
  scale_color_manual(name = NULL, values = c("#969696", "#fc8d59" ,"#3690c0", "#238b45")  , labels = c("EUR", "SAS", "EAS", "AFR"))+
  scale_y_continuous(name = substitute(paste("Prediction accuracy ( ", R^{2}, ")")))+
  scale_x_discrete(labels=x.lab.names)+
  facet_wrap(~ Cat)


# manipulate position of legend using annotation_custom

plotNew <- plot1 + 
  annotation_custom(grob = leg1, xmin = 1.2, xmax = 2, ymin = 0.012, ymax = 0.016)

plotPGS <- grid.draw(plotNew)

pdf("Polygenic_score_figure.pdf", width = 6, height = 4.0)
plotPGS
dev.off()
