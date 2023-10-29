library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(viridis)
library(cowplot)
library(patchwork) 

library(ggpmisc)

filename <- "20230727urca_index2.csv"

filepath <- paste0("C:\\Users\\DELL\\Desktop\\城乡流域\\code\\", filename)
data <- read_csv(filepath)

data$Regions<-
  factor(data$Regions,levels = c("东北","东部","中部","西部"),
         labels = c("Northeast","Eastern","Central","Western"))

colnames(data)[7] <- 'Variance'

font_size <- 10


p1 <- ggplot(data = data, aes(x=urca_w , y=ntl)) +
  geom_point(aes(colour=Regions, shape=Regions, size=Variance), alpha = .8)+
  geom_smooth(method = 'gam', color = "black", fill = "grey", linewidth=1)+
  theme_classic()+
  theme(text = element_text(size = font_size, color = "black"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey",linetype = "dashed", size = 0.5),
        axis.text = element_text(size = font_size, color = "black"))+
  labs(
    x = "Index of URCAs",
    y = "NTL intensity"
  )+
  stat_poly_eq(aes(label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),parse=T,size=3.5,label.x.npc = "right")



p2 <- ggplot(data = data, aes(x=urca_w , y=bulit_h)) +
  geom_point(aes(colour=Regions, shape=Regions, size=Variance), alpha = .8)+
  geom_smooth(method = 'gam', color = "black", fill = "grey", linewidth=1)+
  theme_classic()+
  theme(text = element_text(size = font_size, color = "black"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey",linetype = "dashed", size = 0.5),
        axis.text = element_text(size = font_size, color = "black"))+
  labs(
    x = "Index of URCAs",
    y = "Built-up height (m)"
  )+
  stat_poly_eq(aes(label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),parse=T,size=3.5,label.x.npc = "right")

p3 <- ggplot(data = data, aes(x=urca_w , y=illiteracy)) +
  geom_point(aes(colour=Regions, shape=Regions, size=Variance), alpha = .8)+
  geom_smooth(method = 'gam', color = "black", fill = "grey", linewidth=1)+
  theme_classic()+
  theme(text = element_text(size = font_size, color = "black"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey",linetype = "dashed", size = 0.5),
        axis.text = element_text(size = font_size, color = "black"))+
  labs(
    x = "Index of URCAs",
    y = "Illiteracy rates (%)"
  )+
  stat_poly_eq(aes(label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),parse=T,size=3.5)

p4 <- ggplot(data = data, aes(x=urca_w , y=beds_capital)) +
  geom_point(aes(colour=Regions, shape=Regions, size=Variance), alpha = .8)+
  geom_smooth(method = 'gam', color = "black", fill = "grey", linewidth=1)+
  theme_classic()+
  theme(text = element_text(size = font_size, color = "black"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey",linetype = "dashed", size = 0.5),
        axis.text = element_text(size = font_size, color = "black"))+
  labs(
    x = "Index of URCAs",
    y = "Hospital beds per 10,000 people"
  )+
  stat_poly_eq(aes(label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),parse=T,size=3.5,label.x.npc = "right")

p5 <- ggplot(data = data, aes(x=urca_w , y=budgets)) +
  geom_point(aes(colour=Regions, shape=Regions, size=Variance), alpha = .8)+
  geom_smooth(method = 'gam', color = "black", fill = "grey", linewidth=1)+
  theme_classic()+
  theme(text = element_text(size = font_size, color = "black"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey",linetype = "dashed", size = 0.5),
        axis.text = element_text(size = font_size, color = "black"))+
  labs(
    x = "Index of URCAs",
    y = "General public services (10,000 yuan)"
  )+
  stat_poly_eq(aes(label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),parse=T,size=3.5,label.x.npc = "right")


p6 <- ggplot(data = data, aes(x=urca_w , y=budgets_capital)) +
  geom_point(aes(colour=Regions, shape=Regions, size=Variance), alpha = .8)+
  geom_smooth(method = 'gam', color = "black", fill = "grey", linewidth=1)+
  theme_classic()+
  theme(text = element_text(size = font_size, color = "black"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour = "grey",linetype = "dashed", size = 0.5),
        axis.text = element_text(size = font_size, color = "black"))+
  labs(
    x = "Index of URCAs",
    y = "General public services per capita (10,000 yuan)"
  )+
  stat_poly_eq(aes(label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),parse=T,size=3.5,label.x.npc = "right")

(p1+p2+p3+p4+p5+p6)+
  plot_annotation(tag_levels = 'a') + 
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom',
        legend.title = element_text(size = font_size, face = "bold"),
        legend.text = element_text(size = font_size))

ggsave(
  "C:\\Users\\DELL\\Desktop\\index_of_urcas3.pdf",
  width = 27,
  height = 20,
  units = "cm"
)
