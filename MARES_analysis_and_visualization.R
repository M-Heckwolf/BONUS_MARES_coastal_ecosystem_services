#load packages:
library(ggplot2)
library(cowplot)
library(dplyr)
library(reshape2)
theme_set(theme_cowplot())

# read raw data table
tab <- read.csv2("MARES_analysis_and_visualization_data.csv", header = T)

# subset data, use included papers only
tab_Inc <- subset(tab, Include.Exclude.Full == "Include")
tab_Inc = droplevels(tab_Inc)



#summarize data columns with many entrys (less common entrys will be summarized in the term "Others")
temp <- row.names(as.data.frame(summary(as.factor(tab_Inc$Species..Genus.), max=12)))
tab_Inc$top.Genus <- ifelse(
  tab_Inc$Species..Genus. %in% temp,
  tab_Inc$Species..Genus.,
  "Other")

temp2b <- row.names(as.data.frame(summary(as.factor(tab_Inc$Pressures2), max=7)))
tab_Inc$top.Pressures2 <- ifelse(
  tab_Inc$Pressures2 %in% temp2b,
  tab_Inc$Pressures2,
  "Other")

temp3 <- row.names(as.data.frame(summary(as.factor(tab_Inc$Specify.Service), max=15)))
tab_Inc$top.specify.ES <- ifelse(
  tab_Inc$Specify.Service %in% temp3,
  tab_Inc$Specify.Service,
  "Other")

tab_Inc$Type.of.Ecosystem.Service = as.factor(tab_Inc$Type.of.Ecosystem.Service)
tab_Inc$short.Type.of.ES[tab_Inc$Type.of.Ecosystem.Service == "Cultural"] = paste("C",as.character(tab_Inc$Specify.Service[tab_Inc$Type.of.Ecosystem.Service == "Cultural"]),sep=" - ")
tab_Inc$short.Type.of.ES[tab_Inc$Type.of.Ecosystem.Service == "Provisioning"] = paste("P",as.character(tab_Inc$Specify.Service[tab_Inc$Type.of.Ecosystem.Service == "Provisioning"]),sep=" - ")
tab_Inc$short.Type.of.ES[tab_Inc$Type.of.Ecosystem.Service == "Regulation & Maintenance"] = paste("R & M",as.character(tab_Inc$Specify.Service[tab_Inc$Type.of.Ecosystem.Service == "Regulation & Maintenance"]),sep=" - ")
tab_Inc$short.Type.of.ES[tab_Inc$Type.of.Ecosystem.Service == "Unspecified"] = tab_Inc$Specify.Service[tab_Inc$Type.of.Ecosystem.Service == "Unspecified"]
tab_Inc$short.Type.of.ES[tab_Inc$Type.of.Ecosystem.Service == "Supporting"] = paste("S",as.character(tab_Inc$Specify.Service[tab_Inc$Type.of.Ecosystem.Service == "Supporting"]),sep=" - ")


temp4 <- row.names(as.data.frame(summary(as.factor(tab_Inc$short.Type.of.ES), max=15)))
tab_Inc$short.Type.of.ES <- as.character(tab_Inc$short.Type.of.ES)
tab_Inc$top.Type.specify.ES <- ifelse(
  tab_Inc$short.Type.of.ES %in% temp4,
  tab_Inc$short.Type.of.ES,
  "Other")

tab_Inc$top.Genus <- as.factor(tab_Inc$top.Genus)
tab_Inc$top.Pressures2 <- as.factor(tab_Inc$top.Pressures2)
tab_Inc$top.specify.ES <- as.factor(tab_Inc$top.specify.ES)
tab_Inc$top.Type.specify.ES <- as.factor(tab_Inc$top.Type.specify.ES)
tab_Inc$top.Pressures3 <- factor(tab_Inc$top.Pressures2, levels=c("None","Toxins","Nutrients", "Salinity", "Acidification", "Multiple stressors", "Other"))





# order factors
tab_Inc <- within(tab_Inc, 
                  Specify.Service <- factor(Specify.Service, 
                                            levels=names(sort(table(Specify.Service), 
                                                              increasing=TRUE))))
tab_Inc <- within(tab_Inc, 
                  Species..Genus. <- factor(Species..Genus., 
                                            levels=names(sort(table(Species..Genus.), 
                                                              increasing=TRUE))))
tab_Inc <- within(tab_Inc, 
                  Habitat <- factor(Habitat, 
                                    levels=names(sort(table(Habitat), 
                                                      decreasing=TRUE))))

tab_Inc <- within(tab_Inc, 
                  short.Type.of.ES <- factor(short.Type.of.ES, 
                                    levels=names(sort(table(short.Type.of.ES), 
                                                      increasing=TRUE))))

tab_Inc <- within(tab_Inc, 
                  Region.according.to.HELCOM <- factor(Region.according.to.HELCOM, 
                                                       levels=names(sort(table(Region.according.to.HELCOM), 
                                                                         increasing=TRUE))))

tab_Inc <- within(tab_Inc, 
                  Pressures <- factor(Pressures, 
                                      levels=names(sort(table(Pressures), 
                                                        increasing=TRUE))))


# reorganize table to plot eco-game matrix scores:
tab_GAME <- melt(tab_Inc, 
                 id.vars = c("top.specify.ES","top.Type.specify.ES","Type.of.Ecosystem.Service"), 
                 measure.vars = c("Economic.Scoring.System",
                                  "Natural.Scoring.System",
                                  "Social.Scoring.System",
                                  "Human.Scoring.System"))
GAME_names <- list(
  'Economic.Scoring.System'="Economic Scoring System",
  'Natural.Scoring.System'="Natural Scoring System",
  'Social.Scoring.System'="Social Scoring System",
  'Human.Scoring.System'="Human Scoring System"
)

GAME_labeller <- function(variable,value){
  return(GAME_names[value])
}



# summarizing ecosystem service records per region (Figure 2):
Region <- c(strsplit(as.character(tab_Inc$Region.according.to.HELCOM), split= '; ', fixed=TRUE))
Reg <- data.frame(x=unlist(Region, recursive = TRUE, use.names = TRUE))
table(Reg$x)



# plotting Figure 3 A and B
tab_Inc$Genus_sep <- "X"
tab_Inc$Genus_sep[tab_Inc$top.Genus=="Other"] <- "Other"

P0 <- ggplot(tab_Inc,aes(x=factor(top.Genus, levels=names(sort(table(top.Genus),increasing=TRUE)))))+
  geom_bar(aes(fill=Habitat)) + xlab(label = "") + 
  facet_grid(Genus_sep~., scales="free", space = "free")+
  geom_text(stat='count', aes(label=..count..), hjust=-0.2) +
  coord_flip()+
  scale_fill_manual(values = c("#543005","#bf812d","#dfc27d"))

Leg <- get_legend(P0)

P1 <- P0 + theme(legend.position='none')

P2 <- ggplot(tab_Inc,aes(x=Start.Year,fill=Habitat))+
  geom_histogram(binwidth = 3)+theme(legend.position='none')+
  scale_fill_manual(values = c("#543005","#bf812d","#dfc27d"))

PG1 <- plot_grid(P2, P1, labels = c('A', 'B'), label_size = 14, ncol=2, rel_widths=c(1, 1.2))
#

# Figure 3C:
P3 <- ggplot(tab_Inc,aes(x=short.Type.of.ES))+
  geom_bar(aes(fill=Habitat))+ 
  facet_grid(Type.of.Ecosystem.Service~.,scales = "free", space = "free")+
  coord_flip()+
  xlab("20 unique ESS")+theme(legend.position='none')+
  geom_text(stat='count', aes(label=..count..), hjust=-0.2)+
  scale_fill_manual(values = c("#543005","#bf812d","#dfc27d"),name = "Habitat")
#

#Figure 3:
plot_grid(PG1,P3, labels = c('','C'), label_size = 14, ncol=1, rel_heights = c(1,1.6))
#




# plotting Figure 4:
ggplot(tab_GAME,aes(x=as.factor(value)))+
  geom_bar(aes(fill=Type.of.Ecosystem.Service))+ 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)+
  facet_grid(.~variable, labeller = GAME_labeller)+
  ylim(0,1600)+
  xlab(label = "Eco-GAME matrix evaluation scores")+
  scale_fill_manual(values = c("#c6dbef","#6baed6","#08519c","black"),name = "Ecosystem Service Category")
#







# plotting Figure 5:
p0=ggplot(tab_Inc[tab_Inc$top.Pressures3!="None",],aes(x=Type.of.Ecosystem.Service))+
  geom_bar(aes(fill=top.Pressures3))+ 
  ylim(0,700)+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)+
  scale_fill_manual(values = c("#67001f","#d6604d","#f4a582","#4d4d4d","#878787","#bababa"),name="Pressures")+
  scale_x_discrete(labels=c("Cultural", "Provisioning", "Regulating","Supporting"),name="Ecosystem Service Category")

leg <- get_legend(p0)

p1 <- p0 + theme(legend.position='none')

p2=ggplot(tab_Inc[tab_Inc$top.Pressures3!="None",],aes(x=Habitat))+
  geom_bar(aes(fill=top.Pressures3))+ 
  ylim(0,700)+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)+
  scale_fill_manual(values = c("#67001f","#d6604d","#f4a582","#4d4d4d","#878787","#bababa"),name="Pressures")+
  scale_x_discrete(labels=c("Macroalgae", "Mussel beds", "Seagrass"), name = "Habitat")+
  theme(legend.position='none')

plot_grid(p1, p2, leg, labels = c('A', 'B'), label_size = 12, ncol=3, rel_widths=c(1.3, 1, 0.5))
#







#10,12 pdf supplement A
ggplot(tab_Inc,aes(x=Pressures))+
  geom_bar(aes(fill=Habitat))+ 
  coord_flip()+
  xlab("Pressures")+
  geom_text(stat='count', aes(label=..count..), hjust=-0.2)+
  scale_fill_manual(values = c("#543005","#bf812d","#dfc27d"),name = "Habitat")

#10,12 pdf supplement B
ggplot(tab_Inc,aes(x=Pressures))+
  geom_bar(aes(fill=Type.of.Ecosystem.Service))+ 
  coord_flip()+
  xlab("Pressures")+
  geom_text(stat='count', aes(label=..count..), hjust=-0.2)+
  scale_fill_manual(values = c("#c6dbef","#6baed6","#08519c","black"),name = "Ecosystem Service Category")+
  theme(legend.position='none')
