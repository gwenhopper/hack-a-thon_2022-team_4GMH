
library(tidyverse)
library(sf)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(wesanderson)
library(RColorBrewer)
library(rcompanion)
library(DescTools)
basin_sf = st_read("data/SC_Major_River_Basins", "SC_Major_River_Basins")
counties = st_read("data/tl_2016_45_cousub")
imp = read_csv("data/2018303d_final.csv") # SC Impaired Waters List 303d for 2018
names(imp) <- c("prank","note","basin","huc12","county","desc","station","use","cause")
# generating new rows to deal with multiple causes - for Abby
imp_new = imp %>%
  filter(prank != "") %>%
  #separate(cause, c("cause1", "cause2", "cause3","cause4","cause5"))
  separate_rows(cause, prank, cause, convert = TRUE)

counties_new = counties %>%
  mutate(NAME=toupper(NAME)) %>%
  rename(county = NAME) %>%
  separate(county, c("county", "county2"),sep="-")

plot5 <- ggplot()+
  geom_bar(data=imp_new,aes(y=cause,fill=use)) +
  theme_bw()+
  labs(fill="Uses",y="Causes")
ggsave(plot5,filename='figures/uses_causes_barplot.png',device="png",height=10,width=12)


# Looking only at aquatic life bodies
AL_plot <- imp_new %>%
  filter(use=="AL")

plot_basin <-ggplot()+
  geom_bar(data=AL_plot,aes(y=basin,fill=cause))+
  theme_bw() + 
  labs(fill="Cause",y="Basin")
plot_county <-ggplot()+
  geom_bar(data=AL_plot,aes(y=county,fill=cause))+
  theme_bw() + 
  labs(fill="Cause",y="County")

bigplot=ggarrange(plot_basin,plot_county,ncol =2,nrow=1,labels=c("A","B"),common.legend=TRUE)
ggsave(bigplot,filename='figures/AL_Uses_Causes_TEST.png',device="png",height=10,width=12)

# Looking only at fish bodies
# Looking only at aquatic life bodies
FISH_plot <- imp_new %>%
  filter(use=="FISH")

plot_basin <-ggplot()+
  geom_bar(data=FISH_plot,aes(y=basin,fill=cause))+
  theme_bw() + 
  labs(fill="Cause",y="Basin")
plot_basin
plot_county <-ggplot()+
  geom_bar(data=FISH_plot,aes(y=county,fill=cause))+
  theme_bw() + 
  labs(fill="Cause",y="County")
plot_county

#####
# separating into the differnet categories
AL_plot <- imp_new %>%
  filter(use=="AL")
FISH_plot <- imp_new %>%
  filter(use=="FISH")
REC_plot <- imp_new %>%
  filter(use=="REC")
SHELLFISH_plot <- imp_new %>%
  filter(use=="SHELLFISH")




cols <- c("8" = "red", "4" = "blue", "6" = "darkgreen", "10" = "orange")
ggplot(
  AL_plot,
  aes(x=basin,colour = factor(cause), fill = factor(cause))
) +
  geom_bar(stat="identity") +
  scale_colour_manual(
    values = myColors,
    aesthetics = c("colour", "fill")
  )



cols = scale_colour_manual(rainbow(19, rev=TRUE))
myColors <- rainbow(19, rev=TRUE)
names(myColors) <- unique(imp_new$cause)
colScale <- scale_colour_manual(name = "cause",values  = myColors)

p1<-ggplot(data=AL_plot,aes(y=basin,fill=cause))+
  geom_bar()+
  theme_bw() + 
  labs(fill="Cause",y="Basin") +
  #scale_colour_manual(values = myColors)
  cols
p1

p2<-ggplot()+
  geom_bar(data=FISH_plot,aes(y=basin,fill=cause))+
  theme_bw() + 
  labs(fill="Cause",y="Basin")+
  scale_colour_discrete(type=myColors)
  #scale_colour_manual(values = myColors)
p2

p3<-ggplot()+
  geom_bar(data=REC_plot,aes(y=basin,fill=cause))+
  theme_bw() + 
  labs(fill="Cause",y="Basin")+
  colScale

p4<-ggplot()+
  geom_bar(data=SHELLFISH_plot,aes(y=basin,fill=cause))+
  theme_bw() + 
  labs(fill="Cause",y="Basin")+
  colScale

bigplot=ggarrange(p1,p2,p3,p4,ncol =4,nrow=1,labels=c("A","B","C","D"),common.legend=TRUE)
ggsave(bigplot,filename='figures/AL_Uses_Causes_TEST_ALL_uh.png',device="png",height=8,width=14)

# For the purpose of looking at various causes of impairment in the various water bodies, the
# causes were separated out into different rows in order to count each cause once (ie. in 
# Chester county, a lake at Chester Stake Park has Chlorophyll A, hydrogen ions, and turbidity listed as
# causes.)

# Figure x demonstrates biological causes dominate in counties such as Greenville, Spartanburg, and York. 
# Total phosphorous is listed as a cause in water bodies in over half of the listed counties, being a 
# with a particularly high count in CHester and Berkeley counties.

# Figure x depicts the most frequent causes of impairment in each basin in the state of SC.
# Biological causes dominate in the northwest 

p6<-ggplot()+
  geom_bar(data=imp_new,aes(y=use,fill=cause))+
  theme_bw() + 
  labs(fill="Cause",y="Use")

ggsave(p6,filename='figures/uses_causes_barplot.png',device="png",height=8,width=6)


p1<-ggplot()+
  geom_bar(data=imp_new,aes(y=basin,fill=cause))+
  theme_bw() + 
  labs(fill="Cause",y="Basin")

p2<-ggplot()+
  geom_bar(data=imp_new,aes(y=basin,fill=use))+
  theme_bw() + 
  labs(fill="Use",y="Basin")

bigplot=ggarrange(p1,p2,ncol =2,nrow=1,labels=c("A","B"),common.legend=FALSE)
ggsave(bigplot,filename='figures/USES_CAUSES_FIG.png',device="png",height=8,width=14)

# Conducting a chi-square test between use and cause

res <- chisq.test(imp_new$use, imp_new$cause)
res2 <-cramerV(imp_new$use, imp_new$cause)
nominal_associations

PlotCorr(imp_new$use,imp_new$cause)

library(ggcorrplot)
test <-model.frame(formula,data=imp_new,subset=imp_new$use)
model.matrix(~0+., data=imp_new$cause) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

heat_map
