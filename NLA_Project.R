sites.dat<-read.csv(file.choose())
top.diatoms<-read.csv(file.choose())
bot.diatoms<-read.csv(file.choose())

setwd('Users/carolynduthie/Desktop/NLA_Project')
getwd()

#checking top diatoms for sites in the two ecoregions 
top.diatoms<-read.csv(file.choose())

site.check.top<-(top.diatoms$SITE_ID %in% sites.dat$SITE_ID)
summary(site.check.top)

write.csv(site.check.top, "site_check_top.csv")

#checking bottom diatoms for sites in the two ecoregions 
bot.diatoms<-read.csv(file.choose())

site.check.bot<-(bot.diatoms$SITE_ID %in% sites.dat$SITE_ID)
summary(site.check.bot)
write.csv(site.check.bot, "site_check_bot.csv")


# removing species from top samples with less than 2% relative abundance 
top.diatoms.species<-read.csv(file.choose())

dim(top.diatoms)

top.rel<-as.data.frame(top.diatoms[,2:951])
top.ind<-apply(top.rel, 2, function(x) any(x>0.02))
top.removals<-as.data.frame(top.rel[,!top.ind])
top.keep<-as.data.frame(top.rel[,top.ind])
dim(top.keep)  
list.sp.keep<- as.data.frame(colnames(top.keep))


#removing species from bottom samples with less than 2% relative abundance 
bot.diatoms.species<-read.csv(file.choose())

dim(bot.diatoms)

bot.rel<-as.data.frame(bot.diatoms[,2:1032])
bot.ind<-apply(bot.rel,2,function(x) any(x>0.02))
bot.removals<-as.data.frame(bot.rel[,!bot.ind])
bot.keep<-as.data.frame(bot.rel[,bot.ind])
dim(bot.keep)
list.bot.sp.keep<-as.data.frame(colnames(bot.keep))


write.csv(list.sp.keep, "list_top_sp_keep.csv")
write.csv(list.bot.sp.keep, "list_bot_sp_keep.csv")
####GOT TO THIS POINT


#combining lists of remaining top species and remaining bottom species 
species.check <-(list.sp.keep$colnames(top.keep) %in% list.bot.sp.keep$colnames(bot.keep))
species.check
                
##checking for overlap between all remaining nla species and rimet database
nla.species<-read.csv(file.choose())
rimet.species<-read.csv(file.choose())
new.species<-read.csv(file.choose())

new.diatoms.check<-(new.species$SPECIES %in% rimet.species$Rimet_species)
summary(new.diatoms.check) ##161 matches 
write.csv(new.diatoms.check, "new_diatom_species_checked.csv")

#checking for overlap between edited list of remaining nla species and rimet database
nla.edited.species <-read.csv(file.choose())

nla.edited.species.all.ecoregions<-read.csv(file.choose())

diatoms.more.check<-(nla.edited.species$NLA_Species %in% rimet.species$Rimet_species)
summary(diatoms.more.check) ##152 matches 
write.csv(diatoms.more.check, "diatom_speices_more_checked.csv") 


diatoms.all.check<-(nla.edited.species.all.ecoregions$species %in% rimet.species$Rimet_species)
summary(diatoms.all.check) ##152 matches 
write.csv(diatoms.all.check, "diatom_speices_all_ecoregions_checked.csv") 


#checking list of NLA speices from only UMW and NAP ecoreigons vs. all ecoregions 
diatoms.check.regions<-(nla.edited.species$species%in%nla.edited.species.all.ecoregions$species)
summary(diatoms.check.regions)
write.csv(diatoms.check.regions, "diatom_species_in_regions_checked.csv")


trait.matrix<-read.csv(file.choose())
trait.matrix.true<-subset(trait.matrix, matches=="TRUE", drop=T)

trait.cor<- cor(trait.matrix.true[,3:6], use="complete.obs", method="pearson")


install.packages("ggplot2")
install.packages("plyr")
install.packages("reshape2")
install.packages("vegan")
install.packages("gridExtra")
library("ggplot2")


library(FD)
install.packages("FD")

species.test<-read.csv(file.choose())  
##species by trait matrix (for species present in rimet database)

site.species.test<-read.csv(file.choose())
##top site by species matrix (just for testing)

gowdis(species.test)


functcomp(species.test,site.species.test)
dbFD(species.test,site.species.test)


pctcrops <-read.csv(file.choose())
boxplot(pctcrops,xlab = "ECOREGION", ylab="% CROPS")
cboxplot.matrix(data=pctcropstest)
t.test(pctcrops$UMW,pctcrops$NAP)

pctdevelop<-read.csv(file.choose())
boxplot(pctdevelop,xlab = "ECOREGION", ylab="% DEVELOPPED")
t.test(pctdevelop$UMW,pctdevelop$NAP)


pcthighdevelop<-read.csv(file.choose())
boxplot(pcthighdevelop,xlab = "ECOREGION", ylab="% HIGHLY DEVELOPPED")
t.test(pcthighdevelop$UMW,pcthighdevelop$NAP)

pctwetland<-read.csv(file.choose())
boxplot(pctwetland,xlab = "ECOREGION", ylab="% WETLAND")
t.test(pctwetland$UMW,pctwetland$NAP)

pctagric<-read.csv(file.choose())
boxplot(pctagric,xlab = "ECOREGION", ylab="% AGRICULTURE")
t.test(pctagric$UMW,pctagric$NAP)

pctpasture<-read.csv(file.choose())
boxplot(pctpasture,xlab = "ECOREGION", ylab="% PASTURE")
t.test(pctpasture$UMW,pctpasture$NAP)

ph_data<-read.csv(file.choose())
boxplot(ph_data,xlab = "ECOREGION", ylab="PH")
t.test(ph_data$UMW,ph_data$NAP)

nh4_data<-read.csv(file.choose())
boxplot(nh4_data)

cond_data<-read.csv(file.choose())
cond.data<-as.data.frame(cond_data)
cond.data

boxplot(cond_data,xlab = "ECOREGION", ylab="CONDUCTIVITY")
t.test(cond.data$UMW,cond.data$NAP)
cond_data$UMW

TOC_data<-read.csv(file.choose())
boxplot(TOC_data,xlab = "ECOREGION", ylab="COLOUR")

DOC_data<-read.csv(file.choose())
boxplot(DOC_data,xlab = "ECOREGION", ylab="DOC")
t.test(DOC_data$UMW,DOC_data$NAP)

nitro_data<-read.csv(file.choose())
boxplot(nitro_data,xlab = "ECOREGION", ylab="TOTAL NITROGEN")
t.test(nitro_data$UMW,nitro_data$NAP)

phos_data<-read.csv(file.choose())
boxplot(phos_data,xlab = "ECOREGION", ylab="TOTAL PHOSPHORUS")
t.test(phos_data$UMW,phos_data$NAP)

chla_data<-read.csv(file.choose())
boxplot(chla_data, xlab = "ECOREGION", ylab="CHLOROPHYLL-A")
t.test(chla_data$UMW,chla_data$NAP)

turb_data<-read.csv(file.choose())
boxplot(turb_data)

secchi_data<-read.csv(file.choose())
boxplot(secchi_data,xlab = "ECOREGION", ylab="MEAN SECCHI DEPTH")
t.test(secchi_data$UMW,secchi_data$NAP)

colour_data<-read.csv(file.choose())
boxplot(colour_data, xlab = "ECOREGION", ylab="COLOUR")
colour.boxplot<- ggplot(top.diversity.FULL, aes(x=WSA_ECOREGION, y=colo)) + geom_boxplot()+theme(axis.title.x = element_text(size = rel(2), angle=00))+ theme(axis.title.y = element_text(size = rel(2), angle=90))
t.test(colour_data$UMW,colour_data$NAP)

depth_data<-read.csv(file.choose())
boxplot(depth_data,xlab = "ECOREGION", ylab="MAXIMUM DEPTH (M)")
t.test(depth_data$UMW,depth_data$NAP)

area_data<-read.csv(file.choose())
boxplot(area_data, xlab = "ECOREGION", ylab="LAKE AREA (KM2)")
t.test(area_data$UMW,area_data$NAP)
