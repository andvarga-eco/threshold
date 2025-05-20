library(ggplot2)
library(readxl)

forest <-read_excel(".../forest.xlsx")
forest$sal2<-factor(forest$sal,levels=c("0-5","6-18","19-30",">30"))

s3<-ggplot(forest,aes(x=cpue,y=sal2))+geom_point()+
  geom_errorbar(aes(xmin=ll,xmax=ul))+facet_wrap(vars(arte))+
  xlab("Catch per unit effort (log scale)")+ylab("Salinity (g/kg)")
s3

ggsave('forestplot.png',s3, dpi = 300, height = 5, width = 5, unit = 'in')
