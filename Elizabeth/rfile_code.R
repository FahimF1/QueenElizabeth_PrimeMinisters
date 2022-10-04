library(tidyverse)
library(ggtext)
library(ggimage)
library(sysfonts)
library(showtext)

#import fonts
font_add_google("roboto", "roboto")
font_add_google("open sans", "open sans")
showtext_auto()

#create main dataframe 
df<-data.frame(
  year = 1953:2022,
  age_leo = 27:96,
  gf = c(
    rep("Winston Churchill",4),
    rep("Anthony Eden",2),
    rep("Harold Macmillan",6),
    rep("Alec Douglas",2),
    rep("Harold Wilson",6),
    rep("Edward Heath",4),
    rep("Harold Wilson",2),
    rep("James Callaghan",2),
    rep("Margaret Thatcher",11),
    rep("John Major",6),
    rep("Tony Blair",10),
    rep("Gordon Brown",3),
    rep("David Cameron",6),
    rep("Theresa May",2),
    rep("Boris Johnson",3),
    "Liz Truss"
  ),
  age_gf = c(
    65:68,
    57:58, 
    62:67, 
    60:61,
    48:53,
    53:56, 
    60:61,
    64:65,
    53:63,
    47:52,
    43:52,
    56:58,
    43:48,
    59:60,
    55:57,
    47
  )
)

#data for annotations about max age limit
max_points<-data.frame(x=c(2010, 2015, 2017), y=rep(25,3))

#data for year segments by girlfriend
by_gf<-df|>
  group_by(gf)|>
  summarise(min_year = min(year),
            max_year = max(year))


#color palette for plot
pal_leo<-'#FD7600'
pal_gf <-'#24C4C4'
pal_bg<-'#030623'
pal_annotate<-'#B6B6B6'

#data for images and respective positions on x axis
images<-data.frame(name=c("Queen Elizabeth", unique(df$gf)),
                   pos = seq(from=1952, to=2022, length.out = 16),
                   pal_label<-c(pal_leo, rep(pal_gf,15)))|>
  mutate(path = paste0("images/",str_replace_all(tolower(name)," ","_"),".png"))


#data frame for connector segments from images to years
connectors<-data.frame(
  x= c(2001.5,2004,2005.5,2006.5, 2006.5,2011,2009,2009,2012,2011.5,2013.5,2014,2015,2016.5,2019,2018.5),
  xend = c(2001.5, 2005.5,2005.5,2006.5,2011,2011,2009,2012,2012,2013.5,2013.5,2015,2015,2016.5,2018.5,2018.5),
  y= c(-10,-10,-10,-10,-6,-6,-10,-8,-8,-10,-10,-10,-10,-10,-10,-10),
  yend= c(-4,-10,-4,-6,-6,-4,-8,-8,-4,-10,-4,-10,-4,-4,-10,-4)
)

#create custom title to use with ggtext::element_textbox_simple
title<-'<span style="color:#FD7600;">The QUEEN</span><span style="color:white;"> & </span><span style="color:#24C4C4;font-weight: bold;">HER PRIME MINISTERS </span>'

#plot
ggplot(data=df, aes(x=year))+
  geom_segment(data = data.frame(y = seq(from=0, to=100, by=10)),
               mapping=aes(x=1952, xend=2022, y=y, yend=y), color="white", size=0.1, alpha=0.2)+
  #leos age data
  geom_line(mapping=aes(y=age_leo), color=pal_leo)+
  geom_point(mapping=aes(y=age_leo), shape=21, fill=pal_bg, color=pal_leo, size=4)+
  geom_text(mapping=aes(y=age_leo+1.75, label=age_leo), color=pal_leo)+
  #girlfriend age data
  geom_segment(mapping=aes(x=year, xend=year, y=0, yend=age_gf), color=pal_gf, size=1)+
  geom_text(mapping=aes(y=age_gf+1.5, label=age_gf), color=pal_gf)+
  #adjust scales to allow for pictures
  scale_y_continuous(limits=c(-20,100), breaks=seq(from=0, to=100, by=5))+
  #create new x axis labels
  geom_text(mapping=aes(x=year, label=paste0("'",substr(year,3,4)), y=-1.5), color="white")+
  #max age limit annotations
  
  #custom legend
  geom_segment(data=data.frame(x=rep(2000,2), xend=rep(2002,2), y=c(100,95), color=c(pal_leo,pal_gf), size=c(0.25,1.5)),
               mapping=aes(x=x,xend=xend,y=y, yend=y, color=color, size=size))+
  geom_point(mapping=aes(x=2001, y=100), shape=21, color=pal_leo, fill=pal_bg, size=4)+
  annotate(geom="text", y=95, x=2002.5, label="PM's Age", color=pal_gf, hjust=0)+
  annotate(geom="text", y=100, x=2002.5, label="Queen's Age", color=pal_leo, hjust=0)+
  scale_size_identity()+
  #x axis girlfriend groupings
  geom_segment(data=by_gf, mapping=aes(x=min_year, xend=max_year, y=-4, yend=-4), color=pal_gf)+
  geom_segment(data=by_gf, mapping=aes(x=min_year, xend=min_year, y=-4, yend=-3), color=pal_gf)+
  geom_segment(data=by_gf, mapping=aes(x=max_year, xend=max_year, y=-4, yend=-3), color=pal_gf)+
  #segments to connect images to groupings
  geom_segment(data=images|>filter(name!="Queen Elizabeth"),
               mapping=aes(x=pos, xend=pos, y=-13, yend=-10), color=pal_gf)+
  geom_segment(data=connectors, mapping=aes(x=x,xend=xend,y=y,yend=yend), color=pal_gf)+
  #plot images
  geom_image(data=images, mapping=aes(y=-14, x=pos, image=path), size=0.07)+
  geom_richtext(data=images, 
                mapping=aes(y=-19.5, x=pos, color=pal_label, label=str_replace(name," ","<br>")),
                fill = NA, label.color = NA, hjust=0.4,
                show.legend = FALSE, fontface="bold")+
  scale_color_identity()+
  labs(title=title, x="", y="")+
  theme(
    panel.background = element_rect(fill=pal_bg, color=NA),
    plot.background = element_rect(fill=pal_bg),
    plot.title = element_textbox_simple(size=17, halign=0.5),
    text = element_text(color="white"),
    plot.margin = margin(t=30, l=10, r=10),
    panel.grid = element_blank(),
    axis.text.y=element_text(color="white"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x=element_blank()
  )

#save plot
ggsave("plot/dicaprio-gfs.png", height=8, width=8)