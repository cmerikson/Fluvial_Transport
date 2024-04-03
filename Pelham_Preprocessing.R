library(data.table)
library(openxlsx)
library(sf)
library(ggplot2)
library(patchwork)
library(lwgeom)

output = "C:\\Users\\cmeri\\OneDrive - Dartmouth College\\Research\\Lab\\Pelham\\Passive_Tracers\\R\\Shiny_Data"

#### Pre-processing ####
# Original
Original = setDT(read.xlsx(r"(C:\Users\cmeri\OneDrive - Dartmouth College\Research\Lab\Pelham\Passive_Tracers\Locations\Frank Originals\Pelham_clast_size_and_deploy_dist_centerline_4Christian.xlsx)"))
setnames(Original,'ID','Comment')
Original = Original[,Comment:=gsub('-','',Comment)]
Original = Original[,Comment:=ifelse(substring(Comment,1,1)=='0',substring(Comment,2),Comment)]

Original_sf = st_as_sf(na.omit(Original),coords = c('Longitude','Latitude'),crs=4326)
Original_sf = st_transform(Original_sf,32618)
centerline = read_sf(r"(C:\Users\cmeri\OneDrive - Dartmouth College\Research\Lab\Pelham\Passive_Tracers\R\Shiny_Data\2019_Pehlam_Centerline_Revised.shp)")
centerline = st_transform(centerline,32618)

snap_point_to_line <- function(point, line) {
  nearest_points <- st_nearest_points(point, line)
  if (length(nearest_points) > 1) { 
    length_test = function(points) {
      line_lengths = st_length(points)
    }
    shortest = which.min((lapply(nearest_points, length_test)))
    nearest_points = nearest_points[shortest]
  }
  nearest_points = st_cast(nearest_points,'POINT')[seq(2, 2, 2)]
  nearest_points_coordinates = st_coordinates(nearest_points)
  return(nearest_points_coordinates)
}

snapped_points <- lapply(Original_sf$geometry, snap_point_to_line, line = st_zm(centerline))
snapped_points = as.data.frame(do.call(rbind,snapped_points))

snapped_points = data.table()
for (i in seq_along(Original_sf$geometry)) {
  point = try({
    snap_point_to_line(Original_sf[i,'geometry'], line = st_zm(centerline))
  }, silent = TRUE)
  snapped_points = rbind(snapped_points,point)
}

ggplot()+
  geom_sf(data=st_zm(centerline),color='cornflowerblue')+
  theme_bw()

locate_deployment = function(distance,centerline){
  line_sf = st_zm(centerline)
  
  if(nrow(line_sf)>1) {
    line_sf = st_line_merge(line_sf)
  }
  line_length <- st_length(line_sf)
  if(as.numeric(line_length) > distance) {
    
    fraction_of_length <- distance / as.numeric(line_length)
    
    # Calculate the line segment ending at 60 meters from the start
    line_segment <- st_linesubstring(line_sf, 0, fraction_of_length)
    
    # The end point of this line segment is the point you're looking for
    target_point <- st_point(na.omit(st_coordinates(st_endpoint(line_segment))))

  } else {
    print("The line is shorter than the requested distance.")
  }
}
  
print(locate_deployment(649.6434743,centerline = centerline))
st_transform(st_sfc(st_point(c(707838.4, 4694887)),crs=32618),4326)

# November 2016
Nov2016 = fread(r"(C:\Users\cmeri\OneDrive - Dartmouth College\Research\Lab\Pelham\Passive_Tracers\Locations\Frank Originals\PEL_Nov172016_RRsurvey_SN.csv)")
setnames(Nov2016,'ID','Comment')
setnames(Nov2016,'Survey_Nov2016_USDS','Coordinates')
Nov2016 = Nov2016[,Comment:=gsub('-','',Comment)]
Nov2016 = Nov2016[,c('Latitude','Longitude'):=.(strsplit(Coordinates,',')[[1]][2],strsplit(Coordinates,',')[[1]][1]),by='Comment']
Nov2016 = Nov2016[,Longitude:=gsub('\\(','',Longitude)]
Nov2016 = Nov2016[1:95]
sf_Nov2016 = st_as_sf(Nov2016,coords = c('Longitude','Latitude'))
sf_Nov2016 = st_set_crs(sf_Nov2016,4326)
sf_Nov2016 = st_transform(sf_Nov2016,crs=32618)
Nov2016 = Nov2016[,c('Easting','Northing'):=.(st_coordinates(sf_Nov2016)[,1],st_coordinates(sf_Nov2016)[,2])]
Nov2016 = Nov2016[,GPSTime:='11/17/2016']
Nov2016 = Nov2016[,Comment:=ifelse(substring(Comment,1,1)=='0',substring(Comment,2),Comment)]
write.csv(Nov2016,paste0(output,'\\Pelham_Nov_2016.csv'))

# July 2017
Jul2017 = fread(r"(C:\Users\cmeri\OneDrive - Dartmouth College\Research\Lab\Pelham\Passive_Tracers\Locations\Frank Originals\July2017_GPSRFID_SN.csv)")
setnames(Jul2017,'tag_short','Comment')
Jul2017 = Jul2017[,Comment:=gsub('-','',Comment)]
Jul2017 = Jul2017[,GPSTime:='07/00/2017']
Jul2017 = Jul2017[,Comment:=ifelse(substring(Comment,1,1)=='0',substring(Comment,2),Comment)]
write.csv(Jul2017,paste0(output,'\\Pelham_Jul_2017.csv'))

# May 2018
May2018 = fread(r"(C:\Users\cmeri\OneDrive - Dartmouth College\Research\Lab\Pelham\Passive_Tracers\Locations\Frank Originals\GPSRFID_output180520.csv)")
setnames(May2018,'tag_short','Comment')
May2018 = May2018[,Comment:=gsub('-','',Comment)]
May2018 = May2018[,GPSTime:='05/20/2017']
sf_May2018 = st_as_sf(May2018,coords = c('Long','Lat'))
sf_May2018 = st_set_crs(sf_May2018,4326)
sf_May2018 = st_transform(sf_May2018,crs=32618)
May2018 = May2018[,c('Easting','Northing'):=.(st_coordinates(sf_May2018)[,1],st_coordinates(sf_May2018)[,2])]
May2018 = May2018[,Comment:=ifelse(substring(Comment,1,1)=='0',substring(Comment,2),Comment)]
write.csv(May2018,paste0(output,'\\Pelham_May_2018.csv'))

# June 2018
Jun2018 = fread(r"(C:\Users\cmeri\OneDrive - Dartmouth College\Research\Lab\Pelham\Passive_Tracers\Locations\Frank Originals\GPSRFID_output180622.csv)")
setnames(Jun2018,'tag_short','Comment')
Jun2018 = Jun2018[,Comment:=gsub('-','',Comment)]
Jun2018 = Jun2018[,GPSTime:='06/22/2018']
Jun2018 = Jun2018[,Comment:=ifelse(substring(Comment,1,1)=='0',substring(Comment,2),Comment)]
write.csv(Jun2018,paste0(output,'\\Pelham_Jun_2018.csv'))

# August 13, 2018
Aug2018 = fread(r"(C:\Users\cmeri\OneDrive - Dartmouth College\Research\Lab\Pelham\Passive_Tracers\Locations\Frank Originals\20180813_gps_multi_comb_processed_results.csv)")
setnames(Aug2018,'comment','Comment')
setnames(Aug2018,'V6','Easting')
setnames(Aug2018,'V7','Northing')
Aug2018 = Aug2018[,GPSTime:='08/13/2018']
write.csv(Aug2018,paste0(output,'\\Pelham_Aug_2018.csv'))

#### Post-processing ####
Transport = fread(r"(C:\Users\cmeri\OneDrive - Dartmouth College\Research\Lab\Pelham\Passive_Tracers\R\Pelham_Transport_Table_2016-2021.csv)")
setnames(Transport,'ID','Comment',skip_absent = T)
Transport = Transport[,Comment:=as.character(Comment)]

Analysis = merge(Original,Transport[,c('Comment','TotalDistance')],by='Comment')
Analysis = Analysis[,B_axis:=as.numeric(B_axis)]

#Movement Histogram
ggplot(Analysis)+
  geom_histogram(aes(x=TotalDistance),bins = 20)+
  scale_x_continuous(expand = expansion(c(0.01,0.01)))+
  scale_y_continuous(expand = expansion(c(0,0.1)))+
  labs(x='Total Distance Traveled (m)')+
  theme_bw()

# Size Movement Scatter
Analysis = Analysis[,'Deployment Site':=ifelse(grepl('LWD',DEPLOY_SITE),'LWD',DEPLOY_SITE)]
Analysis = Analysis[,'Deployment Site':=ifelse(grepl('Bar',`Deployment Site`),'Bar',`Deployment Site`)]
ggplot(Analysis)+
  geom_point(aes(x=B_axis,y=TotalDistance,color=`Deployment Site`))+
  scale_y_log10(labels=scales::label_number())+
  scale_color_manual(values = c('Bar'='blue','LWD'='steelblue','Pool Exp. DS'='purple','Res'='navajowhite3','XS 3 US'='red','XS 4 US'='magenta','XS 4.5 US'='darkred','XS 5 DS' ='skyblue','XS 7 US'='darkorange'),
                     labels = c('Bar'='Bar','LWD'='LWD Jam','Pool Exp. DS'='Pool','Res'='Reservoir','XS 3 US'='XS 3 US','XS 4 US'='XS 4 US','XS 4.5 US'='XS 4.5 US','XS 5 DS' ='XS 5 DS','XS 7 US'='XS 7 US'))+
  labs(x='B-axis (mm)',y='Total Distance Traveled (m)',color='Deployment Site')+
  theme_bw()

#Upstream
Upstream = ggplot(Analysis[`Deployment Site`%in%c('XS 3 US','XS 4 US', 'XS 4.5 US','XS 7 US','Res')])+
  geom_point(aes(x=B_axis,y=TotalDistance,color=`Deployment Site`))+
  scale_y_log10(labels=scales::label_number(),limits=c(0.1,1000))+
  scale_color_manual(values = c('Bar'='blue','LWD'='steelblue','Pool Exp. DS'='black','Res'='navajowhite3','XS 3 US'='red','XS 4 US'='magenta','XS 4.5 US'='darkred','XS 5 DS' ='skyblue','XS 7 US'='darkorange'),
                     labels = c('Bar'='Bar','LWD'='LWD Jam','Pool Exp. DS'='Pool','Res'='Reservoir','XS 3 US'='XS 3 US','XS 4 US'='XS 4 US','XS 4.5 US'='XS 4.5 US','XS 5 DS' ='XS 5 DS','XS 7 US'='XS 7 US'))+
  labs(x='B-axis (mm)',y='Total Distance Traveled (m)',color='Upstream',title = 'Upstream')+
  scale_x_continuous(limits = c(25,200))+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))

#Downstream
Downstream = ggplot(Analysis[!(`Deployment Site`%in%c('XS 3 US','XS 4 US', 'XS 4.5 US','XS 7 US','Res'))])+
  geom_point(aes(x=B_axis,y=TotalDistance,color=`Deployment Site`))+
  scale_y_log10(labels=scales::label_number(),limits=c(0.1,1000))+
  scale_color_manual(values = c('Bar'='blue','LWD'='steelblue','Pool Exp. DS'='purple','Res'='navajowhite3','XS 3 US'='red','XS 4 US'='magenta','XS 4.5 US'='darkred','XS 5 DS' ='skyblue','XS 7 US'='darkorange'),
                     labels = c('Bar'='Bar','LWD'='LWD Jam','Pool Exp. DS'='Pool','Res'='Reservoir','XS 3 US'='XS 3 US','XS 4 US'='XS 4 US','XS 4.5 US'='XS 4.5 US','XS 5 DS' ='XS 5 DS','XS 7 US'='XS 7 US'))+
  labs(x='B-axis (mm)',y='Total Distance Traveled (m)',color='Downstream',title='Downstream')+
  scale_x_continuous(limits = c(25,200))+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

FrankPlot = Upstream + Downstream + plot_layout(guides='collect')

# Histograms by Deployment Site
ggplot(Analysis)+
  facet_wrap(vars(`Deployment Site`),labeller = as_labeller(c('Bar'='Bar','LWD'='LWD Jam','Pool Exp. DS'='Pool','Res'='Reservoir','XS 3 US'='XS 3 US','XS 4 US'='XS 4 US','XS 4.5 US'='XS 4.5 US','XS 5 DS' ='XS 5 DS','XS 7 US'='XS 7 US')))+
  geom_histogram(aes(x=TotalDistance),bins = 20)+
  scale_x_continuous(expand = expansion(c(0.01,0.01)))+
  scale_y_continuous(expand = expansion(c(0,0.1)))+
  labs(x='Total Distance Traveled (m)')+
  theme_bw()
  
# Times found
Recovery = Transport[,Recovered:=apply(.SD, 1, function(x) sum(!is.na(x))),.SDcols = -c(1,2,48)]
Analysis = merge(Analysis,Recovery[,c('Comment','Recovered')],by='Comment')
ggplot(Analysis)+
  facet_wrap(vars(`Deployment Site`),labeller = as_labeller(c('Bar'='Bar','LWD'='LWD Jam','Pool Exp. DS'='Pool','Res'='Reservoir','XS 3 US'='XS 3 US','XS 4 US'='XS 4 US','XS 4.5 US'='XS 4.5 US','XS 5 DS' ='XS 5 DS','XS 7 US'='XS 7 US')))+
  geom_histogram(aes(x=Recovered),bins = 6)+
  scale_color_manual(values = c('Bar'='blue','LWD'='steelblue','Pool Exp. DS'='black','Res'='purple','XS 3 US'='forestgreen','XS 4 US'='magenta','XS 4.5 US'='darkred','XS 5 DS' ='skyblue','XS 7 US'='orange'),
                     labels = c('Bar'='Bar','LWD'='LWD Jam','Pool Exp. DS'='Pool','Res'='Reservoir','XS 3 US'='XS 3 US','XS 4 US'='XS 4 US','XS 4.5 US'='XS 4.5 US','XS 5 DS' ='XS 5 DS','XS 7 US'='XS 7 US'))+
  labs(x='Times Recovered',y='count')+
  theme_bw()

# Box and Whisker
UpBox = ggplot(Analysis[`Deployment Site`%in%c('Res','XS 3 US','XS 4 US', 'XS 4.5 US','XS 7 US')])+
  geom_boxplot(aes(x=`Deployment Site`,y=TotalDistance))+
  scale_x_discrete(labels = c('Bar'='Bar','LWD'='LWD Jam','Pool Exp. DS'='Pool','Res'='Reservoir','XS 3 US'='XS 3','XS 4 US'='XS 4','XS 4.5 US'='XS 4.5','XS 5 DS' ='XS 5','XS 7 US'='XS 7'))+
  scale_y_continuous(limits = c(0,700))+
  labs(y='Total Distance (m)',title = 'Upstream')+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5))
  
DownBox = ggplot(Analysis[!(`Deployment Site`%in%c('Res','XS 3 US','XS 4 US', 'XS 4.5 US','XS 7 US'))])+
  geom_boxplot(aes(x=factor(`Deployment Site`,levels = c('Pool Exp. DS','LWD','XS 5 DS','Bar')),y=TotalDistance))+
  scale_x_discrete(labels = c('Bar'='Bar','LWD'='LWD Jam','Pool Exp. DS'='Pool','Res'='Reservoir','XS 3 US'='XS 3','XS 4 US'='XS 4','XS 4.5 US'='XS 4.5','XS 5 DS' ='XS 5','XS 7 US'='XS 7 US'))+
  scale_y_continuous(limits = c(0,700))+
  labs(x='Deployment Site',y='Total Distance (m)',title='Downstream')+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5),axis.title.y = element_blank())

Boxplots = UpBox + DownBox  
