#https://api.covid19india.org/csv/   ~ dataset
easypackages::libraries("dplyr", "ggplot2", "tidyr", "corrplot", "corrr",
                        "magrittr", "e1071","ggplot2","RColorBrewer", "viridis","maps","sf")
options(scipen=999)

nation_lvl <- read.csv('dataset/nation_level_daily.csv')
state_lvl <- read.csv("dataset/state_level_latest.csv")
state_lvl <- state_lvl[-1,]
new <- full_join(state_lvl, geo_state)
geo_state<- read.csv("dataset/geo_state.csv")
str(nation_lvl)

nation_lvl$date <- as.factor(nation_lvl$date)
mnation <- nation_lvl[seq(1, nrow(nation_lvl), 1), ]

ggplot(mnation , aes(x=reorder(date,totalconfirmed), y=totalconfirmed, group =1, fill=date))+
  theme_bw()+
  geom_col()+
  scale_fill_viridis(option="magma",discrete = TRUE)+
  labs(title = "Case through January to May", x="date", y="cases")+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(angle = 90))


#observation on MAP
mymap <- st_read("IND_adm/Indian_States.shp")
st_drivers()
str(mymap)
ggplot(mymap)+
  theme_bw()+
  geom_sf()+
  geom_point( data = new, aes(x=longitude, y=latitude, size=confirmed, color=confirmed), alpha=0.9)+
  scale_color_viridis(option="inferno", trans="log", direction = -1 ) +
  scale_size_continuous(range=c(1,20))

#death,recovery,case rates
ggplot(mnation, aes(x=reorder(date,totalconfirmed),group =1)) + 
  theme_bw()+
  geom_line(aes(y = totalconfirmed),size=2, color = "steelblue") + 
  geom_line(aes(y = totaldeceased), size=2,color="darkred")+
  geom_line(aes(y = totalrecovered), size=2,color="Green")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(x = "date", y = "number of people", title = ' Cornavirus Evolution (India) ')

#gender distribution
patients_data <- read.csv("dataset/patients_data.csv")
patients_data$current_status <- as.factor(patients_data$current_status)
patients_data$gender <- as.factor(patients_data$gender)
patients_data$state <-as.factor(patients_data$state)

ggplot(patients_data, aes(x=gender, fill=current_status))+
  facet_wrap(~ state)+
  theme_bw()+
  geom_bar()+
  labs(title = "Infected demograph based on their Gender", x="gender", y="cases")


#state-wise trend

st_wise <- read.csv("dataset/state_wise_daily.csv")
st_wise$Date <- factor(st_wise$Date, ordered = T)
#st_wise[order(as.Date(st_wise$Date, format="%d/%m/%Y")),] #order by date
st_wise$Status <- as.factor(st_wise$Status)

st_confirmed <-st_wise[st_wise$Status == 'Confirmed',]   #selecting only confirmed
st_recovered <-st_wise[st_wise$Status == 'Recovered',]
st_deceased <- st_wise[st_wise$Status == 'Deceased',]

st_reshaped_cf<-gather(st_confirmed, "id", "value", 3:40)    #changing shape of data
st_reshaped_rc<-gather(st_recovered, "id", "value", 3:40)    #changing shape of data
st_reshaped_dc<-gather(st_deceased, "id", "value", 3:40)    #changing shape of data

#st_reshaped_cf <- within(st_reshaped, 
 #                  Date <- factor(Date, 
  #                                    levels=names(sort(table(Date)))))  

st_reshaped_cf$observation <- 1:nrow(st_reshaped_cf)            #adding an extra ascending coloumn for reordering
st_reshaped_rc$observation <- 1:nrow(st_reshaped_rc)            #adding an extra ascending coloumn for reordering
st_reshaped_dc$observation <- 1:nrow(st_reshaped_dc)            #adding an extra ascending coloumn for reordering

st_reshaped_cf$Recovered <- st_reshaped_rc$value
st_reshaped_cf$Deceased <- st_reshaped_dc$value

st_reshaped_cf <- rename(st_reshaped_cf, Confirmed = value)

p <- ggplot()+
  geom_line(data=st_reshaped_cf, aes(x=reorder(Date,observation),y= Confirmed, group=1),color ='black')+
  geom_line(data=st_reshaped_cf, aes(x=reorder(Date,observation),y= Recovered, group=1),color = 'blue')+ 
  geom_line(data=st_reshaped_cf, aes(x=reorder(Date,observation),y= Deceased, group=1),color = 'red')+
  facet_grid(id ~ .)+
  theme_bw()+
  labs(x = "Date", y = "Cases", title = ' Outcome of Daily Cases (Statewise) ')+
  theme(axis.text.x = element_text(angle = 90))

nc <- length(unique(ggplot_build(p)[["data"]][[1]][["PANEL"]]))

ggsave("tall3.pdf", p, width=10, height = nc * 7 + 2, limitsize = FALSE)
  


