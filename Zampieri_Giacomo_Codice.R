library(ggplot2)
library(dplyr)
library(corrplot)
library(leaflet)
library(ggpubr)
library(plotly)
library(tidyr)
library(tidyverse)
library(maps)
library(mapproj)

GlobalSpace = read.csv("C:/Users/giaco/Desktop/NuovoProgettoDati/GlobalSpace/GlobalSpaceLaunches.csv")

View(GlobalSpace)

GlobalSpace = GlobalSpace %>%
  select(Company.Name,Detail,Status.Rocket,Status.Mission,Country.of.Launch,Companys.Country.of.Origin,Private.or.State.Run,Year)

View(GlobalSpace)

#### Quali sono gli Stati che hanno effettuato più missioni spaziali? #### 


n_comp = GlobalSpace %>% 
  group_by(Companys.Country.of.Origin) %>%
  summarise(count = n())

View(n_comp)

Stato = n_comp$Companys.Country.of.Origin
NumeroLanci = n_comp$count

dataLanci = data.frame(
  Stato,
  NumeroLanci
)

View(dataLanci)

dataLanci$Stato <- factor(dataLanci$Stato,levels = dataLanci$Stato[order(dataLanci$NumeroLanci)]) #riordino in ordine decrescente il numero di lanci

ggplot(dataLanci, aes(Stato, NumeroLanci, fill=Stato)) +                                    
  geom_bar(stat = "identity") +
  coord_flip()

#### Quali sono le Società che hanno effettuato più missioni? #### 

nLancicomp = GlobalSpace %>%  #conto il numero di lanci per ciascuna compagnia
  group_by(Company.Name,Companys.Country.of.Origin) %>%
  summarise(count = n())

View(nLancicomp)

nLancicomp = nLancicomp %>% filter(count >= 76) #filtro solo le compagnie che hanno effettuato almeno 10 lanci

#Creazione barplot:

Compagnia = nLancicomp$Company.Name
NumeroLanci = nLancicomp$count
StatoCompagnia = nLancicomp$Companys.Country.of.Origin

dataLanci = data.frame(
  Compagnia,
  NumeroLanci,
  StatoCompagnia
)

r = ggplot(data=dataLanci, aes(x=reorder(Compagnia,NumeroLanci), y=NumeroLanci,
                               fill=StatoCompagnia)) +
  geom_bar(stat="identity") + xlab("Compagnie") +
  geom_text(size = 3, position = position_stack(vjust = 0.5),aes(label=NumeroLanci))

r + coord_flip()

#### RAPPRESENTAZIONE SU MAPPA DEL NUMERO LANCI PER STATO #### 

gb_count$Companys.Country.of.Origin[gb_count$Companys.Country.of.Origin == "England"] <- "UK" #Cambio il nome England in UK

worldtb = map_data("world") %>%
  as_tibble()

StateMap = left_join(worldtb,gb_count,by=c("region" = "Companys.Country.of.Origin")) #unisco dataset numero lanci per stato con dataset contenente le coordinate regioni del mondo
View(StateMap)

StateMap %>%
  ggplot(aes(long,lat,group=subregion)) +
  geom_map(
    aes(map_id=region),
    map = StateMap,
    color = "gray80", fill = "grey80", size = 0.9
  ) +
  geom_polygon(aes(group=group,fill=count), color="black") +
  theme_minimal()+
  labs(
    title = "Stati che hanno effettuato missioni spaziali", x="", y="",fill="") +
  theme(
    plot.title = element_text(size=26,face="bold",color="navyblue"),
    legend.position = "right"
  )

#### DA CHE STATI VENGONO LANCIATI I RAZZI? #### v

countryLaunch = select(GlobalSpace,c("Country.of.Launch"))

View(countryLaunch)

launchCount = countryLaunch %>% 
  group_by(Country.of.Launch) %>%
  summarise(count = n())

View(launchCount)

worldtb = map_data("world") %>%
  as_tibble()

launchMap = left_join(worldtb,launchCount,by=c("region" = "Country.of.Launch"))
View(launchMap)

launchMap %>%
  ggplot(aes(long,lat,group=subregion)) +
  geom_map(
    aes(map_id=region),
    map = launchMap,
    color = "gray80", fill = "grey80", size = 0.9
  ) +
  geom_polygon(aes(group=group,fill=count), color="black") +
  scale_fill_gradient2(low="royalblue1",mid="lightskyblue",high="navyblue",midpoint = 270) +
  theme_minimal()+
  labs(
    title = "Countries of launch", x="", y="",fill="") +
  theme(
    plot.title = element_text(size=26,face="bold",color="navyblue"),
    legend.position = "right"
  )


#### NUMERO LANCI PER ANNO #### 

nLaunch = GlobalSpace %>% 
  group_by(Year) %>%
  summarise(count = n())

View(nLaunch)

newDataL = data.frame(
  Year = nLaunch$Year,
  Num = nLaunch$count
)

View(newDataL)

ggplot(newDataL,aes(x = Year, y = Num)) + 
  geom_point() + 
  geom_smooth()

### ANDAMENTO LANCI NEL TEMPO PER STATO ###

GlobalSpace = read.csv("C:/Users/giaco/Desktop/NuovoProgettoDati/GlobalSpace/GlobalSpaceLaunches.csv")

GlobalSpace = GlobalSpace %>% select(Companys.Country.of.Origin,Year)

Countries = GlobalSpace %>%  #conto il numero di lanci per ciascuna compagnia
  group_by(Companys.Country.of.Origin) %>%
  summarise(count = n())

Countries = Countries[-c(1,12),]

country = as.vector(Countries$Companys.Country.of.Origin)

ann = c(1957:2020)

names = vector(mode="character")

j = 1
while(j<=960){
  for(i in 1:15){
    for(k in 1:64){
      names[j] = country[i]
      j = j+1
    }
  }}    

Missioni = vector(mode="numeric")
j = 1

while (j<=960) {
  for(i in 1:15){
    for(k in 1:64){
      Missioni[j] = nrow(GlobalSpace %>% filter(Companys.Country.of.Origin == country[i],Year==ann[k]))
      j=j+1
    }}
}

datasetProva = data.frame(Missioni,names)

Anno = c(1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020)

dataAnn = data.frame(datasetProva,Anno)

dataAnn$names[dataAnn$names== "USA"] <- "United States"

dataAnn$names[dataAnn$names== "England"] <- "United Kingdom"

dataAnn$names[dataAnn$names== "Isreal"] <- "Israel"

dataAnn = subset(dataAnn, Missioni != 0)

library("readxl")
library("scales")
library("curl")
library("png")
library("gifski")
library("gganimate")

dataAnn2 = dataAnn %>%  
  filter(names == "China" | names == "Israel" | names == "United States" | names == "Italy" | names ==     "India" | names == "Japan"  | names == "Germany" | names == "Russia")

G20_plot = ggplot(dataAnn2, aes(x=Anno,y=Missioni,
                                group = names,
                                color = names)) +
  geom_line(size = 1) + ylab("") +
  theme_classic() + ggtitle("Andamento Missioni per Stato") +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme(legend.position = "bottom",legend.box = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

G20plot_anim = G20_plot + 
  geom_point(size = 2) + 
  transition_reveal(as.numeric(Anno))

gif = animate(G20plot_anim,height=538,width=800,end_pause = 50)
gif

#Bar race Chart:

datanew = dataAnn %>%
  select(Missioni,names,Anno) %>%
  group_by(Anno,Missioni,names) %>%
  summarise(total = sum(Missioni)) %>%
  mutate(cumtotal = cumsum(total))

data2 = datanew %>%
  group_by(Anno) %>%
  arrange(Anno, -cumtotal) %>%
  mutate(rank = 1:n())

my_plot = data2 %>%
  ggplot() + 
  aes(xmin = 0,
      xmax = cumtotal) +
  aes(ymin = rank - 0.45,
      ymax = rank + 0.45,
      y = rank) +
  facet_wrap(~Anno) +
  geom_rect(alpha = .7) +
  aes(fill = names) +
  scale_fill_viridis_d(option = "brewer yellow-green-blue",
                       direction = -1) +
  scale_x_continuous(
    limits = c(-50,150), #limiti asse x del grafico
    breaks = c(0,25,50,75,100,125)) +
  geom_text(col = "darkblue", #nomi degli stati
            hjust = "right",
            aes(label = names),
            x = -10) +
  geom_text(col = "darkblue", #numero missioni
            hjust = "right",
            aes(label = paste(cumtotal), x = 12)) +
  scale_y_reverse() +
  labs(fill = NULL) +
  ggtitle("Bar Race Chart del numero di missioni spaziali dal 1957 al 2020") +
  labs(x = "Missioni") + #titolo asse x
  theme_classic()

p = my_plot + facet_null() + 
  geom_text(x =  75  , y = -6, #Indicatore dell'anno
            family = "Times",
            aes(label = as.character(Anno)),
            size = 12, col = "darkorchid") +
  aes(group = names) +
  transition_time(Anno) +
  transition_states(Anno,4,1)


gif = animate(p,nframes=350,fps = 7,width=800)

gif

### ANDAMENTO LANCI NEL TEMPO PER SOCIETÀ ###

GlobalSpace = read.csv("C:/Users/giaco/Desktop/NuovoProgettoDati/GlobalSpace/GlobalSpaceLaunches.csv")
View(GlobalSpace)

GlobalSpace = GlobalSpace %>% select(Company.Name,Year)

Companies = GlobalSpace %>%  #conto il numero di lanci per ciascuna compagnia
  group_by(Company.Name) %>%
  summarise(count = n())

View(Companies)

Companies = Companies %>% filter(count >= 100)

company = as.vector(Companies$Company.Name)

ann = c(1957:2020)

CompanyName = vector(mode="character")

j = 1

while(j<=1088){
  for(i in 1:11){
    for(k in 1:64){
      CompanyName[j] = company[i]
      j = j+1
    }
  }}    

Missioni = vector(mode="numeric")
j = 1

while (j<=1088) {
  for(i in 1:11){
    for(k in 1:64){
      Missioni[j] = nrow(GlobalSpace %>% filter(Company.Name == company[i],Year==ann[k]))
      j=j+1
    }}
}

datasetProva = data.frame(Missioni,CompanyName)
View(datasetProva)

Anno = c(1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020,1957:2020)

dataAnn = data.frame(datasetProva,Anno)
View(dataAnn)

G20_plot = ggplot(dataAnn, aes(x=Anno,y=Missioni,
                               group = CompanyName,
                               color = CompanyName)) +
  geom_line(size = 1) + ylab("") +
  theme_classic() + ggtitle("Andamento Missioni per Compagnia") +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme(legend.position = "bottom",legend.box = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

G20plot_anim = G20_plot + 
  geom_point(size = 2) + 
  transition_reveal(as.numeric(Anno))

gif = animate(G20plot_anim,height=538,width=800,end_pause = 50)
gif

#### QUANTE DI QUESTE MISSIONI SONO STATE UN SUCCESSO E QUANTE UN FALLIMENTO? #### 

statusData = select(GlobalSpace,c("Status.Mission"))
View(statusData)

status = as.vector(statusData$Status.Mission)
status

success = 0
partial = 0
failure = 0

for(i in 1:4324){ #Conto il numero di successi, fallimenti e parziali fallimenti
  
  if(status[i] == "Success"){
    success = success + 1
  } else if(status[i] == "Failure"){
    failure = failure + 1
  } else{
    partial = partial + 1
  }
}


dataStatus2 = data.frame(status = c("Successi","Fallimenti",
                                    "Fallimenti Parziali"), 
                         vals = c(success,failure,partial))

ggplot(dataStatus2, aes(x = status, y = vals, fill= status,
                        label = vals)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c(rgb(1,0,0,0.8),rgb(0,0.314,0.88,0.8),rgb(0,0.741,0.369,0.8))) + 
  labs(y= "Numero Missioni", x = "Status")

#### QUANTE DI QUESTE MISSIONI SONO PRIVATE E QUANTE STATALI? ####

GlobalSpace = read.csv("C:/Users/giaco/Desktop/NuovoProgettoDati/GlobalSpace/GlobalSpaceLaunches.csv")

state = 0
private = 0

run = as.vector(GlobalSpace$Private.or.State.Run)

for(i in 1:4324){ 
  if(run[i] == "S"){
    state = state + 1
  } else{
    private = private + 1
  }
}

data = c(state,private)

barplot(data,names.arg = c("Missioni Statali","Missioni Private"), 
        col = c(rgb(0,0.796,0.859,0.8),rgb(0.902, 0, 0,0.6)))

#### COM'È VARIATO IL NUMERO DI MISSIONI STATALI E PRIVATE NEL TEMPO? ####

g <- ggplot(GlobalSpace, aes(Year))  
# Number of cars in each class:
g + geom_bar(aes(fill=Private.or.State.Run)) + labs(y= "Numero Missioni", x = "Anno") +
  scale_fill_manual(values=c(rgb(0.902, 0, 0,0.6),rgb(0,0.796,0.859,0.8)))



 
