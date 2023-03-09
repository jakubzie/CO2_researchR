library(stringr)
library(dplyr)
library(gapminder)

temp <- list.files(pattern="*.csv")
myfiles <- lapply(temp,read.csv)
#str(myfiles[1])

#sprawdzenie czy panstwa sa takie same 
panstwaTest1 <- data.frame(myfiles[1])[1]
panstwaTest2 <- data.frame(myfiles[2])[1]
panstwaTest3 <- data.frame(myfiles[3])[1]
panstwaTest4 <- data.frame(myfiles[4])[1]
panstwaTest5 <- data.frame(myfiles[5])[1]
panstwaTest6 <- data.frame(myfiles[6])[1]
panstwaTest7 <- data.frame(myfiles[7])[1]
panstwaTest8 <- data.frame(myfiles[8])[1]
panstwaTest9 <- data.frame(myfiles[9])[1]

intersect(panstwaTest9,panstwaTest5) #2 panstwa sie nie pojawiaja, bedzie 162 panstwa docelowo
setdiff(panstwaTest9,panstwaTest5)
wspolnePanstwa <- intersect(panstwaTest9,panstwaTest5) #!!!!
setdiff(wspolnePanstwa,panstwaTest9)

#tworzenie ramki !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#panstwaRamka <- data.frame(myfiles[1])[1]
#panstwa <- panstwaRamka$country

wspolnePanstwa <- wspolnePanstwa$country
powtorzonePanstwa <- rep(wspolnePanstwa,15)
powtorzonePanstwa <- sort(powtorzonePanstwa)
lata <- seq(1990,2018,by=2)
powtorzoneLata <- rep(lata, length(wspolnePanstwa))

nazwy <- c("country","year")
nazwy <- c(nazwy,str_remove(temp,".csv"))
ramka <- data.frame()
ramka <- data.frame(panstwo = powtorzonePanstwa, rok = powtorzoneLata)


#data.frame(dane[1],c(dane[seq(grep("^X1990$",colnames(dane)),grep("^X2018$",colnames(dane)),by=2)]))
#dane <- data.frame(myfiles[1])
#daneWLatach <- data.frame(dane[1],c(dane[seq(grep("^X1990$",colnames(dane)),grep("^X2018$",colnames(dane)),by=2)]))
#print(as_tibble(daneWLatach),n=100)
#daneWLatach <- daneWLatach %>%
#  filter(country %in% wspolnePanstwa) %>% arrange(country) 


for (i in 1:9){
  dane <- data.frame(myfiles[i])
  daneWLatach <- data.frame(dane[1],c(dane[seq(grep("^X1990$",colnames(dane)),grep("^X2018$",colnames(dane)),by=2)]))
  daneWLatach <- daneWLatach %>%
    filter(country %in% wspolnePanstwa) %>%
    arrange(country)
  obserwacja <- c()
  for(j in 1:162){
    for(k in 2:16){
      obserwacja <- c(obserwacja,daneWLatach[k][j,])
    }
  }
  ramka <- data.frame(ramka,obserwacja)
}
nazwyPol <- c("panstwo","rok","emisja_co2","smierci_w_katastrofach","hdi","gdpPerCap","%pracownikow_w_przemysle",
           "przewidywana_dlug_zycia","nowe_przyp_raka_pluc_M","nowe_przyp_raka_pluc_K",
           "sustainable_development_indeks")
colnames(ramka) <- nazwyPol
ramka <- as_tibble(ramka)
#head(ramka,32)
#str(ramka)
#print(ramka,n=900)

#regresja dla niektorych danych !!!!!!!!!!!!!!!!!!!!!!!!!!!!
df <- ramka %>% 
  filter(!is.na(ramka$emisja_co2))
fit <- lm(df$emisja_co2~df$rok,df)
df2 <- ramka %>% 
  mutate(emisja_co2=ifelse(is.na(emisja_co2),predict(fit),emisja_co2))

df <- ramka %>% 
  filter(!is.na(ramka$hdi))
fit <- lm(df$hdi~df$rok,df)
df2 <- df2 %>% 
  mutate(hdi=ifelse(is.na(hdi),predict(fit),hdi))

df <- ramka %>% 
  filter(!is.na(ramka$`%pracownikow_w_przemysle`))
fit <- lm(df$`%pracownikow_w_przemysle`~df$rok,df)
df2 <- df2 %>% 
  mutate(`%pracownikow_w_przemysle`=ifelse(is.na(`%pracownikow_w_przemysle`),predict(fit),`%pracownikow_w_przemysle`))

df <- ramka %>% 
  filter(!is.na(ramka$przewidywana_dlug_zycia))
fit <- lm(df$przewidywana_dlug_zycia~df$rok,df)
df2 <- df2 %>% 
  mutate(przewidywana_dlug_zycia=ifelse(is.na(przewidywana_dlug_zycia),predict(fit),przewidywana_dlug_zycia))

df <- ramka %>% 
  filter(!is.na(ramka$sustainable_development_indeks))
fit <- lm(df$sustainable_development_indeks~df$rok,df)
df2 <- df2 %>% 
  mutate(sustainable_development_indeks=ifelse(is.na(sustainable_development_indeks),predict(fit),sustainable_development_indeks))

print(df2,n=1000)
df2

doWyrzuceniaREG <- c()
for(i in 1:2430){
  for(j in 1:11){
    if(is.na(df2[i,][j])){
      doWyrzuceniaREG <- c(doWyrzuceniaREG,df2[i,]$panstwo)
    }
  }
}
doWyrzuceniaREG <- unique(doWyrzuceniaREG)

ramkaDoPracy <- df2 %>%
  filter(!panstwo %in% doWyrzuceniaREG)

#print(df2,n=1000)
#summary(df2)

# ktore panstwa wyrzucic !!!!!!!!!!!!!!!!!!!!!!!!!
doWyrzucenia <- c()
#ramka[1,]
for(i in 1:2430){
  for(j in 1:11){
    if(is.na(ramka[i,][j])){
      doWyrzucenia <- c(doWyrzucenia,ramka[i,]$panstwo)
    }
  }
}
doWyrzucenia <- unique(doWyrzucenia)
print(ramka,n=1000)

#if(is.na(ramka)){
#  doWyrzucenia <- c(doWyrzucenia,ramka$panstwo)
#}
#which(is.na(ramka))

# zmiana danych z k i m na numeryczne !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ramkaDoPracy <- ramkaDoPracy %>% mutate(smierci_w_katastrofach = case_when(
  is.na(smierci_w_katastrofach) ~ 0,
  TRUE ~ smierci_w_katastrofach
))
which(is.na(ramkaDoPracy),arr.ind = T)
#ramkaDoPracy$smierci_w_katastrofach[1246]

ramkaDoPracy <- ramkaDoPracy %>% 
  mutate(smierci_w_katastrofach = case_when(
    grepl("k",smierci_w_katastrofach) ~ as.numeric(substring(smierci_w_katastrofach,1,nchar(smierci_w_katastrofach)-1))*1000,
    TRUE ~ as.numeric(smierci_w_katastrofach))
  )

ramkaDoPracy <- ramkaDoPracy %>%
  mutate(gdpPerCap = case_when(
    grepl("*k",gdpPerCap,fixed=F) ~ as.numeric(substring(gdpPerCap,1,nchar(gdpPerCap)-1))*1000,
    TRUE ~ as.numeric(gdpPerCap)
))

ramkaDoPracy <- as_tibble(ramkaDoPracy)
str(ramkaDoPracy)
summary(ramkaDoPracy)
print(ramkaDoPracy,n=900)


#ramka %>% mutate(smierci_w_katastrofach = case_when(
#  grepl("*k",smierci_w_katastrofach,fixed=T)==T ~ as.numeric(smierci_w_katastrofach[-1])*1000,
#  grepl("*M",smierci_w_katastrofach,fixed=T)==T ~ as.numeric(smierci_w_katastrofach[-1])*1000000,
#  TRUE ~ as.numeric(smierci_w_katastrofach)
#))

#grep("k",ramka$smierci_w_katastrofach)
#grepl("k",ramka$smierci_w_katastrofach,fixed=F)
#print(ramka,n=475)

#if(grepl("*k",disaster_deaths_no_epidemics,fixed=T)){
#  ramka %>% mutate(disaster_deaths_no_epidemics <- as.numeric(disaster_deaths_no_epidemics[-1])*1000)
#  }else if(grepl("*M",disaster_deaths_no_epidemics,fixed=T)){
#    ramka %>% mutate(disaster_deaths_no_epidemics <- as.numeric(disaster_deaths_no_epidemics[-1])*1000000)}else{
#      ramka %>% mutate(disaster_deaths_no_epidemics <- as.numeric(disaster_deaths_no_epidemics))
#}

#ramka$disaster_deaths_no_epidemics
#ramka$gdpPerCap
#ramka$smierci_w_katastrofach

#grep("^X1990$",colnames(dane)) #zwraca index kolumny o nazwie "X1990"
#grep("^X2018$",colnames(dane))

#dane1 <- data.frame(dane[1],c(dane[seq(grep("^X1990$",colnames(dane)),grep("^X2018$",colnames(dane)),by=2)]))
#dane1 <- data.frame(dane1[1],c(dane1[seq(192,220,by=2)]))

#obserwacje <- c(dane1[2][1,],dane1[3][1,])
#obserwacje <- c()
#head(obserwacje,30)
#head(dane1,30)

#for(i in 1:194){
#  for(j in 2:16){
#    obserwacje <- c(obserwacje,dane1[j][i,])
#  }
#}

#obserwacje
#obserwacje <- c(dane1[seq(2,lenght(colnames(dane1))),])

#lata 1990-2018 == 15 okresów