library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(jcolors)
dane <- read.csv(file="CzysteDane.csv")
as_tibble(dane)

nazwyPol <- c("panstwo","rok","smiertelnosc_dzieci","emisja_co2","smierci_w_katastrofach","hdi","gdpPerCap","procent_pracownikow_w_przemysle",
              "przewidywana_dlug_zycia","nowe_przyp_raka_pluc_M","nowe_przyp_raka_pluc_K",
              "sustainable_development_indeks")
colnames(dane) <- nazwyPol
summary(dane$gdpPerCap)

#Pytania badawcze

##1. Wpływ odsetku społeczeństwa zatrudnionego w przemyśle (w grupach państw według PKB na rok 2000) na emisję CO2
#1.1 Czy odsetek społeczeństwa zatrudnionego w przemyśle ma wpływ na emisję CO2 (państwa pogrupowane według PKB)

PKB_grupa1_top <- dane %>%
  filter(gdpPerCap<=5000) %>%
  filter(rok==2000) %>%
  slice_max(gdpPerCap,n=4)
PKB_grupa1_bot <- dane %>%
  filter(gdpPerCap<=5000) %>%
  filter(rok==2000) %>%
  slice_min(gdpPerCap,n=4)

PKB_grupa1_W <- rbind(PKB_grupa1_bot,PKB_grupa1_top)
PKB_grupa1_W$panstwo

PKB_grupa1 <- dane %>%
  filter(panstwo %in% PKB_grupa1_W$panstwo)

PKB_grupa1 %>%
  ggplot() +
  geom_point(mapping = aes(x=procent_pracownikow_w_przemysle,
                           y=emisja_co2,
                           col=panstwo)) +
    theme_bw() +
      scale_color_brewer(palette = "Set1") +
        labs(title = "Odsetek zatrudnionych w przemyśle a emisja CO2",
             subtitle = "Grupa państw o najniższym PKB per capita (<=5000)",
             x = "odsetek zatrudnionych w przemyśle [%]",
             y = "emisja CO2 [tony/osobę]",
             col = "państwo")
###
PKB_grupa2_top <- dane %>%
  filter(gdpPerCap>5000 & gdpPerCap<13000) %>%
  filter(rok==2000) %>%
  slice_max(gdpPerCap,n=4)
PKB_grupa2_bot <- dane %>%
  filter(gdpPerCap>5000 & gdpPerCap<13000) %>%
  filter(rok==2000) %>%
  slice_min(gdpPerCap,n=4)

PKB_grupa2_W <- rbind(PKB_grupa2_bot,PKB_grupa2_top)
PKB_grupa2_W$panstwo

PKB_grupa2 <- dane %>%
  filter(panstwo %in% PKB_grupa2_W$panstwo)

PKB_grupa2 %>%
  ggplot() +
  geom_point(mapping = aes(x=procent_pracownikow_w_przemysle,
                           y=emisja_co2,
                           col=panstwo)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Odsetek zatrudnionych w przemyśle a emisja CO2",
       subtitle = "Grupa państw o średnim PKB per capita (>5000 & <13000)",
       x = "odsetek zatrudnionych w przemyśle [%]",
       y = "emisja CO2 [tony/osobę]",
       col = "państwo")
###
PKB_grupa3_top <- dane %>%
  filter(gdpPerCap>=13000) %>%
  filter(rok==2000) %>%
  slice_max(gdpPerCap,n=4)
PKB_grupa3_bot <- dane %>%
  filter(gdpPerCap>=13000) %>%
  filter(rok==2000) %>%
  slice_min(gdpPerCap,n=4)

PKB_grupa3_W <- rbind(PKB_grupa3_bot,PKB_grupa3_top)
PKB_grupa3_W$panstwo

PKB_grupa3 <- dane %>%
  filter(panstwo %in% PKB_grupa3_W$panstwo)

PKB_grupa3 %>%
  ggplot() +
  geom_point(mapping = aes(x=procent_pracownikow_w_przemysle,
                           y=emisja_co2,
                           col=panstwo)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Odsetek zatrudnionych w przemyśle a emisja CO2",
       subtitle = "Grupa państw o wysokim PKB per capita (>=13000)",
       x = "odsetek zatrudnionych w przemyśle [%]",
       y = "emisja CO2 [tony/osobę]",
       col = "państwo")
as_tibble(PKB_grupa3_bot)

#2. Czy zmieniająca się wysokość emisji dwutlenku węgla wpływa na wzmożenie skutków katastrof ekologicznych (śmierci w katastrofach)

katastrofy_bez_outl <- dane %>%
  filter(smierci_w_katastrofach>100000)
unique(katastrofy_bez_outl$panstwo)

katastrofy_W <- dane %>%
  filter(!panstwo %in% katastrofy_bez_outl$panstwo) 
summary(katastrofy_W)
plot(katastrofy_W$emisja_co2,katastrofy_W$smierci_w_katastrofach)

srednia_smierci <- dane %>%
  group_by(rok) %>%
  summarise_at(vars(smierci_w_katastrofach),list(name=mean))
plot(srednia_smierci)

srednia_smierci_bez_outl <- katastrofy_W %>%
  group_by(rok) %>%
  summarise_at(vars(smierci_w_katastrofach),list(name=mean))
plot(srednia_smierci_bez_outl)

dwutlenek_wegla <- dane %>%
  filter(!panstwo %in% katastrofy_bez_outl$panstwo) %>%
  group_by(rok) %>%
  summarise_at(vars(emisja_co2),list(name=mean))
plot(dwutlenek_wegla)

regresja <- lm(srednia_smierci_bez_outl$name ~ dwutlenek_wegla$name)
plot(dwutlenek_wegla$name, srednia_smierci_bez_outl$name)
lines(dwutlenek_wegla$name, predict(regresja))

pyt2 <- data.frame(srednia_smierci_bez_outl,dwutlenek_wegla$name)
colnames(pyt2) <- c("rok","srednie_smierci","srednia_emisja_CO2")
as_tibble(pyt2)

pyt2 %>%
  ggplot(aes(x = srednia_emisja_CO2,
           y = srednie_smierci)) +
    geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, se=F, col="red") +
        theme_bw() +
          labs(title="Średnia emisja CO2 a średnia liczba śmierci w katastrofach ",
               x = "średnia emisja CO2 [t/os.]",
               y = "średnia liczba śmierci w katastrofach [os.]")

#3. Jak ma się emisja CO2 do przewidywanej długości życia dla państw podzielonych wg. SDI dla roku 2000
#3.1 Czy emisja CO2 ma wpływ na przewidywaną długość życia (dla państw podzielonych wg. SDI)
sdi_grupa1_top <- dane %>% 
  filter(sustainable_development_indeks <= 40) %>%
  filter(rok==2000) %>%
  slice_max(sustainable_development_indeks,n=4)
sdi_grupa1_bot <- dane %>%
  filter(sustainable_development_indeks <= 40) %>%
  filter(rok==2000) %>%
  slice_min(sustainable_development_indeks,n=4)

sdi_grupa1_W <- rbind(sdi_grupa1_bot, sdi_grupa1_top)
sdi_grupa1 <- dane %>%
  filter(panstwo %in% sdi_grupa1_W$panstwo)
as_tibble(sdi_grupa1)

sdi_grupa1 %>%
  ggplot() +
    geom_point(mapping = aes(x=emisja_co2,
                           y=przewidywana_dlug_zycia,
                           col=panstwo)) +
      ylim(40,85) +
        theme_bw() +
          scale_color_brewer(palette = "Set1") +
           labs(title = "Emisja CO2 a przewidywana długość życia",
              subtitle = "Grupa państw o niskim SDI (<=40)",
              x = "emisja CO2 [t/os.]",
              y = "przewidywana długość życia [lata]",
              col = "państwo")

###
sdi_grupa2_top <- dane %>% 
  filter(sustainable_development_indeks > 40 & sustainable_development_indeks < 70) %>%
  filter(rok==2000) %>%
  slice_max(sustainable_development_indeks,n=4)
sdi_grupa2_bot <- dane %>%
  filter(sustainable_development_indeks > 40 & sustainable_development_indeks < 70) %>%
  filter(rok==2000) %>%
  slice_min(sustainable_development_indeks,n=4)

sdi_grupa2_W <- rbind(sdi_grupa2_bot, sdi_grupa2_top)
sdi_grupa2 <- dane %>%
  filter(panstwo %in% sdi_grupa2_W$panstwo)
as_tibble(sdi_grupa2)

sdi_grupa2 %>%
  ggplot() +
  geom_point(mapping = aes(x=emisja_co2,
                           y=przewidywana_dlug_zycia,
                           col=panstwo)) +
  ylim(40,85) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Emisja CO2 a przewidywana długość życia",
       subtitle = "Grupa państw o średnim SDI (>40 & <70)",
       x = "emisja CO2 [t/os.]",
       y = "przewidywana długość życia [lata]",
       col = "państwo")
###
sdi_grupa3_top <- dane %>% 
  filter(sustainable_development_indeks >= 70) %>%
  filter(rok==2000) %>%
  slice_max(sustainable_development_indeks,n=4)
sdi_grupa3_bot <- dane %>%
  filter(sustainable_development_indeks >= 70) %>%
  filter(rok==2000) %>%
  slice_min(sustainable_development_indeks,n=4)

sdi_grupa3_W <- rbind(sdi_grupa3_bot, sdi_grupa3_top)
sdi_grupa3 <- dane %>%
  filter(panstwo %in% sdi_grupa3_W$panstwo)
as_tibble(sdi_grupa3)

sdi_grupa3 %>%
  ggplot() +
  geom_point(mapping = aes(x=emisja_co2,
                           y=przewidywana_dlug_zycia,
                           col=panstwo)) +
  ylim(40,85) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Emisja CO2 a przewidywana długość życia",
       subtitle = "Grupa państw o wysokim SDI (>=70))",
       x = "emisja CO2 [t/os.]",
       y = "przewidywana długość życia [lata]",
       col = "państwo")

#4. Jak wygląda poziom emisji CO2 dla państw, w których HDI i SDI (z roku 2000) skrajnie się różnią
dane2 <- dane %>%
  filter(rok==2000)
plot(dane2$hdi*100,dane2$sustainable_development_indeks,col="red")

rozbiezne_hdi_sdi <- dane %>%
  filter(rok==2000) %>%
  filter(abs(sustainable_development_indeks-hdi*100)>6)

points(rozbiezne_hdi_sdi$hdi*100,rozbiezne_hdi_sdi$sustainable_development_indeks,col="blue")

dane %>%
  ggplot() +
    geom_point(mapping = aes(x = hdi*100,
                             y = sustainable_development_indeks)) +
  filter(dane, abs(sustainable_development_indeks-hdi*100)>6) %>%
    geom_point(mapping = aes(x = hdi*100,
                             y = sustainable_development_indeks),
                             col = "red")

dane2 %>%
  ggplot() +
  geom_point(mapping = aes(x = hdi*100,
                           y = sustainable_development_indeks)) +
  filter(dane2, abs(sustainable_development_indeks-hdi*100)>6) %>%
  geom_point(mapping = aes(x = hdi*100,
                           y = sustainable_development_indeks),
             col = "red")

as_tibble(rozbiezne_hdi_sdi)

rozbiezne_hdi_sdi %>%
  ggplot() +
    geom_point(mapping = aes(x = hdi*100,
               y = sustainable_development_indeks)) +
      labs(title="SDI a HDI dla państw o różnicy w.w. większej niż 6",
           x = "HDI",
           y = "SDI") +
        theme_bw() 

rozbiezne_hdi_sdi %>%
  ggplot() +
    geom_col(mapping = aes(x=panstwo,
                           y=emisja_co2)) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
        labs(title="Emisja CO2 dla państw, w których różnica pomiędzy SDI a HDI jest większa niż 6 pp.",
             x="państwo",
             y="emisja CO2")

rozbiezne_calosc <- dane %>%
  filter(panstwo %in% rozbiezne_hdi_sdi$panstwo)
as_tibble(rozbiezne_calosc)


#5. Czy ilość nowych przypadków zachorowań na raka płuc ma wpływ na indeks SDI

nowe_przypadki_raka <- dane %>%
  mutate(nowe_przypadki_raka = (nowe_przyp_raka_pluc_M+nowe_przyp_raka_pluc_K)/2) %>%
  select(-nowe_przyp_raka_pluc_M,-nowe_przyp_raka_pluc_K)
as_tibble(nowe_przypadki_raka)

plot(nowe_przypadki_raka$sustainable_development_indeks,nowe_przypadki_raka$nowe_przypadki_raka)

nowe_przypadki_raka %>%
  filter(rok==2000) %>%
  ggplot(aes(x=nowe_przypadki_raka,
             y=sustainable_development_indeks)) +
    geom_point() +
      #geom_smooth(method = "lm",formula = y ~ x, se=T) +
        labs(title="SDI a liczba nowych przypadków zachorowania na raka płuc",
             x="nowe przypadki zachorowań na raka płuc [os.]",
             y="SDI")

sdi_grupa1_top <- dane %>% 
  filter(sustainable_development_indeks <= 40) %>%
  filter(rok==2000) %>%
  slice_max(sustainable_development_indeks,n=4)
sdi_grupa1_bot <- dane %>%
  filter(sustainable_development_indeks <= 40) %>%
  filter(rok==2000) %>%
  slice_min(sustainable_development_indeks,n=4)

sdi_grupa1_W <- rbind(sdi_grupa1_bot, sdi_grupa1_top)
rak_grupa1 <- nowe_przypadki_raka %>%
  filter(panstwo %in% sdi_grupa1_W$panstwo)


rak_grupa1 %>%
  ggplot() +
  geom_point(mapping = aes(x=nowe_przypadki_raka,
                           y=sustainable_development_indeks,
                           col=panstwo)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(title="SDI a liczba nowych przypadków zachorowania na raka płuc",
       subtitle = "Grupa państw o najniższym SDI (<=40)",
       x="nowe przypadki zachorowań na raka płuc [os.]",
       y="SDI")
###
sdi_grupa2_top <- dane %>% 
  filter(sustainable_development_indeks > 40 & sustainable_development_indeks < 70) %>%
  filter(rok==2000) %>%
  slice_max(sustainable_development_indeks,n=4)
sdi_grupa2_bot <- dane %>%
  filter(sustainable_development_indeks > 40 & sustainable_development_indeks < 70) %>%
  filter(rok==2000) %>%
  slice_min(sustainable_development_indeks,n=4)

sdi_grupa2_W <- rbind(sdi_grupa2_bot, sdi_grupa2_top)
rak_grupa2 <- nowe_przypadki_raka %>%
  filter(panstwo %in% sdi_grupa2_W$panstwo)

rak_grupa2 %>%
  ggplot() +
  geom_point(mapping = aes(x=nowe_przypadki_raka,
                           y=sustainable_development_indeks,
                           col=panstwo)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(title="SDI a liczba nowych przypadków zachorowania na raka płuc",
       subtitle = "Grupa państw o średnim SDI (>40 & <70)",
       x="nowe przypadki zachorowań na raka płuc [os.]",
       y="SDI")
###
sdi_grupa3_top <- dane %>% 
  filter(sustainable_development_indeks >= 70) %>%
  filter(rok==2000) %>%
  slice_max(sustainable_development_indeks,n=4)
sdi_grupa3_bot <- dane %>%
  filter(sustainable_development_indeks >= 70) %>%
  filter(rok==2000) %>%
  slice_min(sustainable_development_indeks,n=4)

sdi_grupa3_W <- rbind(sdi_grupa3_bot, sdi_grupa3_top)
rak_grupa3 <- nowe_przypadki_raka %>%
  filter(panstwo %in% sdi_grupa3_W$panstwo)

rak_grupa3 %>%
  ggplot() +
  geom_point(mapping = aes(x=nowe_przypadki_raka,
                           y=sustainable_development_indeks,
                           col=panstwo)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(title="SDI a liczba nowych przypadków zachorowania na raka płuc",
       subtitle = "Grupa państw o wysokim SDI (>=70)",
       x="nowe przypadki zachorowań na raka płuc [os.]",
       y="SDI")

#6. Jak ma się emisja CO2 do ilości nowych przypadków zachorowań na raka płuc w grupach według SDI
#6.1 Czy emisja CO2 ma wpływ na ilość nowych przypadków zachorowań na raka płuc (w grupach według SDI)

sdi_grupa1_top <- dane %>% 
  filter(sustainable_development_indeks <= 40) %>%
  filter(rok==2000) %>%
  slice_max(sustainable_development_indeks,n=4)
sdi_grupa1_bot <- dane %>%
  filter(sustainable_development_indeks <= 40) %>%
  filter(rok==2000) %>%
  slice_min(sustainable_development_indeks,n=4)

sdi_grupa1_W <- rbind(sdi_grupa1_bot, sdi_grupa1_top)
sdi_grupa1 <- dane %>%
  filter(panstwo %in% sdi_grupa1_W$panstwo)
as_tibble(sdi_grupa1)

rak_grupa1 %>%
  ggplot() +
  geom_point(mapping = aes(x=emisja_co2,
                           y=nowe_przypadki_raka,
                           col=panstwo)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(title="Emisja CO2 a liczba nowych przypadków zachorowania na raka płuc",
       subtitle = "Grupa państw o niskim SDI (<=40)",
       x="emisja CO2 [t/os.]",
       y="nowe przypadki zachorowań na raka płuc [os.]")
  

###
sdi_grupa2_top <- dane %>% 
  filter(sustainable_development_indeks > 40 & sustainable_development_indeks < 70) %>%
  filter(rok==2000) %>%
  slice_max(sustainable_development_indeks,n=4)
sdi_grupa2_bot <- dane %>%
  filter(sustainable_development_indeks > 40 & sustainable_development_indeks < 70) %>%
  filter(rok==2000) %>%
  slice_min(sustainable_development_indeks,n=4)

sdi_grupa2_W <- rbind(sdi_grupa2_bot, sdi_grupa2_top)
sdi_grupa2 <- dane %>%
  filter(panstwo %in% sdi_grupa2_W$panstwo)
as_tibble(sdi_grupa2)

rak_grupa2 %>%
  ggplot() +
  geom_point(mapping = aes(x=emisja_co2,
                           y=nowe_przypadki_raka,
                           col=panstwo)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(title="Emisja CO2 a liczba nowych przypadków zachorowania na raka płuc",
       subtitle = "Grupa państw o średnim SDI (>40 & <70)",
       x="emisja CO2 [t/os.]",
       y="nowe przypadki zachorowań na raka płuc [os.]")
###
sdi_grupa3_top <- dane %>% 
  filter(sustainable_development_indeks >= 70) %>%
  filter(rok==2000) %>%
  slice_max(sustainable_development_indeks,n=4)
sdi_grupa3_bot <- dane %>%
  filter(sustainable_development_indeks >= 70) %>%
  filter(rok==2000) %>%
  slice_min(sustainable_development_indeks,n=4)

sdi_grupa3_W <- rbind(sdi_grupa3_bot, sdi_grupa3_top)
sdi_grupa3 <- dane %>%
  filter(panstwo %in% sdi_grupa3_W$panstwo)
as_tibble(sdi_grupa3)

rak_grupa3 %>%
  ggplot() +
  geom_point(mapping = aes(x=emisja_co2,
                           y=nowe_przypadki_raka,
                           col=panstwo)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(title="Emisja CO2 a liczba nowych przypadków zachorowania na raka płuc",
       subtitle = "Grupa państw o wysokim SDI (>=70)",
       x="emisja CO2 [t/os.]",
       y="nowe przypadki zachorowań na raka płuc [os.]")

#7. Czy wysoki odsetek zatrudnienia w przemyśle ma związek z wysokością SDI
summary(dane$procent_pracownikow_w_przemysle)
zatrudnienie_w_przemysle_niskie <- dane %>%
  filter(procent_pracownikow_w_przemysle<25)
panstwa_niskie_zatrudnienie <- unique(zatrudnienie_w_przemysle_niskie$panstwo)
panstwa_niskie_zatrudnienie
zatrudnienie_w_przemysle_wysokie <- dane %>%
  filter(!panstwo %in% panstwa_niskie_zatrudnienie)
as_tibble(zatrudnienie_w_przemysle_wysokie)

zatrudnienie_w_przemysle_wysokie %>%
  group_by(panstwo) %>%
  summarise_at(vars(sustainable_development_indeks),list(name=mean)) %>%
  ggplot() +
    geom_histogram(mapping = aes(x=name,
                       y=..density..),
                   fill="white",
                   binwidth = 5,
                   color="black",
                   size=1) +
    geom_density(mapping = aes(x=name),
                 size=1.2,
                 color="red") +
        labs(title="Krzywa gęstości SDI dla państw o wysokim odsetku zatrudnienia w przemyśle",
             x="średnia wysokość SDI",
             y="gęstość") +
          theme_bw()


#8. Związek śmiertelności dzieci na SDI w grupach wg. emisji CO2
#8.1 Czy śmiertelność dzieci ma wpływ na SDI (w grupach według CO2 na rok 2000) 
summary(dane$emisja_co2)
co2_grupa1_top <- dane %>% 
  filter(emisja_co2 <= 2) %>%
  filter(rok==2000) %>%
  slice_max(emisja_co2,n=4)
co2_grupa1_bot <- dane %>%
  filter(emisja_co2 <= 2) %>%
  filter(rok==2000) %>%
  slice_min(emisja_co2,n=4)

co2_grupa1_W <- rbind(co2_grupa1_bot, co2_grupa1_top)
co2_grupa1 <- dane %>%
  filter(panstwo %in% co2_grupa1_W$panstwo)
as_tibble(co2_grupa1)

co2_grupa1 %>%
  ggplot(mapping = aes(x = smiertelnosc_dzieci,
                       y = sustainable_development_indeks,
                       col = panstwo)) +
  geom_point() +
    theme_bw() +
      labs(title = "SDI a wskaźnik śmiertelności dzieci",
          subtitle = "Grupa państw o niskiej emisji CO2 (<=2 t/os.)",
          x = "wskaźnik śmiertelności dzieci",
          y = "SDI",
          col = "państwo")
###
co2_grupa2_top <- dane %>% 
  filter(emisja_co2 > 2 & emisja_co2 < 4) %>%
  filter(rok==2000) %>%
  slice_max(emisja_co2,n=4)
co2_grupa2_bot <- dane %>%
  filter(emisja_co2 > 2 & emisja_co2 < 4) %>%
  filter(rok==2000) %>%
  slice_min(emisja_co2,n=4)

co2_grupa2_W <- rbind(co2_grupa2_bot, co2_grupa2_top)
co2_grupa2 <- dane %>%
  filter(panstwo %in% co2_grupa2_W$panstwo)
as_tibble(co2_grupa2)

co2_grupa2 %>%
  ggplot(mapping = aes(x = smiertelnosc_dzieci,
                       y = sustainable_development_indeks,
                       col = panstwo)) +
  geom_point() +
  theme_bw() +
  labs(title = "SDI a wskaźnik śmiertelności dzieci",
       subtitle = "Grupa państw o średniej emisji CO2 (>2 & <4 t/os.)",
       x = "wskaźnik śmiertelności dzieci",
       y = "SDI",
       col = "państwo")
###
co2_grupa3_top <- dane %>% 
  filter(emisja_co2 >= 4) %>%
  filter(rok==2000) %>%
  slice_max(emisja_co2,n=4)
co2_grupa3_bot <- dane %>%
  filter(emisja_co2 >= 4) %>%
  filter(rok==2000) %>%
  slice_min(emisja_co2,n=4)

co2_grupa3_W <- rbind(co2_grupa3_bot, co2_grupa3_top)
co2_grupa3 <- dane %>%
  filter(panstwo %in% co2_grupa3_W$panstwo)
as_tibble(co2_grupa3)

co2_grupa3 %>%
  ggplot(mapping = aes(x = smiertelnosc_dzieci,
                       y = sustainable_development_indeks,
                       col = panstwo)) +
  geom_point() +
  theme_bw() +
  labs(title = "SDI a wskaźnik śmiertelności dzieci",
       subtitle = "Grupa państw o wysokiej emisji CO2 (>=4 t/os.)",
       x = "wskaźnik śmiertelności dzieci",
       y = "SDI",
       col = "państwo")
## rysunek
dane2rysunek <- dane
plot(dane2rysunek$hdi*100,dane2rysunek$sustainable_development_indeks,col="yellow",xlim=c(20,100))

rozbiezne_hdi_sdi_rysunek <- dane %>%
  filter(abs(sustainable_development_indeks-hdi*100)>5)
points(rozbiezne_hdi_sdi_rysunek$hdi*100,rozbiezne_hdi_sdi_rysunek$sustainable_development_indeks,col="yellow")
##