#Praca domowa z zajêæ 2.03.2015

library(XLConnect)

wb <- loadWorkbook('gospodarstwa.xls')
gosp <- readWorksheet(wb,'gospodarstwa')
vars <- readWorksheet(wb,'opis cech')
vars_labels <- readWorksheet(wb,'opis wariantów cech')

library(dplyr)

gosp <- gosp %>%
  mutate(klm = factor( x = klm,
                       levels = 6:1,
                       labels = c('Wieœ›',
                                  '<20',
                                  '[20,100)',
                                  '[100,200)',
                                  '[200,500)',
                                  '>=500'),
                       ordered = T)) %>%
  mutate(woj = factor(x = woj,
                      labels = c('Dolnoœl¹skie',
                                 'Kujawsko-pomorskie',
                                 'Lubelskie',
                                 'Lubuskie',
                                 '£ódzkie',
                                 'Ma³opolskie',
                                 'Mazowieckie',
                                 'Opolskie',
                                 'Podkarpackie',
                                 'Podlaskie',
                                 'Pomorskie',
                                 'Œl¹skie',
                                 'Œwiêtokrzyskie',
                                 'Warmiñsko-mazurskie',
                                 'Wielkopolskie',
                                 'Zachodniopomorskie'),
                      ordered = T))

library(ggplot2)

wykres <- gosp %>%
  count(klm,woj) %>%
  group_by(woj) %>%
  mutate(procent = n/sum(n)) %>%
  ggplot(data = .,
         aes(x = klm,
             y = procent,
             fill = klm)) +
  geom_bar(stat = 'identity',
           col='black') +
  facet_wrap(~woj) +
  coord_flip() 


library(ggthemes)

wykres + 
  ggtitle('Udzia³ respondentów wed³ug klasy wielkoœci miejscowoœci w poszczególnych województwach') +
  xlab('Klasa wielkoœci miejscowoœci') + 
  ylab('Procent obserwacji') +
  guides(fill=FALSE) +
  theme_hc()