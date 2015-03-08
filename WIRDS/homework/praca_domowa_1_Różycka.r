library(XLConnect)
wb <- loadWorkbook('gospodarstwa.xls')
gosp <- readWorksheet(wb,'gospodarstwa')
vars <- readWorksheet(wb,'opis cech')
vars_labels <- readWorksheet(wb,'opis wariantów cech')

library(dplyr)
gosp <- tbl_df(gosp) 

library(ggplot2)
#library(ggthemes)

gosp <- gosp %>%
  mutate(klm = factor( x = klm,
                       levels = 6:1,
                       labels = c('Wieœ',
                                  '<20',
                                  '[20,100)',
                                  '[100,200)',
                                  '[200,500)',
                                  '>=500'),
                       ordered = T))

gosp <- gosp %>%
  mutate(woj = factor( x = woj,
                       levels = c('02','04','06','08',
                                  '10','12','14','16',
                                  '18','20','22','24',
                                  '26','28','30','32'),
                       labels = c('dolnoœl¹skie','kujawsko-pomorskie',
                                  'lubelskie','lubuskie',
                                  '³ódzkie','ma³opolskie',
                                  'mazowieckie','opolskie',
                                  'podkarpackie','podlaskie',
                                  'pomorskie','œl¹skie',
                                  'œwiêtokrzyskie','warmiñsko-mazurskie',
                                  'wielkopolskie','zachodniopomorskie'),
                       ordered = T))

gosp %>%
  count(klm,woj) %>%
  group_by(woj) %>%
  mutate(procent = n/sum(n)) %>%
  ggplot(data = .,
         aes(x = '',
             y = procent,
             group = klm,
             fill = klm)) +
  geom_bar(stat = 'identity',
           col='black') +
  facet_wrap(~woj, ncol=1) + 
  xlab('Województwa') +
  ggtitle('Udzia³ procentowy danych klas miejscowoœci 
          w poszczególnych województwach') +
  coord_flip()

