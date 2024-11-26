library(tidyverse)

by_region_cnv <- read.csv('~/Projects/drc_genomic_surveillance/data/cnv_discordant_read_calls.csv')

cnv_nondrc <- by_region_cnv[c('dup','Zambia', 'Burkina.Faso','Central.African.Republic','Cameroon','Tanzania','Uganda')]
cnv_nondrc
cnv_long_nondrc <- pivot_longer(cnv_nondrc, cols=2:7)
cnv_long_nondrc$gene <- gsub("_.*","",cnv_long_nondrc$dup)
cnv_long_nondrc$location <- cnv_long_nondrc$name
colnames(cnv_long_nondrc) <- c('dup','cohort','freq','gene','region')


#now mangle drc only cnvs
cnv <- fread('~/Projects/drc_ir_ms_2/data/CNV_calls.csv')
locs <- fread('~/Projects/drc_ir_ms_2/data/loc_dict.csv')
genes <- fread('~/Projects/drc_ir_ms_2/data/geneloc.csv')
cnv_long <- pivot_longer(cnv, cols=2:31)
colnames(cnv_long) <- c('dup','cohort_admin1_year','value')
cnv_long <- left_join(cnv_long, locs)
cnv_long <- left_join(cnv_long, genes)
colnames(cnv_long) <- c('dup','cohort','freq','region','gene')
cnv_long <- cnv_long[c('dup','cohort','freq','gene','region')]

#merge
cnv_all <- rbind(cnv_long, cnv_long_nondrc)

#add zeroes
cnv_full <- cnv_all %>%
  complete(dup, cohort, fill = list(freq = 0)) %>%
  left_join(select(cnv_all, dup, cohort, region, gene), by = c("dup", "cohort")) %>%
  distinct()

cnv_full %>% ggplot(aes(y=as.factor(cohort),x=dup, fill=freq))+
  geom_tile()+
  facet_grid(cols=vars(gene),rows=vars(region),scales="free",space="free")+
  theme_minimal()+
  labs(y="Admin1|Year", x='Gene',fill='Frequency')+
  theme(
    axis.ticks.x = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_text(angle=90),
    axis.text.y = element_text(angle=0),
    strip.text.x.top = element_text(size=8,face="bold"), #THE LINE THAT IS EDITED
    #axis.text.y=element_text(size=15,face="bold"),
    strip.text.y = element_text(size=8,face="bold",angle = 0),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.spacing = element_blank(),
    plot.margin = margin(r = 0.05, l = 0.05))
