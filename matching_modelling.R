library(dplyr)
library(fuzzyjoin)
library(fastDummies)
library(tree)
library(randomForest)
library(maptree)
library(rpart.plot)
library(ipred)
library(caret)
library(ggplot2)
library(reshape2)
library(scales)
library(car)
library(ggpubr)

## FUZZY MATCHING OF DATASETS ##

reed <- read.csv('reed_data.csv')
gpg <- read.csv('gpg_data.csv')

joined_jw <- stringdist_join(reed, gpg, by='name', mode='left', method='jw', max_dist=99, distance_col='dist') %>% 
  group_by(name.x) %>% 
  slice_min(order_by=dist, n=1)

joined_jw <- joined_jw[order(joined_jw$dist),]

definite_match <- joined_jw[joined_jw$dist<=0.15,]
rows_to_check <- joined_jw$dist > 0.15 & joined_jw$dist < 0.22
data_to_check <- joined_jw[identify_rows,]

rows_checked <- c('Enter the index of rows which are a correct match')
  data_checked <- data_to_check[rows,]
  
df <- rbind(definite_match, data_checked) 
  
df <- subset(df, select=-c(name.x, name.y))
               
               
## EXPLORATORY DATA ANALYSIS ## 
               
colours = c('#3d3e67','#6667AB','#a3a4cd', '#999854')
                           
# Record count by gpg category 
                           
gpg_count <- data.frame(df %>% group_by(gpg_above_average) %>% summarize(count_by_category=n()))
p <- ggplot(data=gpg_count, aes(x=reorder(gpg_above_average, count_by_category), y=count_by_category))
p <- p + geom_bar(stat='identity', color=colours[2], fill=colours[2])
p <- p + xlab('Category')
p <- p + ylab('Count')

ggsave('record_count_by_gpg.jpeg')
                          
# Record count by sector 

sector_count <- data.frame(df %>% group_by(sector) %>% summarize(count_by_sector=n()))
attach(sector_count)
sector_count <- sector_count[order(count_by_sector),]
                           
p <- ggplot(data=sector_count, aes(x=reorder(sector, count_by_sector),  y=count_by_sector))
p <- p + geom_bar(stat='identity', color=colours[2], fill=colours[2])
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1), plot.margin=margin(t=15, r=15, b=15, l=15))
p <- p + xlab('Sector')
p <- p + ylab('Count')
p

ggsave('record_count_by_sector.jpeg')
                           
# Removing rows with sector count less than 20 
                           
rows_remove = subset(sector_count, count_by_sector < 20)
rows_remove = rows_remove$sector
                           
for (i in 1:length(rows_remove)){
  df <- df[!(df$sector==rows_remove[i]),]
}
                           
# Correlation plot 
                           
cor_spearman <- round(cor(df[sapply(df, is.numeric)], method='spearman'), 2)
cor_kendal <- round(cor(df[sapply(df, is.numeric)], method='kendall'), 2)
                           
cor <- cor_spearman
                           
cor[,2] <- cor_kendall[,2]
cor[2,] <- cor_kendall[2,]
                           
cor_melt <- melt(cor)
                           
p <- ggplot(df_cor_melt, aes(Var2, Var1, fill=value))
p <- p + geom_tile()
p <- p +  scale_fill_gradient2(low = colours[4], high = colours[2], mid = 'white', midpoint = 0, limit = c(-1,1), space = 'Lab', name='Spearman\nCorrelation')
p <- p + theme_minimal()
p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))
p <- p + xlab('')
p <- p + ylab('')
ggsave('correlation.jpeg')

# Proportion of orgs with above average gpg by sector 

sector_gpg <- data.frame(df %>% group_by(sector, gpg_above_average) %>% summarize(count_by_sector_gpg=n()))

sector_gpg_below <- subset(sector_gpg, gpg_above_average == 0)
sector_gpg_above <- subset(sector_gpg, gpg_above_average == 1)
colnames(sector_gpg_above)[which(colnames(sector_gpg_above) == 'count_by_sector_gpg')] <- 'count_gpg_above'
colnames(sector_gpg_below)[which(colnames(sector_gpg_below) == 'count_by_sector_gpg')] <- 'count_gpg_below'

sector_gpg <- merge(sector_gpg_below, sector_gpg_above, by='sector')
sector_gpg$prop_above <- sector_gpg$count_gpg_above / (sector_gpg$count_gpg_above + sector_gpg$count_gpg_below)


p <- ggplot(data=sector_gpg, aes(x=reorder(sector, prop_above), y=prop_above)) 
p <- p + geom_bar(stat='identity', color=colours[2], fill=colours[2])
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1), plot.margin=margin(t=15, r=15, b=15, l=15))
p <- p + xlab('Sector')
p <- p + ylab('Proportion')
p

ggsave('prop_above_by_sector.jpeg')

# Proportion of orgs with above average gpg by size 

size_gpg <- data.frame(df %>% group_by(size, gpg_above_average) %>% summarize(count_by_size_gpg=n()))

size_gpg_below <- subset(size_gpg, gpg_above_average == 0)
size_gpg_above <- subset(size_gpg, gpg_above_average == 1)
colnames(size_gpg_above)[which(colnames(size_gpg_above) == 'count_by_size_gpg')] <- 'count_gpg_above'
colnames(size_gpg_below)[which(colnames(size_gpg_below) == 'count_by_size_gpg')] <- 'count_gpg_below'

size_gpg <- merge(size_gpg_below, size_gpg_above, by='size')
size_gpg$prop_above <- size_gpg$count_gpg_above / (size_gpg$count_gpg_above + size_gpg$count_gpg_below)

p <- ggplot(data=size_gpg, aes(x=reorder(size, prop_above), y=prop_above)) 
p <- p + geom_bar(stat='identity', color=colours[2], fill=colours[2])
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1), plot.margin=margin(t=15, r=15, b=15, l=15))
p <- p + xlab('Size')
p <- p + ylab('Proportion')
p

ggsave('prop_above_by_size.jpeg')

# Proportion of orgs with above average gpg by word counts in 'What we do' section

wwd_agentic_gpg <- data.frame(df %>% group_by(wwd_agentic, gpg_above_average) %>% summarize(count_by_wwd_agentic_gpg=n()))

wwd_agentic_gpg_below <- subset(wwd_agentic_gpg, gpg_above_average == 0)
wwd_agentic_gpg_above <- subset(wwd_agentic_gpg, gpg_above_average == 1)
colnames(wwd_agentic_gpg_above)[which(colnames(wwd_agentic_gpg_above) == 'count_by_wwd_agentic_gpg')] <- 'count_gpg_above'
colnames(wwd_agentic_gpg_below)[which(colnames(wwd_agentic_gpg_below) == 'count_by_wwd_agentic_gpg')] <- 'count_gpg_below'

wwd_agentic_gpg <- merge(wwd_agentic_gpg_below, wwd_agentic_gpg_above, by='wwd_agentic')
wwd_agentic_gpg$prop_above <- wwd_agentic_gpg$count_gpg_above / (wwd_agentic_gpg$count_gpg_above + wwd_agentic_gpg$count_gpg_below)

wwd_agentic_gpg$word_category = 'agentic'
wwd_agentic_gpg %>% rename(word_count = wwd_agentic) 

wwd_communal_gpg <- data.frame(df %>% group_by(wwd_communal, gpg_above_average) %>% summarize(count_by_wwd_communal_gpg=n()))

wwd_communal_gpg_below <- subset(wwd_communal_gpg, gpg_above_average == 0)
wwd_communal_gpg_above <- subset(wwd_communal_gpg, gpg_above_average == 1)
colnames(wwd_communal_gpg_above)[which(colnames(wwd_communal_gpg_above) == 'count_by_wwd_communal_gpg')] <- 'count_gpg_above'
colnames(wwd_communal_gpg_below)[which(colnames(wwd_communal_gpg_below) == 'count_by_wwd_communal_gpg')] <- 'count_gpg_below'

wwd_communal_gpg <- merge(wwd_communal_gpg_below, wwd_communal_gpg_above, by='wwd_communal')
wwd_communal_gpg$prop_above <- wwd_communal_gpg$count_gpg_above / (wwd_communal_gpg$count_gpg_above + wwd_communal_gpg$count_gpg_below)

wwd_communal_gpg$word_category = 'communal'
wwd_communal_gpg %>% rename(word_count = wwd_communal)

wwd_inclusive_gpg <- data.frame(df %>% group_by(wwd_inclusive, gpg_above_average) %>% summarize(count_by_wwd_inclusive_gpg=n()))

wwd_inclusive_gpg_below <- subset(wwd_inclusive_gpg, gpg_above_average == 0)
wwd_inclusive_gpg_above <- subset(wwd_inclusive_gpg, gpg_above_average == 1)
colnames(wwd_inclusive_gpg_above)[which(colnames(wwd_inclusive_gpg_above) == 'count_by_wwd_inclusive_gpg')] <- 'count_gpg_above'
colnames(wwd_inclusive_gpg_below)[which(colnames(wwd_inclusive_gpg_below) == 'count_by_wwd_inclusive_gpg')] <- 'count_gpg_below'

wwd_inclusive_gpg <- merge(wwd_inclusive_gpg_below, wwd_inclusive_gpg_above, by='wwd_inclusive')
wwd_inclusive_gpg$prop_above <- wwd_inclusive_gpg$count_gpg_above / (wwd_inclusive_gpg$count_gpg_above + wwd_inclusive_gpg$count_gpg_below)

wwd_inclusive_gpg$word_category = 'inclusive'
wwd_inclusive_gpg %>% rename(word_count = wwd_inclusive)

wwd_gpg <- rbind(wwd_agentic, wwd_communal, wwd_inclusive)

p <- ggplot(data_wwd, aes(x = word_count, y = prop_above_average, colour = word_category)) + 
  geom_point(size = 4, position=position_dodge(width=1)) + 
  geom_linerange(aes(x=word_count, ymin=0, ymax=prop_above_average,), position=position_dodge(width=1), lwd=1.5) + 
  scale_color_manual(values = c(colours[1], colours[2], colours[3])) +
  scale_x_continuous(breaks=seq(0,6,1)) + 
  theme(axis.text=element_text(size=15), axis.title=element_text(size=20), legend.text=element_text(size=15), legend.title=element_text(size=20))

ggsave("/Users/elenaleggett/Documents/Masters/Dissertation/gender_pay_gap/data_scripts/figures/wwd_proportions.jpeg", height=9, width=15.96)
                
# Proportion of orgs with above average gpg by word counts in 'What you'll get section

wyg_agentic_gpg <- data.frame(df %>% group_by(wyg_agentic, gpg_above_average) %>% summarize(count_by_wyg_agentic_gpg=n()))

wyg_agentic_gpg_below <- subset(wyg_agentic_gpg, gpg_above_average == 0)
wyg_agentic_gpg_above <- subset(wyg_agentic_gpg, gpg_above_average == 1)
colnames(wyg_agentic_gpg_above)[which(colnames(wyg_agentic_gpg_above) == 'count_by_wyg_agentic_gpg')] <- 'count_gpg_above'
colnames(wyg_agentic_gpg_below)[which(colnames(wyg_agentic_gpg_below) == 'count_by_wyg_agentic_gpg')] <- 'count_gpg_below'

wyg_agentic_gpg <- merge(wyg_agentic_gpg_below, wyg_agentic_gpg_above, by='wyg_agentic')
wyg_agentic_gpg$prop_above <- wyg_agentic_gpg$count_gpg_above / (wyg_agentic_gpg$count_gpg_above + wyg_agentic_gpg$count_gpg_below)

wyg_agentic_gpg$word_category = 'agentic'
wyg_agentic_gpg %>% rename(word_count = wyg_agentic) 

wyg_communal_gpg <- data.frame(df %>% group_by(wyg_communal, gpg_above_average) %>% summarize(count_by_wyg_communal_gpg=n()))

wyg_communal_gpg_below <- subset(wyg_communal_gpg, gpg_above_average == 0)
wyg_communal_gpg_above <- subset(wyg_communal_gpg, gpg_above_average == 1)
colnames(wyg_communal_gpg_above)[which(colnames(wyg_communal_gpg_above) == 'count_by_wyg_communal_gpg')] <- 'count_gpg_above'
colnames(wyg_communal_gpg_below)[which(colnames(wyg_communal_gpg_below) == 'count_by_wyg_communal_gpg')] <- 'count_gpg_below'

wyg_communal_gpg <- merge(wyg_communal_gpg_below, wyg_communal_gpg_above, by='wyg_communal')
wyg_communal_gpg$prop_above <- wyg_communal_gpg$count_gpg_above / (wyg_communal_gpg$count_gpg_above + wyg_communal_gpg$count_gpg_below)

wyg_communal_gpg$word_category = 'communal'
wyg_communal_gpg %>% rename(word_count = wyg_communal)

wyg_inclusive_gpg <- data.frame(df %>% group_by(wyg_inclusive, gpg_above_average) %>% summarize(count_by_wyg_inclusive_gpg=n()))

wyg_inclusive_gpg_below <- subset(wyg_inclusive_gpg, gpg_above_average == 0)
wyg_inclusive_gpg_above <- subset(wyg_inclusive_gpg, gpg_above_average == 1)
colnames(wyg_inclusive_gpg_above)[which(colnames(wyg_inclusive_gpg_above) == 'count_by_wyg_inclusive_gpg')] <- 'count_gpg_above'
colnames(wyg_inclusive_gpg_below)[which(colnames(wyg_inclusive_gpg_below) == 'count_by_wyg_inclusive_gpg')] <- 'count_gpg_below'

wyg_inclusive_gpg <- merge(wyg_inclusive_gpg_below, wyg_inclusive_gpg_above, by='wyg_inclusive')
wyg_inclusive_gpg$prop_above <- wyg_inclusive_gpg$count_gpg_above / (wyg_inclusive_gpg$count_gpg_above + wyg_inclusive_gpg$count_gpg_below)

wyg_inclusive_gpg$word_category = 'inclusive'
wyg_inclusive_gpg %>% rename(word_count = wyg_inclusive)

wyg_gpg <- rbind(wyg_agentic, wyg_communal, wyg_inclusive)

p <- ggplot(data_wyg, aes(x = word_count, y = prop_above_average, colour = word_category)) + 
  geom_point(size = 4, position=position_dodge(width=1)) + 
  geom_linerange(aes(x=word_count, ymin=0, ymax=prop_above_average,), position=position_dodge(width=1), lwd=1.5) + 
  scale_color_manual(values = c(colours[1], colours[2], colours[3])) +
  scale_x_continuous(breaks=seq(0,6,1)) + 
  theme(axis.text=element_text(size=15), axis.title=element_text(size=20), legend.text=element_text(size=15), legend.title=element_text(size=20))

ggsave("/Users/elenaleggett/Documents/Masters/Dissertation/gender_pay_gap/data_scripts/figures/wyg_proportions.jpeg", height=9, width=15.96)

# Proportion of orgs with above average gpg by word counts in 'Who you'll work with' section

wyww_agentic_gpg <- data.frame(df %>% group_by(wyww_agentic, gpg_above_average) %>% summarize(count_by_wyww_agentic_gpg=n()))

wyww_agentic_gpg_below <- subset(wyww_agentic_gpg, gpg_above_average == 0)
wyww_agentic_gpg_above <- subset(wyww_agentic_gpg, gpg_above_average == 1)
colnames(wyww_agentic_gpg_above)[which(colnames(wyww_agentic_gpg_above) == 'count_by_wyww_agentic_gpg')] <- 'count_gpg_above'
colnames(wyww_agentic_gpg_below)[which(colnames(wyww_agentic_gpg_below) == 'count_by_wyww_agentic_gpg')] <- 'count_gpg_below'

wyww_agentic_gpg <- merge(wyww_agentic_gpg_below, wyww_agentic_gpg_above, by='wyww_agentic')
wyww_agentic_gpg$prop_above <- wyww_agentic_gpg$count_gpg_above / (wyww_agentic_gpg$count_gpg_above + wyww_agentic_gpg$count_gpg_below)

wyww_agentic_gpg$word_category = 'agentic'
wyww_agentic_gpg %>% rename(word_count = wyww_agentic) 

wyww_communal_gpg <- data.frame(df %>% group_by(wyww_communal, gpg_above_average) %>% summarize(count_by_wyww_communal_gpg=n()))

wyww_communal_gpg_below <- subset(wyww_communal_gpg, gpg_above_average == 0)
wyww_communal_gpg_above <- subset(wyww_communal_gpg, gpg_above_average == 1)
colnames(wyww_communal_gpg_above)[which(colnames(wyww_communal_gpg_above) == 'count_by_wyww_communal_gpg')] <- 'count_gpg_above'
colnames(wyww_communal_gpg_below)[which(colnames(wyww_communal_gpg_below) == 'count_by_wyww_communal_gpg')] <- 'count_gpg_below'

wyww_communal_gpg <- merge(wyww_communal_gpg_below, wyww_communal_gpg_above, by='wyww_communal')
wyww_communal_gpg$prop_above <- wyww_communal_gpg$count_gpg_above / (wyww_communal_gpg$count_gpg_above + wyww_communal_gpg$count_gpg_below)

wyww_communal_gpg$word_category = 'communal'
wyww_communal_gpg %>% rename(word_count = wyww_communal)

wyww_inclusive_gpg <- data.frame(df %>% group_by(wyww_inclusive, gpg_above_average) %>% summarize(count_by_wyww_inclusive_gpg=n()))

wyww_inclusive_gpg_below <- subset(wyww_inclusive_gpg, gpg_above_average == 0)
wyww_inclusive_gpg_above <- subset(wyww_inclusive_gpg, gpg_above_average == 1)
colnames(wyww_inclusive_gpg_above)[which(colnames(wyww_inclusive_gpg_above) == 'count_by_wyww_inclusive_gpg')] <- 'count_gpg_above'
colnames(wyww_inclusive_gpg_below)[which(colnames(wyww_inclusive_gpg_below) == 'count_by_wyww_inclusive_gpg')] <- 'count_gpg_below'

wyww_inclusive_gpg <- merge(wyww_inclusive_gpg_below, wyww_inclusive_gpg_above, by='wyww_inclusive')
wyww_inclusive_gpg$prop_above <- wyww_inclusive_gpg$count_gpg_above / (wyww_inclusive_gpg$count_gpg_above + wyww_inclusive_gpg$count_gpg_below)

wyww_inclusive_gpg$word_category = 'inclusive'
wyww_inclusive_gpg %>% rename(word_count = wyww_inclusive)

wyww_gpg <- rbind(wyww_agentic, wyww_communal, wyww_inclusive)

p <- ggplot(data_wyww, aes(x = word_count, y = prop_above_average, colour = word_category)) + 
  geom_point(size = 4, position=position_dodge(width=1)) + 
  geom_linerange(aes(x=word_count, ymin=0, ymax=prop_above_average,), position=position_dodge(width=1), lwd=1.5) + 
  scale_color_manual(values = c(colours[1], colours[2], colours[3])) +
  scale_x_continuous(breaks=seq(0,6,1)) + 
  theme(axis.text=element_text(size=15), axis.title=element_text(size=20), legend.text=element_text(size=15), legend.title=element_text(size=20))

ggsave("/Users/elenaleggett/Documents/Masters/Dissertation/gender_pay_gap/data_scripts/figures/wyww_proportions.jpeg", height=9, width=15.96)
           
# Part time proportion histograms grouped by above or below average

df_pt_prop_above <- df %>% filter(gpg_above_average == "Above")
df_pt_prop_below <- df %>% filter(gpg_above_average == "Below")

p_above <- ggplot(df_above, aes(x=pt_prop)) +
  geom_histogram(color=colours[2], fill=colours[2], bins=60) +
  xlim(0, 600) +
  ylim(0, 50) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=20), axis.title.y=element_blank(), axis.title.x=element_blank(), plot.title=element_text(face="bold", size=20, hjust=0.5)) +
  geom_vline(xintercept=mean(df_above$pt_prop), color=colours[4], linetype='dashed', size=1) +
  annotate("text", x=95, y=50, label="Mean = 13.13", col=colours[4], size=5) +
  ggtitle("Above average")

p_below <- ggplot(df_below, aes(x=pt_prop)) +
  geom_histogram(color=colours[2], fill=colours[2], bins=60) +
  xlim(0, 600) +
  ylim(0, 50) +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=20), axis.title.y=element_blank(), axis.title.x=element_blank(), plot.title=element_text(face="bold", size=20, hjust=0.5)) +
  geom_vline(xintercept=mean(df_below$pt_prop), color=colours[4], linetype='dashed', size=1) +
  annotate("text", x=75, y=50, label="Mean = 5.69", col=colours[4], size=5) +
  ggtitle("Below average")

ggarrange(p_above, p_below, ncol=1, nrow=2)
ggsave("/Users/elenaleggett/Documents/Masters/Dissertation/gender_pay_gap/data_scripts/figures/pt_prop_hists.jpeg", width=9, height=15.96)

# Boxplots 
                           
p_1 <- ggplot(df, aes(x=gpg_above_average, y=size, fill=size)) + geom_boxplot(outlier.colour='black', fill=c(colours[2], colours[3]))
p_2 <- ggplot(df, aes(x=gpg_above_average, y=what_we_do_agentic, fill=what_we_do_agentic)) + geom_boxplot(outlier.colour='black', fill=c(colours[2], colours[3]))
p_3 <- ggplot(df, aes(x=gpg_above_average, y=what_we_do_communal, fill=what_we_do_communal)) + geom_boxplot(outlier.colour='black', fill=c(colours[2], colours[3]))
p_4 <- ggplot(df, aes(x=gpg_above_average, y=what_we_do_inclusive, fill=what_we_do_inclusive)) + geom_boxplot(outlier.colour='black', fill=c(colours[2], colours[3]))
p_5 <- ggplot(df, aes(x=gpg_above_average, y=what_youll_get_agentic, fill=what_youll_get_agentic)) + geom_boxplot(outlier.colour='black', fill=c(colours[2], colours[3]))
p_6 <- ggplot(df, aes(x=gpg_above_average, y=what_youll_get_communal, fill=what_youll_get_communal)) + geom_boxplot(outlier.colour='black', fill=c(colours[2], colours[3]))
p_7 <- ggplot(df, aes(x=gpg_above_average, y=what_youll_get_inclusive, fill=what_youll_get_inclusive)) + geom_boxplot(outlier.colour='black', fill=c(colours[2], colours[3]))
p_8 <- ggplot(df, aes(x=gpg_above_average, y=who_youll_work_with_agentic, fill=who_youll_work_with_agentic)) + geom_boxplot(outlier.colour='black', fill=c(colours[2], colours[3]))
p_9 <- ggplot(df, aes(x=gpg_above_average, y=who_youll_work_with_communal, fill=who_youll_work_with_communal)) + geom_boxplot(outlier.colour='black', fill=c(colours[2], colours[3]))
p_10 <- ggplot(df, aes(x=gpg_above_average, y=who_youll_work_with_inclusive, fill=who_youll_work_with_inclusive)) + geom_boxplot(outlier.colour='black', fill=c(colours[2], colours[3]))
p_11 <- ggplot(df, aes(x=gpg_above_average, y=pt_prop, fill=pt_prop)) + geom_boxplot(outlier.colour='black', fill=c(colours[2], colours[3]))
p_1 <- p_1 + coord_flip()
p_2 <- p_2 + coord_flip()
p_3 <- p_3 + coord_flip()
p_4 <- p_4 + coord_flip()
p_5 <- p_5 + coord_flip()
p_6 <- p_6 + coord_flip()
p_7 <- p_7 + coord_flip()
p_8 <- p_8 + coord_flip()
p_9 <- p_9 + coord_flip()
p_10 <- p_10 + coord_flip()
p_11 <- p_11 + coord_flip()
                           
ggarrange(p_1, p_11, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9, p_10, ncol=2, nrow=6)
                           
ggsave('boxplots.jpeg')
                           
# Significance testing 
                           
df_cs <- df[-c(1)]
                           
chi2 <- lapply(df_cs[,-11], function(x) chisq.test(df_cs[,11], x)); chi2
chi_sq <- do.call(rbind, chi2)[,c(1,3)]
chi_sq <- data.frame(chi_sq)
chi_sq$significance <- ifelse(chi_sq$p.value < 0.05, 'significant', 'not significant')
chi_sq
                           
wilcox.test(pt_prop ~ gpg_above_average, data=df, exact=FALSE)
                           
 ## MODELLING ##
                           
# Preparing data 

df <- dummy_cols(df, select_columns ='sector')
                           
set.seed(2)
train <- sample(nrow(df), 0.8*nrow(df))
df_train <- df[train,]
df_validate <- df[-train,]

# Logistic regression - all variables 

full_log <- glm(gpg_above_average~.-sector, data=df_train, family='binomial')
full_log_sum <- summary(full_log)

full_log_prob <- predict(full_log, data=df_train, type='response')
full_log_perf <- factor(full_log_prob > .5, levels=c(FALSE,TRUE), labels=c('below', 'above'))
full_log_cm <- table(df_train$gpg_above_average, full_log_perf, dnn=c('below', 'above'))

# Logistic regression - stepwise selection 

null_log <- glm(gpg_above_average~1, data=df_train, family='binomial')

step(null_log, scope=formula(full_log), direction='both')

step_log = glm(formula = gpg_above_average ~ size + what_youll_get_agentic + 
                who_youll_work_with_agentic + what_youll_get_inclusive + 
                pt_prop + `sector_Accountancy (Qualified)`, 
              family = 'binomial', data = df_train)

step_log_prob <- predict(step_log, data=df_train, type='response')
predicted_labels <- ifelse(step_log_prob > 0.5, 1, 0)
step_log_cm <- confusionMatrix(factor(predicted_labels), factor(df_train$gpg_above_average))
step_log_cm <- step_log_cm$table

# Classification tree

tree <- rpart(gpg_above_average ~., data=df_train)

rpart.plot(tree) 

tree_pred <- predict(tree, df_train, type = 'class')
tree_cm <- confusionMatrix(factor(tree_pred), factor(df_train$gpg_above_average))
tree_cm <- tree_cm$table

# Pruned tree 

tree_cp <- printcp(tree) 

ggplot(data=tree_cp, aes(x=nsplit, y=xerror2)) +
  geom_line(color=colours[2]) +
  geom_point(color=colours[2]) +
  xlab('Number of splits') +
  ylab('Cross validation error')

ggsave('tree_cv_error.jpeg')

pruned_pred <- predict(pruned, df_train, type='class')
pruned_cm <- confusionMatrix(factor(pruned_pred), factor(df_train$gpg_above_average))
pruned_cm <- pruned_cm$table

# Bagged tree 

accuracy_vec=c()

for (i in 1:100){
  bag <- randomForest(gpg_above_average~., 
                      data=df_train, mtry=11, ntree=(i), importance=TRUE)

    predicted_labels = predict(bag, df_train, type='response')
    bag_cm <- table(predicted_labels, df_train$gpg_above_average)

    accuracy <- (bag_cm[1,1]+bag_cm[2,2])/nrow(df_train)

    accuracy_vec[i] <- accuracy
}

tree_number = c(1:100)
plot_colours =c(rep('black', 100))
plot_colours[(which.max(accuracy_vec))] = colours[2]
bag_scores <- data.frame(tree_number, accuracy_vec, f1_vec)
ggplot(data=bag_scores, aes(x=tree_number, y=accuracy_vec)) +
  geom_point(color=plot_colours) +
  xlab('Tree number') +
  ylab('Accuracy')

ggsave('bagging_accuracy.jpeg')

bag <- randomForest(gpg_above_average~sector+what_we_do_agentic
                            +what_we_do_communal+what_we_do_inclusive
                            +what_youll_get_agentic+what_youll_get_communal
                            +what_youll_get_inclusive+who_youll_work_with_agentic
                            +who_youll_work_with_communal+who_youll_work_with_inclusive
                            +pt_prop, 
                            data=df_train, mtry=11, ntree=(which.max(accuracy_vec)), importance=TRUE)


bag_pred <- predict(bag, df_train, type='class')
bag_cm <- confusionMatrix(factor(bag_pred), factor(df_train$gpg_above_average))
bag_cm <- bag_cm$table

# Random forest 

accuracy_vec = c()

for (i in 1:12){
  
    rf <- randomForest(gpg_above_average~size+sector+what_we_do_agentic
            +what_we_do_communal+what_we_do_inclusive
            +what_youll_get_agentic+what_youll_get_communal
            +what_youll_get_inclusive+who_youll_work_with_agentic
            +who_youll_work_with_communal+who_youll_work_with_inclusive
            +pt_prop, 
            data=df_train, mtry=i, ntree=(68), importance=TRUE)
  
      predicted_labels = predict(bag, df_train, type='response')
      bag_cm <- table(predicted_labels, df_train$gpg_above_average)
      precision <- bag_cm[2,2] / (bag_cm[2,2] + bag_cm[2,1])
      recall <- bag_cm[1,1] / (bag_cm[1,1] + bag_cm[2,1])
      f1 <- 2*((precision*recall)/(precision+recall))
      accuracy <- (bag_cm[1,1]+bag_cm[2,2])/nrow(df_train)
  
      f1_vec[i] <- f1
      accuracy_vec[i] <- accuracy
  
}

num_variables = c(1:12)
plot_colours =c(rep('black', 12))
plot_colours[(which.max(accuracy_vec))] = colours[2]
rf_scores <- data.frame(num_variables, accuracy_vec, f1_vec)
ggplot(data=rf_scores, aes(x=num_variables, y=accuracy_vec)) +
  geom_point(color=plot_colours) +
  xlab('Number of variables considered at each split') +
  ylab('Accuracy')

random_forest <- randomForest(gpg_above_average~size+sector+what_we_do_agentic
                  +what_we_do_communal+what_we_do_inclusive
                  +what_youll_get_agentic+what_youll_get_communal
                  +what_youll_get_inclusive+who_youll_work_with_agentic
                  +who_youll_work_with_communal+who_youll_work_with_inclusive
                  +pt_prop, 
                  data=df_train, mtry=7, ntree=(68), importance=TRUE)


random_forest_pred <- predict(random_forest, df_train, type='class')
random_forest_cm <- confusionMatrix(factor(random_forest_pred), factor(df_train$gpg_above_average))
random_forest_cm <- random_forest_cm$table

## VALIDATION ## 

full_log_acc_vec = c()
step_log_acc_vec = c()
tree_acc_vec = c()
pruned_acc_vec = c()
bagged_acc_vec = c()
random_forest_acc_vec = c()

for (i in 1:50){

  set.seed(i)
  
  train <- sample(nrow(df), 0.8*nrow(df))
  df_train <- df[train,]
  df_validate <- df[-train,]
  
  full_log_model <- glm(gpg_above_average~.-sector, data=df_train, family='binomial')
  full_log_prob <- predict(full_log_model, df_validate, type='response')
  full_log_pred_labels <- ifelse(full_log_prob > 0.5, 1, 0)
  full_log_cm <- confusionMatrix(factor(full_log_pred_labels), factor(df_validate$gpg_above_average))
  full_log_acc <- (full_log_cm$table[2,2] + full_log_cm$table[1,1]) / (full_log_cm$table[2,2] + full_log_cm$table[1,1] + full_log_cm$table[2,1] + full_log_cm$table[1,2])
  full_log_acc_vec[i] <- full_log_acc

  step_log_model <- glm(gpg_above_average~size+what_youll_get_agentic+who_youll_work_with_agentic+what_youll_get_inclusive+pt_prop+`sector_Accountancy (Qualified)`, data=df_train, family='binomial')
  step_log_prob <- predict(step_log_model, df_validate, type='response')
  step_log_pred_labels <- ifelse(step_log_prob > 0.5, 1, 0)
  step_log_cm <- confusionMatrix(factor(step_log_pred_labels), factor(df_validate$gpg_above_average))
  step_log_acc <- (step_log_cm$table[2,2] + step_log_cm$table[1,1]) / (step_log_cm$table[2,2] +     step_log_cm$table[1,1] + step_log_cm$table[2,1] + step_log_cm$table[1,2])
  step_log_acc_vec[i] <- step_log_acc
  
  tree_model <- rpart(gpg_above_average~.,  data=df_train, method='class')
  tree_preds <- predict(tree_model, df_validate, type='class')
  tree_cm <- confusionMatrix(factor(tree_preds), df_validate$gpg_above_average)
  tree_acc <- (tree_cm$table[2,2] + tree_cm$table[1,1]) / (tree_cm$table[2,2] + tree_cm$table[1,1] + tree_cm$table[2,1] + tree_cm$table[1,2])
  tree_acc_vec[i] <- tree_acc
                           
  pruned_preds <- predict(pruned_model, df_validate, type='class')
  pruned_cm <- confusionMatrix(factor(pruned_preds), df_validate$gpg_above_average)
  pruned_acc <- (pruned_cm$table[2,2] + pruned_cm$table[1,1]) / (pruned_cm$table[2,2] + pruned_cm$table[1,1] + pruned_cm$table[2,1] + pruned_cm$table[1,2])
  pruned_acc_vec[i] <- pruned_acc
                           
  bagged_model <- randomForest(gpg_above_average~.,  data=df_train, mtry=11, ntree=(68), importance=TRUE)
  bagged_preds <- predict(bagged_model, df_validate, type='response')
  bagged_cm <- confusionMatrix(factor(bagged_preds), df_validate$gpg_above_average)
  bagged_acc <- (bagged_cm$table[2,2] + bagged_cm$table[1,1]) / (bagged_cm$table[2,2] + bagged_cm$table[1,1] + bagged_cm$table[2,1] + bagged_cm$table[1,2])
  bagged_acc_vec[i] <- bagged_acc
                           
  random_forest_model <- randomForest(gpg_above_average~., data=df_train, mtry=7, ntree=(68), importance=TRUE)
  random_forest_preds <- predict(random_forest_model, df_validate, type='response')
  random_forest_cm <- confusionMatrix(factor(random_forest_preds), df_validate$gpg_above_average)
  random_forest_acc <- (random_forest_cm$table[2,2] + random_forest_cm$table[1,1]) / (random_forest_cm$table[2,2] + random_forest_cm$table[1,1] + random_forest_cm$table[2,1] + random_forest_cm$table[1,2])
  random_forest_acc_vec[i] <- random_forest_acc
                           
}

accuracy <- data.frame(full_log_acc_vec, fwd_log_acc_vec, tree_acc_vec, pruned_acc_vec, bagged_acc_vec, random_forest_acc_vec)

# Plotting accuracy histograms 

p_full_log <- ggplot(accuracy, aes(x=full_log_acc_vec)) +
  geom_histogram(color=colours[2], fill=colours[2]) +
  xlim(0.5, 0.8) +
  ylim(0, 15) +
  xlab('Logistic regression (all variables)') +
  geom_vline(xintercept=mean(full_log_acc_vec), color=colours[4], linetype='dashed', size=1)

p_fwd_log <- ggplot(accuracy, aes(x=fwd_log_acc_vec)) +
  geom_histogram(color=colours[2], fill=colours[2]) +
  xlim(0.5, 0.8) +
  ylim(0, 15) +
  xlab('logistic regression (stepwise variable selection)') +
  geom_vline(xintercept=mean(fwd_log_acc_vec), color=colours[4], linetype='dashed', size=1)

p_tree <- ggplot(accuracy, aes(x=tree_acc_vec)) +
  geom_histogram(color=colours[2], fill=colours[2]) +
  xlim(0.5, 0.8) +
  ylim(0, 15) +
  xlab('Unpruned tree') +
  geom_vline(xintercept=mean(tree_acc_vec), color=colours[4], linetype='dashed', size=1)

p_pruned <- ggplot(accuracy, aes(x=pruned_acc_vec)) +
  geom_histogram(color=colours[2], fill=colours[2]) +
  xlim(0.5, 0.8) +
  ylim(0, 15) +
  xlab('Pruned tree') +
  geom_vline(xintercept=mean(pruned_acc_vec), color=colours[4], linetype='dashed', size=1)

p_bagged <- ggplot(accuracy, aes(x=bagged_acc_vec)) +
  geom_histogram(color=colours[2], fill=colours[2]) +
  xlim(0.5, 0.8) +
  ylim(0, 15) +
  xlab('Bagged tree') +
  geom_vline(xintercept=mean(bagged_acc_vec), color=colours[4], linetype='dashed', size=1)

p_rf <- ggplot(accuracy, aes(x=random_forest_acc_vec)) +
  geom_histogram(color=colours[2], fill=colours[2]) +
  xlim(0.5, 0.8) +
  ylim(0, 15) +
  xlab('Random forest') +
  geom_vline(xintercept=mean(random_forest_acc_vec), color=colours[4], linetype='dashed', size=1)

ggarrange(p_pruned, p_tree, p_fwd_log, p_full_log, p_rf, ncol=1, p_bagged, nrow=6)

# McNemar test 

mcnemar_data <- data.frame(pruned_preds, random_forest_preds, df_validate$gpg_above_average)

mcnemar_data$rf_num <- as.numeric(mcnemar_data$random_forest_preds) - 1
mcnemar_data$pruned_num <- as.numeric(mcnemar_data$pruned_preds) - 1
mcnemar_data$actual_num <- as.numeric(mcnemar_data$df_validate.gpg_above_average) - 1
mcnemar_data$sum <- mcnemar_data$rf_num + mcnemar_data$pruned_num + mcnemar_data$actual_num

all_correct <- sum(mcnemar_data['actual_num'] == mcnemar_data['pruned_num'] & mcnemar_data['actual_num'] == mcnemar_data['rf_num'])
rf_correct <- sum(mcnemar_data['rf_num'] == mcnemar_data['actual_num'] & mcnemar_data['actual_num'] != mcnemar_data['pruned_num'])
pt_correct <- sum(mcnemar_data['pruned_num'] == mcnemar_data['actual_num'] & mcnemar_data['actual_num'] != mcnemar_data['rf_num'])
both_incorrect <- sum(mcnemar_data['actual_num'] != mcnemar_data['pruned_num'] & mcnemar_data['actual_num'] != mcnemar_data['rf_num'])

mcnemar_table <- cbind(c(both_correct, pt_correct), c(rf_correct, both_incorrect))
mcnemar.test(mcnemar_table)

## EXPLORING RANDOM FOREST MODEL ## 

rf_imp <- data.frame(importance(random_forest_model))
rf_imp['variable'] <- rownames(rf_imp)

p <- ggplot(data=rf_imp2, aes(x=reorder(variable, mean_decrease), y=mean_decrease)) +
  geom_bar(stat='identity', fill=colours[2]) +
  xlab('') +
  ylab('Mean decrease in accuracy when excluded (%)') +
  scale_y_continuous(breaks=c(2,4,6,8,10))
 
p + coord_flip()

ggsave('rf_var_imp.jpeg')

