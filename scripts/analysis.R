library(tidyverse)
library(dplyr)
library(patchwork)

setwd(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

dat <- read_csv("data/insp_score_flood_demo_nri.csv")

### US OVERALL ###
# in_floodplain
all <- dat
int1 <- (all[all$in_floodplain == 1,]$INSPECTION_SCORE)
int0 <- (all[all$in_floodplain == 0,]$INSPECTION_SCORE)
int1_upper <- median(int1)+1.57*IQR(int1)/sqrt(length(int1))
int1_lower <- median(int1)-1.57*IQR(int1)/sqrt(length(int1))
int0_upper <- median(int0)+1.57*IQR(int0)/sqrt(length(int0))
int0_lower <- median(int0)-1.57*IQR(int0)/sqrt(length(int0))
if ((int0_lower <= int1_upper & int1_upper >= int0_lower)){
  print("The medians of two groups are not significantly different.")
}else{
  print("The medians of two groups are significantly different.")
}

p_flood_us <- ggplot(all,aes(x = factor(in_floodplain), y = INSPECTION_SCORE)) +
  geom_boxplot(notch = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  labs(x = "In_floodplain",
       y = "Inspection score",
       title = "US overall") +
  theme_bw()
p_flood_us

# different risks
all <- dat
names <- colnames(all)
# p_risks <- list()
for (i in 44:50){
  int1 <- (all[all[,i] == 1,]$INSPECTION_SCORE)
  int0 <- (all[all[,i] == 0,]$INSPECTION_SCORE)
  
  int1_upper <- median(int1)+1.57*IQR(int1)/sqrt(length(int1))
  int1_lower <- median(int1)-1.57*IQR(int1)/sqrt(length(int1))
  int0_upper <- median(int0)+1.57*IQR(int0)/sqrt(length(int0))
  int0_lower <- median(int0)-1.57*IQR(int0)/sqrt(length(int0))
  
  if ((int0_lower <= int1_upper & int1_upper >= int0_lower)){
    print(paste(names[i],": The medians of two groups are not significantly different."))
  }else{
    print(paste(names[i],": The medians of two groups are significantly different."))
  }
  
  
}
plot_nri <- function(x,name){
  p <- ggplot(all,aes(x = factor(x), y = INSPECTION_SCORE)) +
    geom_boxplot(notch = TRUE) +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
    stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)),
                 vjust = -0.5, color = "blue")+
    labs(y = "Inspection score",
         x = "",
         title = name) +
    theme_bw()
}
risk_all <- plot_nri(all[[44]],names[44])
soc_vul <- plot_nri(all[[45]],names[45])
low_res <- plot_nri(all[[46]],names[46])
flood <- plot_nri(all[[47]],names[47])
heat <- plot_nri(all[[48]],names[48])
hurricane <- plot_nri(all[[49]],names[49])
wildfire <- plot_nri(all[[50]],names[50])

p_nri_us <- (risk_all + soc_vul + low_res) / (flood + heat + hurricane) / 
  (wildfire + plot_spacer() + plot_spacer())

# in_floodplain and income
all$income <- (all$income_c >= median(all$income_c))*1
all <- all %>%
  mutate(floodxincome = paste(in_floodplain,income, sep = ".")) %>%
  arrange(floodxincome)

int11 <- (all[all$floodxincome == "1.1",]$INSPECTION_SCORE)
int10 <- (all[all$floodxincome == "1.0",]$INSPECTION_SCORE)
int01 <- (all[all$floodxincome == "0.1",]$INSPECTION_SCORE)
int00 <- (all[all$floodxincome == "0.0",]$INSPECTION_SCORE)
int_list <- list(int00,int01,int10,int11)
int11_upper <- median(int11)+1.57*IQR(int11)/sqrt(length(int11))
int11_lower <- median(int11)-1.57*IQR(int11)/sqrt(length(int11))
int10_upper <- median(int10)+1.57*IQR(int10)/sqrt(length(int10))
int10_lower <- median(int10)-1.57*IQR(int10)/sqrt(length(int10))
int01_upper <- median(int01)+1.57*IQR(int01)/sqrt(length(int01))
int01_lower <- median(int01)-1.57*IQR(int01)/sqrt(length(int01))
int00_upper <- median(int00)+1.57*IQR(int00)/sqrt(length(int00))
int00_lower <- median(int00)-1.57*IQR(int00)/sqrt(length(int00))

diff_mat <- matrix(0,nrow = 4,ncol = 4)
for(i in 1:4){
  for (j in 1:4){
    if (i < j){
      x1 <- int_list[[i]]
      x2 <- int_list[[j]]
      
      x1_upper <- median(x1)+1.57*IQR(x1)/sqrt(length(x1))
      x1_lower <- median(x1)-1.57*IQR(x1)/sqrt(length(x1))
      x2_upper <- median(x2)+1.57*IQR(x2)/sqrt(length(x2))
      x2_lower <- median(x2)-1.57*IQR(x2)/sqrt(length(x2))
      
      if (!((x1_lower <= x2_upper) & (x1_upper >= x2_lower))){
        diff_mat[i,j] <- 1
      }
    }
  }
}
diff_mat

p_flood_income_us <- ggplot(all,aes(x = factor(floodxincome), y = INSPECTION_SCORE)) +
  geom_boxplot(notch = TRUE) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
               position = position_nudge(x = 0.2, y = 0), color = "blue") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  scale_x_discrete(labels = c("out_floodplain + low_income","out_floodplain + high_income",
                              "in_floodplain + low_income","in_floodplain + high_income")) +
  labs(x = "In_floodplain",
       y = "Inspection score",
       title = paste("US overall")) +
  theme_bw()

p_flood_income_us

# in_floodplain and age
all$age <- (all$age_c >= median(all$age_c))*1
all <- all %>%
  mutate(floodxage = paste(in_floodplain,age, sep = ".")) %>%
  arrange(floodxage)

int11 <- (all[all$floodxage == "1.1",]$INSPECTION_SCORE)
int10 <- (all[all$floodxage == "1.0",]$INSPECTION_SCORE)
int01 <- (all[all$floodxage == "0.1",]$INSPECTION_SCORE)
int00 <- (all[all$floodxage == "0.0",]$INSPECTION_SCORE)
int_list <- list(int00,int01,int10,int11)
int11_upper <- median(int11)+1.57*IQR(int11)/sqrt(length(int11))
int11_lower <- median(int11)-1.57*IQR(int11)/sqrt(length(int11))
int10_upper <- median(int10)+1.57*IQR(int10)/sqrt(length(int10))
int10_lower <- median(int10)-1.57*IQR(int10)/sqrt(length(int10))
int01_upper <- median(int01)+1.57*IQR(int01)/sqrt(length(int01))
int01_lower <- median(int01)-1.57*IQR(int01)/sqrt(length(int01))
int00_upper <- median(int00)+1.57*IQR(int00)/sqrt(length(int00))
int00_lower <- median(int00)-1.57*IQR(int00)/sqrt(length(int00))

diff_mat <- matrix(0,nrow = 4,ncol = 4)
for(i in 1:4){
  for (j in 1:4){
    if (i < j){
      x1 <- int_list[[i]]
      x2 <- int_list[[j]]
      
      x1_upper <- median(x1)+1.57*IQR(x1)/sqrt(length(x1))
      x1_lower <- median(x1)-1.57*IQR(x1)/sqrt(length(x1))
      x2_upper <- median(x2)+1.57*IQR(x2)/sqrt(length(x2))
      x2_lower <- median(x2)-1.57*IQR(x2)/sqrt(length(x2))
      
      if (!((x1_lower <= x2_upper) & (x1_upper >= x2_lower))){
        diff_mat[i,j] <- 1
      }
    }
  }
}
diff_mat

p_flood_age_us <- ggplot(all,aes(x = factor(floodxage), y = INSPECTION_SCORE)) +
  geom_boxplot(notch = TRUE) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
               position = position_nudge(x = 0.2, y = 0), color = "blue") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  scale_x_discrete(labels = c("out_floodplain + young","out_floodplain + old",
                              "in_floodplain + young","in_floodplain + old")) +
  labs(x = "In_floodplain",
       y = "Inspection score",
       title = paste("US overall")) +
  theme_bw()

p_flood_age_us

# in_floodplain and white
all <- all[-which(is.na(all$total_white_c / all$total_pop_c)),]
all$white <- (all$total_white_c / all$total_pop_c >= median(all$total_white_c / all$total_pop_c))*1
all <- all %>%
  mutate(floodxwhite = paste(in_floodplain,white, sep = ".")) %>%
  arrange(floodxwhite)

int11 <- (all[all$floodxwhite == "1.1",]$INSPECTION_SCORE)
int10 <- (all[all$floodxwhite == "1.0",]$INSPECTION_SCORE)
int01 <- (all[all$floodxwhite == "0.1",]$INSPECTION_SCORE)
int00 <- (all[all$floodxwhite == "0.0",]$INSPECTION_SCORE)
int_list <- list(int00,int01,int10,int11)
int11_upper <- median(int11)+1.57*IQR(int11)/sqrt(length(int11))
int11_lower <- median(int11)-1.57*IQR(int11)/sqrt(length(int11))
int10_upper <- median(int10)+1.57*IQR(int10)/sqrt(length(int10))
int10_lower <- median(int10)-1.57*IQR(int10)/sqrt(length(int10))
int01_upper <- median(int01)+1.57*IQR(int01)/sqrt(length(int01))
int01_lower <- median(int01)-1.57*IQR(int01)/sqrt(length(int01))
int00_upper <- median(int00)+1.57*IQR(int00)/sqrt(length(int00))
int00_lower <- median(int00)-1.57*IQR(int00)/sqrt(length(int00))

diff_mat <- matrix(0,nrow = 4,ncol = 4)
for(i in 1:4){
  for (j in 1:4){
    if (i < j){
      x1 <- int_list[[i]]
      x2 <- int_list[[j]]
      
      x1_upper <- median(x1)+1.57*IQR(x1)/sqrt(length(x1))
      x1_lower <- median(x1)-1.57*IQR(x1)/sqrt(length(x1))
      x2_upper <- median(x2)+1.57*IQR(x2)/sqrt(length(x2))
      x2_lower <- median(x2)-1.57*IQR(x2)/sqrt(length(x2))
      
      if (!((x1_lower <= x2_upper) & (x1_upper >= x2_lower))){
        diff_mat[i,j] <- 1
      }
    }
  }
}
diff_mat

p_flood_white_us <- ggplot(all,aes(x = factor(floodxwhite), y = INSPECTION_SCORE)) +
  geom_boxplot(notch = TRUE) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
               position = position_nudge(x = 0.2, y = 0), color = "blue") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  scale_x_discrete(labels = c("out_floodplain + less white","out_floodplain + white",
                              "in_floodplain + less white","in_floodplain + white")) +
  labs(x = "In_floodplain",
       y = "Inspection score",
       title = paste("US overall")) +
  theme_bw()

p_flood_white_us

# in_floodplain and prop
all$prop <- (all$prop_value_c >= median(all$prop_value_c))*1
all <- all %>%
  mutate(floodxprop = paste(in_floodplain,prop, sep = ".")) %>%
  arrange(floodxprop)

int11 <- (all[all$floodxprop == "1.1",]$INSPECTION_SCORE)
int10 <- (all[all$floodxprop == "1.0",]$INSPECTION_SCORE)
int01 <- (all[all$floodxprop == "0.1",]$INSPECTION_SCORE)
int00 <- (all[all$floodxprop == "0.0",]$INSPECTION_SCORE)
int_list <- list(int00,int01,int10,int11)
int11_upper <- median(int11)+1.57*IQR(int11)/sqrt(length(int11))
int11_lower <- median(int11)-1.57*IQR(int11)/sqrt(length(int11))
int10_upper <- median(int10)+1.57*IQR(int10)/sqrt(length(int10))
int10_lower <- median(int10)-1.57*IQR(int10)/sqrt(length(int10))
int01_upper <- median(int01)+1.57*IQR(int01)/sqrt(length(int01))
int01_lower <- median(int01)-1.57*IQR(int01)/sqrt(length(int01))
int00_upper <- median(int00)+1.57*IQR(int00)/sqrt(length(int00))
int00_lower <- median(int00)-1.57*IQR(int00)/sqrt(length(int00))

diff_mat <- matrix(0,nrow = 4,ncol = 4)
for(i in 1:4){
  for (j in 1:4){
    if (i < j){
      x1 <- int_list[[i]]
      x2 <- int_list[[j]]
      
      x1_upper <- median(x1)+1.57*IQR(x1)/sqrt(length(x1))
      x1_lower <- median(x1)-1.57*IQR(x1)/sqrt(length(x1))
      x2_upper <- median(x2)+1.57*IQR(x2)/sqrt(length(x2))
      x2_lower <- median(x2)-1.57*IQR(x2)/sqrt(length(x2))
      
      if (!((x1_lower <= x2_upper) & (x1_upper >= x2_lower))){
        diff_mat[i,j] <- 1
      }
    }
  }
}
diff_mat

p_flood_prop_us <- ggplot(all,aes(x = factor(floodxprop), y = INSPECTION_SCORE)) +
  geom_boxplot(notch = TRUE) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
               position = position_nudge(x = 0.2, y = 0), color = "blue") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  scale_x_discrete(labels = c("out_floodplain + low prop","out_floodplain + high prop",
                              "in_floodplain + low prop","in_floodplain + high prop")) +
  labs(x = "",
       y = "Inspection score",
       title = paste("US overall")) +
  theme_bw()

p_flood_prop_us


#####
p_flood_us
p_nri_us
p_flood_income_us
p_flood_age_us
p_flood_white_us
p_flood_prop_us

##############################3

region <- list(c('CT', 'ME', 'MA', 'NH', 'RI', 'VT'),c('NJ', 'NY', 'PR', 'VI'),
               c('DE', 'MD', 'PA', 'VA', 'DC', 'WV'),c('AL', 'FL', 'GA', 'KY', 'MS', 'NC', 'SC', 'TN'),
               c('IL', 'IN', 'MI', 'MN','OH', 'WI'),c('AR', 'LA', 'NM', 'OK', 'TX'),
               c('IA', 'KS', 'MO', 'NE'),c('CO', 'MT', 'ND', 'SD', 'UT','WY'),
               c('AZ', 'CA', 'HI', 'NV', 'GU', 'AS', 'MP'),c('AK', 'ID', 'OR', 'WA'))

p_reg_flood <- list()
for (i in 1:10){
  regs <- region[[i]]
  int <- dat[dat$STATE_NAME.x %in% regs,]
  
  int1 <- (int[int$in_floodplain == 1,]$INSPECTION_SCORE)
  int0 <- (int[int$in_floodplain == 0,]$INSPECTION_SCORE)
  
  p<- ggplot(int,aes(x = factor(in_floodplain), y = INSPECTION_SCORE)) +
    geom_boxplot(notch = TRUE) +
    stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
                 position = position_nudge(x = 0.2, y = 0), color = "blue") +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
    labs(x = "In_floodplain",
         y = "Inspection score",
         title = paste("Region ",i)) +
    theme_bw()
  p_reg_flood[[i]] <- p
  print(paste("out: ",
              median(int0)-1.57*IQR(int0)/sqrt(length(int0)),median(int0)+1.57*IQR(int0)/sqrt(length(int0)),
              "; in: ",
              median(int0)-1.57*IQR(int1)/sqrt(length(int1)),median(int1)+1.57*IQR(int1)/sqrt(length(int1))))
}

p_flood_reg <- (p_reg_flood[[1]] + p_reg_flood[[2]]) / (p_reg_flood[[3]] + p_reg_flood[[4]]) / 
  (p_reg_flood[[5]] + p_reg_flood[[6]]) / (p_reg_flood[[7]] + p_reg_flood[[8]]) / 
  (p_reg_flood[[9]] + p_reg_flood[[10]]) 



p2 <- list()
for (i in 1:10){
  regs <- region[[i]]
  int <- dat[dat$STATE_NAME.x %in% regs,]
  int$income <- (int$income_c >= median(int$income_c))*1
  int <- int %>%
    mutate(combi = paste(in_floodplain,income, sep = ".")) %>%
    arrange(combi)
  
  int1_high <- (int[int$in_floodplain == 1 & int$income_c >= median(int$income_c),]$INSPECTION_SCORE)
  int1_low <- (int[int$in_floodplain == 1 & int$income_c < median(int$income_c),]$INSPECTION_SCORE)
  int0_high <- (int[int$in_floodplain == 0 & int$income_c >= median(int$income_c),]$INSPECTION_SCORE)
  int0_low <- (int[int$in_floodplain == 0 & int$income_c < median(int$income_c),]$INSPECTION_SCORE)
  
  p<- ggplot(int,aes(x = factor(combi), y = INSPECTION_SCORE)) +
    geom_boxplot(notch = TRUE) +
    stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), 
                 position = position_nudge(x = 0.2, y = 0), color = "blue") +
    stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
    labs(x = "",
         y = "Inspection score",
         title = paste("Region ",i)) +
    scale_x_discrete(labels = c("out_floodplain + low income","out_floodplain + high income",
                                "in_floodplain + low income","in_floodplain + high income")) +
    theme_bw()
  p2[[i]] <- p
  print(paste("out_high: ",
              median(int0_high)-1.57*IQR(int0_high)/sqrt(length(int0_high)),median(int0_high)+1.57*IQR(int0_high)/sqrt(length(int0_high)),
              "out_low: ",
              median(int0_low)-1.57*IQR(int0_low)/sqrt(length(int0_low)),median(int0_low)+1.57*IQR(int0_low)/sqrt(length(int0_low)),
              "in_high: ",
              median(int1_high)-1.57*IQR(int1_high)/sqrt(length(int1_high)),median(int1_high)+1.57*IQR(int1_high)/sqrt(length(int1_high)),
              "in_low: ",
              median(int1_low)-1.57*IQR(int1_low)/sqrt(length(int1_low)),median(int1_low)+1.57*IQR(int1_low)/sqrt(length(int1_low))))
}

p_floodxincome_reg <- (p2[[1]] + p2[[2]]) / (p2[[3]] + p2[[4]]) / (p2[[5]] + p2[[6]]) / (p2[[7]] + p2[[8]]) / (p2[[9]] + p2[[10]]) 


###############
p_reg_flood

p_floodxincome_reg
