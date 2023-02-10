library(tidyverse)
library(ggplot2)
library(patchwork)
library(reshape2)


data <- read.table("~/file.txt", header = T)
head(data)
#      target     


data$group <- ifelse(data$target >= median(data$target), "High", "Low")
data$group <- factor(data$group, levels = c("Low", "High"))
data<- data[order(data$target), ]
data$id <- 1:nrow(data)


### plot
p1 <- ggplot() +
  geom_bar(data = data, 
           aes(x = id, y = target, color=group, fill = group),
           stat = 'identity', position = 'dodge') +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), axis.line.x = element_blank())

data1 <- data %>% 
  select(-group, -target) %>% 
  remove_rownames() %>% 
  column_to_rownames("id") %>% 
  scale() %>% as.data.frame() %>% 
  rownames_to_column("id") %>% 
  melt()

p2 <- 
ggplot(data = data1, aes(x = id, y = as.numeric(variable), fill = value)) +
  geom_raster() +
  scale_fill_gradientn(colors = c("#4DBBD5", "#FFFFFF", "#E64B35")) +
  scale_y_continuous(expand = c(0,0), limits = c(0.5,5.5), breaks = 1:5,
                     labels = levels(data1$variable)) +
  theme(axis.title = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), axis.line.x = element_blank()) 

p1 / p2  