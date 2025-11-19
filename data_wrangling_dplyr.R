# packages ####
library(tidyverse)

# read in the data ####
algae_ros <- read.csv("data/algae_ros.csv")
algae_cellgroup <- read.csv("data/algae_cellgroups.csv")
algae_growth <- read.csv("data/algae_growth.csv")

# Descriptions of the datasets #### 
# 16 algae species were grown in 5 nutritional treatments
# algae_ros records the total reactive oxygen species (ROS) each sample generated 
# algae_cellgroup records the area of cells in 4 cell-group categories: single, double, quartet and multicellular
# algae_growth records the maximum daily increase of algal biomass 

# First inspect the datasets using functions such as str(), head(), View() or glimpse() 
glimpse(algae_growth)

# Task1: join #### 
# option 1: join function (left_join in this case):
algae_1 <- left_join(algae_growth, algae_ros, by=join_by(species==species, treat==treat, rep==rep))

# option 2: merge function by unique identifier
# create unique identifier "id" 
algae_ros$id <- paste(algae_ros$species,algae_ros$treat,algae_ros$rep, sep="_")
algae_growth$id <- paste(algae_growth$species,algae_growth$treat,algae_growth$rep, sep="_")
algae_2 <- merge(algae_ros, algae_growth[, !names(algae_growth) %in% c("treat", "species", "rep")], by="id") # what does the arguments mean here? 

# Task 2: select ####
## select works on columns ####
# select columns by names, preferable over by column number e.g. algae[, c(3,5,6)]
select(algae_1, species:treat, type:ros_read)
# select columns by name patterns:
select(algae_1, contains("_max"))
# select columns by end: 
select(algae_1, ends_with("_growth"))

# Task 3: filter ####
## filter works on rows ####
filter(algae_1, species == "fio27")
# multiple conditions using boolean operators 
filter(algae_cellgroup, type == "s" & mean_area_Double != "0")
# since many variables contain "0", we can use "filter", "if_any" to remove all numeric variables that has 0s in them, same idea works with "NA"s. 
filter(algae_cellgroup, if_any(where(is.numeric), ~. !=0))


# Task 4: pivot ####
## pivot to change dataset format (longer or wider) ####
# inspect algae_ros, is it a wide or long table? 
head(algae_cellgroup)
algae_cellgroup_long <- pivot_longer(algae_cellgroup, 
                                     cols = -c(species, rep, treat), # don't pivot these calumns
                                     names_to = "traits", 
                                     values_to = "value")
# convert it back to original wide format: 
head(algae_cellgroup_long)
str(algae_cellgroup_long)
pivot_wider(algae_cellgroup_long, names_from = "traits", values_from = "value" )

# Why pivot? 
# running models and ggplot only understand long format:
ggplot(algae_ros, aes(x = species, y = ros_read, color = treat)) + 
  geom_point(size = 2)+
  theme_classic()

# create mean and error bar plot using ggplot::geom_pointrange
# summarise data with help of group_by:
ros_sum <- algae_ros %>% 
  group_by(species) %>% 
  summarise(mean_ros = mean(ros_read), 
            se_ros = sd(ros_read)/sqrt(n()),
            upper_error = mean_ros + se_ros,    # upper error bar: mean + se
            lower_error = mean_ros - se_ros)    # lower error bar: mean - se

ggplot(data = algae_ros, aes(x=species, y=log(ros_read))) +    # background points from dataset: algae_ros
  geom_point(color = "grey")+ 
  geom_pointrange(data = ros_sum,                              # mean and error bars from summarised dataset: ros_sum
                  mapping = aes(x = species,                
                                y = log(mean_ros), 
                                ymin = log(lower_error), 
                                ymax = log(upper_error), 
                                color = species), 
                  size = .8)+
  theme_classic()


# Task 5: mutate ####
## mutate new variables with help of group_by ####
algae_3 <- algae %>% 
  group_by(treat) %>% 
  mutate(sample_size_per_treatment = n()) # function n() gives the sample size in each levels of the grouped 
                                          #variable, in this case "treat"



# Exercise: 
# 1, create a fully joined dataset with ros, cellgroups and growth.
# 2. calculate total mean areas of all 4 cell groups Single, Double, Quartet and Multi
# 3. create a new variable called ros_standard = ros_read/(total mean areas) 
# 4. summarise the ros_standard by each treatment, and calculate, mean, se 
# 5. plot the mean & error bar plot of ros_standard by each treatment, with scattered ros_standard points in the background


























algae <- left_join(algae_1, algae_cellgroup, by=join_by(species== species, treat==treat, rep==rep)) %>% 
  group_by(species,rep,treat) %>% 
  mutate(total_mean_area = sum(mean_area_Double, mean_area_Quartet, mean_area_Single, mean_area_Multi),
         ros_standard = ros_read/total_mean_area)

algae_sum <- algae %>% 
  group_by(treat) %>%
  summarise(mean_ros = mean(ros_standard),
            se_ros = sd(ros_standard)/sqrt(n()),
            upper = mean_ros + se_ros,
            lower = mean_ros - se_ros)
  
ggplot(algae, aes(x=treat, y=ros_standard))+
  geom_point(color = "grey79")+
  geom_pointrange(algae_sum, mapping= aes(x=treat, y=mean_ros, ymin=lower, ymax=upper, color=treat), size=0.8)+
  theme_classic() +
  xlab("Nutrient treatments")+
  ylab("Standardized reactive oxygen species ")









