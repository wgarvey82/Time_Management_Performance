# My attempts at data wrangling for Student Performance and Time Management.
#Rename the data frame.
#Try to shorten the function implementation. Maybe shorten the whole data$X thing.
#Add return specification for functions.
library(tidyverse)
library(readr)
library(dplyr)
setwd(here::here())
data <- read.csv("time_man_performance.csv")

 
# Creates data frame to be used for codifying questionnaire responses.

revcode <- c(5:1)
fwdcode <- c(1:5)

codec <- c("Strong Disagree", 
           "Disagree", 
           "Neither", 
           "Agree", 
           "Strong Agree")

revcodedf <- data.frame(codec, 
                        revcode)
fwdcodedf <- data.frame(codec, 
                        fwdcode)

#These functions match *revcode* and *fwdcode*, respectively, 
#based on the associated *codec* 
#in the *revcodedf* and *fwdcodedf* data frames 
#to specified columns of another data frame *q*.

revcodefunc <- function(q){
  revcodedf$revcode[match(q, revcodedf$codec)]
}

fwdcodefunc <- function(q){
  fwdcodedf$fwdcode[match(q, fwdcodedf$codec)]
}

#These commands utilize the *revcodefunc* and *fwdcodefunc* functions
#to replace the reverse coded and forward coded responses, respectively, 
#in the data frame. 
data$X6 <- revcodefunc(data$X6)
data$X9 <- revcodefunc(data$X9) 
data$X11 <- revcodefunc(data$X11)
data$X12 <- revcodefunc(data$X12)
data$X13 <- revcodefunc(data$X13)
data$X15 <- revcodefunc(data$X15)
data$X16 <- revcodefunc(data$X16)

data$X7 <- fwdcodefunc(data$X7)
data$X8 <- fwdcodefunc(data$X8)
data$X10 <- fwdcodefunc(data$X10)
data$X14 <- fwdcodefunc(data$X14)
data$X17 <- fwdcodefunc(data$X17)

#Creating total scores for time management (TM) for each participant.

data$TM <- data %>%
  select(X6:X17) %>%
  rowSums(na.rm=FALSE)

#Relabels number as participants and changes gender to lowercase. 

colnames(data)[colnames(data) %in% c("Number", "Gender")] <- c("participant", "gender") 

#Because academic performance and English proficiency were recorded as ranges, these values have 
#both discrete and continuous qualities. The ranges are converted to numeric here by relabeling 
#them according to their lowest value. After also excluding participants with missing values,
# the lone participant remaining with a score of "<40" is removed for simplicity. Some axis labels 
#will be created for the graphs.

#Removes participant with complete responses but score under 40%

.
data <- data[-c(52),]

#Relabels missing values as "NA" and converts character values to numeric.
data <- data %>% 
  mutate(academic=
           parse_number(Academic, 
                        na=c("%"))) %>% 
  mutate(english=
           parse_number(English, 
                        na=c(""))) %>% 
  mutate_if(is.factor, 
            as.character)

#Clears all participants that have missing values. These missing values were 
#largely concentrated in time_management performance scores. 
data <- na.omit(data)

#Creates labels for figure axes.
indiv_test_labels <- c("40-50","50-60","60-70", ">70")

#Comparison of academic scores and total time management ability scores reveals an
#apparent slightly positive correlation.
academic_plot <- ggplot(data, 
                        aes(x=academic, 
                            y=TM)) 
academic_plot+
  geom_point(color="red")+
  geom_smooth(method="lm", 
              formula=y~x,
              color="black")+
  labs(x="Academic Performance (%)",
       y="Time Management Score",
       title="Time Management and Academic Performance")+
  scale_x_continuous(labels=indiv_test_labels)

#Because the students reported their academic performance in ranges, 
#academic performance has a more discrete, less interesting distribution.

#The same problem happens when comparing English language proficiency scores
#and time management...

english_plot <- ggplot(data, 
                       aes(x=english, 
                          y=TM))
english_plot+
  geom_point(color="blue")+
  geom_smooth(method="lm",
              color="black",
              formula=y~x)+
  labs(x="English Proficiency Score (%)",
       y="Time Management Score",
       title="Time Management and English Proficiency")+
  scale_x_continuous(labels=indiv_test_labels)
  

#and academic performance and English proficiency scores.

academic_english <- ggplot(data, 
                           aes(x=academic, 
                               y=english))+
  labs(x="Academic Performance (%)",
       y="English Language Proficiency (%)",
       title="English Proficiency and Academic Performance")

academic_english+
  geom_point(color="purple")+
  geom_smooth(method="lm",
              color="black")+
  scale_x_continuous(labels=indiv_test_labels)+
  scale_y_continuous(labels=indiv_test_labels)

#Academic performance and English proficiency scores particularly have significant overlap.
#Using *geom_jitter* reduces overlap by adding small, random variation in the points.

academic_english+
  geom_jitter(color="purple")+
  geom_smooth(method="lm",
              color="black",
              formula=y~x)


#Using *geom_jitter* may appear like a solution to the overlapping variables and also the discreteness of
#of the scores, but adding random variation in point distribution does not actually add anything meaningful to the data.
#To further explore this data, I summed the academic performance and English proficiency scores to get a total
#score of test performance. 

#Creates scores for combined academic performance and English proficiency scores for comparison
#with time management.

data$tests <- data %>% #Combined test scores (tests)
  select("academic", 
         "english") %>% 
  rowSums()

tt <- ggplot(data, #Time management and tests(tt)
             aes(x=tests, 
                 y=TM))  
tt+
  geom_point(color="orange")+
  geom_smooth(method="lm",
              color="black")+
  labs(x="Combined Test Scores (%)",
       y="Time Management Score",
       title="Time Management and Test Scores")

  scale_x_continuous(minor_breaks=NULL, 
                     n.breaks=6, 
                     labels=c("80-100","90-110",
                              "100-120", "110-130", 
                              ">120",
                              ">130", 
                              ">140"))

#It becomes apparent that combining these scores would not provide an accurate visualization or further insight into 
# the distribution of time management scores, because 
#all of the values now contain possible overlap. Adding these variables may give the appearance of greater
#variability, but this factor has not changed. Instead by examining another variable alongside either academic performance
#or English language proficiency scores, we may find a moderating 
#or mediating effect. Academic performance will be utilized over English proficiency,
#because understanding its relationship with time management scores could benefit a larger population.

#There seem to be many business students.

#Re-labels all non-business courses as "not business."

data$business <- ifelse(data$Course=="Business", 
                      "business", 
                      "not business")

data %>% count(business)

#Indeed, after omitting participants with missing values, business students outnumber
#non-business students 2:1. Perhaps business and non-business students differ in dependence on time
#management skills for academic performance.

data %>% group_by(business) %>% 
  summarise("Mean TM"=mean(TM), 
            "SD TM"=sd(TM),
            "Mean Academic"=mean(academic),
            "SD Academic"=sd(academic),
            "TM/Academic Correlation"=cor(TM, academic)) 

#Non-business students appear to have slightly higher academic scores, but the correlations with 
#academic performance seem similar.

#Adds colorblind-friendly palette. From http://www.cookbook-r.com/Graphs/Colors_%28ggplot2%29/
cbbPalette <- c("#000000", "#E69F00", 
                "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", 
                "#D55E00", "#CC79A7")

cbbPalette_fill <- c("#000000", "#999999", "#E69F00", 
                     "#56B4E9", "#009E73", "#F0E442", 
                     "#0072B2", "#D55E00", "#CC79A7", 
                     "#FFFFFF")

cbbPalette_fill <- rev(cbbPalette_fill) #Reverses palette order for more pleasant 

aca_business <- ggplot(data, 
                     aes(
                       x=academic, 
                       y=TM,
                       color=business, 
                       shape=business))
aca_business+
  geom_point(size=3, 
             alpha=0.6)+
  geom_smooth(method="lm")+
  labs(x="Academic Performance (%)",
       y="Time Management Score",
       title="Time Management and Academic Performance by Business")+
  scale_colour_manual(values=cbbPalette)+ #Applies the colorblind-friendly color palette.
  scale_shape_manual(values=c(19,17))+
  scale_x_continuous(labels=indiv_test_labels)

#There does not appear to be a difference in correlation with academic performance and time management scores between
#business and non-business students, but at first glance non-business students appear
#to have higher test scores than business students. 
           
#The differences between these groups seems negligible. The TM scores, especially,
#are very similar between the groups, so not much is likely to be learned about this variable
#by proceeding in this comparison. Moreover, even after combining all non-business students into 
#one group, they are still heavily outnumbered by business students.

#The gender distribution in this sample is much more similar.

data %>% count(gender) 

#Before proceeding, this group was checked for interesting difference in means and standard deviations
#in test and TM performance.

data %>% group_by(gender) %>% 
  summarise("Mean TM"=mean(TM), 
            "SD TM"=sd(TM),
            "Mean Academic"=mean(academic),
            "SD Academic"=sd(academic),
            "TM/Academic Correlation"=cor(TM, academic))

#These groups seem much more promising for delivering interesting results. The TM and academic performance correlations and the SD in TM, especially,
#seem significant.

aca_gender <- ggplot(data, 
                   aes(
                     x=academic, 
                     y=TM,
                     color=gender, 
                     shape=gender))+
  labs(x="Academic Performance (%)",
       y="Time Management Score",
       title="Time Management and Academic Performance by Gender")+
  scale_x_continuous(labels=indiv_test_labels)+
  scale_color_manual(values=cbbPalette,
                     labels=c("Female",
                              "Male"))+
  scale_fill_manual(values=cbbPalette_fill, name="gender")+
  scale_shape_manual(values=c(19,17),
                     labels=c("Female",
                              "Male"))

aca_gender+
  geom_point(size=3, 
             alpha=0.6)+
  geom_smooth(method="lm",
              level=0.9)

#Even with a 90% confidence interval, the TM and academic performance correlation 
#do not appear significantly different across genders. Examining the difference in the distribution 
#may still provide insight on a relationship between these variables. 

#Creates a violin plot examining the density of academic performance and time management scores between 
#male and female students.
aca_gender+geom_violin(scale="area", 
              draw_quantiles = .5, 
              adjust=1)+
  geom_point(size=3, 
             alpha=0.5)+
  labs(x="Academic Performance (%)",
       y="Time Management Score",
       title="Time Management and Academic Performance by Gender",
       caption="Lines in the violins divide the violin areas in half.")

#The violin plot gives pleasant images of the density of scores and highlights the difference
#in performance between these groups based off of the shape of the "violin". The female violin
#is shaped a bit more like a tall vase, while the male violin is shaped more like a yo-yo.
#This suggests that the male TM scores are more concentrated (around 36) and the academic scores are 
#more distributed than the female scores.


#This plot, however, gives the impression that male students have higher
#average test scores than female students. This is not the function of this graph, 
#however, as is shown by flipping the values of the x-axis.

aca_gender+geom_violin(scale="area", 
                     draw_quantiles = .5)+ 
  geom_point(size=3, 
             alpha=0.5)+
  scale_x_reverse(labels=indiv_test_labels)+
  labs(x="Academic Performance (%)",
       y="Time Management Score",
       title="Time Management and Academic Performance by Gender",
       subtitle="x-axis reversed)",
       caption="Lines in the violins divide the violin areas in half.")

#To get a better visualization of the difference in test scores between these two
#types of students, a different density plot may be better.

aca_gender+
  stat_density_2d(contour_var="ndensity", #The figure is computed based off of the density estimate scaled to 1. 
                 size=1, 
                 adjust=1.35)

#This plot nicely visualizes differences in test and time management scores between the
#two groups. The central density for non-business students is slightly higher in test
#scores, but the overall density extends further towards lower test scores. 
#It also appears that non-business students tend towards higher time management scores.

#These differences appear only slightly and have no statistical backing. This plot is also
#a bit too busy. It may be better to utilize this density plot to examine the distribution of 
#test and time management scores for students overall.

aca_gender+stat_density_2d_filled(contour_var="ndensity",
                                size=0.75, 
                                alpha=0.3,
                                geom="polygon", #Borrows the geom_polygon function which allows the figures to be filled with different colors
                               adjust=1.35)+ #Adjusts the contours of the bandwidth by 1.35 times.
  guides(fill=FALSE)+ #Removes unnecessary label.
  labs(x="Academic Performance (%)",
       y="Time Management Score",
       title="Time Management and Academic Performance
        for International Students",
       subtitle="Score Density for Males and Females")+
  theme(panel.background=element_blank(),#Removes background panels and adds axis lines.
        axis.line=element_line(lineend="round"))+
  ylim(20, 60)+
  coord_fixed(ratio=0.80)

