library(dplyr)
library(ggplot2)
library(readr)
library(ggmap)
library(magrittr)
library(stringr)
library(stringdist)
library(tidyr)
library(lubridate)

#read data files
click<-read.table("ClickStreamData.txt",header=T, sep="\t", quote="\"",stringsAsFactors = FALSE)
click<-click[,1:7]
user<-read.table("/UserProfileData.txt",header=T, sep="\t", fill=T, quote="",stringsAsFactors = FALSE)
category<-read.table("ProductCategoryData.txt",header=T, sep="\t", fill=T, quote="",stringsAsFactors = FALSE)
#merge files

#perform outer join
df<-full_join(click,user,by="User.ID")
# add category
df$category<-NULL

#populate category
#clean the data
newdf<-df%>%mutate(category=ifelse(URL=="https://www.ideatory-store.com/","books",
                          ifelse(URL=="https://www.ideatory-store.com/SH51443900/VD21190582","movies",
                                 ifelse(URL=="https://www.ideatory-store.com/SH51443900/VD66504762","games",
                                        ifelse(URL=="https://www.ideatory-store.com/SH51443900/VD90147750","electronics",
                                               ifelse(URL=="https://www.ideatory-store.com/SH51443900/VD96818936","computers",
                                                      ifelse(URL=="https://www.ideatory-store.com/SH51443900/VD61965752","home&garden",
                                                             ifelse(URL=="https://www.ideatory-store.com/SH51443900/VD50250356","handbags",
                                                                    ifelse(URL=="https://www.ideatory-store.com/SH51443900/VD30782423","clothing",
                                                                           ifelse(URL=="https://www.ideatory-store.com/SH51443900/VD05732995","shoes",
                                                                                  ifelse(URL=="https://www.ideatory-store.com/SH95413515/VD22615851","outdoors",
                                                                                         ifelse(URL=="https://www.ideatory-store.com/SH71385357/VD84093603","automotive",
                                                                                                ifelse(URL=="https://www.ideatory-store.com/SH79068897/VD43931037","clothing",
                                                                                                       ifelse(URL=="https://www.ideatory-store.com/SH79068897/VD33761081","tools",
                                                                                                              ifelse(URL=="https://www.ideatory-store.com/SH29978718/VD61428707","accessories",
                                                                                                                     ifelse(URL=="https://www.ideatory-store.com/SH87807518/VD32519266","grocery",
                                                                                                                            ifelse(URL=="https://www.ideatory-store.com/SH87807518/VD50647383","clothing",
                                                                                                                                   ifelse(URL=="https://www.ideatory-store.com/SH28141648/VD75236732","clothing",
                                                                                                                                          ifelse(URL=="https://www.ideatory-store.com/SH28141648/VD18727364","clothing",
                                                                                                                                                 ifelse(URL=="https://www.ideatory-store.com/SH94844021/VD39965501","clothing",
                                                                                                                                                        ifelse(URL=="https://www.ideatory-store.com/SH94844021/VD47053396","shoes",
                                                                                                                                                               ifelse(URL=="https://www.ideatory-store.com/SH94844021/VD33046844","shoes",
                                                                                                                                                                      ifelse(URL=="https://www.ideatory-store.com/SH94844021/VD23829396","shoes",
                                                                                                                                                                             ifelse(URL=="https://www.ideatory-store.com/SH94844021/VD56960204","shoes",
                                                                                                                                                                                    ifelse(URL=="https://www.ideatory-store.com/SH94844021/VD72293071","shoes",
                                                                                                                                                                                           ifelse(URL=="https://www.ideatory-store.com/SH81099722/VD38327020","handbags",
                                                                                                                                                                                                  ifelse(URL=="https://www.ideatory-store.com/SH81099722/VD86857969","handbags",
                                                                                                                                                                                                         ifelse(URL=="https://www.ideatory-store.com/SH71795094/VD85353798","handbags",
                                                                                                                                                                                                                ifelse(URL=="https://www.ideatory-store.com/SH92865249/VD04293136","handbags",
                                                                                                                                                                                                                       ifelse(URL=="https://www.ideatory-store.com/SH78531697/VD10477648","handbags",
                                                                                                                                                                                                                              ifelse(URL=="https://www.ideatory-store.com/SH78531697/VD65915990","handbags",
                                                                                                                                                                                                                                     ifelse(URL=="https://www.ideatory-store.com/SH78531697/VD99549384","handbags",NA
                                                                                                                                                                                                                                     ))))))))))))))))))))))))))))))))



# plot the data
#remove NA rows
newdf=newdf[!is.na(newdf$category),]

#plot the data
g1<-ggplot(newdf,aes(category))+geom_bar()+coord_flip()
ggsave(g1,file="bounce1.png", width=5, height=5)

df<-newdf%>%select(category)%>%group_by(category)%>%summarise(n=n())
df%<>%mutate(normal=ifelse(category=="handbags",n/8,
                          ifelse(category=="shoes",n/6,
                                 ifelse(category=="clothing",n/6,n))))
#reoder the factors
df=transform(df,category=reorder(category,-normal))
g2<-ggplot(df,aes(category,normal))+geom_bar(stat="identity")+coord_flip() +ylab("count") +
  ggtitle("normalized count by number of links")
ggsave(g2,file="bounce2.png", width=5, height=5)


#
x<-newdf%>%select(category,User.ID)%>%
  group_by(User.ID,category)%>%summarise(visited=n())%>%mutate(unique=ifelse(visited==1,"unique","non-unique"))

ggplot(x,aes(unique))+geom_bar()

#extract hour and day information from Timestamp column in the data
newdf$Timestamp<-dmy_hm(newdf$Timestamp)
newdf$dayofweek<-wday(newdf$Timestamp,label=TRUE)
newdf$hour<-hour(newdf$Timestamp)
newdf<-newdf[!is.na(newdf$hour),]

#plot the data
g3<-ggplot(newdf,aes(hour))+geom_bar(aes(fill=category))+facet_grid(~dayofweek) + 
  xlab("hour of the day")
ggsave(g3,file="bounce3.pdf")


# Create a counts table for the weekday and hour:
table(newdf$dayofweek,newdf$hour)
# Save this to a data frame:
DayHourCounts = as.data.frame(table(newdf$hour,newdf$dayofweek))
str(DayHourCounts)

# Convert the second variable, Var2, to numbers and call it Hour:
DayHourCounts$Hour = as.numeric((DayHourCounts$Var2))

# Create out plot:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1))


g6<-ggplot(DayHourCounts, aes(x = Var1, y = Var2)) + 
  geom_tile(aes(fill = Freq)) + 
  scale_fill_gradient(name="Count", low="white", high="red") + 
  theme(axis.title.y = element_blank())+xlab("hour of the day")
ggsave(g6,file="bounce6.png")

uniquesByDay<-newdf%>%filter(!is.na(dayofweek))%>%group_by(User.ID)%>%group_by(dayofweek)%>%summarize(uniques=n_distinct(User.ID))

#plot unique visits
ggplot(uniquesByDay,aes(dayofweek,uniques))+geom_line(aes(group=1))

#depth of user engagement
newdf$URL<-as.factor(newdf$URL)
visits<-newdf%>%select(Timestamp,URL,User.ID)%>%
  group_by(User.ID)%>%summarise(number_of_events=n(),distinct_pages_visited=n_distinct(URL))


#user engagement
#number of uniques over time
g4<-ggplot(uniquesByDay,aes(dayofweek,uniques))+geom_bar(stat='identity') 
ggsave(g4,file="bounce4.png", width=5, height=5)

g5<-ggplot(visits,aes(factor(distinct_pages_visited), number_of_events)) + 
         geom_boxplot() + scale_y_continuous(limits=c(0,500))
ggsave(g5,file="bounce5.png", width=5, height=5)


#association rules
df<-newdf%>%select(College.Education,City,Number.of.friends,category)
df%<>%ungroup()
df<-as.data.frame(df)
class(df)

df<-df[!is.na(df$category),]
df<-df[!is.na(df$College.Education),]
df<-df[!is.na(df$Number.of.friends),]

df$Number.of.friends<-as.factor(df$Number.of.friends)
df$City<-as.factor(df$City)
df$College.Education<-as.factor(df$College.Education)
df$category<-as.factor(df$category)
df%<>%select(-City)
df%<>%select(-category)

#load packages 
#
library(arules)
library(arulesViz)

#find the rules
rules <- apriori(df, parameter = list(minlen=1, supp=0.001, conf=0.4, maxlen=4),
             appearance = list(rhs=c("College.Education=No", "College.Education=Yes"),
                  default="lhs"),
                  control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

plot(rules,method="grouped")
plot(rules,method="paracoord")

pdf("bounce4.pdf")
plot(rules, method="graph", control=list(type="items"))
dev.off()

df<-select(newdf,User.ID,category)%>%
  group_by(User.ID,category)%>%
  summarize(n=n())%>%filter(n>10) %>% 
  select(-n)%>%ungroup()

df$category<-as.factor(df$category)
df=df[!is.na(df$category),]
df%<>%spread(User.ID,category)
df<-newdf %>% select(User.ID,category)%>%group_by(User.ID,category)%>%unique()%>%ungroup()
df=df[!is.na(df$category),]

df%<>%ungroup()
df<-as.data.frame(df)

#item frequency plots
trans <- as(split(df$category, df$User.ID), "transactions")
str(trans)

rules<-apriori(trans,parameter=list(minlen=1,support=0.001, confidence=0.5))
inspect(head(sort(rules, by="lift"),3));

plot(rules);

head(quality(rules));
plot(rules, measure=c("support","lift"), shading="confidence");
plot(rules, shading="order", control=list(main ="Two-key plot"));
sel = plot(rules, measure=c("support","lift"), shading="confidence", interactive=TRUE);
subrules = rules[quality(rules)$confidence > 0.8];
subrules

plot(subrules, method="matrix", measure="lift");
plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE));
plot(subrules, method="matrix3D", measure="lift");
plot(subrules, method="matrix3D", measure="lift", control = list(reorder=TRUE));
plot(subrules, method="matrix", measure=c("lift", "confidence"));
plot(subrules, method="matrix", measure=c("lift","confidence"), control = list(reorder=TRUE));
plot(rules, method="grouped");

png("bounce10.eps")
plot(rules, method="grouped", control=list(k=50));
dev.off()

sel = plot(rules, method="grouped", interactive=TRUE);

subrules2 = head(sort(rules, by="lift"), 30);
plot(subrules2, method="graph");
png("bounce9.png")
plot(subrules2, method="graph", control=list(type="items"));
dev.off()

plot(subrules2, method="paracoord");

png("bounce8.png")
plot(subrules2, method="paracoord", control=list(reorder=TRUE));
dev.off()

plot(subrules2, method="grouped",control=list(k=50));

oneRule = sample(rules, 1);

inspect(oneRule);
itemFrequencyPlot(df, support = 0.06, cex.names=0.8);
itemFrequencyPlot(df, support = 0.1, cex.names=0.8);

fsets = eclat(trans, parameter = list(support = 0.05), control = list(verbose=FALSE));
singleItems = fsets[size(items(fsets)) == 1];
singleSupport = quality(singleItems)$support;
names(singleSupport) = unlist(LIST(items(singleItems), decode = FALSE));

head(singleSupport, n = 5);
itemsetList = LIST(items(fsets), decode = FALSE);
allConfidence = quality(fsets)$support / sapply(itemsetList, function(x)
  max(singleSupport[as.character(x)]));
quality(fsets) = cbind(quality(fsets), allConfidence);
summary(fsets);

#spatial plots of users visit rates
#
geocodes <- geocode(as.character(mydat$cities))

g7<-ggplot(newmap, aes(x = long, y = lat, group = group, fill = Rate)) + geom_polygon(color = "black") + 
  scale_fill_gradient(low = "black", high = "red", guide = "legend")
ggsave(g7,file="bounce7.png")

