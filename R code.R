#####Scrapping data from Amazon########
# Install / Load relevant packages
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(rvest, dplyr, tidyr, stringr)
# product code
prod_code <- "B07DJD1RTM"
url <- paste0("https://www.amazon.in/Test-Exclusive-740/dp/", prod_code)
doc <- read_html(url)
#obtain the text in the node, remove "\n" from the text, and remove white space
prod <- html_nodes(doc, "#productTitle") %>% 
  html_text() %>% 
  gsub("\n", "", .) %>% 
  trimws()
prod
# Function to scrape elements from Amazon reviews
scrape_amazon <- function(url, throttle = 0){
  # Install / Load relevant packages
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(RCurl, XML, dplyr, stringr, rvest, purrr)
  # Set throttle between URL calls
  sec = 0
  if(throttle < 0) warning("throttle was less than 0: set to 0")
  if(throttle > 0) sec = max(0, throttle + runif(1, -1, 1))
  # obtain HTML of URL
  doc <- read_html(url)
  # Parse relevant elements from HTML
  comments <- doc %>%
    html_nodes(".a-expander-partial-collapse-content span") %>%
    html_text() 
  
  head <- doc %>%
    html_nodes(".a-text-bold span") %>%
    html_text() 
  gsub("\n\n \\s*|found this helpful.*", "", .) %>%
    gsub("One", "1", .) %>%
    map_chr(~ str_split(string = .x, pattern = " ")[[1]][1]) %>%
    as.numeric()
  # Combine attributes into a single data frame
  df <- data.frame(comments, stringsAsFactors = F)
  return(df)
}
scrape_amazon(url)
# load DT packege
pacman::p_load(DT)

# run scraper function
url <- "https://www.amazon.in/Test-Exclusive-740/dp/B07DJD1RTM/?pageNumber=1"
reviews <- scrape_amazon(url)
# display data
str(reviews)
# Set # of pages to scrape. Note: each page contains 8 reviews.
pages <- 100

# create empty object to write data into
reviews_all <- NULL

# loop over pages
for(page_num in 1:pages){
  url <- paste0("https://www.amazon.in/Test-Exclusive-740/dp/,prod_code,"/?pageNumber=", page_num)
  reviews <- scrape_amazon(url, throttle = 3)
  reviews_all <- rbind(reviews_all, cbind(prod, reviews))
}
str(reviews_all)

###clean function for data###

clean=function(tab,b)
{
    data=as.data.frame(tab)
    data$.names=as.numeric(data$row.names)
    data$stars=as.numeric(sapply(strsplit(as.character(data$stars),"\\ "),'[',1))
    data=na.omit(data)
    d=data.frame(Brand=rep(b,nrow(data)),review_id=1:nrow(data),c=data$comments,stars=data$stars)
    return(d)
    return(nrow(data))
}

#=====>> Importing data for all 21 phones
#===> Huwai

brand1="Huwai"
huwai8x=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
h1=clean(huwai8x,brand1)
nrow(h3)

huwai10lite=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
h2=clean(huwai10lite,brand1)
h2$review_id=h2$review_id+nrow(h1)

huwaiy9=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)

#View(huwaiy9)
h3=clean(huwaiy9,brand1)
h3$review_id=h3$review_id+1473

huwai=rbind(h1,h2,h3)
#View(huwai)

#=====>>Nokia
brand2="Nokia"
nokia6.1=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
n1=clean(nokia6.1,brand1)
n2$review_id=n2$review_id+nrow(n1)

nokia6.2=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
n2=clean(nokia6.2,brand2)

nokia7=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
n3=clean(nokia7,brand2)
n3$review_id=n3$review_id+nrow(n2)

nokia=rbind(n2,n3)
#View(nokia)


#=====>>Oppo
brand3="Oppo"
oppoA5=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
o1=clean(oppoA5,brand3)
View(o1)

oppoA5s=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
o2=clean(oppoA5s,brand3)
o2$review_id=o2$review_id+nrow(o1)

oppo83=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
o3=clean(oppo83,brand3)
o3$review_id=o3$review_id+999

oppo=rbind(o1,o2,o3)
#View(oppo)

#===>>Realme
brand4="Realme"
realme2pro=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
r1=clean(realme2pro,brand4)

realme3=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
r2=clean(realme3,brand4)
r2$review_id=r2$review_id+nrow(r1)

realmeu1=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
r3=clean(realmeu1,brand4)
r3$review_id=r3$review_id+1102

realme=rbind(r1,r2,r3)
#View(realme)

#===>>Redmi
brand5="Redmi"
redmi6pro=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
re1=clean(redmi6pro,brand5)

redmi8=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
re2=clean(redmi8,brand5)
re2$review_id=re2$review_id+nrow(re1)

redmiy2=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
re3=clean(redmiy2,brand5)
re3$review_id=re3$review_id+1303

redmi=rbind(re1,re2,re3)
#View(redmi)

#===>>Samsung
brand6="Samsung"
samsung20=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
s1=clean(samsung20,brand6)

samsung30=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
s2=clean(samsung30,brand6)
s2$review_id=s2$review_id+nrow(s1)

samsung30s=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
s3=clean(samsung30s,brand6)
s3$review_id=s3$review_id+1069

samsung=rbind(s1,s2,s3)
#View(samsung)

#===>>Vivo
brand7="Vivo"
vivou10=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
v1=clean(vivou10,brand7)

vivoy15=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
v2=clean(vivoy15,brand7)
v2$review_id=v2$review_id+nrow(v1)

vivo83=read.table(file.choose(),header=T,sep=",",row.names=NULL,fill=T)
v3=clean(vivo83,brand7)
v3$review_id=v3$review_id+1425
vivo=rbind(v1,v2,v3)
#View(vivo)
brand=rbind(huwai,nokia,oppo,realme,redmi,samsung,vivo)
View(brand)
nrow(brand)
table(brand$Brand)
names(brand)
#=======>>>
#===Data Cleaning & Seggregating======>>>

library(lexRankr)
brand$c=as.character(brand$c)
mapping=unnest_sentences_(brand,"sents","c")
View(mapping)
nrow(mapping)
library(tm)
mapping$c=gsub("[[:punct:]]"," ",mapping$sents)
mapping$c=tolower(gsub("\\d"," ",mapping$c))
mapping$c=removeWords(mapping$c,stopwords())
mapping$c=stripWhitespace(gsub("\\b[a-z]\\b{1}","",mapping$c))

#View(mapping)
#======>>
#======Sentimetal Score
library(sentimentr)
a=get_sentences(mapping$c)
score=sentiment(a)
head(score)
b=cbind(mapping,score[,3:4])
View(b)
#======>>
#========Corpus                       
mycorpus=Corpus(VectorSource(b$c))
dtm=TermDocumentMatrix(mycorpus)
m=as.matrix(dtm)
m[1:20,1:20]
v=sort(rowSums(m),decreasing=TRUE)
d=data.frame(word=names(v),freq=v)
nrow(d)
head(d,10)
#===>>>
#======Feature Extracting

camera=c("camera","quality","good","poor","lens","photos","picture","beautifull","pixel","cameras",
	"speedcamera","image","clear","camerabelow","image","clear","clarity","pixelated","photography",
	"agocamera","images","zoom","pictures","selfie","clicked","resolution","gcam","lights",
	"megapixels","natural","snappy","pic","focus","selfies","lifecamera","clicking","usercamera",
	"defocus","sensing","captures","blur","lenses","glossy","unclear","video calling")
data.frame(security)
charge=c("charge","plugged","charges","wire","good","active","better","hardly","breaker",
	"modebattery","die","charger","heating","heat","slow","charging","battery","time",
	"low","hours","heats","chargeprocessor","charged","heated","socket","slot",
	"goodcharging","chargingout","batteryhangs","batteryonly","overheating","dies",
	"chargebattery","plugged","cable","hang","tempering","week","average","bare")


hang=c("frequently","restart","laggy","poped","regularly","hectic","bare","annoying",
	"annoy","hang","slow","hangs","lags","hanged","issue")

service=c("problems","best","issues","satisfied","good","poor","life","solution",
	"repair","available","service","direspectful","center","issue")

software=c("problem","average","app","gamming","satisfied","good","poor","backandroid",
	"backup","features","worst","android","update","hardware","games","comfortable",
	"androiddone","restart","resolved","problem","stopped","reset","phoneapp","memory","stop",
	"install","update","accessible","application","repairs","upgrade","complaining","goodsoftware","games","software")

display_touch=c("screen","display","best","design","looks","good","touch","smooth",
	"speaker","lagdisplay","speed","greatlook","speaker","music","classdisplay","comfortable",
	"brightness","sliperry","gorilla","classic","brillianttouch","struggles","dual","stylish","speakers","slow","pretty"
	,"comfortable","touchinng","touches")

security=c("fingerprint","security","finger","goodfingerprint","camfinger","secure",
	"security","pattern","face lock","unlock","block","privacy")
#=>>CAMERA
s1=paste0(camera,collapse="|")
b$camera=ifelse(grepl(s1,b$c),1,0)
sum(b$camera)
length(b$camera)

#=>>CHARGE
s2=paste0(charge,collapse="|")
b$charge=ifelse(grepl(s2,b$c),1,0)
sum(b$charge)
length(b$charge)

#=>>>HANG
s3=paste0(hang,collapse="|")
b$hang=ifelse(grepl(s3,b$c),1,0)
sum(b$hang)
length(b$hang)

#=>>SERVICE
s4=paste0(service,collapse="|")
b$service=ifelse(grepl(s4,b$c),1,0)
sum(b$service)
length(b$service)

#=>>SOFTWARE
s5=paste0(software,collapse="|")
b$software=ifelse(grepl(s5,b$c),1,0)
sum(b$software)
length(b$software)

#=>>DISPLAY & TOUCH
s6=paste0(display_touch,collapse="|")
b$display.touch=ifelse(grepl(s3,b$c),1,0)
sum(b$display.touch)
length(b$display.touch)

#=>>SECURITY
s7=paste0(security,collapse="|")
b$security=ifelse(grepl(s7,b$c),1,0)
sum(b$security)
length(b$security)


#====>>>
#========Sentiment Score by Features

b$camera_score=b$sentiment*b$camera
b$charge_score=b$sentiment*b$charge
b$hang_score=b$sentiment*b$hang
b$service_score=b$sentiment*b$service
b$software_score=b$sentiment*b$software
b$display.touch_score=b$sentiment*b$display.touch
b$security_score=b$sentiment*b$security

#View(b)

d=round(b[,-c(1:6)],3)
head(d,10)
table(b$Brand)
table(brand$Brand)
#View(b)
names(b)

#===== NA for zero values

b$camera_score=ifelse(b$camera_score==0,NA,b$camera_score)
b$charge_score=ifelse(b$charge_score==0,NA,b$charge_score)
b$hang_score=ifelse(b$hang_score==0,NA,b$hang_score)
b$service_score=ifelse(b$service_score==0,NA,b$service_score)
b$software_score=ifelse(b$software_score==0,NA,b$software_score)
b$display.touch_score=ifelse(b$display.touch_score==0,NA,b$display.touch_score)
b$security_score=ifelse(b$security_score==0,NA,b$security_score)
View(b)


#######Aggregating

library(dplyr)
brand_mapping= data.frame(b %>%
			group_by(Brand) %>%
			summarise(camera=mean(camera_score,na.rm=T),
				    charge=mean(charge_score,na.rm=T),
				    hang=mean(hang_score,na.rm=T),
				    service=mean(service_score,na.rm=T),
				    software=mean(software_score,na.rm=T),
				    display=mean(display.touch_score,na.rm=T),
				    security=mean(security_score,na.rm=T)))
brand_mapping
bm=brand_mapping
fit=prcomp(bm[2:8],scale=F,center=T)
summary(fit)
plot(fit,col="brown",main="scree plot")
biplot(fit,cex=0.5,col=c("gray50","black"))
library(factoextra)
fviz_pca_biplot(fit,repel=T,pointsize=6,pointshape=21,col.var="red",
	arrowsize=0.6,labelsize=5,col.ind=bm$Brand)
#==============>>
#=======k means clustering
brand_mapping
kdata=data.frame(scale(brand_mapping[2:7],center=TRUE,scale=TRUE))
kdata
wilks_lambda=numeric(6)
for(i in 1:6)
{
set.seed(i)
fit=kmeans(kdata,i)
wilks_lambda[i]=sum(fit$withinss)/fit$totss
}
plot(1:6,wilks_lambda,type="b",xlab="Number of Clusters",ylab="ClusterWSS/TSS")

fit=kmeans(kdata,3)
dat=data.frame(brand_mapping[2:7])
aggregate(dat,by=list(fit$cluster),FUN=mean)
da=data.frame(dat,fit$cluster)
da
#===>Word cloud
write.table(d,file="review",sep=",")
library(tm)
library(wordcloud)
library(RColorBrewer)
text = "C:\\Users\\hp\\Documents\\sentimental\\text.txt"
txt = readLines(text)
p=gsub('[[:digit:]]+', '', txt)
q=tolower(gsub("\\d"," ",p))
r=gsub("[[:punct:]]"," ",q)
clean <- function(r){
r <-tolower(r)
r <-removeWords(r,stopwords('en'))
r <-removePunctuation(r)
r <-stripWhitespace(r)
return(r) }
clean(r)
modi<-Corpus(VectorSource(r))
modi
mo<-tm_map(modi,stripWhitespace)
mod<-tm_map(mo,tolower)
mo<-tm_map(mo,removeNumbers)
mo<-tm_map(mo,removePunctuation)
mo<-tm_map(mo,removeWords, stopwords("english"))
mo
ma=mo[1:10000]
ma<-tm_map(ma,removeWords,c("and","the","our","that","for","are","also","more","has","must","have","should","this","with"))
tdm_mod<-TermDocumentMatrix (ma) #Creates a TDM
TDM1<-as.matrix(tdm_mod) #Convert this into a matrix format
v = sort(rowSums(TDM1), decreasing = TRUE) #Gives you the frequencies for every word
Summary(v)
wordcloud (ma, scale=c(5,0.5), max.words=500, random.order=FALSE, 
rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))




