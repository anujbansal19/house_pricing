df=read.csv("put your own file location of house pricing",header=TRUE)
str(df)

summary(df)

hist(df$crime_rate)
#pairs(~price+crime_rate+n_hot_rooms+rainfall,data=df)
barplot(table(df$waterbody),border = FALSE)

uv=3*quantile(df$n_hot_rooms,0.99)

df$n_hot_rooms[df$n_hot_rooms>uv]<-uv
summary(df$n_hot_rooms)

lv=0.95*quantile(df$rainfall,0.01)
df$rainfall[df$rainfall<quantile(df$rainfall,0.01)]=lv
summary(df$rainfall)

mean(df$n_hos_beds)
mean(df$n_hos_beds,na.rm=TRUE)
which(is.na(df$n_hos_beds))
df$n_hos_beds[is.na(df$n_hos_beds)]=mean(df$n_hos_beds,na.rm=TRUE)

summary(df)


pairs(~price+crime_rate,data=df)
plot(df$price,df$crime_rate)

df$crime_rate=log(1+df$crime_rate)
plot(df$price,df$crime_rate)

df$avg_dist=(df$dist1+df$dist2+df$dist3+df$dist4)/4
plot(df$price,df$avg_dist)

df2=df[,-7:-10]
df=df2
rm(df2)

df=df[,-14]
df=dummy_cols(df)
df=df[,-16]
df=df[,-19]


simple_model=lm(price~room_num,data=df)
summary(simple_model)

plot(df$room_num,df$price)
abline(simple_model)

mul_model=lm(price~.,data = df)
summary(mul_model)
