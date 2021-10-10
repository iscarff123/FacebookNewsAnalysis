
FB <- read.csv("FacebookData.csv")

library(lubridate)

FB$posted_at <- ymd_hms(FB$posted_at,tz = "America/New_York")

FB[23] <- year(FB$posted_at)
colnames(FB)[23] <- "year"


library(stringr)

FB$name <- tolower(FB$name)

FB <- FB[which(!str_detect(FB$name, "trump")),]
FB <- FB[which(!str_detect(FB$name, "clinton ")),]
FB <- FB[which(!str_detect(FB$name, "obama")),]
FB <- FB[which(!str_detect(FB$name, "romney")),]

FB <- FB[,-c(2,19,20)]
FB[21] <- month(FB$posted_at)
colnames(FB)[21] <- "month"

FB$year <- as.factor(FB$year)
FB$month <- month.name[FB$month]
FB$month <- as.factor(FB$month)




##################


FB4 <- read.csv("FacebookDataReduced.csv")

cnn <- FB4[FB4$Source == "CNN",]
abc <- FB4[FB4$Source == "ABC_NEWS",]
fox <- FB4[FB4$Source == "FOX_NEWS",]
huff <- FB4[FB4$Source == "HUFF_POST",]


final <- rbind(cnn,abc, fox, huff)

final <- droplevels(final)

final$name <- as.character(final$name)
final <- final[final$name != "null",]



write.csv(final, file = "4Sources.csv", row.names = F)


f <- read.csv("4Sources.csv")

f$posted_at <- as.POSIXct(f$posted_at, format = "%Y-%m-%d %H:%M:%OS")


f$posted_at <- format(f$posted_at, tz = "America/New_York")


write.csv(f, file = "4Sources2.csv", row.names = F)





