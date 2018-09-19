setwd("C:/Users/BJXY/Desktop/lcq/R")
library(xlsx)
source("jiou.R")
source("swap.R")
source("tiaojian.R")
source("chushi.R")
source("fenpei.R")

time.start <- 0
time.total <- 8*3600
startp <- 1
# ѡ����N��
cat("��ѡ��ڼ������ݣ�1~3��\n")
N <- scan("")# N <- c(1,2,3)
th <- matrix(0,nrow = 400,ncol = 5)
tt <- rep(0)

data <- tiaojian(N)
time.cnc <- data$cnc;time.rgv <- data$rgv
time.wash <- data$wash;th <- data$th
tt <- data$time;time.move <- data$move

cnc.id <- c(1,2,3,6,8,7,5,4)

for (i in 2:8) {
  th <- chushi(i,cnc.id)
}

result <- fenpei(th)
number <- result$num
th <- result$fn

cat("��",N,"�����8Сʱ�ӹ����",number,"��")
out <- th[1:number,1:4]
Rdata <- c("1.Rdata","2.Rdata","3.Rdata")
save(out,file=Rdata[N])

file <- c("data11.xls","data12.xls","data13.xls")
sheet <- c("��һ��","�ڶ���","������")
write.xlsx(out,file[N],sheetName = sheet[N],
col.names = F,row.names = F)
