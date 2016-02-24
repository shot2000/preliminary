#Remove all previous data
rm(list=ls(all=TRUE))

# read data into R
df<-read.table("C:/20090930/904851_001_002.dat",header=T,sep=",")
# remove the final raw of data
b <- df[-nrow(df),]
# Subset 1500 data points for this test only
b <- b[1:1500,]
#converting speed from kmh to mph
names(b)[3]<-paste("SpeedKMH") 
b$SpeedMPH <- b$SpeedKMH*0.621371    
b$SpeedMPS <- b$SpeedKMH*0.277778   
# Remove HDOP higher than 4, and NSAT less than 4
#c<-b[!(b$Number.Of.Satellites<4 | b$HDOP>4),]
# Conditions to remove starting empty GPS data points with speed equals to zero (look into next 5 non-zero speed data points)
for(i in 1:length(b$SpeedKMH)) {
	if (b$SpeedKMH[i] > 0 & b$SpeedKMH[i+1] > 0 & b$SpeedKMH[i+2] > 0 & b$SpeedKMH[i+3] > 0 & b$SpeedKMH[i+4] > 0) {
		print(i)
		n<- i
		break
		}
	}
c <- b[n:nrow(b),]

# Calculate the acceleration in metres per second
c$Acc <- 0 
c$Acc[1] <- 0 
for( i in 2:nrow(b) ){ 
        c$Acc[i] <- c$SpeedMPS[i] - c$SpeedMPS[i-1] 
} 

# Calculate the VSP
c$VSP <- 0.3048 * c$SpeedMPS * (1.1 * 0.3048 * c$Acc + 0.132)+ 0.000302 * (0.3048 * c$SpeedMPS) ^ 3

# Calculate the VSP Distribution by bins
c$opModeID <- 0
for( i in 1:nrow(c) ){ 
        c$opModeID  <- ifelse(c$SpeedMPH>=-1 & c$SpeedMPH<1,1,
		ifelse(c$VSP<0  & c$SpeedMPH>= 1 & c$SpeedMPH<25,11,
		ifelse(c$VSP>=0  & c$VSP<3 & c$SpeedMPH>= 0 & c$SpeedMPH<25,12,
		ifelse(c$VSP>=3  & c$VSP<6 & c$SpeedMPH>= 0 & c$SpeedMPH<25,13,
		ifelse(c$VSP>=6  & c$VSP<9 & c$SpeedMPH>=0 & c$SpeedMPH<25,14,
		ifelse(c$VSP>=9  & c$VSP<12 & c$SpeedMPH>= 0 & c$SpeedMPH<25,15,
		ifelse(c$VSP>=12 & c$SpeedMPH>= 0 & c$SpeedMPH<25,16,
		ifelse(c$VSP<=0 & c$SpeedMPH>= 25 & c$SpeedMPH<50,21,
		ifelse(c$VSP>=0 & c$VSP<3 & c$SpeedMPH>= 25 & c$SpeedMPH<50,22,
		ifelse(c$VSP>=3 & c$VSP<6 & c$SpeedMPH>= 25 & c$SpeedMPH<50,23,
		ifelse(c$VSP>=6 & c$VSP<9 & c$SpeedMPH>= 25 & c$SpeedMPH<50,24,
		ifelse(c$VSP>=9 & c$VSP<12 & c$SpeedMPH>= 25 & c$SpeedMPH<50,25,
		ifelse(c$VSP>=12 & c$VSP<18 & c$SpeedMPH>= 25 & c$SpeedMPH<50,27,
		ifelse(c$VSP>=18 & c$VSP<24 & c$SpeedMPH>= 25 & c$SpeedMPH<50,28,
		ifelse(c$VSP>=24 & c$VSP<30 & c$SpeedMPH>= 25 & c$SpeedMPH<50,29,
		ifelse(c$VSP<=30 & c$SpeedMPH>= 25 & c$SpeedMPH<50,30,
		ifelse(c$VSP<6 & c$SpeedMPH>50,33,
		ifelse(c$VSP>=6 & c$VSP<12 & c$SpeedMPH>50,35,
		ifelse(c$VSP>=12 & c$VSP<18 & c$SpeedMPH>50,37,
		ifelse(c$VSP>=18 & c$VSP<24 & c$SpeedMPH>50,38,
		ifelse(c$VSP>=24 & c$VSP<30 & c$SpeedMPH>50,39,
		ifelse(c$VSP<=30 & c$SpeedMPH>50,40,0))))))))))))))))))))))} 



