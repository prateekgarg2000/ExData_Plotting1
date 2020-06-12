create_column_list<-function(col_name)
{
  col_list=list()
  j<-1
  k<-1
  for(i in 1:nchar(column_names))
  {
    if(substr(col_name,i,i)==".")
    {
      col_list[k]<-substr(col_name,j,i-1)
      k<-k+1
      j<-i+1
    }
  }
  col_list
}
create_data<-function(data,column_names)
{
  x<-data.frame(column_names)
  colnames(x)<-column_names
  k<-1
  for (i in 21997:24876)
  {
    ith<-strsplit(data[i,1],split = ";")
    x[k,]<-ith[[1]]
    k<-k+1
  }
  x
}
data<-read.delim("household_power_consumption.txt")
column_names<-names(data)
column_names<-create_column_list(column_names)
data<-create_data(data,column_names)
data[,1]<-as.Date(data[,1])
for(i in 3:8)
{
  data[,i]<-as.numeric(data[,i])
}
hist(data$Global_active_power,xlab = "Global Active Power (kilowatt)",main = "Global Active Power",col="red")
png("plot1.png")