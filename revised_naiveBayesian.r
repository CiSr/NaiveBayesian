#Reading the data
data = read.csv("/home/sridharan/Naive.csv")
data

ncols = ncol(data)
nrows = nrow(data)


Output = table(data[,5])

#Satisfying the naive assumption
#Extracting the independent features


OutLook=subset(data, select=c(outlook,play))
Temp=subset(data, select=c(temp,play))
humidity=subset(data,select=c(humidity, play))
windy=subset(data, select=c(windy,play))


#Model built
Outlook=table(OutLook)
Temp=table(Temp)
humidity=table(humidity)
windy=table(windy)
class=table(class)




GenericFeature<-function(feature_value,class, feature_name)
{
	
		result=1
	{	regex=feature_value
		tmp=grepl(pattern=regex,x=row.names(feature_name))
		if(any(tmp=="TRUE"))
		{
		result*result*feature_name[regex,class]/5
		}
	}

  		
}


calculate<-function(a,b,c,d)
{
	temp=a*b*c*d
	final_temp=(temp*5)/nrows

}


#Test model
FullGenericOutlook<-GenericFeature("sunny", "no",Outlook)
FullGenericTemperature<-GenericFeature("cool", "no",Temp)
FullGenerichumidity<-GenericFeature("high", "no",humidity)
FullGenericwindy<-GenericFeature("t", "no",windy)
Predict_Class_No<-calculate(FullGenericwindy,FullGenerichumidity,FullGenericTemperature,FullGenericOutlook)


FullGenericOutlookyes<-GenericFeature("sunny", "yes",Outlook)
FullGenericTemperatureyes<-GenericFeature("cool", "yes",Temp)
FullGenerichumidityyes<-GenericFeature("high", "yes",humidity)
FullGenericwindyyes<-GenericFeature("t", "yes",windy)
Predict_Class_Yes<-calculate(FullGenericwindyyes,FullGenerichumidityyes,FullGenericTemperatureyes,FullGenericOutlookyes)


if(min(Predict_Class_Yes, Predict_Class_No)==Predict_Class_No)
{
	preidict_class_label=" The class belongs to is: No"
	preidict_class_label
	Predict_Class_No
}

if(min(Predict_Class_Yes, Predict_Class_No)==Predict_Class_Yes)
{
	preidict_class_label="Yes"
	preidict_class_label
}
