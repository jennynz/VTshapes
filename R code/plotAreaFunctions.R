# Plotting area functions (a.k.a. vocal tract shape) derived from MRI images.

# Area functions are compared within speakers for different vowels, and across speakers for all vowels, as well as subgroups such as high, low, front and back.
# Specific to the current file structure in project directory.
# Assumes one set only. See the original file plottingDanielsMRIdata.txt to use both sets.

# Adapted by Jenny Sahng
# 01/08/2016



rm(list=ls()) # clear workspace
graphics.off() # close all graphics windows

path<<-"H:\\Documents\\Part IV Project\\All VT data"

"maxVTvals"<-function(spk="VT03")
{
	Dirpath=paste(path,spk,"distance_area",sep="\\")
	filesInDir=dir(Dirpath)
	alldat=NULL
	for(i in 1:length(filesInDir))
	{
		filepath=paste(Dirpath,filesInDir[i],sep="\\")
		datfile=read.table(filepath)
		alldat=rbind(alldat,datfile)
	}
	return(apply(alldat,2,max))
}

"plotMRI"<-function(spk="VT03",vow="had",col="red",xlim=c(0,180),ylim=c(0,250))
{
	fullpath=paste(path,spk,"distance_area",paste(vow,"txt",sep="."),sep="\\")
	datfile=read.table(fullpath)
	plot(datfile[,1],datfile[,2],type="l",col=col,xlim=xlim,ylim=ylim,xlab="distance from lips",ylab="cross-sectional area")
}

"plotMRI.all"<-function(spk="VT03",xlim=c(0,175),ylim=c(0,510))
{
	
	plotMRI(vow="heed",col="red",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="hid",col="maroon",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="head",col="orange",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="had",col="yellow",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="hud",col="green1",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="hard",col="green4",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="hod",col="aquamarine3",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="hoard",col="aquamarine1",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	if(spk != "VT01")
	{
		plotMRI(vow="hood",col="blue4",xlim=xlim,ylim=ylim,spk=spk)
		par(new=T)
	}
	plotMRI(vow="whod",col="blue1",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="herd",col="purple",xlim=xlim,ylim=ylim,spk=spk)
	title(paste("All vowels for ",spk))
}


"plotMRI.front"<-function(spk="VT03",xlim=c(0,175),ylim=c(0,510))
{	
	plotMRI(vow="heed",col="red",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="hid",col="maroon",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="head",col="orange",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="had",col="yellow",xlim=xlim,ylim=ylim,spk=spk)
	title(paste("Front vowels for ",spk))
}


"plotMRI.back"<-function(spk="VT03",xlim=c(0,175),ylim=c(0,510))
{
	plotMRI(vow="hud",col="green1",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="hard",col="green4",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="hod",col="aquamarine3",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="hoard",col="aquamarine1",xlim=xlim,ylim=ylim,spk=spk)
	if(spk != "VT01")
	{
		par(new=T)
		plotMRI(vow="hood",col="blue4",xlim=xlim,ylim=ylim,spk=spk)
	}
	title(paste("Back vowels for ",spk))
}


"plotMRI.mid"<-function(spk="VT03",xlim=c(0,175),ylim=c(0,510))
{
	plotMRI(vow="hid",col="maroon",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)	
	if(spk != "VT01")
	{
		plotMRI(vow="hood",col="blue4",xlim=xlim,ylim=ylim,spk=spk)
		par(new=T)
	}
	plotMRI(vow="whod",col="blue1",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="herd",col="purple",xlim=xlim,ylim=ylim,spk=spk)
	title(paste("Mid vowels for ",spk))
}


"plotMRI.high"<-function(spk="VT03",xlim=c(0,175),ylim=c(0,510))
{
	plotMRI(vow="heed",col="red",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="hoard",col="aquamarine1",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="whod",col="blue1",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	title(paste("High vowels for ",spk))
}

"plotMRI.cen"<-function(spk="VT03",xlim=c(0,175),ylim=c(0,510))
{
	plotMRI(vow="hid",col="maroon",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="head",col="orange",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="had",col="yellow",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="herd",col="purple",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	plotMRI(vow="hud",col="green1",xlim=xlim,ylim=ylim,spk=spk)
	par(new=T)
	title(paste("Central vowels for ",spk))
}


## Maximum vocal tract values

maxVTvals(spk="VT01")
maxVTvals(spk="VT02")
maxVTvals(spk="VT03")
maxVTvals(spk="VT04")
maxVTvals(spk="VT05")
maxVTvals(spk="VT06")
maxVTvals(spk="VT07")
maxVTvals(spk="VT08")
maxVTvals(spk="VT09")
maxVTvals(spk="VT10")
maxVTvals(spk="VT11")
maxVTvals(spk="VT12")


## Front Vowels

dev.new()
par(mfrow=c(3,4),lwd=1)
plotMRI.front(spk="VT01",xlim=c(0,190),ylim=c(0,650))
plotMRI.front(spk="VT02",xlim=c(0,190),ylim=c(0,650))
plotMRI.front(spk="VT03",xlim=c(0,175),ylim=c(0,510))
plotMRI.front(spk="VT04",xlim=c(0,175),ylim=c(0,510))
plotMRI.front(spk="VT05",xlim=c(0,155),ylim=c(0,400))
plotMRI.front(spk="VT06",xlim=c(0,190),ylim=c(0,650))
plotMRI.front(spk="VT07",xlim=c(0,190),ylim=c(0,650))
plotMRI.front(spk="VT08",xlim=c(0,160),ylim=c(0,640))
plotMRI.front(spk="VT09",xlim=c(0,205),ylim=c(0,780))
plotMRI.front(spk="VT10",xlim=c(0,190),ylim=c(0,650))
plotMRI.front(spk="VT11",xlim=c(0,190),ylim=c(0,650))
plotMRI.front(spk="VT12",xlim=c(0,195),ylim=c(0,890))


## Back vowels

dev.new()
par(mfrow=c(3,4),lwd=1)
plotMRI.back(spk="VT01",xlim=c(0,190),ylim=c(0,650))
plotMRI.back(spk="VT02",xlim=c(0,190),ylim=c(0,650))
plotMRI.back(spk="VT03",xlim=c(0,175),ylim=c(0,510))
plotMRI.back(spk="VT04",xlim=c(0,175),ylim=c(0,510))
plotMRI.back(spk="VT05",xlim=c(0,155),ylim=c(0,400))
plotMRI.back(spk="VT06",xlim=c(0,190),ylim=c(0,650))
plotMRI.back(spk="VT07",xlim=c(0,190),ylim=c(0,650))
plotMRI.back(spk="VT08",xlim=c(0,160),ylim=c(0,640))
plotMRI.back(spk="VT09",xlim=c(0,205),ylim=c(0,780))
plotMRI.back(spk="VT10",xlim=c(0,190),ylim=c(0,650))
plotMRI.back(spk="VT11",xlim=c(0,190),ylim=c(0,650))
plotMRI.back(spk="VT12",xlim=c(0,195),ylim=c(0,890))


# Mid vowels

dev.new()
par(mfrow=c(3,4),lwd=1)
plotMRI.mid(spk="VT01",xlim=c(0,190),ylim=c(0,650))
plotMRI.mid(spk="VT02",xlim=c(0,190),ylim=c(0,650))
plotMRI.mid(spk="VT03",xlim=c(0,175),ylim=c(0,510))
plotMRI.mid(spk="VT04",xlim=c(0,175),ylim=c(0,510))
plotMRI.mid(spk="VT05",xlim=c(0,155),ylim=c(0,400))
plotMRI.mid(spk="VT06",xlim=c(0,190),ylim=c(0,650))
plotMRI.mid(spk="VT07",xlim=c(0,190),ylim=c(0,650))
plotMRI.mid(spk="VT08",xlim=c(0,160),ylim=c(0,640))
plotMRI.mid(spk="VT09",xlim=c(0,205),ylim=c(0,780))
plotMRI.mid(spk="VT10",xlim=c(0,190),ylim=c(0,650))
plotMRI.mid(spk="VT11",xlim=c(0,190),ylim=c(0,650))
plotMRI.mid(spk="VT12",xlim=c(0,195),ylim=c(0,890))


## High Vowels

dev.new()
par(mfrow=c(3,4),lwd=1)
plotMRI.high(spk="VT01",xlim=c(0,190),ylim=c(0,650))
plotMRI.high(spk="VT02",xlim=c(0,190),ylim=c(0,650))
plotMRI.high(spk="VT03",xlim=c(0,175),ylim=c(0,510))
plotMRI.high(spk="VT04",xlim=c(0,175),ylim=c(0,510))
plotMRI.high(spk="VT05",xlim=c(0,155),ylim=c(0,400))
plotMRI.high(spk="VT06",xlim=c(0,190),ylim=c(0,650))
plotMRI.high(spk="VT07",xlim=c(0,190),ylim=c(0,650))
plotMRI.high(spk="VT08",xlim=c(0,160),ylim=c(0,640))
plotMRI.high(spk="VT09",xlim=c(0,205),ylim=c(0,780))
plotMRI.high(spk="VT10",xlim=c(0,190),ylim=c(0,650))
plotMRI.high(spk="VT11",xlim=c(0,190),ylim=c(0,650))
plotMRI.high(spk="VT12",xlim=c(0,195),ylim=c(0,890))


## All vowels

dev.new()
par(mfrow=c(3,4),lwd=1)
plotMRI.all(spk="VT01",xlim=c(0,190),ylim=c(0,650))
plotMRI.all(spk="VT02",xlim=c(0,190),ylim=c(0,650))
plotMRI.all(spk="VT03",xlim=c(0,175),ylim=c(0,510))
plotMRI.all(spk="VT04",xlim=c(0,175),ylim=c(0,510))
plotMRI.all(spk="VT05",xlim=c(0,155),ylim=c(0,400))
plotMRI.all(spk="VT06",xlim=c(0,190),ylim=c(0,650))
plotMRI.all(spk="VT07",xlim=c(0,190),ylim=c(0,650))
plotMRI.all(spk="VT08",xlim=c(0,160),ylim=c(0,640))
plotMRI.all(spk="VT09",xlim=c(0,205),ylim=c(0,780))
plotMRI.all(spk="VT10",xlim=c(0,190),ylim=c(0,650))
plotMRI.all(spk="VT11",xlim=c(0,190),ylim=c(0,650))
plotMRI.all(spk="VT12",xlim=c(0,195),ylim=c(0,890))


## Centre vowels

dev.new()
par(mfrow=c(3,4),lwd=1)
plotMRI.cen(spk="VT01",xlim=c(0,190),ylim=c(0,650))
plotMRI.cen(spk="VT02",xlim=c(0,190),ylim=c(0,650))
plotMRI.cen(spk="VT03",xlim=c(0,175),ylim=c(0,510))
plotMRI.cen(spk="VT04",xlim=c(0,175),ylim=c(0,510))
plotMRI.cen(spk="VT05",xlim=c(0,155),ylim=c(0,400))
plotMRI.cen(spk="VT06",xlim=c(0,190),ylim=c(0,650))
plotMRI.cen(spk="VT07",xlim=c(0,190),ylim=c(0,650))
plotMRI.cen(spk="VT08",xlim=c(0,160),ylim=c(0,640))
plotMRI.cen(spk="VT09",xlim=c(0,205),ylim=c(0,780))
plotMRI.cen(spk="VT10",xlim=c(0,190),ylim=c(0,650))
plotMRI.cen(spk="VT11",xlim=c(0,190),ylim=c(0,650))
plotMRI.cen(spk="VT12",xlim=c(0,195),ylim=c(0,890))

