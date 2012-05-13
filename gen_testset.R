#!/home/parkx408/root/bin/Rscript --verbose
#!/usr/bin/Rscript --verbose

rm(list=ls());

K <- 1024;
pct <- 100;

data <-read.table("rosi_data/basic_8_120s.out", header=T)
#ds.names <- c("emc-3disk-raid0", "DASStore", "srs7diskfc_netapp", "netapp-3diskata-sp");
ds.names <- c("emc-3disk-raid0", "emc-6diskfc-sp", "srs7diskfc_netapp", "netapp-3diskata-sp");
ds.names.new <- c("E1", "E2", "N2", "N1");

data<-data[which(as.character(data$datastore) %in% ds.names), ];
size<-unique(data$size);
read<-unique(data$read);
random<-unique(data$random);
oio<-unique(data$oio);
ds<-unique(data$datastore);

res<-data.frame();
print ("lq slope calculation");
for (i in size) {
	for (j in read) {
		for (k in random) {
			for (l in ds) {
				lq.data <- data[data$size == i 
						& data$read == j
						& data$random == k
						& data$datastore == l, ];
				lq.fit <- lm(lq.data$latency ~ lq.data$oio);
				lq.fit.sum <- summary(lq.fit);
				lq.data$lq_slope<-lq.fit$coefficient[2];
				lq.data$lq_slope_p<-lq.fit.sum$coefficient[2,4];
				lq.data$lq_intercept<-lq.fit$coefficient[1];
				lq.data$lq_intercept_p<-lq.fit.sum$coefficient[1,4];
				lq.data$cod<-lq.fit.sum$r.squared;
				res<-rbind(res, lq.data);
			}
		}
	}
}

print ("W calculation");
data<-data.frame();
for (i in size) {
	for (j in read) {
		for (k in random) {
			for (l in oio) {
				basil.data <-res[res$size == i
            & res$read == j
            & res$random == k
            & res$oio == l, ];
				basil.data$basil.W <- (l+2)*(i/K+20)*(j/pct+0.05)*(k/pct+0.4);
				data<-rbind(data, basil.data);
			}
		}
	}
}				

print ("Sort data in to Romano sequence");
res<-data;
res<-res[order(res$oio),];
res<-res[order(res$size),];
res<-res[order(res$read),];
res<-res[order(res$random),];
res<-res[order(res$datastore),];

fn.get.key.var<-function (sum) {
  ps<-sum$coefficients[ ,"Pr(>|t|)"];
  kps<-ps[ps<0.05];
	if ("(Intercept)" %in% names(kps)) {
		kps<-kps[-which(names(kps)=="(Intercept)")];
  }
	return (kps);
}

fn.get.split.var<-function (sum) {
  ps<-sum$coefficients[ ,"Pr(>|t|)"];
	ps<-ps[c("random", "read", "size")];
  kps<-ps[ps==max(ps, na.rm=T)];
	print(ps);
	return (na.exclude(kps));
}

fn.piecewise.lm<-function (ds.data, lq.fit) {
	#piecewise scheme -experimental
	sv<-fn.get.split.var(summary(lq.fit));
	print (names(sv))
	print (sv)
	len<-lapply(apply(ds.data[,c("read", "random", "size")],2,unique), length);
	if ((sv < 0.05) || (len$read == 2) || (len$random == 2) || (len$size ==2)) {
		plq.pred<-predict(lq.fit, ds.data[,2:4], interval="prediction");
		ds.data$plq.pred<-plq.pred[,1];
		ds.data$plq.lpred<-plq.pred[,2];
		ds.data$plq.hpred<-plq.pred[,3];
		return (ds.data);
	}
	med<-median(ds.data[, names(sv)]);
	
	data1<-ds.data[ds.data[, names(sv)]<med,];
	data2<-ds.data[ds.data[,names(sv)]>=med,];

	pfit1<-lm(lq_slope~random*read*size, data=data1);
	pfit2<-lm(lq_slope~random*read*size, data=data2); 

	ds.data1<-fn.piecewise.lm(data1, pfit1);
	ds.data2<-fn.piecewise.lm(data2, pfit2);
	return(rbind(ds.data1, ds.data2));
}

data<-data.frame();
ff.fit<-list();
for (i in ds) {
	print(i);
	ds.data <- res[res$datastore == i, ];
	ds.data$datastore <- ds.names.new[which(as.character(i) == ds.names)];

	ds.data$pesto.lq <- ds.data[ds.data$size == 4*K
								& ds.data$read == 100
								& ds.data$random == 100, ]$lq_slope;

	fit <- lm(latency ~ random*read*size*oio, data=ds.data);
	pred <- predict(fit, ds.data[,2:5], interval="prediction")
	conf <- predict(fit, ds.data[,2:5], interval="confidence")
	ds.data$lm.res<-fit$residuals;
	ds.data$lm.pred<-pred[,1];
	ds.data$lm.lpred<-pred[,2];
	ds.data$lm.hpred<-pred[,3];
	ds.data$lm.lconf<-conf[,2];
	ds.data$lm.hconf<-conf[,3];
	
	kv <- fn.get.key.var(summary(fit));
	kfit <-lm (as.formula(paste("latency ~ ", paste(names(kv), collapse="*"))), data=ds.data);
	kpred <- predict(kfit, ds.data[,2:5], interval="prediction")
	kconf <- predict(kfit, ds.data[,2:5], interval="confidence")
	ds.data$klm.res<-kfit$residuals;
	ds.data$klm.pred<-kpred[,1];
	ds.data$klm.lpred<-kpred[,2];
	ds.data$klm.hpred<-kpred[,3];
	ds.data$klm.lconf<-kconf[,2];
	ds.data$klm.hconf<-kconf[,3];

	lq.fit <- lm(lq_slope ~ random*read*size, data=ds.data);
	lq.pred <- predict(lq.fit, ds.data[,2:4], interval="prediction");
	lq.conf <- predict(lq.fit, ds.data[,2:4], interval="confidence");
	ds.data$lq.res<-lq.fit$residuals;
	ds.data$lq.pred<-lq.pred[,1];
	ds.data$lq.lpred<-lq.pred[,2];
	ds.data$lq.hpred<-lq.pred[,3];
	ds.data$lq.lconf<-lq.conf[,2];
	ds.data$lq.hconf<-lq.conf[,3];	

	klq.fit <-lm (step(lq.fit));
	klq.pred <- predict(klq.fit, ds.data[,2:4], interval="prediction");
	klq.conf <- predict(klq.fit, ds.data[,2:4], interval="confidence");
	ds.data$klq.res<-klq.fit$residuals;
	ds.data$klq.pred<-klq.pred[,1];
	ds.data$klq.lpred<-klq.pred[,2];
	ds.data$klq.hpred<-klq.pred[,3];
	ds.data$klq.lconf<-klq.conf[,2];
	ds.data$klq.hconf<-klq.conf[,3];	

	#filtering scheme - experimental
	oio.err<-decompose(ts(ds.data$lq.res, freq=8));
	#size.err<-decompose(ts(oio.err$trend,freq=(64)));
	#size.err<-decompose(ts(ds.data$lq.res,freq=(64)));
	#read.err<-decompose(ts(size.err$trend+size.err$random,freq=(64*5)));
	#size.filter<-size.err$seasonal[is.na(size.err$seasonal)]<-0;
	#read.filter<-read.err$seasonla[is.na(read.err$seasonal)]<-0;
	#random.filter<-read.err$trend[is.na(read.err$trend)]<-0;
	size.err<-stl(ts(ds.data$lq.res, freq=64), s.window=64, robust=T);
	read.err<-stl(size.err$time.series[,"trend"]+size.err$time.series[,"remainder"], s.window=64, robust=T);
	size.filter<-as.vector(size.err$time.series[,"seasonal"]);
	read.filter<-as.vector(read.err$time.series[,"seasonal"]);
	random.filter<-as.vector(read.err$time.series[, "trend"])
	
	filter<-size.filter+read.filter+random.filter;
	ds.data$lq.pred.new<-ds.data$lq.pred+filter;
	ds.data$lq.res.new<-ds.data$lq_slope - ds.data$lq.pred.new; 
	
	#piecewise scheme -experimental
	print("TEST")
	ds.data<-fn.piecewise.lm(ds.data, lq.fit);
#	sv<-fn.get.split.var(summary(lq.fit));
#	med<-list();
#	for (j in sv) {
#		med[[j]]<-median(ds.data[, j]);
#	}
#	for (j in names(med)) {
#		data1<-ds.data[ds.data[,j]<med[[j]],];
#		data2<-ds.data[ds.data[,j]>=med[[j]],];
#		pfit1<-lm(lq_slope~random*read*size, data=data1);
#		pfit2<-lm(lq_slope~random*read*size, data=data2); 
#		lq.pred1<- predict(pfit1, data1[,2:4], interval="prediction");
#		lq.pred2<- predict(pfit2, data2[,2:4], interval="prediction");
#		lq.conf1<- predict(pfit1, data1[,2:4], interval="confidence");
#		lq.conf2<- predict(pfit2, data2[,2:4], interval="confidence");
#		data1$plq.pred<-lq.pred1[,1];
#		data1$plq.lpred<-lq.pred1[,2];
#		data1$plq.hpred<-lq.pred1[,3];
#		data1$plq.lconf<-lq.conf1[,2];
#		data1$plq.hconf<-lq.conf1[,3];
#		data2$plq.pred<-lq.pred2[,1];
#		data2$plq.lpred<-lq.pred2[,2];
#		data2$plq.hpred<-lq.pred2[,3];
#		data2$plq.lconf<-lq.conf2[,2];
#		data2$plq.hconf<-lq.conf2[,3];
#		ds.data<-rbind(data1, data2);
#	} 
	ds.data<-ds.data[order(ds.data$oio),];
	ds.data<-ds.data[order(ds.data$size),];
	ds.data<-ds.data[order(ds.data$read),];
	ds.data<-ds.data[order(ds.data$random),];
	
	save(file=paste("RData/",ds.data$datastore[1],"_fit.RData", sep=""), fit, kfit, lq.fit, ds.data, size.err, read.err, filter, klq.fit)
	
	data<-rbind(data,ds.data);
}

data$basil.res <- data$latency - data$basil.W;
data$ros.fit <- data$lq.pred * data$oio + data$lq_intercept;

data$ros.fit.no.intercept <-data$lq.pred * data$oio;
data$ros.fit.ni.lpred <- data$lq.lpred * data$oio;
data$ros.fit.ni.hpred <- data$lq.hpred * data$oio;
data$ros.res <- data$latency - data$ros.fit.no.intercept;

data$ros.filtered <- data$lq.pred.new * data$oio;
data$ros.filtered.res <- data$latency - data$ros.filtered;	
data$pesto.pred <- data$basil.W * (data$pesto.lq/1000);
data$pesto.res <- data$latency - data$pesto.pred;

data$kros.fit <- data$klq.pred * data$oio;
data$kros.fit.ni.lpred <- data$klq.lpred * data$oio;
data$kros.fit.ni.hpred <- data$klq.hpred * data$oio;
data$kros.res <- data$latency - data$kros.fit;

data$pros.fit <- data$plq.pred * data$oio;
data$pros.res <- data$latency - data$pros.fit;

save(file="RData/romano.RData", data);
