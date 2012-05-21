#!/home/parkx408/root/bin/Rscript --verbose
#!/usr/bin/Rscript --verbose

rm(list=ls());

#Load 200 load-balance test initial state
W1<-read.table("rosi_data/lb_initial_placement1.out", header=T);
W2<-read.table("rosi_data/lb_initial_placement2.out", header=T);

#Initial state per data store
W.E1<-W1[W1$datastore=="emc-3disk-raid0",];
W.E2<-W2[W2$datastore=="emc-5disk-sp",];
W.N1<-W2[W2$datastore=="netapp-7disk-sp",];
W.N2<-W1[W1$datastore=="srs7diskfc-netapp",];
rm(W1, W2);

#anonymize data store names
W.E1$datastore<-"E1";
W.E2$datastore<-"E2";
W.N1$datastore<-"N1";
W.N2$datastore<-"N2";

#First initial state for each data store
W.E1<-W.E1[1:W.E1$workers[1],][,c(2,4,5,6,7,8,9)];
W.E2<-W.E2[1:W.E2$workers[1],][,c(2,4,5,6,7,8,9)];
W.N1<-W.N1[1:W.N1$workers[1],][,c(2,4,5,6,7,8,9)];
W.N2<-W.N2[1:W.N2$workers[1],][,c(2,4,5,6,7,8,9)];

#Load romano model for each data store
pm<-list();
load("RData/E1_fit.RData");
pm[["E1"]]<-klq.fit;
load("RData/E2_fit.RData");
pm[["E2"]]<-klq.fit;
load("RData/N1_fit.RData");
pm[["N1"]]<-klq.fit;
load("RData/N2_fit.RData");
pm[["N2"]]<-klq.fit;
rm(ds.data, filter, fit, kfit, klq.fit, lq.fit, read.err, size.err);

#input data[,1] = random%
#input data[,2] = throughput(IOPS)
#return w.mean = average_random%
fn.avg.rand<-function(data) {
  w.mean<-weighted.mean(data[,1]/100, data[,2]);
  return (w.mean);
}

#input data = random%
#input w.mean = average_random%
#return lseek = average_lseek%
fn.lseek<-function(data, w.mean) {
	n.workload<-nrow(data);
  lseek<-(min(data[,2])/sum(data[,2]))*n.workload*w.mean;
  return (lseek);
}

#input data[,1] = random%
#input data[,2] = throughput(IOPS)
#return random = aggregated_random%
fn.aggr.rand<-function(data) {
  w.mean<-fn.avg.rand(data);
  lseek<-fn.lseek(data, w.mean);
  random<-1-w.mean*(1-lseek);
  return (random);
}

#input data[,1] = read%
#input data[,2] = throughput(IOPS)
#return read = aggregated_read%
fn.aggr.read<-function(data) {
  read<-weighted.mean(data[,1]/100, data[,2]);
  return (read);
}

#input data[,1] = size
#input data[,2] = throughput(IOPS)
#return size = aggregated_size
fn.aggr.size<-function(data) {
  size<-weighted.mean(data[,1], data[,2]);
  return (size);
}

#input data[,1] = oio
#return oio = aggregated_oio
fn.aggr.oio<-function(data) {
  oio<-sum(data[,1]);
  return (oio);
}

#input data[,1]=latency
#input data[,2] = throughput(IOPS)
#return latency = aggregated_latency
fn.aggr.latency<-function(data) {
	latency<-weighted.mean(data[,1], data[,2]);
	return (latency);
}

#input data[,1] = random%
#input data[,2] = read%
#input data[,3] = size%
#input data[,4] = oio
#input data[,5] = latency
#input data[,6] = predicted_lq_slope
#return aggr = aggrgated_characteristics
fn.aggr<-function(data) {
	data$iops<-data$iops[,1];
  random<-fn.aggr.rand(data[,c(1,6)]);
  read<-fn.aggr.read(data[,c(2,6)]);
  size<-fn.aggr.size(data[,c(3,6)]);
  oio<-fn.aggr.oio(data[,c(4,6)]);
	latency<-fn.aggr.latency(data[,c(5,6)]);
  aggr<-data.frame(random=random, read=read, size=size, oio=oio, latency=latency);
  return(aggr);
}

#predict lq slope for current state
W.E1$lq<-predict(pm$E1,W.E1[,c(2,3,4)],interval="prediction")
W.E2$lq<-predict(pm$E2,W.E2[,c(2,3,4)],interval="prediction")
W.N1$lq<-predict(pm$N1,W.N1[,c(2,3,4)],interval="prediction")
W.N2$lq<-predict(pm$N2,W.N2[,c(2,3,4)],interval="prediction")

#workload state
W<-rbind(W.E1, W.E2, W.N1, W.N2);
rm(W.E1, W.E2, W.N1, W.N2);

W$iops<-1/W$lq*1000;

#data store state
#FIXME: using list was stupid => change to data frame
S<-list();

#get aggregated workload characteristics per data store
S[["E1"]]<-fn.aggr(W[W$datastore=="E1", c(2,3,4,5,6,9)]);
S[["E2"]]<-fn.aggr(W[W$datastore=="E2", c(2,3,4,5,6,9)]);
S[["N1"]]<-fn.aggr(W[W$datastore=="N1", c(2,3,4,5,6,9)]);
S[["N2"]]<-fn.aggr(W[W$datastore=="N2", c(2,3,4,5,6,9)]);

#predict the data store level lq slope
S[["E1"]]$lq<-predict(pm$E1, S$E1[,c(1,2,3)], interval="prediction");
S[["E2"]]$lq<-predict(pm$E1, S$E2[,c(1,2,3)], interval="prediction");
S[["N1"]]$lq<-predict(pm$E1, S$N1[,c(1,2,3)], interval="prediction");
S[["N2"]]$lq<-predict(pm$E1, S$N2[,c(1,2,3)], interval="prediction");

#predict the data store latency
S[["E1"]]$latency_pred<-S[["E1"]]$lq*S[["E1"]]$oio;
S[["E2"]]$latency_pred<-S[["E2"]]$lq*S[["E1"]]$oio;
S[["N1"]]$latency_pred<-S[["N1"]]$lq*S[["E1"]]$oio;
S[["N2"]]$latency_pred<-S[["N2"]]$lq*S[["E1"]]$oio;


fn.new.char<-function(S,W) {
	w_test<-data.frame();
	for (ds.names in names(S)) {
		w_new<-W[W$datastore==ds.names,c(2,3,4,5,6)];
		w_new$random<-S[[ds.names]]$random;
		w_new$lq<-predict(pm[[ds.names]], w_new[,c(1,2,3)], interval="prediction");
		w_new$iops<-1/w_new$lq*1000;
		w_new$lat_pred<-w_new$lq*w_new$oio;
		w_new$datastore<-ds.names;
		print(w_new);
		w_test<-rbind(w_test, w_new);
	}
	return(w_test);
}

#w_test<-fn.new.char(S,W);
#VM latency test
#pdf("vm_latency_prediction_with_interference.pdf", width=6, height=6, onefile=F);
#matplot(cbind(W[,c(6,8)], w_test[,8]), pch=1:7, lty=1:7, type="b");
#legend("topleft", c("measured", "predicted", "upper", "lower", "pred_new", "upper_new", "lower_new"), pch=1:7, lty=1:7);
#dev.off();



fn.updateMerit<-function(W, S, State) {
	w.src<-fn.W(fn.S(W));
	w.dst<-fn.W(S);
}

fn.new.temperature<-function(T) {
	return (T/1.1);
}

fn.goodness<-function(E, Enew) {
	if (Enew > E) {
		return (exp((E-Enew)/T))
	}
	else {
		return (1);
	}
}
		

