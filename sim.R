#!/home/parkx408/root/bin/Rscript --verbose
#!/usr/bin/Rscript --verbose

rm(list=ls());

min_lq<-0.1

#Get workload specification
workload<-read.table("load_balance_test/load_balance_test.cfg", skip=16, nrows=12, row.names=1, col.names=c("workload", "random", "read", "size"), header=F);

#Get oio
oio<-scan("load_balance_test/load_balance_test.cfg", skip=13, nlines=1);

#Load 200 load-balance test initial state
data<-read.table("load_balance_test/init_placement");
aggr<-ncol(data);
W.E1<-as.character(unlist(data[data$V1=="E1",2:aggr]));
W.E2<-as.character(unlist(data[data$V1=="E2",2:aggr]));
W.N1<-as.character(unlist(data[data$V1=="N1",2:aggr]));
W.N2<-as.character(unlist(data[data$V1=="N2",2:aggr]));
W<-data.frame(datastore=c(rep("E1", aggr-1), rep("E2", aggr-1), rep("N1", aggr-1), rep("N2", aggr-1)));
W$workload<-c(W.E1, W.E2, W.N1, W.N2);
W<-W[W$workload != "Idle",];
W$oio<-oio;
W<-cbind(W, workload[W$workload,]);
W$VM<-1:nrow(W)
rm(W.E1, W.E2, W.N1, W.N2, data, oio, aggr);

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
	print(data);
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
	print(data);
  random<-fn.aggr.rand(data[,c(1,5)]);
  read<-fn.aggr.read(data[,c(2,5)]);
  size<-fn.aggr.size(data[,c(3,5)]);
  oio<-fn.aggr.oio(data[,c(4,5)]);
	#latency<-fn.aggr.latency(data[,c(5,6)]);
  #aggr<-data.frame(random=random, read=read, size=size, oio=oio, latency=latency);
  aggr<-data.frame(random=random, read=read, size=size, oio=oio);
  return(aggr);
}

#predict lq slope for current state
pred<-apply(W,1, function(x)(predict(pm[[x["datastore"]]], data.frame(random=as.numeric(x["random"]), read=as.numeric(x["read"]), size=as.numeric(x["size"])), interval="prediction")));

pred[pred<0]<-min_lq;

W$lq_pred<-pred[1,];
W$lq_low<-pred[2,];
W$lq_high<-pred[3,];
rm(pred);

#predict latency
W$lat_pred<-W$lq_pred*W$oio;
W$lat_low<-W$lq_low*W$oio;
W$lat_high<-W$lq_high*W$oio;

W$iops_pred<-1/W$lq_pred*1000;
W$iops_low<-1/W$lq_high*1000;
W$iops_high<-1/W$lq_low*1000;

#data store state
#FIXME: using list was stupid => change to data frame
S<-by (W[,c("random", "read", "size", "oio", "iops_pred")], list(W$datastore), fn.aggr);
S<-do.call(rbind, S);
S$datastore<-as.factors(row.names(S));

pred<-apply(S,1, function(x)(predict(pm[[x["datastore"]]], data.frame(random=as.numeric(x["random"]), read=as.numeric(x["read"]), size=as.numeric(x["size"])), interval="prediction")));

pred[pred<0]<-min_lq;

S$lq_pred<-pred[1,];
S$lq_low<-pred[2,];
S$lq_high<-pred[3,];

S$lat_pred<-S$lq_pred*S$oio;
S$lat_low<-S$lq_low*S$oio;
S$lat_high<-S$lq_high*S$oio;

S$iops_pred<-1/S$lq_pred*1000;
S$iops_low<-1/S$lq_high*1000;
S$iops_high<-1/S$lq_low*1000;

#
#
#fn.new.char<-function(S,W) {
#	w_test<-data.frame();
#	for (ds.names in names(S)) {
#		w_new<-W[W$datastore==ds.names,c(2,3,4,5,6)];
#		w_new$random<-S[[ds.names]]$random;
#		w_new$lq<-predict(pm[[ds.names]], w_new[,c(1,2,3)], interval="prediction");
#		w_new$iops<-1/w_new$lq*1000;
#		w_new$lat_pred<-w_new$lq*w_new$oio;
#		w_new$datastore<-ds.names;
#		print(w_new);
#		w_test<-rbind(w_test, w_new);
#	}
#	return(w_test);
#}
#
##w_test<-fn.new.char(S,W);
##VM latency test
##pdf("vm_latency_prediction_with_interference.pdf", width=6, height=6, onefile=F);
##matplot(cbind(W[,c(6,8)], w_test[,8]), pch=1:7, lty=1:7, type="b");
##legend("topleft", c("measured", "predicted", "upper", "lower", "pred_new", "upper_new", "lower_new"), pch=1:7, lty=1:7);
##dev.off();
#
#
#

fn.W<-function(S) {
	W<-W$VM[W$datastore==S];
	return (W);
}

fn.S<-function(W) {
	S<-W$datastore[W$VM==W];
	return(S);
}

fn.Merit<-function(P) {
	return(sum(P^5)^(1/5));
}

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
		
fn.loadbalance<-function(S,W) {
	merit<-fn.Merit(S$lat_pred);
	W.new<-fn.minimizeMerit(S, W, merit);
