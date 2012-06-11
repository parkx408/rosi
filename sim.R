#!/home/parkx408/root/bin/Rscript --verbose
#!/usr/bin/Rscript --verbose

##################
#Global env setup#
##################
rm(list=ls());

full.run<-"single-test"; #output file name
min_lq<-0.1;	#min_lq for negative predition

workload<-read.table("load_balance_test/load_balance_test.cfg", skip=16, nrows=12, row.names=1, col.names=c("workload", "random", "read", "size"), header=F); #Get workload specification
oio<-scan("load_balance_test/load_balance_test.cfg", skip=13, nlines=1);	#Get oio
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



############################
#Initial placement handling#
############################
fn.generate.W<-function(data) {
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
	W$VM<-1:nrow(W);
	return (W);
}

############################
#Workload Aggregation Model#
############################
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
  return (random*100);
}

#input data[,1] = read%
#input data[,2] = throughput(IOPS)
#return read = aggregated_read%
fn.aggr.read<-function(data) {
  read<-weighted.mean(data[,1]/100, data[,2]);
  return (read*100);
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
	##print(data);
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
	##print(data);
  random<-fn.aggr.rand(data[,c(1,5)]);
  read<-fn.aggr.read(data[,c(2,5)]);
  size<-fn.aggr.size(data[,c(3,5)]);
  oio<-fn.aggr.oio(data[,c(4,5)]);
  aggr<-data.frame(random=random, read=read, size=size, oio=oio);
  return(aggr);
}

#################################
#Predict on individual worklaods#
#################################
fn.predict.W<-function(W, pm) {
	#predict lq slope for current state
	pred<-apply(W,1, function(x)(predict(pm[[x["datastore"]]], data.frame(random=as.numeric(x["random"]), read=as.numeric(x["read"]), size=as.numeric(x["size"])), interval="prediction")));

	pred[pred<0]<-min_lq;

	W$lq_pred<-pred[1,];
	W$lq_low<-pred[2,];
	W$lq_high<-pred[3,];

	#predict latency
	W$lat_pred<-W$lq_pred*W$oio;
	W$lat_low<-W$lq_low*W$oio;
	W$lat_high<-W$lq_high*W$oio;

	W$iops_pred<-1/W$lq_pred*1000;
	W$iops_low<-1/W$lq_high*1000;
	W$iops_high<-1/W$lq_low*1000;
	return (W);
}

###########################
#Prediction on Aggregation#
###########################
fn.generate.S <- function (W, pm) {
	#data store state
	S<-by (W[,c("random", "read", "size", "oio", "iops_pred")], list(W$datastore), fn.aggr);
	S<-do.call(rbind, S);
	S$datastore<-as.factor(row.names(S));

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
	return (S);
}

################
#Load Balancing#
################
fn.W<-function(s) {
	w<-W$VM[W$datastore==s];
	return (w);
}

fn.S<-function(w) {
	s<-W$datastore[W$VM==w];
	return(s);
}

fn.Merit<-function(P) {
	P<-P[which(!is.nan(P))];
	return(sum(P^5)^(1/5));
}

fn.updateMerit<-function(w.cand, s.cand, W, S) {
	s.src<-fn.S(w.cand);
	w.src<-fn.W(s.src);								#get all workloads at source
	w.dst<-fn.W(s.cand);							#get all workloads at dest
	w.src.new<-w.src[w.src!=w.cand]; 	#remove w.cand from source
	w.dst.new<-c(w.dst, w.cand);			#add w.cand to dest
	##print (w.src.new);
	##print (w.dst.new);

	#predicting new aggregation
	lq.pred.cand<-predict(pm[[s.cand]], W[W$VM==w.cand, c("random", "read", "size")], interval="prediction");
	lq.pred.cand[lq.pred.cand<0] <- min_lq;
	iops.pred.cand<-1/lq.pred.cand*1000;
	##print(iops.pred.cand);

	src.aggr<-W[w.src.new ,c("random", "read", "size", "oio", "iops_pred")];	
	dst.aggr<-W[w.dst.new ,c("random", "read", "size", "oio", "iops_pred")];	
	dst.aggr[nrow(dst.aggr), "iops_pred"]<-iops.pred.cand[1,1];
	##print(src.aggr);
	##print(dst.aggr);
	w.src.aggr<-fn.aggr(src.aggr);
	w.dst.aggr<-fn.aggr(dst.aggr);
	##print (w.src.aggr);	
	##print (w.dst.aggr);	

	#predict new latencies	
	lq.pred.src<-predict(pm[[s.src]], w.src.aggr, interval="prediction");
	lq.pred.dst<-predict(pm[[s.cand]], w.dst.aggr, interval="prediction");
	##print (lq.pred.src);
	##print (lq.pred.dst);
	lq.pred.src[lq.pred.src<0] = min_lq;
	lq.pred.dst[lq.pred.dst<0] = min_lq;
	lat.pred.src<-lq.pred.src*w.src.aggr$oio;
	lat.pred.dst<-lq.pred.dst*w.dst.aggr$oio;

	#calculate new merit
	p<-S[,c("lat_pred", "lat_low", "lat_high")];
	p[as.character(s.src),]<-lat.pred.src;
	p[as.character(s.cand),]<-lat.pred.dst;
	##print (p)
	merit<-apply(p, 2, fn.Merit);
	return (merit);
}

fn.new.temperature<-function(T) {
	return (T/1.2);
}

fn.goodness<-function(E, Enew, T) {
	##print (E);
	##print (Enew);
	if (is.nan(E) | is.nan(Enew)) {
		print (paste("E:", E));
		print (paste("Enew:", Enew));
	}
	if (Enew > E) {
		return (exp((E-Enew)/T))
	}
	else {
		return (1);
	}
}

fn.minimize.merit<-function(W.i, S.i, merit.i) {
	max.cycle<-200;
	w.cand.list<-sample(W.i$VM, max.cycle, replace=T);
	s.cand.list<-sample(rownames(S.i), max.cycle, replace=T);
	
	m<-merit.i;
	m.f<-m;
	W<-W.i;
	S<-S.i;
	W.f<-W;
	S.f<-S;
	K<-0;
	m.list<-m;

	for (i in 1:max.cycle) {
		T<-fn.new.temperature(K/max.cycle);
		w.cand<-w.cand.list[i];
		s.cand<-s.cand.list[i];
		if (fn.S(w.cand) == s.cand) {
			next();
		}
		m.new<-fn.updateMerit(w.cand, s.cand, W, S);
		##print(w.cand);
		##print(s.cand);
		##print(W);
		##print(S);
		#print(m.new);
		if (fn.goodness(m[1], m.new[1], T)>runif(1,0,1)) {
			m.list<-c(m.list, m.new["lat_pred"]);
			#Accept the move
			W[W$VM==w.cand,"datastore"]<-s.cand;
			pred<-apply(W,1, function(x)(predict(pm[[x["datastore"]]], data.frame(random=as.numeric(x["random"]), read=as.numeric(x["read"]), size=as.numeric(x["size"])), interval="prediction")));

			pred[pred<0]<-min_lq;

			W$lq_pred<-pred[1,];
			W$lq_low<-pred[2,];
			W$lq_high<-pred[3,];
			rm(pred);

			W$lat_pred<-W$lq_pred*W$oio;
			W$lat_low<-W$lq_low*W$oio;
			W$lat_high<-W$lq_high*W$oio;

			W$iops_pred<-1/W$lq_pred*1000;
			W$iops_low<-1/W$lq_high*1000;
			W$iops_high<-1/W$lq_low*1000;

			S<-by (W[,c("random", "read", "size", "oio", "iops_pred")], list(W$datastore), fn.aggr);
			S<-do.call(rbind, S);
			S$datastore<-as.factor(row.names(S));

			pred<-apply(S,1, function(x)(predict(pm[[x["datastore"]]], data.frame(random=as.numeric(x["random"]), read=as.numeric(x["read"]), size=as.numeric(x["size"])), interval="prediction")));

			pred[pred<0]<-min_lq;

			S$lq_pred<-pred[1,];
			S$lq_low<-pred[2,];
			S$lq_high<-pred[3,];
			rm(pred);

			S$lat_pred<-S$lq_pred*S$oio;
			S$lat_low<-S$lq_low*S$oio;
			S$lat_high<-S$lq_high*S$oio;

			S$iops_pred<-1/S$lq_pred*1000;
			S$iops_low<-1/S$lq_high*1000;
			S$iops_high<-1/S$lq_low*1000;
			m<-m.new;
		}
		if (m < m.f) {
			#store the best state seen so far
			S.f<-S;
			W.f<-W;
			m.f<-m;
		}
		K<-K+1;
	}
	plot(m.list);
	##print (m.list);
	return (list(S=S.f, W=W.f, m=m.f));
}

fn.load.balance<-function(S,W) {
	m.i<-fn.Merit(S$lat_pred); #initial merit;
	print(paste("initial_merit", m.i));
	m.list<-data.frame(); #initial merit list
	
	S.f<-fn.minimize.merit(W, S, m.i); #find pseudo-optimal
	moved<-S.f$W[S.f$W$datastore != W$datastore, c("VM", "datastore")];
	#print(S.f)
	#print(moved);
	
	m.next<-m.i;
	for (i in 1:nrow(moved)) {
		for (j in 1:nrow(moved)) {
			m.new<-fn.updateMerit(moved[j,1], moved[j,2], W, S);
			m.list<-rbind(m.list, as.data.frame(c(m.new, as.vector(moved[j,])))); 
		}
		m.list<-m.list[order(m.list$lat_pred),];
		#print(m.list);
		if (m.list[1,1] > m.next) {
		#	#print(m.next);
		#	#print(m.list);
			next();
		}
		print( paste("Move VM", m.list[1,4], W$workload[W$VM==m.list[1,4]], "of", fn.S(m.list[1,4]), "to", m.list[1,5], ":", m.next[1], "->", m.list[1,1]));
		m.next<-m.list[1,1];
		w.cand<-m.list[1,4];
		s.cand<-m.list[1,5];
		m.list<-data.frame();
		moved<-moved[-which(moved$VM==w.cand),]
		#print(moved);
		#print(w.cand);
		#print(s.cand);
    W[W$VM==w.cand,"datastore"]<-s.cand;
    pred<-apply(W,1, function(x)(predict(pm[[x["datastore"]]], data.frame(random=as.numeric(x["random"]), read=as.numeric(x["read"]), size=as.numeric(x["size"])), interval="prediction")));

    pred[pred<0]<-min_lq;

    W$lq_pred<-pred[1,];
    W$lq_low<-pred[2,];
    W$lq_high<-pred[3,];
    rm(pred);

    W$lat_pred<-W$lq_pred*W$oio;
    W$lat_low<-W$lq_low*W$oio;
    W$lat_high<-W$lq_high*W$oio;

    W$iops_pred<-1/W$lq_pred*1000;
    W$iops_low<-1/W$lq_high*1000;
    W$iops_high<-1/W$lq_low*1000;

    S<-by (W[,c("random", "read", "size", "oio", "iops_pred")], list(W$datastore), fn.aggr);
    S<-do.call(rbind, S);
    S$datastore<-as.factor(row.names(S));

    pred<-apply(S,1, function(x)(predict(pm[[x["datastore"]]], data.frame(random=as.numeric(x["random"]), read=as.numeric(x["read"]), size=as.numeric(x["size"])), interval="prediction")));

    pred[pred<0]<-min_lq;
    S$lq_pred<-pred[1,];
    S$lq_low<-pred[2,];
    S$lq_high<-pred[3,];
    rm(pred);

    S$lat_pred<-S$lq_pred*S$oio;
    S$lat_low<-S$lq_low*S$oio;
    S$lat_high<-S$lq_high*S$oio;

    S$iops_pred<-1/S$lq_pred*1000;
    S$iops_low<-1/S$lq_high*1000;
    S$iops_high<-1/S$lq_low*1000;

		#print(W);
		#print(S);
		out<-list();
	 	out[["E1"]]<-W$workload[W$datastore=="E1"];
	 	out[["E2"]]<-W$workload[W$datastore=="E2"];
	 	out[["N1"]]<-W$workload[W$datastore=="N1"];
	 	out[["N2"]]<-W$workload[W$datastore=="N2"];
		#max.length<-max(unlist(lapply(out, length)));
		max.length<-8;
		#print(length(out[["E1"]]));
		#print(length(out[["E2"]]));
		#print(length(out[["N1"]]));
		#print(length(out[["N2"]]));
		out[["E1"]]<-c(out[["E1"]], rep("Idle", max.length-length(out[["E1"]])));
		out[["E2"]]<-c(out[["E2"]], rep("Idle", max.length-length(out[["E2"]])));
		out[["N1"]]<-c(out[["N1"]], rep("Idle", max.length-length(out[["N1"]])));
		out[["N2"]]<-c(out[["N2"]], rep("Idle", max.length-length(out[["N2"]])));
		out<-t(as.data.frame(out));
		data<-cbind(data,out);
		
		#out<-rbind(out, c("E2", W$workload[W$datastore=="E2"]));
		#out<-rbind(out, c("N1", W$workload[W$datastore=="N1"]));
		#out<-rbind(out, c("N2", W$workload[W$datastore=="N2"]));
		#print(out)
	}
	print (paste("final merit", m.next));
	print("");
	return(list(S=S,W=W,m=m.next, o=data));	
}


#Load load-balance test initial state
init<-list.files("load_balance_test/init");
Idle<-c("Idle", "Idle", "Idle", "Idle");
S.f<-list();
for (i in init) {
	data<-read.table(paste("load_balance_test/init/",i, sep=""));
	data<-cbind(data, Idle, Idle, Idle);
	#print (data);
	W<-fn.generate.W(data);
	W<-fn.predict.W(W, pm);
	S<-fn.generate.S(W, pm);
	S.f[[i]]<-fn.load.balance(S, W);
}

data<-data.frame(row.names=c("E1", "E2", "N1", "N2"));
len<-vector();
for (i in S.f) {
	data<-cbind(data, i$o[,-1]);
	len<-c(len,ncol(i$o[,-1])/8);
}
write.table(data, file=full.run, row.names=T, col.names=F, sep=" ", quote=F, append=F);
	
		
#fn.loadbalance<-function(S,W) {
#	merit<-fn.Merit(S$lat_pred);
#	W.new<-fn.minimizeMerit(S, W, merit);
