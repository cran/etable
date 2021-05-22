stat_cell <-
function(x, y, z, w, cell_ids, row_ids, col_ids, vnames, vars, n_min, digits=3, digits2=1){
kwordlist<-c('N','MEAN','SD','MSD', 'MCI', 'VAR','MEDIAN','MD','MAD','MQQ','PROP','PCI','RANGE','CV','MAX','MIN','SUM','IQR','MODE','MISS','SKEW','KURT','P1','P2','P2.5','P5','P10','P15','P20','P25','P30','P35','P40','P45','P50','P55','P60','P65','P70','P75','P80','P85','P90','P95','P97.5','P98','P99', 'P99.9','PNM', 'M2SD', 'M1SD', 'M3SD', 'MM2SD', 'MM1SD', 'MM3SD','POP', 'COMB', 'GEO', 'HARM', 'NORM50', 'NORM90', 'NORM95', 'NORM99', 'TM1', 'TM5', 'TM10', 'TM25', 'WM1', 'WM5', 'WM10', 'WM25')
if(vars[1]%in%kwordlist) keyword<- vars[1]
if(vars[2]%in%kwordlist) keyword<- vars[2]

if(is.null(x) & !is.null(y)){
x<- y
}



out<-''
if(length(cell_ids)>= n_min){
#########################################
if(keyword=='N'){
if(is.null(w)) w<- rep(1, length(x))
w[which(is.na(x))]<-NA
out<- a.round.ade(sum(w[cell_ids], na.rm=TRUE), 0)
}
#########################################

#########################################
if(keyword=='PROP'){
if(is.null(w)) w<- rep(1, length(x))
w[which(is.na(x))]<-NA
out<- paste(a.round.ade((sum(w[cell_ids], na.rm=TRUE)/sum(w, na.rm=TRUE))*100, digits2), '%', sep='')
}
#########################################

#########################################
if(keyword=='POP'){
if(is.null(w)) w<- rep(1, length(x))
out<- ' '
if(nlevels(as.factor(x))==2){
w[which(is.na(x))]<-NA
x<- as.factor(x)
out<- paste(a.round.ade((sum(w[cell_ids][which(x[cell_ids]==levels(x)[2])], na.rm=TRUE)/(sum(w[cell_ids], na.rm=TRUE)))*100, digits2), '% (', round(sum(w[cell_ids][which(x[cell_ids]==levels(x)[2])], na.rm=TRUE), digits=0), ')', sep='')
}
}
#########################################

#########################################
if(keyword=='PCI'){
if(is.null(w)) w<- rep(1, length(x))
out<- ' '
if(nlevels(as.factor(x))==2){
w[which(is.na(x))]<-NA
x<- as.factor(x)
pout<- prop.test(sum(w[cell_ids][which(x[cell_ids]==levels(x)[2])], na.rm=TRUE), sum(w[cell_ids], na.rm=TRUE))
out<- paste(a.round.ade(pout$estimate*100, digits2), ' [', a.round.ade(pout$conf.int[1]*100, digits=digits2), '; ', a.round.ade(pout$conf.int[2]*100, digits=digits2), ']', sep='')
}
}
#########################################


#########################################
if(keyword=='COMB'){


if(is.factor(x) & nlevels(x)==2){
if(is.null(w)) w<- rep(1, length(x))
out<- ' '
if(nlevels(as.factor(x))==2){
w[which(is.na(x))]<-NA
x<- as.factor(x)
out<- paste(a.round.ade((sum(w[cell_ids][which(x[cell_ids]==levels(x)[2])], na.rm=TRUE)/(sum(w[cell_ids], na.rm=TRUE)))*100, digits2), '% (', round(sum(w[cell_ids][which(x[cell_ids]==levels(x)[2])], na.rm=TRUE), digits=0), ')', sep='')
}
}


if(is.factor(x) & nlevels(x)>2){
out<- ''
}


if(is.numeric(x)){

if(is.null(w)){
m1<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.5),   na.rm=TRUE, type=8), digits=digits)
q1<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.25),  na.rm=TRUE, type=8), digits=digits)
q3<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.75),  na.rm=TRUE, type=8), digits=digits)
out<- paste(m1,' (',q1,'/',q3,')', sep='')
}

if(!is.null(w)){
m1<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.5)), digits=digits)
q1<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.25)), digits=digits)
q3<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.75)), digits=digits)
out<- paste(m1,' (',q1,'/',q3,')', sep='')
}
}

}
#########################################


#########################################
if(keyword=='RANGE'){
out<- paste(a.format_n.ade(min(as.numeric(x[cell_ids]), na.rm=TRUE),digits),'--',a.format_n.ade(max(as.numeric(x[cell_ids]), na.rm=TRUE),digits))
}
#########################################

#########################################
if(keyword=='MEAN'){
if(is.null(w))   w<- rep(1, length(x))
out<-a.format_n.ade(    wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE),  digits=digits)
}
#########################################

#########################################
if(keyword=='MCI'){
alpha <- 0.05
if(is.null(w))   w<- rep(1, length(x))
mw  <- wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)
sdw <- sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE))
w[which(is.na(x))]<-NA
nw  <- sum(w[cell_ids], na.rm=TRUE)
sew <- sdw/sqrt(nw)
ll <- mw - qt(p = (1 - alpha/2), df = nw - 1) * sew
ul <- mw + qt(p = (1 - alpha/2), df = nw - 1) * sew
out<-paste(a.format_n.ade(mw,digits=digits), ' [', a.format_n.ade(ll,digits=digits), '; ', a.format_n.ade(ul,digits=digits), ']', sep='')
}
#########################################


#########################################
if(keyword=='SD'){
if(is.null(w))   w<- rep(1, length(x))
out<- a.format_n.ade(sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE)), digits=digits)
}
#########################################

#########################################
if(keyword=='VAR'){
if(is.null(w))   w<- rep(1, length(x))
out<- a.format_n.ade(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE), digits=digits)
}
#########################################

#########################################
if(keyword=='MEDIAN'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.5),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.5)), digits=digits)
}
}
#########################################

#########################################
if(keyword=='MD'){
if(is.null(w)){
v<- x[cell_ids]
out<-a.format_n.ade(mean(abs(v-mean(v,na.rm=TRUE)), na.rm=TRUE)*1.253, digits=digits)
}
if(!is.null(w)){
v<- x[cell_ids]
vw<-w[cell_ids]
out<-a.format_n.ade(wtd.mean(abs(v-wtd.mean(v,vw)), vw)*1.253, digits=digits)
}
}
#########################################

#########################################
if(keyword=='MAD'){
if(is.null(w)){
v<- x[cell_ids]
out<- a.format_n.ade(mad(v, na.rm = TRUE), digits=digits)
}
if(!is.null(w)){
v<- x[cell_ids]
vw<-w[cell_ids]
out<-a.format_n.ade(wtd.quantile(abs(v-wtd.quantile(v,vw,probs=c(0.5))), vw,probs=c(0.5))*1.4826, digits=digits)
}
}
#########################################

#########################################
if(keyword=='CV'){
if(is.null(w))   w<- rep(1, length(x))
mu  <- wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)
msd <- sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE))

out<- a.format_n.ade(msd/mu, digits=digits)
}
#########################################

#########################################
if(keyword=='MAX'){
out<- a.format_n.ade(as.numeric(max(x[cell_ids], na.rm=TRUE)),digits)
}
#########################################

#########################################
if(keyword=='MIN'){
out<- a.format_n.ade(as.numeric(min(x[cell_ids], na.rm=TRUE)),digits)
}
#########################################

#########################################
if(keyword=='SUM'){
if(!is.null(w)) out<- a.format_n.ade(wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)*sum(w[cell_ids][which(!is.na(x[cell_ids]))], na.rm=TRUE),digits)
if(is.null(w))  out<- a.format_n.ade(sum(x[cell_ids], na.rm=TRUE),digits)

}
#########################################


#########################################
#########################################
if(keyword=='P1'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.01),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.01)), digits=digits)
}}
#########################################
if(keyword=='P2'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.02),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.02)), digits=digits)
}}
#########################################
if(keyword=='P2.5'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.025),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.025)), digits=digits)
}}
#########################################
if(keyword=='P5'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.05),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.05)), digits=digits)
}}
#########################################
if(keyword=='P10'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.10),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.10)), digits=digits)
}}
#########################################
if(keyword=='P15'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.15),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.15)), digits=digits)
}}
#########################################
if(keyword=='P20'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.2),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.2)), digits=digits)
}}
#########################################

#########################################
if(keyword=='P25'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.25),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.25)), digits=digits)
}}
#########################################
if(keyword=='P30'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.3),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.3)), digits=digits)
}}
#########################################
if(keyword=='P35'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.35),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.35)), digits=digits)
}}
#########################################
if(keyword=='P40'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.4),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.4)), digits=digits)
}}
#########################################
if(keyword=='P45'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.45),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.45)), digits=digits)
}}
#########################################

#########################################
if(keyword=='P50'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.5),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.5)), digits=digits)
}}
#########################################

if(keyword=='P55'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.55),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.55)), digits=digits)
}}
#########################################

if(keyword=='P60'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.6),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.6)), digits=digits)
}}
#########################################
if(keyword=='P65'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.65),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.65)), digits=digits)
}}
#########################################
if(keyword=='P70'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.7),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.7)), digits=digits)
}}
#########################################

#########################################
if(keyword=='P75'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.75),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.75)), digits=digits)
}}
#########################################
if(keyword=='P80'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.8),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.8)), digits=digits)
}}
#########################################
if(keyword=='P85'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.85),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.85)), digits=digits)
}}
#########################################
if(keyword=='P90'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.90),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.90)), digits=digits)
}}
#########################################
if(keyword=='P95'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.95),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.95)), digits=digits)
}}
#########################################
if(keyword=='P97.5'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.975),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.975)), digits=digits)
}}

#########################################
if(keyword=='P98'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.98),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.98)), digits=digits)
}}

#########################################
if(keyword=='P99'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.99),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.99)), digits=digits)
}}
#########################################

#########################################
if(keyword=='P99.9'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.999),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.999)), digits=digits)
}}
#########################################

#########################################
if(keyword=='IQR'){
if(is.null(w))  out<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.75),  na.rm=TRUE, type=8)-quantile(x[cell_ids], probs=c(0.25),  na.rm=TRUE, type=8), digits=digits)
if(!is.null(w)){
out<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.75))-wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.25)), digits=digits)
}
}
#########################################

#########################################
if(keyword=='MISS'){
out<- sum(is.na(x[cell_ids]))
}
#########################################

#########################################
if(keyword=='PNM'){
nmiss<- sum(is.na(x[cell_ids]))
nnonm<- sum(!is.na(x[cell_ids]))

out<- paste(a.round.ade((nnonm/(nmiss+nnonm))*100, 1), '%', sep='')

}
#########################################


#########################################
if(keyword=='SKEW'){
if(is.null(w))   w<- rep(1, length(x))
mu  <- wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)
vsd <- sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE))
n<- sum(!is.na(x[cell_ids]))
skew<- (1/n)*(sum(w[cell_ids]^(3/2)*((x[cell_ids]-mu)/vsd)^3, na.rm=T))
out<-a.format_n.ade(skew, digits=digits)
}
#########################################


#########################################
if(keyword=='KURT'){
if(is.null(w))   w<- rep(1, length(x))
mu  <- wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)
vsd <- sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE))
n<- sum(!is.na(x[cell_ids]))
kurt<- ((1/n)*(sum((w[cell_ids]^2) * ((x[cell_ids]-mu)/vsd)^4, na.rm=T)))-3
out<-a.format_n.ade(kurt, digits=digits)
}
#########################################

#########################################
if(keyword=='MODE'){
if(is.null(w))      w<- rep(1, length(x))
if(is.factor(x))    v<- factor(x[cell_ids])
if(is.numeric(x))   v<- x[cell_ids]
if(is.character(x)) v<- x[cell_ids]
ul <- sort(unique(v))
tab <- wtd.table(v, w[cell_ids], type='table')
on <- which(tab==max(tab, na.rm=TRUE))
value<- ul[on]
N<- tab[on]
if(is.numeric(x))  out<- paste(a.format_n.ade(value,digits), ' (N:', round(N) , ')',  sep='', collapse='| ')
if(!is.numeric(x)) out<- paste(value, ' (N:', round(N) , ')',  sep='', collapse='|')
}
#########################################


#########################################
if(keyword=='MQQ'){
if(is.null(w)){
m1<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.5),   na.rm=TRUE, type=8), digits=digits)
q1<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.25),  na.rm=TRUE, type=8), digits=digits)
q3<-a.format_n.ade(quantile(x[cell_ids], probs=c(0.75),  na.rm=TRUE, type=8), digits=digits)
out<- paste(m1,' (',q1,'/',q3,')', sep='')
}

if(!is.null(w)){
m1<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.5)), digits=digits)
q1<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.25)), digits=digits)
q3<-a.format_n.ade(wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.75)), digits=digits)
out<- paste(m1,' (',q1,'/',q3,')', sep='')
}
}
#########################################


#########################################
if(keyword=='MSD'){
if(is.null(w)) w<- rep(1, length(x))
m1<-  a.format_n.ade(    wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE),  digits=digits)
vsd<- a.format_n.ade(sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE)), digits=digits)
out<- paste(m1,' (',vsd,')', sep='')
}
#########################################

#########################################
if(keyword=='M1SD'){
if(is.null(w)) w<- rep(1, length(x))
onesd<- sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE))
amean<- wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)
low<- a.format_n.ade(amean-onesd, digits=digits)
upp<- a.format_n.ade(amean+onesd, digits=digits)
out<- paste(low,' - ',upp, sep='')
}
#########################################

#########################################
if(keyword=='M2SD'){
if(is.null(w)) w<- rep(1, length(x))
onesd<- sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE))
amean<- wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)
low<- a.format_n.ade(amean-onesd*2, digits=digits)
upp<- a.format_n.ade(amean+onesd*2, digits=digits)
out<- paste(low,' - ',upp, sep='')
}
#########################################

#########################################
if(keyword=='M3SD'){
if(is.null(w)) w<- rep(1, length(x))
onesd<- sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE))
amean<- wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)
low<- a.format_n.ade(amean-onesd*3, digits=digits)
upp<- a.format_n.ade(amean+onesd*3, digits=digits)
out<- paste(low,' - ',upp, sep='')
}
#########################################

#########################################
if(keyword=='MM1SD'){
if(is.null(w)) w<- rep(1, length(x))
onesd<- sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE))
amean<- wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)
low<- a.format_n.ade(amean-onesd, digits=digits)
upp<- a.format_n.ade(amean+onesd, digits=digits)
out<- paste(a.format_n.ade(amean, digits=digits), ' (',low,' - ',upp,')', sep='')
}
#########################################

#########################################
if(keyword=='MM2SD'){
if(is.null(w)) w<- rep(1, length(x))
onesd<- sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE))
amean<- wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)
low<- a.format_n.ade(amean-onesd*2, digits=digits)
upp<- a.format_n.ade(amean+onesd*2, digits=digits)
out<- paste(a.format_n.ade(amean, digits=digits), ' (',low,' - ',upp,')', sep='')
}
#########################################

#########################################
if(keyword=='MM3SD'){
if(is.null(w)) w<- rep(1, length(x))
onesd<- sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE))
amean<- wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)
low<- a.format_n.ade(amean-onesd*3, digits=digits)
upp<- a.format_n.ade(amean+onesd*3, digits=digits)
out<- paste(a.format_n.ade(amean, digits=digits), ' (',low,' - ',upp,')', sep='')
}
#########################################


#########################################
if(keyword=='NORM50'){
if(is.null(w)) w<- rep(1, length(x))
onesd<- sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE))
amean<- wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)
low<- a.format_n.ade(amean-onesd*0.675, digits=digits)
upp<- a.format_n.ade(amean+onesd*0.675, digits=digits)
out<- paste(low,' - ',upp, sep='')
}
#########################################

#########################################
if(keyword=='NORM90'){
if(is.null(w)) w<- rep(1, length(x))
onesd<- sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE))
amean<- wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)
low<- a.format_n.ade(amean-onesd*1.645, digits=digits)
upp<- a.format_n.ade(amean+onesd*1.645, digits=digits)
out<- paste(low,' - ',upp, sep='')
}
#########################################

#########################################
if(keyword=='NORM95'){
if(is.null(w)) w<- rep(1, length(x))
onesd<- sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE))
amean<- wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)
low<- a.format_n.ade(amean-onesd*1.96, digits=digits)
upp<- a.format_n.ade(amean+onesd*1.96, digits=digits)
out<- paste(low,' - ',upp, sep='')
}
#########################################

#########################################
if(keyword=='NORM99'){
if(is.null(w)) w<- rep(1, length(x))
onesd<- sqrt(wtd.var(x[cell_ids], w[cell_ids], na.rm=TRUE))
amean<- wtd.mean(x[cell_ids], w[cell_ids], na.rm=TRUE)
low<- a.format_n.ade(amean-onesd*2.576, digits=digits)
upp<- a.format_n.ade(amean+onesd*2.576, digits=digits)
out<- paste(low,' - ',upp, sep='')
}
#########################################

#########################################
if(keyword=='GEO'){
if(is.null(w)) w<- rep(1, length(x))
if(any(x[cell_ids]<=0, na.rm=TRUE)) warning('geometric mean is only meaningful for true positive values')
w[cell_ids][which(is.na(x[cell_ids]))] <- NA
x[cell_ids][which(is.na(w[cell_ids]))] <- NA
ws <- sum(w[cell_ids], na.rm=TRUE)
gmean <- exp(sum(log(x[cell_ids])*w[cell_ids], na.rm=TRUE)/ws)
gmean <- a.format_n.ade(gmean, digits=digits)
out<- gmean
}
#########################################

#########################################
if(keyword=='HARM'){
if(is.null(w)) w<- rep(1, length(x))
if(any(x[cell_ids]==0, na.rm=TRUE)) gmean<-0
if(any(x[cell_ids]==0, na.rm=TRUE)) warning('harmonic mean with 0 values is set to 0')
if(!any(x[cell_ids]==0, na.rm=TRUE)){
w[cell_ids][which(is.na(x[cell_ids]))] <- NA
gmean <- sum(w[cell_ids], na.rm=TRUE)/sum(w[cell_ids]/x[cell_ids], na.rm=TRUE)
gmean <- a.format_n.ade(gmean, digits=digits)
}
out<- gmean
}
#########################################

#########################################
if(keyword=='TM1'){
if(is.null(w))   w<- rep(1, length(x))
ques <- wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.01, 0.99))
x_cut <- x[cell_ids]
w_cut <- w[cell_ids]
x_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- NA
w_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- NA
out<-a.format_n.ade(wtd.mean(x_cut, w_cut, na.rm=TRUE),  digits=digits)
}
#########################################

#########################################
if(keyword=='TM5'){
if(is.null(w))   w<- rep(1, length(x))
ques <- wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.05, 0.95))
x_cut <- x[cell_ids]
w_cut <- w[cell_ids]
x_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- NA
w_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- NA
out<-a.format_n.ade(wtd.mean(x_cut, w_cut, na.rm=TRUE),  digits=digits)
}
#########################################

#########################################
if(keyword=='TM10'){
if(is.null(w))   w<- rep(1, length(x))
ques <- wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.10, 0.90))
x_cut <- x[cell_ids]
w_cut <- w[cell_ids]
x_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- NA
w_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- NA
out<-a.format_n.ade(wtd.mean(x_cut, w_cut, na.rm=TRUE),  digits=digits)
}
#########################################

#########################################
if(keyword=='TM25'){
if(is.null(w))   w<- rep(1, length(x))
ques <- wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.25, 0.75))
x_cut <- x[cell_ids]
w_cut <- w[cell_ids]
x_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- NA
w_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- NA
out<-a.format_n.ade(wtd.mean(x_cut, w_cut, na.rm=TRUE),  digits=digits)
}
#########################################


#########################################
if(keyword=='WM1'){
if(is.null(w))   w<- rep(1, length(x))
ques <- wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.01, 0.99))
x_cut <- x[cell_ids]
w_cut <- w[cell_ids]
x_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- ques[1]
w_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- ques[2]
out<-a.format_n.ade(wtd.mean(x_cut, w_cut, na.rm=TRUE),  digits=digits)
}
#########################################

#########################################
if(keyword=='WM5'){
if(is.null(w))   w<- rep(1, length(x))
ques <- wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.05, 0.95))
x_cut <- x[cell_ids]
w_cut <- w[cell_ids]
x_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- ques[1]
w_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- ques[2]
out<-a.format_n.ade(wtd.mean(x_cut, w_cut, na.rm=TRUE),  digits=digits)
}
#########################################

#########################################
if(keyword=='WM10'){
if(is.null(w))   w<- rep(1, length(x))
ques <- wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.10, 0.90))
x_cut <- x[cell_ids]
w_cut <- w[cell_ids]
x_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- ques[1]
w_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- ques[2]
out<-a.format_n.ade(wtd.mean(x_cut, w_cut, na.rm=TRUE),  digits=digits)
}
#########################################

#########################################
if(keyword=='WM25'){
if(is.null(w))   w<- rep(1, length(x))
ques <- wtd.quantile(x[cell_ids], w[cell_ids], probs=c(0.25, 0.75))
x_cut <- x[cell_ids]
w_cut <- w[cell_ids]
x_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- ques[1]
w_cut[which(x_cut<ques[1] | x_cut>ques[2])] <- ques[2]
out<-a.format_n.ade(wtd.mean(x_cut, w_cut, na.rm=TRUE),  digits=digits)
}
#########################################


}
return(out)
}
