disco.all = pc.or(pc.skel(data.all, method="comb.fast"))
amat.all = disco.all$G==3
dag.all = pcalg2dagitty(amat.all, colnames(data.all))
adjustmentSets(dag.all, exposure="prisontime", outcome="recid")
coordinates(dag.all) =
  list(x=c(age=0, gender=1, race=2, drugs=3, married=4, school=5,
           alcohol=1, priors=4.4, property=0, personal=2, felony=5,
           prisontime=2, recid=4),
       y=c(age=0, gender=0, race=0, drugs=0, married=0, school=0,
           alcohol=1, priors=1, property=2, personal=2.2, felony=2,
           prisontime=3, recid=3))
plot(dag.all)
data.adj = data.frame(cbind(recid, prisontime, alcohol, priors, 
                            felony, personal, property))
model.adj = glm(recid~., data=data.adj, family="binomial")
summary(model.adj)

data.adjpar = data.adj[parole==1,]
model.adjpar = glm(recid~., data=data.adjpar, family="binomial")
summary(model.adjpar)
data.adjfel = data.adj[felony==1,]
model.adjfel = glm(recid~.-felony, data=data.adjfel, family="binomial")
summary(model.adjfel)
data.adjmis = data.adj[felony==0,]
model.adjmis = glm(recid~.-felony, data=data.adjmis, family="binomial")
summary(model.adjmis)

dag.data = list(data.adj, data.adjpar, data.adjfel, data.adjmis)
dag.model = list(model.adj, model.adjpar, model.adjfel, model.adjmis)
for(i in 1:length(dag.data)){
  dag.frame = dag.data[[i]]
  print(mean(predict(dag.model[[i]], dag.frame, type="response")))
  dag.frame$prisontime = dag.frame$prisontime + 1
  print(mean(predict(dag.model[[i]], dag.frame, type="response")))
}

survtime = ifelse(recid==1, returntime, followup)
data.cox = cbind(survtime, data.adj)
model.cox = coxph(Surv(survtime, recid)~., data=data.cox)
data.coxpar = data.cox[parole==1,]
model.coxpar = coxph(Surv(survtime, recid)~., data=data.coxpar)
data.coxfel = data.cox[felony==1,]
model.coxfel = coxph(Surv(survtime, recid)~., data=data.coxfel)
data.coxmis = data.cox[felony==0,]
model.coxmis = coxph(Surv(survtime, recid)~., data=data.coxmis)

cox.data = list(data.cox, data.coxpar, data.coxfel, data.coxmis)
cox.model = list(model.cox, model.coxpar, model.coxfel, model.coxmis)
for(i in 1:length(cox.data)){
  cox.frame = cox.data[[i]]
  print(1-mean(summary(survfit(cox.model[[i]], cox.frame), time=5)$surv))
  cox.frame$prisontime = cox.frame$prisontime + 1
  print(1-mean(summary(survfit(cox.model[[i]], cox.frame), time=5)$surv))
}
