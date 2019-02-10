# a couple things that might make resampling easier (possibly?)

#function to take an abundance vector and subsample to size
subsam<-function(ab_vec, size=sum(ab_vec)){
    inds<-unlist(lapply(1:length(ab_vec), function(x){
        rep(x, ab_vec[x])
    }))
    sam<-sample(inds, size=size, replace=F)
    ss<-unlist(lapply(1:length(ab_vec), function(y){
        length(which(sam==y))
    }))
    return(ss)
}
#take a subset of a bunch of community vectors, each subset of equal size 
subcom<-function(com, size){
    t(apply(com, 1, function(x){
        subsam(x, size)}
    ))
}