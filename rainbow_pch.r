
rainbow_pch<-function(x){
    	if(x<0){
		stop("no negative values are acepted in rainbow_pch function");
	}
	if(x<=30){
		sep=30/x
		list=c(0:25,"@","+","%","#");
		return(list[seq(1,30,by=sep)]);
	}else{
		list=c(LETTERS[seq( from = 1, to = 26)],letters[seq( from = 1, to = 26)],0:25,"@","+","%","#",",",".","'","?","*","/");
		if(x>88){
			sep=x/88;
			for(i in 1:(sep+1)){
				list2=c(list2,list)
			}
			list=list2;
		}
		return(list[c(1:x)]);
	}
}



