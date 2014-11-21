fptp2av <-
function(data=NULL, link=NULL){
	if(is.null(data)){
	stop("A valid FPTP vote dataset is needed to perform the simulations.")
	}
	else{
		if(is.null(link)){
		stop("A valid link relation for candidates/parties is expected.")
		}
		else{
	start_time<-(Sys.time())
	new1<-NULL;candidates<-NULL;vote_spread<-NULL;
	
	.District <-
	function(name, electorate, each_row, link, max_vote_length){
		votes<-list()
		for(candid in 1:length(each_row[,candidates])){
			followers<-link[[as.character(each_row[candid,candidates])]]
			votes_vec<-list()
			if(!is.null(followers)){
				ball_s<-vector(length=max_vote_length)
				for(i in 1:max_vote_length){
					ballot<-vector()
					ballot[[1]]<-as.character(each_row[candid,candidates])
					ball<-.add_followers(i, followers, each_row)
					ballot<-na.omit(c(ballot,ball))
					ball_s[i]<-list(c(ballot))
				}
				depth_vec<-sample(1:max_vote_length, as.numeric(as.character(each_row[candid,vote_spread])), replace=T)
				depth_vec<-data.table::data.table(depth_vec)
				depth_vec<-depth_vec[,c("new1"):=.add_ballot(depth_vec,ball_s)]
				votes_1<-depth_vec[,new1]
			} else {
				ballot<-vector()
				ballot[[1]]<-as.character(each_row[candid,candidates])
				votes_1<-rep(list(c(ballot)), as.numeric(as.character(each_row[candid,vote_spread])))
			}
		
	    # votes_1<-rep(list(c(ballot)), as.numeric(as.character(each_row[candid,vote_spread])))
	    votes<-c(votes,list(votes_1))
		names(votes)<-c(names(votes)[1:(length(votes)-1) ], as.character(each_row[candid,candidates]))
	  }
	  return(votes)
	}
	.add_ballot <-
	function(x, ball_s){
		r1<-ball_s[x]
		return(r1)
	}
	.add_followers <- function(depth, followers, each_row){
	#vs<-as.numeric(as.character(each_row[,vote_spread]))
	ball<-character(depth)
	for(i in 1: depth){
		if(length(followers)>=i){
		#.follower_probability_map<-.construct_follower_probability_map(followers, vs)[[1]]
		#.vote_count<-.construct_follower_probability_map(followers, vs)[[2]]
		tmp<-.construct_follower_probability_map(followers, each_row)
		follower_probability_map<-tmp[[1]]
		vote_count<-tmp[[2]]
		follower<-.get_follower(follower_probability_map, vote_count)
		}else break
		if(length(follower!=0)) ball[i]<-follower
	}
	return(ball)
}
.construct_follower_probability_map <-
function(followers, each_row){
	follower_probability_map<-list()
	follower_probability_names<-vector()
	counter<-0
	vt<-F
	for(f in 1: length(followers)){
		for(v in 1:length(each_row[,candidates])){
			if(followers[[f]]==as.character(each_row[v,candidates])) {vt<-T}
		}
		if(vt){
		follower_probability_map<-c(follower_probability_map, list(c(counter, counter + as.numeric(as.character(each_row[each_row$candidates==followers[[f]],vote_spread])))))
		follower_probability_names<-c(follower_probability_names, followers[[f]])
		counter<-counter+as.numeric(as.character(each_row[each_row$candidates==followers[[f]],vote_spread]))+1
		follower_probability_map<-structure(follower_probability_map, .Names=follower_probability_names)
		}else{as.null(follower_probability_map)}
		vt<-F
	}
		return(c(list(follower_probability_map),list(counter)))
}
.get_follower <-
function(follower_probability_map, vote_count){
	follower_names<-names(follower_probability_map)
	new_f<-vector()
	idx<-sample(1:vote_count,size=1)
	#idx<-min(5,vote_count)
	if(length(follower_probability_map)!=0){
		for(f in 1:length(follower_probability_map)){
		#print(follower_probability_map[[f]])
			l_u<-follower_probability_map[[f]]
			if(idx>=l_u[[1]] && idx<=l_u[[2]]) new_f<-c(new_f, follower_names[[f]])
		}
	}else(new_f<-as.null(new_f))
	return(new_f)
}
.is.winner <-
function(count_first,limit){
	is_winner<-F
	if(length(count_first)==1)is_winner<-T
	for(n in 1:length(count_first)){
		if(length(count_first[[n]])>limit) is_winner<-T
	}
	return(is_winner)
}

.av.count <-
function(votes, counter){
	limit<-as.integer(sum(sapply(1:length(votes),function(x) length(votes[[x]])))/2)
	count_first<-votes
	is_w<-.is.winner(count_first,limit)
	counter1<-counter+1
	while(!is_w){
		list_names<-unlist(lapply(count_first,length))
		loser<-names(list_names)[which(list_names==min(list_names))]
		weakest_votes<-count_first[[which(list_names==min(list_names))]]
		count_first<-count_first[which(list_names==min(list_names))*-1]
		#is_w<-.is.winner(count_first,limit)
		if(length(count_first)<2)break
		count_first<-.redistributed_votes(count_first,weakest_votes)
		is_w<-.is.winner(count_first,limit)
		#print(is_w)
		#unlist(lapply(count_first,length))
		counter1<-counter1+1
	}
	return(c(list(count_first),list(counter1)))
}
.redistributed_votes <-
function(count_first,votes){
	for(i in 1:length(votes)){
		b=F
		if(length(votes[[i]])>1){
			for(j in 2:length(votes[[i]])){
				#if(!is.na(votes[[i]][[j]])){
				name<-votes[[i]][[j]]
				for(k in 1:length(count_first)){
					if(name==names(count_first)[[k]]){
					    #print(paste(i,j,k,length(votes[[i]])))
					    count_first[[k]]<-c(count_first[[k]],list(c(votes[[i]])))
					    b=T
					    #print(paste(i,j,k,b,length(votes[[i]])))
					}
				}
						if(b) break
			}
		}
	}
	return(count_first)
}
.get_results <-
function(votes, counter){
	fptp_c<-unlist(lapply(votes,length))
	av1<-.av.count(votes, counter)
	cc1<-av1[[1]]
	counter<-av1[[2]]
	av_c<-unlist(lapply(cc1,length))
  print(av_c)
	fptp_winner<-names(fptp_c)[which(fptp_c==max(fptp_c))]
	fptp_votes<-max(fptp_c)
	av_winner<-names(av_c)[which(av_c==max(av_c))]
	av_votes<-max(av_c)
	return(c(fptp_winner, fptp_votes, av_winner, av_votes, counter))
}
.timediff <-
function(start_time) {
  start_time <- as.POSIXct(start_time)
  dt <- difftime(Sys.time(), start_time, units="secs")
  # Since I only want the H:M:S, it can ignore the date...
  # be careful about time-zone issues
  format(.POSIXct(dt,tz="GMT"), "%H:%M:%S")
}


column.names<-c("District", "Turnout", "Outcome", "FPTP.Winner", "FPTP.Winner.Votes",
	 "AV.Winner", "AV.Winner.Votes")

data.to.read<-data.table::data.table(data)
parties<-names(data.to.read)[c(7:ncol(data.to.read))]

first<-T
for(row in 1:(nrow(data.to.read)-1)){
#for (row in 1:4){
  each_row<-cbind(c(names(data.to.read[row])),c(t(data.to.read[row])))
  name<-each_row[2,2]
  electorate<-as.integer(each_row[6,2])
  each_row<-data.table::as.data.table(each_row[complete.cases(each_row),])
  each_row<-each_row[7:nrow(each_row)]
  data.table::setnames(each_row,c("V1","V2"),c("candidates", "vote_spread"))

	print(paste(name," - ",electorate," - ", paste(each_row[,candidates], collapse=" ")) )
	print(each_row)
	#write the data to a matrix
	if(first==T){
	first=F
	new.colnames<-c("District", "Turnout",
		 "Outcome", "FPTP.Winner", "FPTP.Winner.Votes", "AV.Winner", "AV.Winner.Votes", "Rounds")
	data.to.write<-matrix(nrow=0,ncol=8,dimnames=list(c(),new.colnames))
	}
	votess<-.District(name, electorate, each_row, link, 4)
	counter<-0
	results_vec<-.get_results(votess, counter)
  #print(.District(name, electorate, each_row, link,4))
	fptp_winner<-results_vec[[1]]
	fptp_votes<-results_vec[[2]]
	av_winner<-results_vec[[3]]
	av_votes<- results_vec[[4]]
	av_counter<- results_vec[[5]]
if(fptp_winner==av_winner){outcome<-"Unaltered"} 	else{outcome<-"Altered"}
	newrow<-c(name,toString(electorate),outcome,fptp_winner,toString(fptp_votes),av_winner,toString(av_votes),toString(av_counter))
	data.to.write<-rbind(data.to.write,c(newrow))
	message("Done at ", .timediff(start_time))
	} 
outfile <- paste("Sim_data_in",format(Sys.time(),".%d.%b.%Y-%H:%M:%S"),".csv",sep="")
write.csv(data.to.write, file=outfile, row.names=FALSE)
		}
	}
}
