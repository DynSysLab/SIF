#--------------------------------------
# cooperation
#--------------------------------------

# import data
setwd(parent_dir)	# directory of raw data for cooperation
child_dir <- list.files()

# select data with > 6 minutes
dataset <- data.frame()
for(f in 1:length(child_dir)) {

	setwd(paste(parent_dir, child_dir[f], sep=''))

	tagger <- read.csv('taggers.csv', header=F)
	colnames(tagger) <- c('userID', 'gameID', 'clock_time', 'game_milliseconds', 'team_points',
		'button', 'action', 'mode', 'boat_position', 'rotation_y', 'rotation_x',
		'category', 'new_tag_screen_x', 'new_tag_screen_y', 'new_tag_screen_width',
		'new_tag_screen_height', 'new_tage_sphere_x', 'new_tage_sphere_y', 'new_tage_sphere_z',
		'new_tage_sphere_width', 'new_tage_sphere_height', 'zoom', 'target_0_tags',
		'target_0_time', 'target_1_tags', 'target_1_time', 'target_2_tags', 'target_2_time',
		'target_3_tags', 'target_3_time', 'target_4_tags', 'target_4_time', 'target_5_tags', 'target_5_time')

	valid_gameID <- c()	# game ID with 3 players & at least one tag / player
	for(g in 1:max(tagger$gameID)) {

		tag <- tagger[tagger$gameID==g,]
		tag$userID <- factor(tag$userID)
		for(t in 1:nlevels(tag$userID)) {
			bar <- tag$button[tag$userID==levels(tag$userID)[t]]			
			if('submit new tag' %in% bar) valid_gameID <- c(valid_gameID, g)
		}
		
	}
	valid_gameID <- as.numeric(names(table(valid_gameID))[table(valid_gameID)==3])

	duration <- c()
	for(g in 1:length(valid_gameID)) {

		tag <- tagger[tagger$gameID==valid_gameID[g],]
		duration <- c(duration, min(max(tag$game_milliseconds), 600000))
	}

	foo <- data.frame(batch=child_dir[f], gameID=valid_gameID, duration=duration)
	dataset <- rbind(dataset, foo)

}

dataset <- dataset[dataset$duration > 360000,]	# played more than 6 minutes

# clean up data
ALL <- list()
DECISION <- list()
DECISION_TIME <- list()
CURRENT_LOC <- OTHERS <- DISTANCE <- list()
MYTIME <- OTHERSTIME <- ALLTIME <- list()
MYTAGS <- OTHERSTAGS <- ALLTAGS <- list()

for(x in 1:nrow(dataset)) {

	setwd(paste(parent_dir, dataset$batch[x], sep=''))

	tagger <- read.csv('taggers.csv', header=F)
	colnames(tagger) <- c('userID', 'gameID', 'clock_time', 'game_milliseconds', 'team_points',
		'button', 'action', 'mode', 'boat_position', 'rotation_y', 'rotation_x',
		'category', 'new_tag_screen_x', 'new_tag_screen_y', 'new_tag_screen_width',
		'new_tag_screen_height', 'new_tage_sphere_x', 'new_tage_sphere_y', 'new_tage_sphere_z',
		'new_tage_sphere_width', 'new_tage_sphere_height', 'zoom', 'target_0_tags',
		'target_0_time', 'target_1_tags', 'target_1_time', 'target_2_tags', 'target_2_time',
		'target_3_tags', 'target_3_time', 'target_4_tags', 'target_4_time', 'target_5_tags', 'target_5_time')

	tag <- tagger[tagger$gameID==dataset$gameID[x],]
	tag$userID <- factor(tag$userID)

	enter <- c('enter 0', 'enter 1', 'enter 2', 'enter 3', 'enter 4', 'enter 5')
	exit <- c('exit 0', 'exit 1', 'exit 2', 'exit 3', 'exit 4', 'exit 5')
	submit <- 'submit new tag'

	speed <- 0.0002    # unit distance / mill`isecond -> 5 s to move between the targets

	max_time <- min(max(tag$game_milliseconds), 600000)

	XX <- list()
	for(t in 1:3) {

		X <- Y <- tag[tag$userID==levels(tag$userID)[t],]

		X <- X[X$button %in% c(enter, exit),]
		X$button <- as.character(X$button)

		XX[[t]] <- data.frame(time=0, loc=-1, action="start", direction=NA)

		# enter, exit, move
		for(i in 1:nrow(X)) {

			if(X$button[i] %in% c(exit)) {

				loc <- as.numeric(substr(X$button[i], nchar(X$button[i]), nchar(X$button[i])))
				foo <- data.frame(time=X$game_milliseconds[i], loc=loc, action='exit', direction=NA) 
				XX[[t]] <- rbind(XX[[t]], foo)
			}

			if(X$button[i] %in% c(enter)) {
				direction <- as.numeric(substr(X$button[i], nchar(X$button[i]), nchar(X$button[i])))
				foo <- data.frame(time=X$game_milliseconds[i], loc=NA, action='move', direction=direction) 
				XX[[t]] <- rbind(XX[[t]], foo)
			}
		}

		XX[[t]] <- XX[[t]][order(XX[[t]]$time),]
		XX[[t]]$loc[2] <- -1

		# remove consecutive "move" to the same direction
		flag <- c()
		for(i in 1:(nrow(XX[[t]])-1)) {
			if(XX[[t]]$action[i]=='move' & XX[[t]]$action[i+1]=='move') {
				if(XX[[t]]$direction[i]==XX[[t]]$direction[i+1]) {
					flag <- c(flag, i+1)
				}
			}
		}

		if(length(flag) > 0) XX[[t]] <- XX[[t]][-flag,]

		# fill in actions and locations
		for(i in 1:(nrow(XX[[t]])-1)) {

			if(XX[[t]]$action[i]=='exit') XX[[t]]$loc[i+1] <- XX[[t]]$loc[i]

			if(XX[[t]]$action[i]=='move' & XX[[t]]$action[i+1]=='move') {

				duration <- XX[[t]]$time[i+1] - XX[[t]]$time[i]
				sign <- sign(XX[[t]]$direction[i] - XX[[t]]$loc[i])
				XX[[t]]$loc[i+1] <- XX[[t]]$loc[i] + sign*duration*speed

				if(sign==sign(XX[[t]]$loc[i+1] - XX[[t]]$direction[i])) {
					XX[[t]]$loc[i+1] <- XX[[t]]$direction[i] - sign*0.0001  
				}
			}

			if(XX[[t]]$action[i]=='move' & XX[[t]]$action[i+1]=='exit') {

				dist <- abs(XX[[t]]$loc[i] - XX[[t]]$loc[i+1])
				time <- XX[[t]]$time[i] + dist/speed
				loc <- XX[[t]]$loc[i+1]
				foo <- data.frame(time=time, loc=loc, action='enter', direction=NA)

				XX[[t]] <- rbind(XX[[t]], foo)
			}
		}

		XX[[t]] <- XX[[t]][order(XX[[t]]$time),]

		# cut off data above max time
		XX[[t]] <- XX[[t]][XX[[t]]$time <= max_time,]

		# extend until end
		if(XX[[t]]$action[nrow(XX[[t]])]=='move') {

			dist <- abs(XX[[t]]$loc[nrow(XX[[t]])] - XX[[t]]$direction[nrow(XX[[t]])])
			time <- XX[[t]]$time[nrow(XX[[t]])] + dist/speed
			
			if(time < max_time) {
				foo <- data.frame(time=time, loc=XX[[t]]$direction[nrow(XX[[t]])],
				action='enter', direction=NA)
				bar <- data.frame(time=max_time, loc=XX[[t]]$direction[nrow(XX[[t]])],
				action='over', direction=NA)

				XX[[t]] <- rbind(XX[[t]], foo, bar)
			}

			if(time > max_time) {
				
				dist <- speed * (max_time - XX[[t]]$time[nrow(XX[[t]])])
				sign <- sign(XX[[t]]$direction[nrow(XX[[t]])] - XX[[t]]$loc[nrow(XX[[t]])])
				loc <- XX[[t]]$loc[nrow(XX[[t]])] + sign*dist

				bar <- data.frame(time=max_time, loc=loc, action='over', direction=NA)

				XX[[t]] <- rbind(XX[[t]], bar)
			}
		}

		if(XX[[t]]$action[nrow(XX[[t]])] %in% c('exit', 'enter')) {

			bar <- data.frame(time=max_time, loc=XX[[t]]$loc[nrow(XX[[t]])],
					action='over', direction=NA)
			XX[[t]] <- rbind(XX[[t]], bar)
			
		}

		# add tag times & locations
		Y <- Y[Y$button %in% submit,]
		YY <- data.frame(time=Y$game_milliseconds, loc=NA, action='tag', direction=NA)
		XX[[t]] <- rbind(XX[[t]], YY)
		XX[[t]] <- XX[[t]][order(XX[[t]]$time),]

		for(i in 2:nrow(XX[[t]])) {
			if(XX[[t]]$action[i]=='tag') XX[[t]]$loc[i] <- XX[[t]]$loc[i-1]
		}	

		XX[[t]]$min <- XX[[t]]$time/1000/60
		XX[[t]]$loc <- XX[[t]]$loc + 1
		XX[[t]]$direction[!is.na(XX[[t]]$direction)] <- XX[[t]]$direction[!is.na(XX[[t]]$direction)] + 1

		# remove "tag" or "exit" when loc is not an integer
		aa <- which(XX[[t]]$action %in% c('tag', 'exit') & XX[[t]]$loc%%1!=0)
		if(length(aa) > 0) XX[[t]] <- XX[[t]][-aa,]

		# remove duplicated time at the bottom of the df
		bb <- rle(XX[[t]]$time)$lengths
		if(bb[length(bb)] > 1) XX[[t]] <- XX[[t]][1:(nrow(XX[[t]])-bb[length(bb)]+1),]

		# remove 0 intevals
		XX[[t]]$interval <- NA

		for(i in 2:nrow(XX[[t]])) {

			if(XX[[t]]$action[i]=='tag') {
				XX[[t]]$interval[i] <- XX[[t]]$time[i] - XX[[t]]$time[i-1]
			}
		}

		aa <- which(XX[[t]]$interval==0)
		if(length(aa) > 0) XX[[t]] <- XX[[t]][-aa,]

	}

	decision <- list()
	decisionTime <- list()
	current_loc <- list()
	distance <- others <- list()
	myTime <- othersTime <- allTime <- list()
	myTags <- othersTags <- allTags <- list()
	for(t in 1:3) {

		# get decisions
		idx <- which(XX[[t]]$action=='move')
		decision[[t]] <- XX[[t]]$direction[idx]

		time <- XX[[t]]$time[idx]
		decisionTime[[t]] <- time

		##-----------------------------------------
		## 1. current location of themselves (choose close ones?)
		##-----------------------------------------
		current_loc[[t]] <- XX[[t]]$loc[idx]		# can be non-integer if they change direction during movement

		this_distance <- matrix(nrow=length(idx), ncol=6)
		for(i in 1:length(idx)) {
			this_distance[i,] <- 1:6 - current_loc[[t]][i]
		}

		distance[[t]] <- abs(this_distance)

		##-----------------------------------------
		## 2. previously visited locations (choose unvisited ones?)
		##-----------------------------------------
		this_myTime <- matrix(0, nrow=length(idx), ncol=6)
		for(i in 1:length(idx)) {

			bar <- XX[[t]][1:idx[i],]
		
			time_enter <- bar$time[bar$action=='enter']
			if(length(time_enter) > 0) {

				x1 <- bar$time[bar$action=='exit'] - bar$time[bar$action=='enter']
				x2 <- bar$loc[bar$action=='enter']

				for(j in 1:length(x2)) this_myTime[i,x2[j]] <- this_myTime[i,x2[j]] + x1[j]
			}
		}

		myTime[[t]] <- this_myTime

		##-----------------------------------------
		## 3. current locations of others (avoid others?)
		##-----------------------------------------
		a <- 1:3
		foo1 <- XX[[a[a!=t][1]]]
		foo2 <- XX[[a[a!=t][2]]]

		this_others <- matrix(0, nrow=length(idx), ncol=6)
		for(i in 1:length(idx)) {

			idx1 <- max(which(foo1$time < time[i]))
			idx2 <- max(which(foo2$time < time[i]))

			if(length(idx1) > 0 & foo1$action[idx1]!='move'){
				this_others[i, foo1$loc[idx1]] <- this_others[i, foo1$loc[idx1]] + 1
			}
			if(length(idx2) > 0 & foo2$action[idx2]!='move'){
				this_others[i, foo2$loc[idx2]] <- this_others[i, foo2$loc[idx2]] + 1
			}
		}

		others[[t]] <- this_others

		##-----------------------------------------
		## 4. numbers of tags at each location (less tagged locations?)
		##-----------------------------------------
		foo <- XX[[t]]

		this_tags <- matrix(0, nrow=length(idx), ncol=6)

		for(i in 1:length(idx)) {

			bar <- foo[foo$time < time[i],]
			a <- bar$loc[bar$action=='tag']

			for(j in 1:6) this_tags[i,j] <- length(a[a==j])

		}

		myTags[[t]] <- this_tags


		##-----------------------------------------
		## 4. numbers of tags at each location (less tagged locations?)
		##-----------------------------------------
		foo <- rbind(XX[[1]], XX[[2]], XX[[3]])
		foo <- foo[order(foo$time),]

		this_tags <- matrix(0, nrow=length(idx), ncol=6)

		for(i in 1:length(idx)) {

			bar <- foo[foo$time < time[i],]
			a <- bar$loc[bar$action=='tag']

			for(j in 1:6) this_tags[i,j] <- length(a[a==j])

		}

		allTags[[t]] <- this_tags

		##-----------------------------------------
		## 5. cumulative time at each location (less explored locations?)
		##-----------------------------------------
		this_sojourn <- matrix(0, nrow=length(idx), ncol=6)

		for(i in 1:length(idx)) {

			for(j in 1:3) {

				bar <- XX[[j]][XX[[j]]$time < time[i],]
				bar <- rbind(bar, bar[nrow(bar),])
				bar$time[nrow(bar)] <- time[i]
				levels(bar$action) <- c(levels(bar$action), 'exit')
				bar$action[nrow(bar)] <- 'exit'

				bar <- bar[bar$action %in% c('enter', 'exit'),]

				time_enter <- bar$time[bar$action=='enter']

				if(length(time_enter) > 0) {
					
					time_exit <- bar$time[bar$action=='exit']
					time_exit <- time_exit[1:length(time_enter)]
					tt <- time_exit - time_enter
					loc <- bar$loc[bar$action=='enter']

					for(k in 1:length(loc)) this_sojourn[i,loc[k]] <- this_sojourn[i,loc[k]] + tt[k]

				}		
			}
		}

		allTime[[t]] <- this_sojourn

		#--------------
		othersTime[[t]] <- Reduce('-', list(allTime[[t]], myTime[[t]]))
		othersTags[[t]] <- Reduce('-', list(allTags[[t]], myTags[[t]]))

	}

	ALL[[x]] <- XX
	DECISION[[x]] <- decision
	DECISION_TIME[[x]] <- decisionTime
	CURRENT_LOC[[x]] <- current_loc
	DISTANCE[[x]] <- distance
	OTHERS[[x]] <- others

	MYTIME[[x]] <- myTime
	ALLTIME[[x]] <- allTime
	OTHERSTIME[[x]] <- othersTime
	MYTAGS[[x]] <- myTags
	ALLTAGS[[x]] <- allTags
	OTHERSTAGS[[x]] <- othersTags
}

# unlist
decision <- list()
decisionTime <- list()
current_loc <- dist <- others <- list()
myTime <- othersTime <- allTime <- list()
myTags <- othersTags <- allTags <- list()	

for(i in 1:(length(ALL)*3)) {

	decision[[i]] <- DECISION[[ceiling(i/3)]][[(i-1)%%3+1]]
	decisionTime[[i]] <- DECISION_TIME[[ceiling(i/3)]][[(i-1)%%3+1]]
	current_loc[[i]] <- CURRENT_LOC[[ceiling(i/3)]][[(i-1)%%3+1]]
	dist[[i]] <- DISTANCE[[ceiling(i/3)]][[(i-1)%%3+1]]
	others[[i]] <- OTHERS[[ceiling(i/3)]][[(i-1)%%3+1]]
	myTags[[i]] <- MYTAGS[[ceiling(i/3)]][[(i-1)%%3+1]]
	othersTags[[i]] <- OTHERSTAGS[[ceiling(i/3)]][[(i-1)%%3+1]]
	allTags[[i]] <- ALLTAGS[[ceiling(i/3)]][[(i-1)%%3+1]]
	myTime[[i]] <- MYTIME[[ceiling(i/3)]][[(i-1)%%3+1]]
	othersTime[[i]] <- OTHERSTIME[[ceiling(i/3)]][[(i-1)%%3+1]]
	allTime[[i]] <- ALLTIME[[ceiling(i/3)]][[(i-1)%%3+1]]

}

# get rank of the selected
dist2 <- myTags2 <- othersTags2 <- myTime2 <- othersTime2 <- list()
others2 <- list()

for(player in 1:length(decision)) {

	dist2[[player]] <- dist[[player]]
	myTags2[[player]] <- myTags[[player]]
	othersTags2[[player]] <- othersTags[[player]]
	myTime2[[player]] <- myTime[[player]]
	othersTime2[[player]] <- othersTime[[player]]
	others2[[player]] <- others[[player]]

	for(i in 1:length(decision[[player]])) {

		# remove the first choice (no information)
		# remove the choice on the move
		if(current_loc[[player]][i]==0 | current_loc[[player]][i]%%1!=0) {
			dist2[[player]][i,] <- rep(NA, 6)
			myTags2[[player]][i,] <- rep(NA, 6)
			othersTags2[[player]][i,] <- rep(NA, 6)
			myTime2[[player]][i,] <- rep(NA, 6)
			othersTime2[[player]][i,] <- rep(NA, 6)
			others2[[player]][i,] <- rep(NA, 6)
		} else {	# NA for the current loc
			dist2[[player]][i, current_loc[[player]][i]] <- NA
			myTags2[[player]][i, current_loc[[player]][i]] <- NA
			othersTags2[[player]][i, current_loc[[player]][i]] <- NA
			myTime2[[player]][i, current_loc[[player]][i]] <- NA
			othersTime2[[player]][i, current_loc[[player]][i]] <- NA
			others2[[player]][i, current_loc[[player]][i]] <- NA

		}
	}
}

## convert the decision matrix to ranks
dist_rank <- list()
myTime_rank <- othersTime_rank <- list()
myTags_rank <- othersTags_rank <- list()
others_rank <- list()
for(player in 1:length(decision)) {

	dist_rank[[player]] <- matrix(nrow=nrow(dist2[[player]]), ncol=6)
	myTags_rank[[player]] <- matrix(nrow=nrow(myTags2[[player]]), ncol=6)
	othersTags_rank[[player]] <- matrix(nrow=nrow(othersTags2[[player]]), ncol=6)
	myTime_rank[[player]] <- matrix(nrow=nrow(myTime2[[player]]), ncol=6)
	othersTime_rank[[player]] <- matrix(nrow=nrow(othersTime2[[player]]), ncol=6)
	others_rank[[player]] <- matrix(nrow=nrow(others2[[player]]), ncol=6)

	for(i in 1:nrow(dist2[[player]])) {
	
		dist_rank[[player]][i,] <- rank(dist2[[player]][i,], ties.method='min', na.last='keep')
		myTags_rank[[player]][i,] <- rank(myTags2[[player]][i,], ties.method='min', na.last='keep')
		othersTags_rank[[player]][i,] <- rank(othersTags2[[player]][i,], ties.method='min', na.last='keep')
		myTime_rank[[player]][i,] <- rank(myTime2[[player]][i,], ties.method='min', na.last='keep')
		othersTime_rank[[player]][i,] <- rank(othersTime2[[player]][i,], ties.method='min', na.last='keep')
		others_rank[[player]][i,] <- rank(others2[[player]][i,], ties.method='min', na.last='keep')

	}
}

# decision matrix
summary_coll <- data.frame()
for(player in 1:length(decision)) {

	rank <- c()
	for(i in 1:length(decision[[player]])) {

		rank[1] <- dist_rank[[player]][i, decision[[player]][i]]
		rank[2] <- myTags_rank[[player]][i, decision[[player]][i]]
		rank[3] <- othersTags_rank[[player]][i, decision[[player]][i]]
		rank[4] <- myTime_rank[[player]][i, decision[[player]][i]]
		rank[5] <- othersTime_rank[[player]][i, decision[[player]][i]]
		rank[6] <- others_rank[[player]][i, decision[[player]][i]]

		foo <- data.frame(groupID=ceiling(player/3),
			playerID=player,
			choiceID=rep(i, 6),
			var=c('dist', 'myTags', 'othersTags', 'myTime', 'othersTime', 'others'),
			rank=rank,
			time=rep(decisionTime[[player]][i],6))

		summary_coll <- rbind(summary_coll, foo)
	}
}

summary_coll <- summary_coll[!is.na(summary_coll$rank),]
summary_coll$condition <- 'collaboration'

# permutation
a <- 1:6
set.seed(1212)
n_decisions <- unlist(lapply(decision, length))
perm_summary_coll <- matrix(nrow=sum(n_decisions)*6*10000, ncol=6)
for(itr in 1:10000) {

	for(player in 1:length(decision)) {

		rank <- c()
		for(i in 1:length(decision[[player]])) {

			if(current_loc[[player]][i]==0 | current_loc[[player]][i]%%1!=0) {

				random_choice <- decision[[player]][i]
				
			} else {

				random_choice <- sample(a[-current_loc[[player]][i]],1)

			}
			
			rank[1] <- dist_rank[[player]][i, random_choice]
			rank[2] <- myTags_rank[[player]][i, random_choice]
			rank[3] <- othersTags_rank[[player]][i, random_choice]
			rank[4] <- myTime_rank[[player]][i, random_choice]
			rank[5] <- othersTime_rank[[player]][i, random_choice]
			rank[6] <- others_rank[[player]][i, random_choice]

			# row index
			x <- sum(n_decisions)*(itr-1) + sum(c(0, n_decisions)[1:player])*6 + 6*i - 5
			perm_summary_coll[x:(x+5),1] <- rep(itr, 6)					# itr
			perm_summary_coll[x:(x+5),2] <- rep(ceiling(player/3),6) 	# groupID
			perm_summary_coll[x:(x+5),3] <- rep(player,6)				# playerID
			perm_summary_coll[x:(x+5),4] <- rep(i,6)					# choiceID
			perm_summary_coll[x:(x+5),5] <- rep(random_choice, 6)		# random_choice
			perm_summary_coll[x:(x+5),6] <- rank						# rank

		}		
	}
}

perm_summary_coll <- as.data.frame(perm_summary_coll)
colnames(perm_summary_coll) <- c('itr', 'groupID', 'playerID', 'choiceID',
	'random_choice', 'rank')
perm_summary_coll$var <- c('dist', 'myTags', 'othersTags', 'myTime', 'othersTime', 'others')
perm_summary_coll <- perm_summary_coll[!is.na(perm_summary_coll$rank),]

#--------------------------------------
# chisq goodness of fit test
p <- 1
var_name <- c('dist', 'myTags', 'othersTags', 'myTime', 'othersTime')
foo <- summary_coll[summary_coll$var==var_name[p],]
observed <- table($rank)

# expected null proportions
bar <- perm_summary_coll[perm_summary_coll$var==var_name[p],]
expected <- table(bar$rank)
expected <- expected/sum(expected)

chisq.test(x = observed, p = expected)




#--------------------------------------
# cooperation
#--------------------------------------

# import data
setwd(parent_dir)	# directory of raw data for competition
child_dir <- list.files()

# select data with > 6 minutes
dataset <- data.frame()
for(f in 1:length(child_dir)) {

	setwd(paste(parent_dir, child_dir[f], sep=''))

	tagger <- read.csv('taggers.csv', header=F)
	colnames(tagger) <- c('userID', 'gameID', 'clock_time', 'game_milliseconds', 'team_points',
		'button', 'action', 'mode', 'boat_position', 'rotation_y', 'rotation_x',
		'category', 'new_tag_screen_x', 'new_tag_screen_y', 'new_tag_screen_width',
		'new_tag_screen_height', 'new_tage_sphere_x', 'new_tage_sphere_y', 'new_tage_sphere_z',
		'new_tage_sphere_width', 'new_tage_sphere_height', 'zoom', 'target_0_tags',
		'target_0_time', 'target_1_tags', 'target_1_time', 'target_2_tags', 'target_2_time',
		'target_3_tags', 'target_3_time', 'target_4_tags', 'target_4_time', 'target_5_tags', 'target_5_time')

	valid_gameID <- c()	# game ID with 3 players & at least one tag / player
	for(g in 1:max(tagger$gameID)) {

		tag <- tagger[tagger$gameID==g,]
		tag$userID <- factor(tag$userID)
		for(t in 1:nlevels(tag$userID)) {
			bar <- tag$button[tag$userID==levels(tag$userID)[t]]			
			if('submit new tag' %in% bar) valid_gameID <- c(valid_gameID, g)
		}
		
	}
	valid_gameID <- as.numeric(names(table(valid_gameID))[table(valid_gameID)==3])

	duration <- c()
	for(g in 1:length(valid_gameID)) {

		tag <- tagger[tagger$gameID==valid_gameID[g],]
		duration <- c(duration, min(max(tag$game_milliseconds), 600000))
	}

	foo <- data.frame(batch=child_dir[f], gameID=valid_gameID, duration=duration)
	dataset <- rbind(dataset, foo)

}

dataset <- dataset[dataset$duration > 360000,]	# played more than 6 minutes

# clean up data
ALL <- list()
DECISION <- list()
DECISION_TIME <- list()
CURRENT_LOC <- OTHERS <- DISTANCE <- list()
MYTIME <- OTHERSTIME <- ALLTIME <- list()
MYTAGS <- OTHERSTAGS <- ALLTAGS <- list()

for(x in 1:nrow(dataset)) {

	setwd(paste(parent_dir, dataset$batch[x], sep=''))

	tagger <- read.csv('taggers.csv', header=F)
	colnames(tagger) <- c('userID', 'gameID', 'clock_time', 'game_milliseconds', 'team_points',
		'button', 'action', 'mode', 'boat_position', 'rotation_y', 'rotation_x',
		'category', 'new_tag_screen_x', 'new_tag_screen_y', 'new_tag_screen_width',
		'new_tag_screen_height', 'new_tage_sphere_x', 'new_tage_sphere_y', 'new_tage_sphere_z',
		'new_tage_sphere_width', 'new_tage_sphere_height', 'zoom', 'target_0_tags',
		'target_0_time', 'target_1_tags', 'target_1_time', 'target_2_tags', 'target_2_time',
		'target_3_tags', 'target_3_time', 'target_4_tags', 'target_4_time', 'target_5_tags', 'target_5_time')

	tag <- tagger[tagger$gameID==dataset$gameID[x],]
	tag$userID <- factor(tag$userID)

	enter <- c('enter 0', 'enter 1', 'enter 2', 'enter 3', 'enter 4', 'enter 5')
	exit <- c('exit 0', 'exit 1', 'exit 2', 'exit 3', 'exit 4', 'exit 5')
	submit <- 'submit new tag'

	speed <- 0.0002    # unit distance / mill`isecond -> 5 s to move between the targets

	max_time <- min(max(tag$game_milliseconds), 600000)

	XX <- list()
	for(t in 1:3) {

		X <- Y <- tag[tag$userID==levels(tag$userID)[t],]

		X <- X[X$button %in% c(enter, exit),]
		X$button <- as.character(X$button)

		XX[[t]] <- data.frame(time=0, loc=-1, action="start", direction=NA)

		# enter, exit, move
		for(i in 1:nrow(X)) {

			if(X$button[i] %in% c(exit)) {

				loc <- as.numeric(substr(X$button[i], nchar(X$button[i]), nchar(X$button[i])))
				foo <- data.frame(time=X$game_milliseconds[i], loc=loc, action='exit', direction=NA) 
				XX[[t]] <- rbind(XX[[t]], foo)
			}

			if(X$button[i] %in% c(enter)) {
				direction <- as.numeric(substr(X$button[i], nchar(X$button[i]), nchar(X$button[i])))
				foo <- data.frame(time=X$game_milliseconds[i], loc=NA, action='move', direction=direction) 
				XX[[t]] <- rbind(XX[[t]], foo)
			}
		}

		XX[[t]] <- XX[[t]][order(XX[[t]]$time),]
		XX[[t]]$loc[2] <- -1

		# remove consecutive "move" to the same direction
		flag <- c()
		for(i in 1:(nrow(XX[[t]])-1)) {
			if(XX[[t]]$action[i]=='move' & XX[[t]]$action[i+1]=='move') {
				if(XX[[t]]$direction[i]==XX[[t]]$direction[i+1]) {
					flag <- c(flag, i+1)
				}
			}
		}

		if(length(flag) > 0) XX[[t]] <- XX[[t]][-flag,]

		# fill in actions and locations
		for(i in 1:(nrow(XX[[t]])-1)) {

			if(XX[[t]]$action[i]=='exit') XX[[t]]$loc[i+1] <- XX[[t]]$loc[i]

			if(XX[[t]]$action[i]=='move' & XX[[t]]$action[i+1]=='move') {

				duration <- XX[[t]]$time[i+1] - XX[[t]]$time[i]
				sign <- sign(XX[[t]]$direction[i] - XX[[t]]$loc[i])
				XX[[t]]$loc[i+1] <- XX[[t]]$loc[i] + sign*duration*speed

				if(sign==sign(XX[[t]]$loc[i+1] - XX[[t]]$direction[i])) {
					XX[[t]]$loc[i+1] <- XX[[t]]$direction[i] - sign*0.0001  
				}
			}

			if(XX[[t]]$action[i]=='move' & XX[[t]]$action[i+1]=='exit') {

				dist <- abs(XX[[t]]$loc[i] - XX[[t]]$loc[i+1])
				time <- XX[[t]]$time[i] + dist/speed
				loc <- XX[[t]]$loc[i+1]
				foo <- data.frame(time=time, loc=loc, action='enter', direction=NA)

				XX[[t]] <- rbind(XX[[t]], foo)
			}
		}

		XX[[t]] <- XX[[t]][order(XX[[t]]$time),]

		# cut off data above max time
		XX[[t]] <- XX[[t]][XX[[t]]$time <= max_time,]

		# extend until end
		if(XX[[t]]$action[nrow(XX[[t]])]=='move') {

			dist <- abs(XX[[t]]$loc[nrow(XX[[t]])] - XX[[t]]$direction[nrow(XX[[t]])])
			time <- XX[[t]]$time[nrow(XX[[t]])] + dist/speed
			
			if(time < max_time) {
				foo <- data.frame(time=time, loc=XX[[t]]$direction[nrow(XX[[t]])],
				action='enter', direction=NA)
				bar <- data.frame(time=max_time, loc=XX[[t]]$direction[nrow(XX[[t]])],
				action='over', direction=NA)

				XX[[t]] <- rbind(XX[[t]], foo, bar)
			}

			if(time > max_time) {
				
				dist <- speed * (max_time - XX[[t]]$time[nrow(XX[[t]])])
				sign <- sign(XX[[t]]$direction[nrow(XX[[t]])] - XX[[t]]$loc[nrow(XX[[t]])])
				loc <- XX[[t]]$loc[nrow(XX[[t]])] + sign*dist

				bar <- data.frame(time=max_time, loc=loc, action='over', direction=NA)

				XX[[t]] <- rbind(XX[[t]], bar)
			}
		}

		if(XX[[t]]$action[nrow(XX[[t]])] %in% c('exit', 'enter')) {

			bar <- data.frame(time=max_time, loc=XX[[t]]$loc[nrow(XX[[t]])],
					action='over', direction=NA)
			XX[[t]] <- rbind(XX[[t]], bar)
			
		}

		# add tag times & locations
		Y <- Y[Y$button %in% submit,]
		YY <- data.frame(time=Y$game_milliseconds, loc=NA, action='tag', direction=NA)
		XX[[t]] <- rbind(XX[[t]], YY)
		XX[[t]] <- XX[[t]][order(XX[[t]]$time),]

		for(i in 2:nrow(XX[[t]])) {
			if(XX[[t]]$action[i]=='tag') XX[[t]]$loc[i] <- XX[[t]]$loc[i-1]
		}	

		XX[[t]]$min <- XX[[t]]$time/1000/60
		XX[[t]]$loc <- XX[[t]]$loc + 1
		XX[[t]]$direction[!is.na(XX[[t]]$direction)] <- XX[[t]]$direction[!is.na(XX[[t]]$direction)] + 1

		# remove "tag" or "exit" when loc is not an integer
		aa <- which(XX[[t]]$action %in% c('tag', 'exit') & XX[[t]]$loc%%1!=0)
		if(length(aa) > 0) XX[[t]] <- XX[[t]][-aa,]

		# remove duplicated time at the bottom of the df
		bb <- rle(XX[[t]]$time)$lengths
		if(bb[length(bb)] > 1) XX[[t]] <- XX[[t]][1:(nrow(XX[[t]])-bb[length(bb)]+1),]

		# remove 0 intevals
		XX[[t]]$interval <- NA

		for(i in 2:nrow(XX[[t]])) {

			if(XX[[t]]$action[i]=='tag') {
				XX[[t]]$interval[i] <- XX[[t]]$time[i] - XX[[t]]$time[i-1]
			}
		}

		aa <- which(XX[[t]]$interval==0)
		if(length(aa) > 0) XX[[t]] <- XX[[t]][-aa,]

	}

	decision <- list()
	decisionTime <- list()
	current_loc <- list()
	distance <- others <- list()
	myTime <- othersTime <- allTime <- list()
	myTags <- othersTags <- allTags <- list()
	for(t in 1:3) {

		# get decisions
		idx <- which(XX[[t]]$action=='move')
		decision[[t]] <- XX[[t]]$direction[idx]

		time <- XX[[t]]$time[idx]
		decisionTime[[t]] <- time

		##-----------------------------------------
		## 1. current location of themselves (choose close ones?)
		##-----------------------------------------
		current_loc[[t]] <- XX[[t]]$loc[idx]		# can be non-integer if they change direction during movement

		this_distance <- matrix(nrow=length(idx), ncol=6)
		for(i in 1:length(idx)) {
			this_distance[i,] <- 1:6 - current_loc[[t]][i]
		}

		distance[[t]] <- abs(this_distance)

		##-----------------------------------------
		## 2. previously visited locations (choose unvisited ones?)
		##-----------------------------------------
		this_myTime <- matrix(0, nrow=length(idx), ncol=6)
		for(i in 1:length(idx)) {

			bar <- XX[[t]][1:idx[i],]
		
			time_enter <- bar$time[bar$action=='enter']
			if(length(time_enter) > 0) {

				x1 <- bar$time[bar$action=='exit'] - bar$time[bar$action=='enter']
				x2 <- bar$loc[bar$action=='enter']

				for(j in 1:length(x2)) this_myTime[i,x2[j]] <- this_myTime[i,x2[j]] + x1[j]
			}
		}

		myTime[[t]] <- this_myTime

		##-----------------------------------------
		## 3. current locations of others (avoid others?)
		##-----------------------------------------
		a <- 1:3
		foo1 <- XX[[a[a!=t][1]]]
		foo2 <- XX[[a[a!=t][2]]]

		this_others <- matrix(0, nrow=length(idx), ncol=6)
		for(i in 1:length(idx)) {

			idx1 <- max(which(foo1$time < time[i]))
			idx2 <- max(which(foo2$time < time[i]))

			if(length(idx1) > 0 & foo1$action[idx1]!='move'){
				this_others[i, foo1$loc[idx1]] <- this_others[i, foo1$loc[idx1]] + 1
			}
			if(length(idx2) > 0 & foo2$action[idx2]!='move'){
				this_others[i, foo2$loc[idx2]] <- this_others[i, foo2$loc[idx2]] + 1
			}
		}

		others[[t]] <- this_others

		##-----------------------------------------
		## 4. numbers of tags at each location (less tagged locations?)
		##-----------------------------------------
		foo <- XX[[t]]

		this_tags <- matrix(0, nrow=length(idx), ncol=6)

		for(i in 1:length(idx)) {

			bar <- foo[foo$time < time[i],]
			a <- bar$loc[bar$action=='tag']

			for(j in 1:6) this_tags[i,j] <- length(a[a==j])

		}

		myTags[[t]] <- this_tags


		##-----------------------------------------
		## 4. numbers of tags at each location (less tagged locations?)
		##-----------------------------------------
		foo <- rbind(XX[[1]], XX[[2]], XX[[3]])
		foo <- foo[order(foo$time),]

		this_tags <- matrix(0, nrow=length(idx), ncol=6)

		for(i in 1:length(idx)) {

			bar <- foo[foo$time < time[i],]
			a <- bar$loc[bar$action=='tag']

			for(j in 1:6) this_tags[i,j] <- length(a[a==j])

		}

		allTags[[t]] <- this_tags

		##-----------------------------------------
		## 5. cumulative time at each location (less explored locations?)
		##-----------------------------------------
		this_sojourn <- matrix(0, nrow=length(idx), ncol=6)

		for(i in 1:length(idx)) {

			for(j in 1:3) {

				bar <- XX[[j]][XX[[j]]$time < time[i],]
				bar <- rbind(bar, bar[nrow(bar),])
				bar$time[nrow(bar)] <- time[i]
				levels(bar$action) <- c(levels(bar$action), 'exit')
				bar$action[nrow(bar)] <- 'exit'

				bar <- bar[bar$action %in% c('enter', 'exit'),]

				time_enter <- bar$time[bar$action=='enter']

				if(length(time_enter) > 0) {
					
					time_exit <- bar$time[bar$action=='exit']
					time_exit <- time_exit[1:length(time_enter)]
					tt <- time_exit - time_enter
					loc <- bar$loc[bar$action=='enter']

					for(k in 1:length(loc)) this_sojourn[i,loc[k]] <- this_sojourn[i,loc[k]] + tt[k]

				}		
			}
		}

		allTime[[t]] <- this_sojourn

		#--------------
		othersTime[[t]] <- Reduce('-', list(allTime[[t]], myTime[[t]]))
		othersTags[[t]] <- Reduce('-', list(allTags[[t]], myTags[[t]]))

	}

	ALL[[x]] <- XX
	DECISION[[x]] <- decision
	DECISION_TIME[[x]] <- decisionTime
	CURRENT_LOC[[x]] <- current_loc
	DISTANCE[[x]] <- distance
	OTHERS[[x]] <- others

	MYTIME[[x]] <- myTime
	ALLTIME[[x]] <- allTime
	OTHERSTIME[[x]] <- othersTime
	MYTAGS[[x]] <- myTags
	ALLTAGS[[x]] <- allTags
	OTHERSTAGS[[x]] <- othersTags
}

# unlist
decision <- list()
decisionTime <- list()
current_loc <- dist <- others <- list()
myTime <- othersTime <- allTime <- list()
myTags <- othersTags <- allTags <- list()	

for(i in 1:(length(ALL)*3)) {

	decision[[i]] <- DECISION[[ceiling(i/3)]][[(i-1)%%3+1]]
	decisionTime[[i]] <- DECISION_TIME[[ceiling(i/3)]][[(i-1)%%3+1]]
	current_loc[[i]] <- CURRENT_LOC[[ceiling(i/3)]][[(i-1)%%3+1]]
	dist[[i]] <- DISTANCE[[ceiling(i/3)]][[(i-1)%%3+1]]
	others[[i]] <- OTHERS[[ceiling(i/3)]][[(i-1)%%3+1]]
	myTags[[i]] <- MYTAGS[[ceiling(i/3)]][[(i-1)%%3+1]]
	othersTags[[i]] <- OTHERSTAGS[[ceiling(i/3)]][[(i-1)%%3+1]]
	allTags[[i]] <- ALLTAGS[[ceiling(i/3)]][[(i-1)%%3+1]]
	myTime[[i]] <- MYTIME[[ceiling(i/3)]][[(i-1)%%3+1]]
	othersTime[[i]] <- OTHERSTIME[[ceiling(i/3)]][[(i-1)%%3+1]]
	allTime[[i]] <- ALLTIME[[ceiling(i/3)]][[(i-1)%%3+1]]

}

# get rank of the selected
dist2 <- myTags2 <- othersTags2 <- myTime2 <- othersTime2 <- list()
others2 <- list()

for(player in 1:length(decision)) {

	dist2[[player]] <- dist[[player]]
	myTags2[[player]] <- myTags[[player]]
	othersTags2[[player]] <- othersTags[[player]]
	myTime2[[player]] <- myTime[[player]]
	othersTime2[[player]] <- othersTime[[player]]
	others2[[player]] <- others[[player]]

	for(i in 1:length(decision[[player]])) {

		# remove the first choice (no information)
		# remove the choice on the move
		if(current_loc[[player]][i]==0 | current_loc[[player]][i]%%1!=0) {
			dist2[[player]][i,] <- rep(NA, 6)
			myTags2[[player]][i,] <- rep(NA, 6)
			othersTags2[[player]][i,] <- rep(NA, 6)
			myTime2[[player]][i,] <- rep(NA, 6)
			othersTime2[[player]][i,] <- rep(NA, 6)
			others2[[player]][i,] <- rep(NA, 6)
		} else {	# NA for the current loc
			dist2[[player]][i, current_loc[[player]][i]] <- NA
			myTags2[[player]][i, current_loc[[player]][i]] <- NA
			othersTags2[[player]][i, current_loc[[player]][i]] <- NA
			myTime2[[player]][i, current_loc[[player]][i]] <- NA
			othersTime2[[player]][i, current_loc[[player]][i]] <- NA
			others2[[player]][i, current_loc[[player]][i]] <- NA

		}
	}
}

## convert the decision matrix to ranks
dist_rank <- list()
myTime_rank <- othersTime_rank <- list()
myTags_rank <- othersTags_rank <- list()
others_rank <- list()
for(player in 1:length(decision)) {

	dist_rank[[player]] <- matrix(nrow=nrow(dist2[[player]]), ncol=6)
	myTags_rank[[player]] <- matrix(nrow=nrow(myTags2[[player]]), ncol=6)
	othersTags_rank[[player]] <- matrix(nrow=nrow(othersTags2[[player]]), ncol=6)
	myTime_rank[[player]] <- matrix(nrow=nrow(myTime2[[player]]), ncol=6)
	othersTime_rank[[player]] <- matrix(nrow=nrow(othersTime2[[player]]), ncol=6)
	others_rank[[player]] <- matrix(nrow=nrow(others2[[player]]), ncol=6)

	for(i in 1:nrow(dist2[[player]])) {
	
		dist_rank[[player]][i,] <- rank(dist2[[player]][i,], ties.method='min', na.last='keep')
		myTags_rank[[player]][i,] <- rank(myTags2[[player]][i,], ties.method='min', na.last='keep')
		othersTags_rank[[player]][i,] <- rank(othersTags2[[player]][i,], ties.method='min', na.last='keep')
		myTime_rank[[player]][i,] <- rank(myTime2[[player]][i,], ties.method='min', na.last='keep')
		othersTime_rank[[player]][i,] <- rank(othersTime2[[player]][i,], ties.method='min', na.last='keep')
		others_rank[[player]][i,] <- rank(others2[[player]][i,], ties.method='min', na.last='keep')

	}
}

# decision matrix
summary_comp <- data.frame()
for(player in 1:length(decision)) {

	rank <- c()
	for(i in 1:length(decision[[player]])) {

		rank[1] <- dist_rank[[player]][i, decision[[player]][i]]
		rank[2] <- myTags_rank[[player]][i, decision[[player]][i]]
		rank[3] <- othersTags_rank[[player]][i, decision[[player]][i]]
		rank[4] <- myTime_rank[[player]][i, decision[[player]][i]]
		rank[5] <- othersTime_rank[[player]][i, decision[[player]][i]]
		rank[6] <- others_rank[[player]][i, decision[[player]][i]]

		foo <- data.frame(groupID=ceiling(player/3),
			playerID=player,
			choiceID=rep(i, 6),
			var=c('dist', 'myTags', 'othersTags', 'myTime', 'othersTime', 'others'),
			rank=rank,
			time=rep(decisionTime[[player]][i],6))

		summary_comp <- rbind(summary_comp, foo)
	}
}

summary_comp <- summary_comp[!is.na(summary_comp$rank),]
summary_comp$condition <- 'competition'

# permutation
a <- 1:6
set.seed(1212)
n_decisions <- unlist(lapply(decision, length))
perm_summary_comp <- matrix(nrow=sum(n_decisions)*6*10000, ncol=6)
for(itr in 1:10000) {

	for(player in 1:length(decision)) {

		rank <- c()
		for(i in 1:length(decision[[player]])) {

			if(current_loc[[player]][i]==0 | current_loc[[player]][i]%%1!=0) {

				random_choice <- decision[[player]][i]
				
			} else {

				random_choice <- sample(a[-current_loc[[player]][i]],1)

			}
			
			rank[1] <- dist_rank[[player]][i, random_choice]
			rank[2] <- myTags_rank[[player]][i, random_choice]
			rank[3] <- othersTags_rank[[player]][i, random_choice]
			rank[4] <- myTime_rank[[player]][i, random_choice]
			rank[5] <- othersTime_rank[[player]][i, random_choice]
			rank[6] <- others_rank[[player]][i, random_choice]

			# row index
			x <- sum(n_decisions)*(itr-1) + sum(c(0, n_decisions)[1:player])*6 + 6*i - 5
			perm_summary_comp[x:(x+5),1] <- rep(itr, 6)					# itr
			perm_summary_comp[x:(x+5),2] <- rep(ceiling(player/3),6) 	# groupID
			perm_summary_comp[x:(x+5),3] <- rep(player,6)				# playerID
			perm_summary_comp[x:(x+5),4] <- rep(i,6)					# choiceID
			perm_summary_comp[x:(x+5),5] <- rep(random_choice, 6)		# random_choice
			perm_summary_comp[x:(x+5),6] <- rank						# rank

		}		
	}
}

perm_summary_comp <- as.data.frame(perm_summary_comp)
colnames(perm_summary_comp) <- c('itr', 'groupID', 'playerID', 'choiceID',
	'random_choice', 'rank')
perm_summary_comp$var <- c('dist', 'myTags', 'othersTags', 'myTime', 'othersTime', 'others')
perm_summary_comp <- perm_summary_comp[!is.na(perm_summary_comp$rank),]

#--------------------------------------
# chisq goodness of fit test
p <- 1
var_name <- c('dist', 'myTags', 'othersTags', 'myTime', 'othersTime')
foo <- summary_comp[summary_comp$var==var_name[p],]
observed <- table($rank)

# expected null proportions
bar <- perm_summary_comp[perm_summary_comp$var==var_name[p],]
expected <- table(bar$rank)
expected <- expected/sum(expected)

chisq.test(x = observed, p = expected)




#-----------------------------
# ordinal regression
#-----------------------------
library(ordinal)
library(emmeans)

summary <- rbind(summary_coll, summary_comp)
summary$condition <- as.factor(summary$condition)

var_name <- c('dist', 'myTags', 'othersTags', 'myTime', 'othersTime')
foo <- summary[summary$var %in% var_name,]
foo$var <- factor(foo$var)
foo$rank <- factor(foo$rank, ordered=T, levels=1:5)

model <- clmm(rank ~ var * condition + (1|playerID), data = foo)
model0 <- clmm(rank ~ var + condition + (1|playerID), data = foo)

anova(model, model0)

### posthoc
means <- emmeans(model, ~ var * condition)
contrast(means, list(
	'distCollab-distComp' = c(1,0,0,0,0,-1,0,0,0,0),
	'myTagsCollab-myTagsComp' = c(0,1,0,0,0,0,-1,0,0,0),
	'othersTagCollab-othersTagComp' = c(0,0,1,0,0,0,0,-1,0,0),
	'myTimeCollab-myTimeComp' = c(0,0,0,1,0,0,0,0,-1,0),
	'othesTimeCollab-othesTimeComp' = c(0,0,0,0,1,0,0,0,0,-1),

	'distCollab-myTagsCollab' = c(1,-1,0,0,0,0,0,0,0,0),
	'distCollab-othersTagsCollab' = c(1,0,-1,0,0,0,0,0,0,0),
	'distCollab-myTimeCollab' = c(1,0,0,-1,0,0,0,0,0,0),
	'distCollab-othersTimeCollab' = c(1,0,0,0,-1,0,0,0,0,0),
	'myTagsCollab-othersTagsCollab' = c(0,1,-1,0,0,0,0,0,0,0),
	'myTagsCollab-myTimeCollab' = c(0,1,0,-1,0,0,0,0,0,0),
	'myTagsCollab-othersTimeCollab' = c(0,1,0,0,-1,0,0,0,0,0),
	'othersTagsCollab-myTimeCollab' = c(0,0,1,-1,0,0,0,0,0,0),
	'othersTagsCollab-othersTimeCollab' = c(0,0,1,0,-1,0,0,0,0,0),
	'myTimeCollab-othersTimeCollab' = c(0,0,0,1,-1,0,0,0,0,0),

	'distComp-myTagsComp' = c(0,0,0,0,0,1,-1,0,0,0),
	'distComp-othersTagsComp' = c(0,0,0,0,0,1,0,-1,0,0),
	'distComp-myTimeComp' = c(0,0,0,0,0,1,0,0,-1,0),
	'distComp-othersTimeComp' = c(0,0,0,0,0,1,0,0,0,-1),
	'myTagsComp-othersTagsComp' = c(0,0,0,0,0,0,1,-1,0,0),
	'myTagsComp-myTimeComp' = c(0,0,0,0,0,0,1,0,-1,0),
	'myTagsComp-othersTimeComp' = c(0,0,0,0,0,0,1,0,0,-1),
	'othersTagsComp-myTimeComp' = c(0,0,0,0,0,0,0,1,-1,0),
	'othersTagsComp-othersTimeComp' = c(0,0,0,0,0,0,0,1,0,-1),
	'myTimeComp-othersTimeComp' = c(0,0,0,0,0,0,0,0,1,-1)),
	adjust = 'fdr')