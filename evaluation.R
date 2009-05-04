##############################################################################
#    Default values for parameters                                           #
##############################################################################

turns      = 40    # Length of the sequence of items to select
iterations = 20    # Number of times each experiment is repeated
PERSONS    = 5     # Number of persons in the group
RETRIEVAL  = 15    # Number of candidate items selected at each turn
WEIGHTED   = 1     # Whether past satisfaction affects next selections
CHI        = 0.8   # Measure by which past satisfaction affects satisfaction
IOTA       = 0.4   # Initial satisfaction of each person
MISERY     = -0.75 # Threshold of minimum preference acceptable
BIMODAL    = FALSE # Whether preferences follow a bimodal distribution
BUMP       = -1    # Factor of bump in a bimodal distribution of preferences


##############################################################################
#    Generation of random individual preferences                             #
##############################################################################

generate <- function(persons=PERSONS, retrieval=RETRIEVAL, misery=MISERY, iota=IOTA, weighted=WEIGHTED, bimodal=BIMODAL, bump=BUMP, ratings=NULL, chi=CHI) {

 combine_preferences <- function(current_preferences, past_satisfactions, misery, weighted)
  if (any(current_preferences < misery)) NA else 
  if(all(past_satisfactions == 1)) 0 else # rep(0, length(current_preferences)) else 
  sum((1-past_satisfactions)**weighted*current_preferences)/(sum((1-past_satisfactions)**weighted))
 
 combine_satisfactions <- function(pref, sat, step, chi=CHI)
  sat + (0.5*(pref + 1)-sat)/sum(chi**(0:step))
  
 create_preferences  <- function(rows, cols, bimodal=BIMODAL, bump=BUMP) {
  pref = array(runif(rows*cols), dim=c(rows, cols))*(1-bump) + bump
  if(bimodal) {
   pref[(1:ceiling(rows/2)),(1:ceiling(cols/2))]   = -pref[(1:ceiling(rows/2)),(1:ceiling(cols/2))]
   pref[-(1:ceiling(rows/2)),-(1:ceiling(cols/2))] = -pref[-(1:ceiling(rows/2)),-(1:ceiling(cols/2))]
  }
  pref   
 }

 preferences       <<- array(dim=c(persons, turns, iterations)) 
 satisfactions     <<- array(dim=c(persons, turns, iterations))
 group_preferences <<- array(dim=c(1, turns, iterations))
 
 for (iteration in 1:iterations) {
  for (song in 1:turns) {
   past_satisfactions <- if(song == 1) rep(iota, persons) else satisfactions[, song-1,iteration]
   preferences_matrix <- if(is.null(ratings)) create_preferences(retrieval, persons, bimodal, bump) else array(ratings[song], dim=c(retrieval, persons))  
   group_pref_matrix  <- apply(preferences_matrix, 1, combine_preferences, past_satisfactions=past_satisfactions, misery=misery, weighted=weighted)
   if(all(is.na(group_pref_matrix))) { # If every preference is below misery
    # Take the candidate with the less worse misery
    less_worse = apply(preferences_matrix, 1, function(x) max(x[x < misery]))
    best_index <- which(less_worse == max(less_worse, na.rm = TRUE))[1]
    group_preferences[1, song, iteration] <<- mean(preferences_matrix[best_index,])
   }
   else {
    best_pref = max(group_pref_matrix, na.rm = TRUE)  
    best_index <- which(group_pref_matrix == best_pref)[1]
    group_preferences[1, song, iteration] <<- best_pref    
   }
   preferences[,song,iteration] <<- preferences_matrix[best_index,]
   satisfactions[,song,iteration] <<- combine_satisfactions(preferences_matrix[best_index,], past_satisfactions, iteration, chi)
  }
 }
}  

##############################################################################
#    Plotting functions                                                      #
##############################################################################

plot_values <- function(values, plot_box=FALSE, plot_title="", plot_save=TRUE, plot_add=FALSE, plot_misery=FALSE, persons=PERSONS, retrieval=RETRIEVAL, misery=MISERY, iota=IOTA, weighted=WEIGHTED, bimodal=BIMODAL, bump=BUMP, chi=CHI, ylim=c(-1,1), plot_aggregate=NULL, ...) {
 means = apply(values, c(1,2), mean, na.rm=TRUE)
 if(!is.null(plot_aggregate))
    means = array(apply(means, 2, plot_aggregate, na.rm=TRUE), dim=c(1,ncol(means)))
 ylab = if(nrow(means) == 1) plot_title else paste(plot_title, "s", sep="")
 for (l in 1:nrow(means)) {
  main = create_title(ylab, plot_box, which = if(nrow(means) > 1) l, persons, retrieval, misery, iota, weighted, bimodal, bump, chi)
  if(l > 1 && plot_box && plot_save) save_plot(main)
  if((l == 1 || plot_box) && !plot_add)
   plot(1:ncol(means), xlim=c(1,ncol(means)), main=main, type="n",xlab="Songs",ylab=strsplit(ylab, " and ")[[1]][1], font=3,cex=1.3, cex.lab=1.3, mgp=c(2.4,1,0), ylim=ylim)
  if(plot_add) {
   axis(4)
   mtext(ylab, side=4, line=2.5, font=1,cex=1.3, cex.lab=1.3)    
  }
  par(new=TRUE)
  if(plot_box) {
   data = lapply(as.data.frame(aperm(values[l,,])), function(x) {x[x==-1] = NA; x}) # I'm sure there's a better way to write this
   boxplot(data, range=0, xlim=c(1,ncol(means)), na.action = na.pass, boxcol=rainbow(nrow(means))[l], whiskcol=rainbow(nrow(means))[l], col="transparent", lwd=0.75, xlab="",  ylab="", xaxt="n",  yaxt="n", axes=FALSE, font=3,cex=1.3,cex.main=1, ylim=ylim)
   par(new=TRUE)
  }
  plot(means[l,], xlim=c(1,ncol(means)), col=rainbow(nrow(means))[l], type="l", xlab="",  ylab="", xaxt="n",  yaxt="n", axes=FALSE, font=3, cex=1.3, ylim=ylim, ...) 
  if(plot_misery && !plot_add) {
   par(new=TRUE)
   plot(c(1-5,ncol(means)+5),c(misery,misery), xlim=c(1,ncol(means)), lwd=0.4, lty="12", type="l", xlab="",  ylab="", xaxt="n",  yaxt="n", axes=FALSE, font=3,cex=1.3,cex.main=1, ylim=ylim)     
  }
 }
 if(plot_save) save_plot(main)
}

create_title <- function(plot_title="", plot_box=FALSE, which=NULL, persons=PERSONS, retrieval=RETRIEVAL, misery=MISERY, iota=IOTA, weighted=WEIGHTED, bimodal=BIMODAL, bump=BUMP, chi=CHI) {
 title=paste(plot_title, ", ", 
        if(!weighted)                   "unweighted, ",
        if(bimodal)                     "bimodal, ", 
        if(persons != PERSONS)      paste(persons, 
         if(persons == 1) "person, " else "persons, "), 
        if(retrieval != RETRIEVAL)      paste(retrieval, "retrieved, "), 
        if(misery != MISERY)            paste("misery ", misery, ", ", sep=""),
        if(bump != BUMP)                paste("bump ", bump, ", ", sep=""),
        if(chi != CHI)                  paste("chi ", chi, ", ", sep=""),
        if(iota != IOTA)                paste("iota ", iota, ", ", sep=""),
        if(plot_box && !is.null(which)) paste("person ", which, ", ", sep=""),
        sep="")
 substring(title, 0, nchar(title)-2) 
}

save_plot <- function(name) {
 name = sapply(strsplit(tolower(name), ", "), paste, collapse="_")
 name = sapply(strsplit(tolower(name), " "), paste, collapse="")
 quartz.save(paste(name, ".png", sep=""),type="png")    
}


plot_preferences <- function(...) {
 plot_values(preferences, plot_title="Preference", ylim=c(-1,1), lty="12", lwd=1.5, ...)   
}

plot_satisfactions <- function(...) {
 plot_values(satisfactions, plot_title="Satisfaction", ylim=c(0,1), lwd=2, ...)   
}

plot_group_preferences <- function(...) {
 plot_values(group_preferences, plot_title="Group preference", ylim=c(-1,1), plot_box=TRUE, ...)   
}

plot_preferences_and_satisfactions <- function(...) {
 # Plots only for the first listener
 par(mar=c(5, 4, 4, 4) + 0.1)
 plot_values(array(preferences[1,,], dim=c(1,dim(preferences)[2:3])), plot_save = FALSE, plot_title="Preference and satisfaction", ylim=c(-1,1), lty="12", lwd=1.5, ...)   
 plot_values(array(satisfactions[1,,], dim=c(1,dim(satisfactions)[2:3])), plot_add = TRUE, plot_title="Satisfaction", ylim=c(0,1), lwd=2, ...)   
 par(mar=c(5, 4, 4, 2) + 0.1)
}

plot_satisfactions_deviation <- function(...) {
 plot_values(satisfactions, plot_title="Satisfaction (std dev)", plot_aggregate="sd", ylim=c(0,0.5), lwd=2, ...)   
}

##############################################################################
#    Setup: Draw plotting area                                               #
##############################################################################

quartz(family="Avenir LT Std",width=10,height=6) # ,bg="white")

##############################################################################
#    Experiment 0: Basic preferences and satisfactions plots                 #
##############################################################################

exp0 <- function() {
 params = list()
 do.call("generate", params) 
 do.call("plot_preferences", params) 
 do.call("plot_preferences", c(params, plot_box=TRUE)) 
 do.call("plot_satisfactions", params) 
 do.call("plot_satisfactions", c(params, plot_box=TRUE)) 
 do.call("plot_group_preferences", params) 
}

##############################################################################
#    Experiment 1: How the number users affects the satisfaction             #
##############################################################################

exp1 <- function(persons_range = 1:20) {
 for (persons in persons_range) {

  params = list(persons=persons)
  do.call("generate", params) 
  do.call("plot_satisfactions", params)
 }
}

##############################################################################
#    Experiment 2: How the retrieval size affects the satisfaction           #
##############################################################################

exp2 <- function(retrieval_range = 1:30) {
 for (retrieval in retrieval_range) {

  params = list(retrieval=retrieval)
  do.call("generate", params) 
  do.call("plot_satisfactions", params) 
 }
}

##############################################################################
#    Experiment 3: How the misery threshold affects the preferences          #
##############################################################################

exp3 <- function(misery_range = seq(-1,1,by=0.1)) {
 for (misery in misery_range) {

  params = list(misery=misery)
  do.call("generate", params) 
  do.call("plot_preferences", c(params, plot_misery=TRUE)) 
  do.call("plot_group_preferences", c(params, plot_misery=TRUE))
 }
}

##############################################################################
#    Experiment 4: How past satisfaction affects the satisfaction            #
##############################################################################

exp4 <- function(bump_range = seq(-1,1,by=0.1)) { # nice example with 0.5
 for (bump in bump_range) {

  # Generate heterogeneous persons, NOT WEIGHTING with past satisfactions
  params = list(bimodal=TRUE, weighted=0, bump=bump)
  do.call("generate", params) 
  do.call("plot_preferences", params) 
  do.call("plot_satisfactions", params) 
  
  # Generate heterogeneous persons, WEIGHTING with past satisfactions
  params = list(bimodal=TRUE, weighted=1, bump=bump)
  do.call("generate", params) 
  do.call("plot_preferences", params) 
  do.call("plot_satisfactions", params) 
 }
}

##############################################################################
#    Experiment 5: How chi affects the satisfaction                          #
##############################################################################

# i will plot pref and sat on the same graph for one user, 
# and in case also the avg group pref and sat?

exp5 <- function(chi_range = seq(0,1,by=0.1)) {
 ratings = runif(turns)*2-1
 for (chi in chi_range) {
  params = list(chi=chi)
  do.call("generate", c(params, ratings = list(ratings))) 
  do.call("plot_preferences_and_satisfactions", params) 
 }
}

##############################################################################
#    Experiment 6: How the initial satisfaction affects the satisfaction     #
##############################################################################

exp6 <- function(iota_range = seq(0,1,by=0.1)) {
 for (iota in iota_range) {

  params = list(iota=iota)
  do.call("generate", params) 
  do.call("plot_satisfactions", params) 
 }
}


##############################################################################
#    Experiment 7: How memory influences standard deviation of satisfaction  #
##############################################################################

exp7 <- function(persons_range = seq(3,20,by=2)) {
 for (persons in persons_range) {
  # Generate heterogeneous persons, WITH memory of past satisfaction
  params = list(persons=persons, bimodal=TRUE, weighted=1, bump=0)
  do.call("generate", params) 
  do.call("plot_satisfactions_deviation", c(params, plot_save = FALSE))

  # Generate heterogeneous persons, WITHOUT memory of past satisfaction
  params = list(persons=persons, bimodal=TRUE, weighted=0, bump=0)
  do.call("generate", params) 
  do.call("plot_satisfactions_deviation", c(params, plot_add = TRUE, lty="12"))
 }
}
