# *in RStudio, first go under Session -> Set Working -> To Source File
# you'll need a few libraries...

install.packages(pkgs=c('urltools','pwr','lsr')) # run this once... 

library(urltools) # to clean our headlines
library(readr)  # for reading CSV files
library(dplyr)
# load in CSV file, quick check, cleaning...

# reading in a comma-separate value (CSV) file; variable names in header (TRUE)
headlines = read.csv('lab_1_data.csv',header=TRUE)

# let's look at the first row... 
headlines[1,]

# we need to decode the title into a more readable format... so:
headlines$title = url_decode(headlines$title)
  write.csv(headlines, 'rick153.csv')  
headlines$description = url_decode(headlines$description)
write.csv(headlines, 'rick154.csv')  

# how many of each source?
table(headlines$source)
headlines$charnum <- nchar(headlines$title)
table(headlines$source)
hist(headlines$charnum)

hist(headlines$charnum[headlines$source == "Business Insider"])
hist(headlines$charnum[headlines$source == "New York Times"])

bi_headlines <- subset(headlines$source == "Business Insider")
which.max(headlines$charnum)
headlines[13760,]
nyt_headlines <- subset(headlines$source == "New York Times")
which.max(nyt_headlines$charnum)

nyt_headlines[21709,]
aggregate(charnum~source, data=headlines,mean)
grep

nyt_hl <- subset(headlines, source == "New York Times")
nyt_avg_length <- mean(nchar(nyt_hl$title))
bi_hl <- subset(headlines, source == "Business Insider")
bi_avg_length <- mean(nchar(bi_hl$title))
print(nyt_avg_length)
print(bi_avg_length)


#LAB2

headlines$num_chars = nchar(headlines$title) 

# trick to words based on # spaces... (also see tidytext, tidyverse...)
headlines$num_words = headlines$num_chars - nchar(gsub(' ','',headlines$title))+1

# stylistic complexity from punctuation?... try help(regex)
headlines$puncs = headlines$num_chars - nchar(gsub('[[:punct:]]','',headlines$title))

# dollar/money amount mentions...
headlines$money_ref = 1:nrow(headlines) %in% grep('\\$',headlines$title) 

# keyword search



# statistical summaries of a measure



# frequency distribution of terms
sort(table(unlist(strsplit(paste(headlines$title,collapse=' '), ' '))),decreasing=T)
# what about case? try strtolower(...)
# is this line too complicated for future users of our code? 
#         bonus challenge: split this into a few lines for clarity



# histogram of a measure
hist(headlines$money_ref,n=100)



# "aggregate" can be very useful
aggregate(money_ref~source,data=headlines,FUN=mean) # aggregating...

# one way to subset
nyt = headlines[headlines$source=='New York Times',]
biz = headlines[headlines$source=='Business Insider',]
t.test(nyt$money_ref,biz$puncs)


#LAB3
run_model = function(agent_states,edge_list,rounds_of_interaction,influence_rate) {
  history=c()
  for (i in 1:rounds_of_interaction) {
    # print(paste('Running interaction round',i,'of',rounds_of_interaction))
    if (nrow(edge_list)>0) { # in case there are no edges at all!
      pair = edge_list[sample(1:nrow(edge_list),1),]
      agent_1 = pair[1] # choose a random pair
      agent_2 = pair[2]
      connection = connection_matrix[agent_1,agent_2] # what is their connection?
      agent_states[agent_2] = agent_states[agent_2] + 
        influence_rate*agent_states[agent_1]*connection # update agent state
      agent_states[agent_states>1] = 1 # make sure agents don't go above 1
    }
    history = rbind(history,agent_states) # save history
  }
  return(history)
}

make_connections = function(n_agents,n_delete=0,type='random',del_self=TRUE) {
  if (type=='random') {
    connection_matrix = matrix(runif(n_agents*n_agents),nrow=n_agents)
  } else if (type=='ones') {
    connection_matrix = matrix(1,nrow=n_agents,ncol=n_agents)
  } else if (type=='circle') {
    # ixes = sample(1:n_agents,n_agents)
    ixes = 1:n_agents
    connection_matrix = matrix(0,nrow=n_agents,ncol=n_agents)
    for (i in 1:(n_agents-1)) {
      connection_matrix[ixes[i],ixes[i+1]]=1
    }
    connection_matrix[ixes[n_agents],ixes[1]]=1
  }
  
  if (del_self) {
    connection_matrix[diag(n_agents)==1] = 0
  }
  
  connection_matrix[sample(1:(n_agents*n_agents),n_delete)] = 0
  
  return(connection_matrix)
}

plot_history = function(history,colors=NULL,line_width=2,line_type='l',y_axis_range=c(0,1)) {
  if (is.null(colors)) {
    colors = rainbow(ncol(history))
  }
  for (i in 1:ncol(history)) {
    if (i==1) {
      plot(history[,i],type=line_type,lwd=line_width,
           ylim=y_axis_range,xlim=c(1,nrow(history)),
           xlab='Iteration',ylab='Agent state',
           col=colors[i])    
    } else {
      points(history[,i],type=line_type,lwd=line_width,col=colors[i])    
    }
  }
}

plot_graph = function(connection_matrix,vertex_sizes,colors) {
  # par(mfrow=c(1,1)) 
  edge_list = which(connection_matrix>0,arr.ind=TRUE)
  g = graph_from_edgelist(edge_list)
  V(g)$color=colors
  plot(g,layout=layout_in_circle(g),vertex.label.cex=.5,vertex.size=20*vertex_sizes,edge.width=.25,edge.arrow.size=0.1)
}

###################

install.packages('igraph')
install.packages('scales')
library(igraph)
library(scales)
source('functions.R')

# //////////////////////////////////////////////////////
# here are our model parameters...
# //////////////////////////////////////////////////////

n_agents = 7 
influence_rate = .7 # information "impulse" between agents
have_info = n_agents*.2 # how many agents start with the information (=1)
connectedness = 1 # proportion network that stays connected
rounds_of_interaction = n_agents*20 # number of pairwise "conversations"

set.seed(11) # set seed to make code reproducible!

# //////////////////////////////////////////////////////
# initialize agents, connections, and run
# //////////////////////////////////////////////////////

agent_states = rep(0,n_agents) # initialize n_agents to 0 (rep = repeat)
agent_states[sample(1:n_agents,have_info)] = 1 # randomly choose who starts with info

n_delete = round((1-connectedness)*n_agents^2) # # connections to zero out
connection_matrix = make_connections(n_agents,n_delete=n_delete,type='circle')
# BLOCK 1 //////////////////////////////////////////////
connection_matrix[1,1] = 1 # make a long range (a 'hub') 
connection_matrix[2,4] = 1 # make a long range (a 'hub')
# //////////////////////////////////////////////////////
edge_list = which(connection_matrix>0,arr.ind=TRUE)

history = run_model(agent_states=agent_states,
                    edge_list=edge_list,
                    rounds_of_interaction=rounds_of_interaction,
                    influence_rate=influence_rate)

# //////////////////////////////////////////////////////
# let's do some plotting of history and average agent states
# //////////////////////////////////////////////////////

colors = alpha(rainbow(n_agents),.25)
par(mfrow=c(2,2))

plot_graph(connection_matrix,history[1,],colors) 
plot_graph(connection_matrix,history[nrow(history),],colors) 

plot_history(history,colors=colors,line_width=3,
             line_type='l',y_axis_range=c(0,1))
plot(rowMeans(history),type='l',ylim=c(0,1),
     xlab='Iteration',ylab='Average state')


