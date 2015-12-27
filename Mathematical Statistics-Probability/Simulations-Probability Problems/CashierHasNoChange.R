#n people have a $5 bill, and n people have a $10 bill. They line up single file one night to buy
#a ticket that costs $5. The agent has no change at the beginning of the night. No one ever changes
#place in line. If the agent does not have change for the next customer, the ticket boot shuts
#down. Each ordering of the customers is equally likely. 

#What is the probability that every customer who lined up at the beginning of the process will be 
#able to purchase a ticket? Remember, there are 2n customers, so the answer will depend on n.

#Simulate for n=5,n=10, and n=20

#Intitialize sample, length of sample, iteration counter, and # of simulations.
samp = rep(c(5,10), 5)
l.samp = length(samp)
iter = 0
sim = 10000

#For loop to go through # of simulations
for (j in 1:sim)
{#for
  
  #Sample from previoius samp, set iteration b=0 for counter in next for loop and get numeric of l.samp, which is the length
  #of the samp.
  a = sample(samp, l.samp, replace = FALSE)
  b = 0
  c = numeric(l.samp)
  
  #This for loop will go through each sample. It will look to see if the number of $10 is greater than the number of $5.
  #It will increment +1 for every $5 and -1 for every $10 bill. Once the incrementation is negative, that means there are
  #more $10 than $5 and the system will shut down.
  for (i in 1:l.samp)
  {#for
    if (a[i] == 5)
    {#if
      b = b+1
    }#if
    else
    {#else
      b = b-1
    }#else
    c[i] = b
  }#for
  
  #Sum to get total # of samples with all values >0, i.e., successes/everyone got in that was in line. 
  if(sum(c>=0)==l.samp)
  {#if
    iter = iter + 1
  }#if
  
}#for

#Get probability after previous simulation. 
prob = iter/sim