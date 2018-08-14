#############################################
# Decide informative loci for double donor
# Input: ddata.txt; rdata.txt
# Output: rcode.profile; rcode.markers; rcode.donor; rcode.recipient;
#############################################

#markers = c('D3S1358','TH01','D21S11','D18S51','Penta E','D5S818','D13S317','D7S820','D16S539','CSF1PO','Penta D','vWA','D8S1179','TPOX','FGA');
locDD <- function(donor1_data, donor2_data,recipient_data,markers) {
#rData = read.delim('rdata.txt');
#d1Data = read.delim('d1data.txt');
#d2Data = read.delim('d2data.txt');
ddata <- donor1_data
d2data <- donor2_data
rdata <- recipient_data

d1Data <- read.delim(ddata$datapath)
d2Data <- read.delim(d2data$datapath)
rData <- read.delim(rdata$datapath)

coherent_input <- function(any_input) {
  if(ncol(any_input)<7 || nrow(any_input)<1) {
    return(paste("Cannot read ",deparse(substitute(any_input)),sep=""))
  }
  return(NULL)
}

ci <- c(coherent_input(d1Data),coherent_input(d2Data),coherent_input(rData))
ci_r <- ci[which(!is.null(ci))]

if(!is.null(ci_r)) {
  return(ci_r)
}


#clean up the raw data (OL, X, '')
r = rData[grep("[^[:alpha:]]",rData[,4]),c(3:4,6)];
d1 = d1Data[grep("[^[:alpha:]]",d1Data[,4]),c(3:4,6)];
d2 = d2Data[grep("[^[:alpha:]]",d2Data[,4]),c(3:4,6)];

rr = droplevels(r); #fantom levels can cause problem, rr, dd are tem variables
dd1 = droplevels(d1);
dd2 = droplevels(d2);

#get rid of the noise (less than half of the maximum height)
maxR = tapply(rr[,3],rr[,1],max)/2;
r = rr[rr[,3]>maxR[rr[,1]],];

maxD1 = tapply(dd1[,3],dd1[,1],max)/2;
d1 = dd1[dd1[,3]>maxD1[dd1[,1]],];

maxD2 = tapply(dd2[,3],dd2[,1],max)/2;
d2 = dd2[dd2[,3]>maxD2[dd2[,1]],];

#Allele matrix (the matrix) and calculation;
r[,4] = 'r';
d1[,4] = 'd1';
d2[,4] = 'd2';
rd = rbind(r,d1,d2);

#Compare rd with markers, end program if discrepancy
xtra_in_markers <- setdiff(markers,rd[,1])
defic_in_markers <- setdiff(rd[,1],markers)
if (xtra_in_markers != 0) {
  return(paste("'",xtra_in_markers,"'"," from markers not found in input data",
               sep = ""))
} else if (defic_in_markers != 0) {
  return(paste("'",defic_in_markers,"'"," from input not found in markers",
               sep = ""))
}

trd = table(rd[,c(1,2,4)]);
rt = trd[,,'r'];
rt = rt[markers,];
rt = rt[,sort(colnames(rt))];
d1t = trd[,,'d1'];
d1t = d1t[markers,];
d1t = d1t[,sort(colnames(d1t))];
d2t = trd[,,'d2'];
d2t = d2t[markers,];
d2t = d2t[,sort(colnames(d2t))];

###########
# Find the unique alleles in r, d1, and d2 (will be using them in chiDD.R to calculate percentage of r, d1, and d2
###########

ru = rt-d1t-d2t;
ru[ru!=1] = 0;
run = apply(ru,1,sum);
d1u = d1t-d2t-rt;
d1u[d1u!=1] = 0;
d1un = apply(d1u,1,sum);
d2u = d2t-d1t-rt;
d2u[d2u!=1] = 0;
d2un = apply(d2u,1,sum);

rn = apply(rt,1,sum);
d1n = apply(d1t,1,sum);
d2n = apply(d2t,1,sum);

rur = sum(run)/sum(rn);
d1ur = sum(d1un)/sum(d1n);
d2ur = sum(d2un)/sum(d2n);

# Find the heterozygous loci with one unique allele (will do a x 2 in chiDD.R)
rnn = rn;
d1nn = d1n;
d2nn = d2n;

rnn[!((rn==2)&(run==1))]=1;
d1nn[!((d1n==2)&(d1un==1))]=1;
d2nn[!((d2n==2)&(d2un==1))]=1;

d1m = cbind(d1t,d1n,d1un,d1nn);
colnames(d1m)[length(colnames(d1m))-2] = 'Sum';
colnames(d1m)[length(colnames(d1m))-1] = 'Unique';
colnames(d1m)[length(colnames(d1m))] = 'Factor';
d2m = cbind(d2t,d2n,d2un,d2nn);
colnames(d2m) = colnames(d1m);
rm = cbind(rt,rn,run,rnn);
colnames(rm) = colnames(d1m);

#save.image(file='locusDD.RData');

print("Donor 1 Allele Matrix", quote=F);
print(d1m);
print("Donor 2 Allele Matrix", quote=F);
print(d2m);
print("Receipient Allele Matrix", quote = F);
print(rm);
return(list(markers,profile,ru,rt,rnn,d1nn,d2nn,d1u,d2u,d1t,d2t,r,d1m,d2m,rm))
}
