

#' Rchimerism locSD
#'
#' An internal function to determine informative loci for a single donor
#'
#'
#' @param ddata Donor data input text file
#' @param rdata Recipient data input text file
#' @param markers List of locus markers
#'
#' @return Internal variables used by chiSD.R

locSD <- function(ddata,rdata,markers) {
#markers = c('D3S1358','TH01','D21S11','D18S51','Penta E','D5S818','D13S317','D7S820','D16S539','CSF1PO','Penta D','vWA','D8S1179','TPOX','FGA');

#rData = read.delim('rdata.txt');
#dData = read.delim('ddata.txt');
dData <- read.delim(ddata$datapath)
rData <- read.delim(rdata$datapath)

#Function to handle invalid data text files
coherent_input <- function(any_input) {
  if(ncol(any_input)<7 || nrow(any_input)<1) {
    return(paste("Cannot read ",deparse(substitute(any_input)),sep=""))
  }
  return(NULL)
}

ci <- c(coherent_input(dData),coherent_input(rData))
ci_r <- ci[which(!is.null(ci))]

if(!is.null(ci_r)) {
  return(ci_r)
}

#clean up the raw data (OL, X, '')
r = rData[grep("[^[:alpha:]]",rData[,4]),c(3:4,7)];
d = dData[grep("[^[:alpha:]]",dData[,4]),c(3:4,7)];

rr = droplevels(r); #fantom levels can cause problem, rr, dd are tem variables
dd = droplevels(d);

#get rid of the noise (less than half of the maximum area)
maxR = tapply(rr[,3],rr[,1],max)/2;
r = rr[rr[,3]>maxR[rr[,1]],];

maxD = tapply(dd[,3],dd[,1],max)/2;
d = dd[dd[,3]>maxD[dd[,1]],];

#Allele matrix and calculation;
r[,4] = 'r';
d[,4] = 'd';
rd = rbind(r,d);

#Compare rd with markers, end program if user defined markers not in input
xtra_in_markers <- setdiff(markers,rd[,1])
if (length(xtra_in_markers) != 0) {
  return(paste("'",xtra_in_markers,"'"," from markers not found in input data",
               sep = ""))
}

trd = table(rd[,c(1,2,4)]);
rt = trd[,,'r'];
rt = rt[markers,];
rt = rt[,sort(colnames(rt))];
dt = trd[,,'d'];
dt = dt[markers,];
dt = dt[,sort(colnames(dt))];
sum = dt + rt;
diff = dt - rt;
###########
#classify loci according to the matrix manipulation
###########

profile = diff[,1];
profile[apply(diff,1,any)] = 1; #informative locus maked as "1"
profile[!apply(diff,1,any)] = 0; #non-inforamtive locus marked as "0"

# classify locus (ssum/sdiff: D allele number + R allele number): 4/0: 2+2; 3/1: 2+1; 3/-1: 1+2; 2/0: 1+1
ssum = apply(sum,1,sum);
sdiff = apply(diff,1,sum);

# Further identify locus for three situations that use unique formulas
# Label them uniquely

l221 = apply(sum==1,1,any)& apply(sum==2,1,any)& ssum==4 & sdiff==0; #221: 2(donor)2(receipient)1(one shared)
profile[l221] = 221;

l121 = ssum==3 & sdiff==-1 & apply(sum==2,1,any);
profile[l121] = 121;

l211 = ssum==3 & sdiff==1 & apply(sum==2,1,any);
profile[l211] = 211;
profile=profile[markers];

dm = cbind(dt,apply(dt,1,sum),profile);
colnames(dm)[length(colnames(dm))-1] = 'Sum';
colnames(dm)[length(colnames(dm))] = 'Profile';
rm = cbind(rt,apply(rt,1,sum));
colnames(rm)[length(colnames(rm))] = 'Sum';

#save.image(file='data/locusSD.RData');


print("Donor Allele Matrix", quote = F);
print(dm);
print("Recipient Allele Matrix", quote = F);
print(rm);
return(list(markers,profile,rt,dt,dm,rm,d,r))
}
