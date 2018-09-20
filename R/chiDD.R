
#' Rchimerism chiDD
#'
#' An internal function to determine donor percentages for a single donor
#'
#'
#' @param sdata Sample data input text file
#' @param markers List of locus markers
#' @param profile,ru,rt,rnn,d1nn,d2nn,d1u,d2u,d1t,d2t,r Internal variables for matrix operations
#'
#' @return A data frame with donor percentage results, and sample allele matrix
#'
#'


#load('locusDD.RData'); #load the previous workplace (r, d, markers, profile)
chiDD <- function(sdata,markers,profile,ru,rt,rnn,d1nn,d2nn,d1u,d2u,d1t,d2t,r) {
print(getwd(),quote=F);

sData <- read.delim(sdata$datapath);

#Function to handle invalid data text files
coherent_input <- function(any_input) {
  if(ncol(any_input)<7 || nrow(any_input)<1) {
    return(paste("Cannot read ",deparse(substitute(any_input)),sep=""))
  }
  return(NULL)
}

ci <- c(coherent_input(sData))
ci_r <- ci[which(!is.null(ci))]

if(!is.null(ci_r)) {
  return(ci_r)
}


s = sData[grep("[^[:alpha:]]",sData[,4]),c(3:4,7)]; #clean up the raw data
s = droplevels(s);
s$Allele = as.factor(s$Allele);

# Fill in the matrix with alleles
st = ru;
st[,] = 0;

#check point: any locus present in sample but not in recipient/donors
#print("The following alleles are possible false calls by ABI GeneAnalyzer",quote = F)
#print(s[!(s[,2] %in% colnames(st)),]);
#browser()

#Handles noisy sample data, outputs table of possible false calls
if (nrow(s[!(s[,2] %in% colnames(st)),]) != 0) {
  false_calls <- s[!(s[,2] %in% colnames(st)),]
  return(false_calls)
}
#chck point
#print("Donor 1 Allele Matrix", quote=F);
#print(d1m);
#print("Donor 2 Allele Matrix", quote=F);
#print(d2m);
#print("Receipient Allele Matrix", quote = F);
#print(rm);


st[as.matrix(s[,1:2])] = 1;
st[(d1t + d2t + rt) == 0 & st!=0] = 999; # the alleles not in donor or recipient
sn = apply(st,1,sum);

# Fill in the matrix with area values
sa = ru;
sa[,] = 0;
sa[as.matrix(s[,1:2])] = s[,3];

# Only keep the unique allele area values
saru = sa;
saru[ru==0] = 0;
sad1u = sa;
sad1u[d1u==0] = 0;
sad2u = sa;
sad2u[d2u==0] = 0;

# Put NA in non-informative loci
ss = apply(sa,1,sum);
srs = apply(saru,1,sum);
srs[apply(ru,1,sum)==0] = NA; #put NA in non-informative loci
sd1s = apply(sad1u,1,sum);
sd1s[apply(d1u,1,sum)==0] = NA; #put NA in non-informative loci
sd2s = apply(sad2u,1,sum);
sd2s[apply(d2u,1,sum)==0] = NA; #put NA in non-informative loci

# Ratio/Percentage results
sr = srs * rnn / ss;
me = mean(sr,na.rm=T);
sd = sd(sr,na.rm=T);
l = !((abs(sr-me) > 2*sd)| is.na(sr)); #non-informative locus or 2 SD locus are excluded
srMean = sr;
srMean[!l] = NA;
srMean[l] = mean(srMean,na.rm=T);

srSD = sr;
srSD[!l] = NA;
srSD[l] = sd(sr,na.rm=T);
srCV = srSD/srMean;

sd1 = sd1s * d1nn / ss;
me = mean(sd1,na.rm=T);
sd = sd(sd1,na.rm=T);
l = !((abs(sd1-me) > 2*sd)| is.na(sd1)); #non-informative locus or 2 SD locus are excluded
sd1Mean = sd1;
sd1Mean[!l] = NA;
sd1Mean[l] = mean(sd1,na.rm=T);
sd1SD = sd1;
sd1SD[!l] = NA;
sd1SD[l] = sd(sd1,na.rm=T);
sd1CV = sd1SD/sd1Mean;


sd2 = sd2s * d2nn / ss;
me = mean(sd2,na.rm=T);
sd = sd(sd2,na.rm=T);
l = !((abs(sd2-me) > 2*sd)| is.na(sd2)); #non-informative locus or 2 SD locus are excluded
sd2Mean = sd2;
sd2Mean[!l] = NA;
sd2Mean[l] = mean(sd2Mean,na.rm=T);
sd2SD = sd2;
sd2SD[!l] = NA;
sd2SD[l] = sd(sd2,na.rm=T);
sd2CV = sd2SD/sd2Mean;


results = cbind(sd1,sd1Mean,sd1SD,sd1CV,sd2,sd2Mean,sd2SD,sd2CV,sr,srMean,srSD,srCV,sd1+sd2+sr);
results = rbind(results, apply(!is.na(results),2,sum));
rownames(results)[nrow(results)] = "Info#";
colnames(results) = c('Donor_1%', 'Donor_1%_Mean','Donor_1%_SD','Donor_1%_CV','Donor_2%',
                      'Donor_2%_Mean','Donor_2%_SD','Donor_2%_CV','Recipient%', 'Recipient%_Mean',
                      'Recipient%_SD','Recipient%_CV',"Sum");
sm = cbind(st,sn);
colnames(sm)[length(colnames(sm))] = 'Sum';

#write.table(results,file='results.xls',sep="\t",col.names=FALSE);



print("Sample Allele Matrix", quote = F);
print(sm);
print("Sample Allele Area", quote = F);
print(sa);
print("Final Results", quote = F);
print(results);
return(list(results,sm))
}


