##############################################################
# Single Donor
# Based on the informative alleles, calculate donor proportion
# Input: rcode.profile; rcode.markers; sdata.txt
# Output: results.xls
##############################################################


#sdata,markers,rt,dm,rm
#
chiSD <- function(sdata,markers,profile,rt,dt,d,r) {
#load('data/locusSD.RData'); #load the previous workplace (r, d, markers, profile)
print(getwd(),quote=F);

sData <- read.delim(sdata$datapath);

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
st =  rt;
st[,] = 0

#check point: any locus present in sample but not in recipient/donor
print("The following alleles are possible false calls by ABI GeneAnalyzer",quote = F)
print(s[!(s[,2] %in% colnames(st)),]);

#check point: donor and receipient matrix
#print("Donor Allele Matrix", quote = F);
#print(dm);
#print("Receipient Allele Matrix", quote = F);
#print(rm);

st[as.matrix(s[,1:2])] = 1;
st[(dt+rt)==0 & st!=0]= 999; # the alleles not in donor or recipient
sa = rt;
sa[,] = 0;
sa[as.matrix(s[,1:2])] = s[,3];

# profile for sample, receipient, and donor
#proS = rep(profile,table(s[,1])[markers]);
#proR = rep(profile,table(r[,1])[markers]);
#proD = rep(profile,table(d[,1])[markers]);

C = profile; #chimerism matrix (donor percentage)

# calculate percent donor chimerism based on general formula and specific formula
for (m in markers){

	if (profile[m]==211){
		Ad = s[s[,1]==m & s[,2]==setdiff(d[d[,1]==m,2],r[r[,1]==m,2]),3];
		A = s[s[,1]==m & s[,2]==intersect(d[d[,1]==m,2],r[r[,1]==m,2]),3];

		if (length(Ad)==0){
			Ad = 0;
		}
		C[m]=2*Ad/(Ad+A);
	}

	if (profile[m]==221){
		Ad = s[s[,1]==m & s[,2]==setdiff(d[d[,1]==m,2],r[r[,1]==m,2]),3];
		Ar = s[s[,1]==m & s[,2]==setdiff(r[r[,1]==m,2],d[d[,1]==m,2]),3];

		if (length(Ar)==0){
			Ar = 0;
		}

		if (length(Ad)==0){
			Ad = 0;
		}
		C[m]=Ad/(Ad+Ar);
	}

	if (profile[m]==121){
		Ar = s[s[,1]==m & s[,2]==setdiff(r[r[,1]==m,2],d[d[,1]==m,2]),3];
		A = s[s[,1]==m & s[,2]==intersect(r[r[,1]==m,2],d[d[,1]==m,2]),3];

		if (length(Ar)==0){
			Ar = 0;
		}
		C[m]=1-(2*Ar/(Ar+A));
	}

	if (profile[m]==1){
		Ad = sum(s[s[,1]==m & s[,2] %in% setdiff(d[d[,1]==m,2],r[r[,1]==m,2]),3]);
		A = sum(s[s[,1]==m,3]);

		if (length(Ad)==0){
			Ad = 0;
		}
		C[m]=Ad/A
	}

	if (profile[m]==0){
		C[m]=NA
	}

}

#Generate finial result data frame
results = cbind(profile,C,NA);
results[,3] = NA;
me = mean(results[,2],na.rm=T);
sd = sd(results[,2],na.rm=T);
l = !((abs(results[,2]-me) > 2*sd)| is.na(results[,2])); #non-informative locus or 2 SD locus are excluded
results[l,3]=mean(results[l,2]);
colnames(results)[1] = 'Profile';
colnames(results)[2] = 'Donor%';
colnames(results)[3] = 'Donor%_Mean';

sm = cbind(st,apply(st,1,sum));
colnames(sm)[length(colnames(sm))] = 'Sum';



print("Sample Allele Matrix", quote = F);
print(sm);
print("Sample Allele Area Matrix", quote = F);
print(sa);
print("Final Results", quote = F);
print(results);

return(list(results,sm))
}


