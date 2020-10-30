#Change the genotype and ecotype file of crops in DSSAT.

GenotypeChange<-function(GD, DSSATD, OD, CropName, GenotypeFileName, CultivarID, TotalParameterNumber, ModelRunNumber, RandomMatrix)
{
eval(parse(text=paste('GenotypeFilePath="',GD,'/',GenotypeFileName,'.CUL"',sep = '')));
ReadLine<-readLines(GenotypeFilePath, n=-1)
GenotypeFile<-as.character(ReadLine); #Get the genotype file saved as a template.

LineNumber<-grep(pattern=CultivarID, GenotypeFile); #Get the number of the line where the cultivar "GLUECUL" is located.
OldLine<-GenotypeFile[LineNumber];#Get the line according to the line number.

EcotypeName = substr(OldLine, 31, 36)

R<-ModelRunNumber;#Get what parameter set will be used to change the genotype file.

if (CropName != "SC")
{
  ParameterStep<-6;
  ValuePosition1<-(38-ParameterStep);
  ValuePosition2<-(42-ParameterStep);

  for (i in 1:7)
  {
  ValuePosition1<-ValuePosition1+ParameterStep;
  ValuePosition2<-ValuePosition2+ParameterStep;

  eval(parse(text = paste("Parameter<-RandomMatrix[R,",i,"]",sep = '')));
  #To solve the format problem for parameters with negative values. Modified by He, 2015-6-18.
  if(Parameter < 0 & Parameter > -1.0)                                                          #
  {                                                                                             #
  ParameterFormat<-sprintf('%1.3f', Parameter);                                                 #
  ParameterFormat<-paste(substring(ParameterFormat,1,1), substring(ParameterFormat,3), sep=''); #
  } else if (Parameter <= -1.0 & Parameter > -10.0)                                             # 
  {                                                                                             #
  ParameterFormat<-sprintf('%2.2f', Parameter);                                                 #
  } else if (Parameter <= -10.0 & Parameter > -100.0)                                           #
  {                                                                                             #
  ParameterFormat<-sprintf('%3.1f', Parameter);                                                 #
  }                                                                                             #
  
  if(Parameter >= 0 & Parameter < 10)
  {
  ParameterFormat<-sprintf('%1.3f', Parameter);
  } else if (Parameter >= 10 & Parameter < 100)
  {
  ParameterFormat<-sprintf('%2.2f', Parameter);
  } else if (Parameter >= 100)
  {
  ParameterFormat<-sprintf('%3.1f', Parameter);
  }

  substr(OldLine, ValuePosition1, ValuePosition2)<-ParameterFormat;
  }

  GenotypeFile[LineNumber]<-OldLine;#Replace the old line with new generated line in the Genotype file.
} else
{
  ParameterStep<-15;

  #chp modified
  ValuePosition1<-(47-ParameterStep); #The initial starting point was 42, but it was changed to 46 since "EXPNO" was added by Cheryl recently.
  ValuePosition2<-(61-ParameterStep); #The initial ending point was 47, but it is 51 now.

  for (i in 1:7)
  {
  ValuePosition1<-ValuePosition1+ParameterStep;
  ValuePosition2<-ValuePosition2+ParameterStep;

  eval(parse(text = paste("Parameter<-RandomMatrix[R,",i,"]",sep = '')));

  if(Parameter>=0 & Parameter<10)
  {
  ParameterFormat<-sprintf('%1.3f', Parameter);
  } else if (Parameter>=10 & Parameter<100)
  {
  ParameterFormat<-sprintf('%2.2f', Parameter);
  
  # chp added extra format statement for values between 100 and 1000
  } else if (Parameter>=100 & Parameter<1000)
  {
  ParameterFormat<-sprintf('%3.1f', Parameter);
  } else
  {
  ParameterFormat<-sprintf('%4.0f', Parameter);
  }

  ##chp
  #print(ParameterFormat);
  #print (" ");
  
  substr(OldLine, ValuePosition1, ValuePosition2)<-'      ';# Delete initial values.
  substr(OldLine, ValuePosition1, ValuePosition2)<-ParameterFormat;
  
 }

  GenotypeFile[LineNumber]<-OldLine;#Replace the old line with new generated line in the Genotype file.
  
}

#update ecotype file with new line of random matrix
eval(parse(text=paste('EcotypeFilePath="',GD,"/",GenotypeFileName,'.ECO"',sep = '')));
EcotypeFile = readLines(EcotypeFilePath)
oldline_eco = EcotypeFile[stringr::str_which(EcotypeFile,EcotypeName)]
title_eco = EcotypeFile[stringr::str_which(EcotypeFile,"@ECO")]

LineNumber_eco = stringr::str_which(EcotypeFile,EcotypeName)

df_eco = read.table(textConnection(oldline_eco),header = F)
header = read.table(textConnection(title_eco),header = F,comment.char = "")
header = unlist(strsplit(apply(header,FUN = as.character,MARGIN = 1 ),"\\.."))
colnames(df_eco) = header
# print(df_eco)

Ecoparameter_cali = c("P1","P2","P3","P4","PARUE","PARU2","SLAS","HTSTD","KCAN")
OldValue_eco = df_eco[1,Ecoparameter_cali]

#change these values: stringr::str_locate(title_eco,"HTSTD")
Loca1 = c(9,21,27,45,57,63,99,159,171)
Loca2 = c(12,24,30,48,60,66,102,162,174)

NewValue_eco = c()
for (iEcoPara in 1:length(Ecoparameter_cali)){
  
  thisEcoPara = RandomMatrix[R,(iEcoPara+7)]
  
  if(thisEcoPara>=0 & thisEcoPara<10)
  {
    singleEcoPara = sprintf('%1.2f', thisEcoPara)
  }else if(thisEcoPara>=10 & thisEcoPara<100){
    singleEcoPara = sprintf('%2.2f', thisEcoPara)
  }else if(thisEcoPara>=100 & thisEcoPara<1000){
    singleEcoPara = sprintf('%4.0f', thisEcoPara)
  }else {
    singleEcoPara = sprintf('%4i', thisEcoPara)
  }
  NewValue_eco = c(NewValue_eco, singleEcoPara)
  
  substr(oldline_eco, Loca1[iEcoPara], Loca2[iEcoPara])<- '    ';# Delete initial values.
  substr(oldline_eco, Loca1[iEcoPara], Loca2[iEcoPara])<- singleEcoPara;
  
}

EcotypeFile[LineNumber_eco]<-oldline_eco;#Replace the old line with new generated line in the Ecotype file.

                                                   
eval(parse(text=paste("NewGenotypeFilePath='",OD,"/",GenotypeFileName,".CUL'",sep = '')));
write(GenotypeFile, file=NewGenotypeFilePath);
#Save the new genotype file as "cul" file in the GLWork directory.

eval(parse(text=paste("NewEcotypeFilePath='",OD,"/",GenotypeFileName,".ECO'",sep = '')));
write(EcotypeFile, file=NewEcotypeFilePath);

}




 
