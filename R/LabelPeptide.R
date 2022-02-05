
#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
LabelPeptide = function (Peptide_FC_P){
  for(n in 1:nrow(Peptide_FC_P)){
    { 
      test<-subset(Peptide_FC_P,Protein.Id==Peptide_FC_P[n,"Protein.Id"])
      if((max(test$selected)==1)){# not know why have a ".1"
        
        Peptide_FC_P[n,"selected"]=1
        print(Peptide_FC_P[n,"Protein.Id"])
      }
      
    }
  }
  
  for(n in 1:nrow(Peptide_FC_P)){
    Peptide_FC_P[n,"Protein.Id"]# get protein.Id
    test<-subset(Peptide_FC_P,Protein.Id==Peptide_FC_P[n,"Protein.Id"])
    if((min(test$Plasma_FC)<1)&&(Peptide_FC_P[n,"Plasma_FC"]>1.5)&&(Peptide_FC_P[n,"Plasma_p"]<0.05)){# now small means <1
      print("OK")
      Peptide_FC_P[n,"selected"]=1
      print(Peptide_FC_P[n,1])
      
    }
    print(n)
  }
  # add all contain 1 row
  # the Sequence may change, 
  for(n in 1:nrow(Peptide_FC_P)){
    { 
      test<-subset(Peptide_FC_P,Protein.Id==Peptide_FC_P[n,"Protein.Id"])
      if((max(test$selected)==1)){# not know why have a ".1"
        
        Peptide_FC_P[n,"selected"]=1
        print(Peptide_FC_P[n,"Protein.Id"])
      }
      
    }
  }
  return(Peptide_FC_P);
}