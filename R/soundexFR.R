soundexFR <-
function(term){
        termo<-accent(term)
        termo<-toupper(termo)
        termo<-gsub("([A-Z])\\1", "\\1", termo)
        N<-nchar(termo)

        termo<-ifelse(substr(termo,1,2)=="WA",sub("W","V",termo),termo)
        termo<-ifelse(substr(termo,1,1)=="H",substr(termo,2,N),termo)
        termo<-ifelse(substr(termo,1,2)=="KA"|substr(termo,1,2)=="KO"|substr(termo,1,2)=="KU",sub("K","C",termo),termo)
        termo<-ifelse(substr(termo,1,1)=="Y",sub("Y","I",termo),termo)
        termo<-ifelse(substr(termo,1,2)=="CE"|substr(termo,1,2)=="CI",sub("C","S",termo),termo)
        termo<-ifelse(substr(termo,1,2)=="GE"|substr(termo,1,2)=="GI",sub("G","J",termo),termo)
        
        termo.1<-substr(termo,1,1)
        termo.2<-substr(termo,2,N)
        termo.2<-gsub("[A,E,I,O,U,H,W,Y]",0,termo.2)
        termo.2<-gsub("[B,F,P,V]",1,termo.2)
        termo.2<-gsub("[C,G,J,K,Q,S,X,Z]",2,termo.2)
        termo.2<-gsub("[D,T]",3,termo.2) 
        termo.2<-gsub("L",4,termo.2)
        termo.2<-gsub("[M,N]",5,termo.2)
        termo.2<-gsub("R",6,termo.2)
        termo.2<-gsub(0,'',termo.2)
        termo.2<-gsub('([0-9])\\1', '\\1', termo.2)
         
        termo.3<-paste(termo.1,termo.2,sep='-')
        termo.comp<-paste(termo.3,'0000',sep='')
        soundex<-substr(termo.comp,1,5)
        return(soundex)
    }
