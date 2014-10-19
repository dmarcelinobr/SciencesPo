\encoding{UTF-8}
\name{fptp2av}
\alias{fptp2av}

\title{FPTP to AV
}
\description{A Program for Performing Simulations of Alternative Vote using the actual FPTP votes.
}
\usage{
fptp2av(data = NULL, link = NULL)
}

\arguments{
  \item{data}{A flat table or data frame containing FPTP votes for each district. 
  	}
  	\item{link}{A link function containing a structure for candidates/parties relations. 
}
}

\value{A data frame containing FPTP votes for each district.
}

\references{ Marcelino, Daniel (2013). \emph{SciencesPo: A Tool Set for Analyzing Political Behaviour Data}. Available at SSRN: \url{http://dx.doi.org/10.2139/ssrn.2320547}
}

\author{Daniel Marcelino <dmarcelino@live.com>
}

\note{This function is a very beta so it may present issues. Moreover, for the algorithm to work properly, the data set must have the same format (flat table) as the example here presented. Besides the data, a ``link'' structure input for party/candidates relations is necessary. See the example above.
}

\examples{
# Feeding data:
data(ge2010)

# Let's take a small portion of the data.
IR <-ge2010[ge2010$Region=="Northern Ireland",]

## First, we create vectors for parties/candidates link 
## For simplicity, I'm considering only parties which received some votes.

APNI<- c("SF", "DUP", "SDLP", "TUV", "UCUNF")
BNP<-c("Ch P", "Con", "CPA", "NF", "UKIP");
Ch_P<-c("BNP", "CPA", "Con", "ED")
Con<-c("LD", "UKIP", "ED", "Ch P", "CPA")
CPA<-c("BNP", "Ch P", "Con", "ED") 
DUP<-c("SDLP", "TUV", "UCUNF", "APNI") 
ED<-c("Con", "BNP", "NF", "CPA", "Ch P")
Grn<-c("LD", "Lab", "SDLP", "PC", "SSP", "TUSC", "TUV") 
Lab<-c("LD", "Grn", "SDLP", "Soc", "SSP", "TUSC", "TUV") 
LD<-c("Lab", "Con", "Grn", "PC", "SDLP")
MRLP<-c("Con", "Lab", "LD", "Grn", "PC", "SDLP", "SNP", "SSP", "UKIP")
NF<-c("BNP", "Con", "ED", "UKIP")
PC<-c("Con", "Lab", "LD") 
Respect<-c("Con", "Lab", "LD") 
SDLP<-c("Grn", "Lab", "LD") 
SF<-c("APNI", "USUNF") 
Soc<-c("Lab", "SSP", "SDLP", "TUSC", "TUV") 
SNP<-c("Ch P", "CPA", "UKIP")
SSP<-c("Grn", "Lab", "LD", "SDLP", "TUSC", "TUV") 
TUSC<-c("SDLP", "Soc", "SSP", "TUV") 
TUV<-c("SDLP", "Soc", "SSP", "TUSC") 
UCUNF<-c("APNI", "SF", "DUP") 
UKIP<-c("BNP", "NF", "Con") 

## vector with party/candidate relations:
party_chains <-structure(list(APNI,BNP,Ch_P,Con,CPA,DUP,ED,Grn,
	Lab,LD,MRLP,NF,PC,Respect,SDLP,SF,Soc,SNP,SSP,TUSC,TUV,UCUNF,
	UKIP), .Names = c("APNI","BNP","Ch_P","Con","CPA","DUP","ED","Grn","Lab",
"LD","MRLP","NF","PC","Respect","SDLP","SF","Soc","SNP","SSP","TUSC",
"TUV","UCUNF","UKIP" ));

## Run the algorithm
 \dontrun{fptp2av(data=IR, link=party_chains)}
}

\keyword{ political behaviour }
\keyword{ simulations }
