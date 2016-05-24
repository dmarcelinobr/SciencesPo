\name{NEWS}
\title{News from Package 'SciencesPo'}
\section{Changes in SciencesPo version 1.4.0 (2016-02-20) }{
  \subsection{Introduction}{
    \itemize{
      \item Version 1.4.0 is the development release of the SciencesPo library. A more advanced version may be found at \url{http://github.com/danielmarcelino/SciencesPo}.
    }
  }
  \subsection{News}{
  This release brings important changes for the \dQuote{SciencesPo} package in terms of function names systematization (UpperCamelCase), and also in displaying results.

   \itemize{
	   \item {UpperCamelCase} {Firstly, you will notice that many functions now begin with capital letter instead of former lower case version, see \code{help.search("deprecated", package = "SciencesPo")} for details. This decision was made to decrease function name conflicts, and to make the package's function names style more uniform.
	   }
	    \item {Display} {Next, the output of several functions gained good-looking display with knitting dispensed by \code{pander} package, see for instance \code{link{Summary}}, \code{link{Freq}}.
	}
  }

  \subsection{To do}{
    \itemize{
      \item Suggestions are welcome, but my current priority list includes programming: (1) mediation analysis functions, (2) qualitative indices.
      }
  }
}
}
