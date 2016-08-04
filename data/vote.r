# vote.r
# Time-stamp: <01 Jul 2011 17:28:12 c:/x/rpack/corrgram/data/vote.r>

vote <- matrix(c(1,-0.38,-.5,.11,.75,-.44,.34,-.57,-.13,.12,.8,-.49,
                 rep(0,1),1,.37,-.38,-.22,-.01,-.54,-.09,.66,-.42,-.45,.55,
                 rep(0,2),1,-.24,-.49,.33,-.66,.14,.46,-.08,-.28,.52,
                 rep(0,3),1,.51,-.43,.71,-.27,.1,-.51,-.19,.06,
                 rep(0,4),1,-.56,.49,-.76,.14,-.18,.48,-.20,
                 rep(0,5),1,-.27,.14,-.16,.61,-.06,.01,
                 rep(0,6),1,-.18,-.24,-.22,.17,-.2,
                 rep(0,7),1,-.48,.08,-.37,.02,
                 rep(0,8),1,-.64,-.168,.63,
                 rep(0,9),1,.38,-.41,
                 rep(0,10),1,-.51,
                 rep(0,11),1),12,12)
vote <- vote + t(vote)
diag(vote) <- 1

rownames(vote) <- colnames(vote) <-
  c("Redistribution","Inequality","Partisanship","Turnout",
           "Unionization","Veto points","Electoral system",
           "Left fragmentation","Right overrepresentation",
           "Per capita income","Female LF participation","Unemployment")
