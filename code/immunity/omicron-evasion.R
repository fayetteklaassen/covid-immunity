gen_scn_omi <- function(){
vac.ev <- c(.9, .7, .5)
inf.ev <- c(.9, .7, .5)
both.ev <- c(.9, .7, .5)
boost.ev <- c(.6, .4, .2)
vac.evs <- c(.3, .2, .1)
inf.evs <- c(.3, .2, .1)
both.evs <- c(.3, .2, .1)
boost.evs <- c(.2, .1, .05)
scn.omi <-  c("pessimistic", "neutral", "optimistic")

scn.omi.df <- data.frame(vac.ev, inf.ev, both.ev, boost.ev,
                         vac.evs, inf.evs, both.evs,boost.evs,
                         scn.omi)
return(scn.omi.df)
}