###### For waning curvves ####
gen_waning <- function(){
aa <- .8
aahi <- .9
AA <- .95
AAlo <- .9
AAhi <- 1
bb <- .5
bbhi <- .75
decl <- .1/(6*30)
decllo <- .2/(6*30)
declhi <- .05/(6*30)
cc <- .25
cchi <- .5
bboth <- .9
bbothlo <- .8
ccbothlo <- .1/(6*30)
BBoth <- .95
BBothlo <- .9
CCbothlo <- .1/(6*30)

dd <- 2*30
DD <- 6*30
ee <- 2*30
ff <- 5*30
ndays <- 900

ep_infvac <- c(rep(aa, dd),
               aa - (1:ee)/ee * (aa - bb),
               bb - (1:ff)/ff * (bb - cc),
               rep(cc, ndays - dd - ee - ff))
ep_infvac_lo <- ep_infvac - .05
ep_infvac_hi  <- c(rep(aahi, dd),
                   aahi - (1:ee)/ee * (aahi - bbhi),
                   bbhi - (1:ff)/ff * (bbhi - cchi),
                   rep(cchi, ndays - dd - ee - ff))

ep_both <- c(rep(bboth, ndays))
ep_both_lo <- c(rep(bbothlo, DD),
                bbothlo - (1:(ndays - DD)) * ccbothlo)
ep_both_hi <- ep_both + .05

sp_infvac <- c(rep(AA, DD),
               AA - (1:(ndays - DD)) * decl)
sp_infvac_lo <- c(rep(AAlo, DD),
                  AAlo - (1:(ndays - DD)) * decllo)
sp_infvac_hi<- c(rep(AAhi, DD),
                 AAhi - (1:(ndays - DD)) * declhi)

sp_both <- c(rep(BBoth, ndays))
sp_both_lo <- c(rep(BBothlo, DD),
                BBothlo - (1:(ndays - DD)) * CCbothlo)
sp_both_hi <- sp_both + .05

# wanei <- list(ep_infvac, ep_infvac_lo, ep_infvac_hi)
# waneiboth <- list(ep_both, ep_both_lo, ep_both_hi)
# wanes <- list(sp_infvac, sp_infvac_lo, sp_infvac_hi)
# wanesboth <- list(sp_both, sp_both_lo, sp_both_hi)

waneScn <- list(
  "basecase" = list("wanei" = ep_infvac, "waneiboth" = ep_both, 
                    "wanes" = sp_infvac, "wanesboth" = sp_both),
  "pessimistic" = list("wanei" = ep_infvac_lo, "waneiboth" = ep_both_lo, 
                       "wanes" = sp_infvac_lo, "wanesboth" = sp_both_lo),
  "optimistic" = list("wanei" = ep_infvac_hi, "waneiboth" = ep_both_hi, 
                      "wanes" = sp_infvac_hi, "wanesboth" = sp_both_hi)
                )
return(waneScn)
}
