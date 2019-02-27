collapseFleets <- function (stock) {
  if (dims(stock)$area == 1) return (stock) # do nothing
  out <- FLStock(
    name         = name(stock),
    desc         = FLCore::desc(stock),
    range        = range(stock),
    
    #sums
    harvest      = areaSums(harvest(stock)),
    catch.n      = areaSums(catch.n(stock)),
    landings.n   = areaSums(landings.n(stock)),
    discards.n   = areaSums(discards.n(stock)),
    catch        = areaSums(catch(stock)),
    landings     = areaSums(landings(stock)),
    discards     = areaSums(discards(stock)),
    
    # straight forward averages
    m.spwn       = areaMeans(m.spwn(stock)),
    harvest.spwn = areaMeans(harvest.spwn(stock)),
    m            = areaMeans(m(stock)),
    stock        = areaMeans(stock(stock)),
    stock.n      = areaMeans(stock.n(stock)),
    stock.wt     = areaMeans(stock.wt(stock)),
    mat          = areaMeans(mat(stock)),
    
    # weighted averages
    catch.wt     = areaSums(catch.n(stock) * catch.wt(stock)) / areaSums(catch.n(stock)),
    landings.wt  = areaSums(landings.n(stock) * landings.wt(stock)) / areaSums(landings.n(stock)),
    discards.wt  = areaSums(discards.n(stock) * discards.wt(stock)) / areaSums(discards.n(stock))
  )
  units(out) <- units(stock)
  
  message("Fleet information is assumed to be stored in the 'area' dimension.")
  message("stock, stock.n, stock.wt, m, mat, m.spwn and harvest.spwn \nare assumed to be constant and common accross fleets.")
  
  out
}

