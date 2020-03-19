collapseAreas <- function (stock) {
  if (dims(stock)$area == 1) return (stock) # do nothing
  out <- FLStock(
    name         = name(stock),
    #desc         = desc(stock),

    #sums
    harvest      = areaSums(harvest(stock)),
    stock.n      = areaSums(stock.n(stock)),
    catch.n      = areaSums(catch.n(stock)),
    landings.n   = areaSums(landings.n(stock)),
    discards.n   = areaSums(discards.n(stock)),
    stock        = areaSums(stock(stock)),
    catch        = areaSums(catch(stock)),
    landings     = areaSums(landings(stock)),
    discards     = areaSums(discards(stock)),

    # straight forward averages
    m            = areaMeans(m(stock)),
    m.spwn       = areaMeans(m.spwn(stock)),
    harvest.spwn = areaMeans(harvest.spwn(stock)),

    # weighted averages
    catch.wt     = areaSums(catch.n(stock) * catch.wt(stock)) / areaSums(catch.n(stock)),
    landings.wt  = areaSums(landings.n(stock) * landings.wt(stock)) / areaSums(landings.n(stock)),
    discards.wt  = areaSums(discards.n(stock) * discards.wt(stock)) / areaSums(discards.n(stock)),
    stock.wt     = areaSums(stock.n(stock) * stock.wt(stock)) / areaSums(stock.n(stock)),
    mat          = areaSums(stock.n(stock) * mat(stock)) / areaSums(stock.n(stock))
  )
  units(out) <- units(stock)

  out
}

