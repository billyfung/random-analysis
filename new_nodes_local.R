library(emhTradeR)
library(dplyr)
library(plyr) 

prices = emht_db('aws_prices', '')

ARA = dbGetQuery(prices, "select distinct node from view_FinalSpot_latest
                 where node like '%ARA%' and priceType = 'F' and nzdate > '2000-01-01'")

get_node = function(code){
  node = dbGetQuery(prices, paste("select distinct node from view_FinalSpot_latest
                                  where priceType = 'F' and nzdate > '2000-01-01' and node like '%", code,
                                  "%'", sep=""))
  return(node)
}

# read in list of node codes
# format is in Code, Location 
mydata = read.csv("potentials_nodes.csv")
mydata$Code = as.character(mydata$Code)

# get the node from sql server for each code
# there may be more than one node
# priority: xyz2201, xyz2202, xyz1101, xyz1102, xyz(any_other_number)
nodes = mapply(get_node, code=mydata$Code)

# dbGetQuery(prices, "
#            select year(nzdate),
#            month(nzdate),
#            avg(b_a_opt),
#            avg(a_b_opt)
#            from
#            ( select nzdate,
#            tp,
#            (max(node_a) - max(node_b)) as b_a_opt,
#            (max(node_b) - max(node_a)) as a_b_opt
#            from (
#            select nzdate, tp,
#            case node when 'ARA2201' then price end as node_a,
#            case node when 'WKM2201' then price end as node_b
#            from view_finalspot_latest
#            group by nzdate,tp,node, price
#            ) foo group by nzdate, tp) bar
#            group by year(nzdate), month(nzdate)")

# get all spot prices for the latest day for all the 66 nodes
node_prices = dbGetQuery(prices,"
                         select node, nzdate, tp, price
                         from view_finalspot_latest
                         where node in
                         ('OTA2201', 'BEN2201', 'HAY2201', 'ISL2201', 'INV2201', 'ALB1101', 'ARA2201', 'ASB0661', 'ATI2201', 'AVI2201', 'BPE2201', 'BRB0331', 'BRK0331', 'BRY0661', 'CML0331', 'CYD2201', 'EDG0331', 'GLN0331', 'HAM2201', 'HEN2201', 'HLY2201', 'HOB1101', 'HWB2201', 'KAW2201', 'KIK0111', 'KMO0331', 'LTN0331', 'MAN2201', 'MDN2201', 'MTI2201', 'NAP2201', 'NMA0331', 'NPL2201', 'NSY0331', 'OHA2201', 'OHB2201', 'OHC2201', 'OHK2201', 'OKI2201', 'PAK0331', 'PEN1101', 'PPI2201', 'RDF2201', 'ROX2201', 'RPO2201', 'SDN0331', 'SFD2201', 'STK2201', 'SVL0331', 'SWN2201', 'TAK0331', 'THI2201', 'TIM0111', 'TKB2201', 'TKU2201', 'TMN0551', 'TNG0551', 'TRK2201', 'TWC2201', 'TWH0331', 'TWI2201', 'TWZ0331', 'WHI2201', 'WIL0331', 'WKM2201', 'WPA2201', 'WPR0661', 'WRD0331', 'WRK2201', 'WTK2201', 'WTU0331')
                         and pricetype = 'F' and nzdate > '1999-12-31'")

node_prices$nzdate = as.Date(node_prices$nzdate)
node_prices$node = as.factor(node_prices$node)
list_nodes = c('OTA2201', 'BEN2201', 'HAY2201', 'ISL2201', 'INV2201', 'ALB1101', 'ARA2201', 'ASB0661', 'ATI2201', 'AVI2201', 'BPE2201', 'BRB0331', 'BRK0331', 'BRY0661', 'CML0331', 'CYD2201', 'EDG0331', 'GLN0331', 'HAM2201', 'HEN2201', 'HLY2201', 'HOB1101', 'HWB2201', 'KAW2201', 'KIK0111', 'KMO0331', 'LTN0331', 'MAN2201', 'MDN2201', 'MTI2201', 'NAP2201', 'NMA0331', 'NPL2201', 'NSY0331', 'OHA2201', 'OHB2201', 'OHC2201', 'OHK2201', 'OKI2201', 'PAK0331', 'PEN1101', 'PPI2201', 'RDF2201', 'ROX2201', 'RPO2201', 'SDN0331', 'SFD2201', 'STK2201', 'SVL0331', 'SWN2201', 'TAK0331', 'THI2201', 'TIM0111', 'TKB2201', 'TKU2201', 'TMN0551', 'TNG0551', 'TRK2201', 'TWC2201', 'TWH0331', 'TWI2201', 'TWZ0331', 'WHI2201', 'WIL0331', 'WKM2201', 'WPA2201', 'WPR0661', 'WRD0331', 'WRK2201', 'WTK2201', 'WTU0331')
current_nodes = c('OTA2201', 'BEN2201', 'HAY2201', 'ISL2201', 'INV2201')
paths = combn(x=list_nodes, m=2)

# get two nodes and calculate ftr_payoff from node_prices
# a_b_opt 
get_ftr_payoff = function(node_a, node_b){
  ftr_path_a = paste(substr(node_a, 1, 3), substr(node_b, 1, 3), sep='_')
  # ftr_path_b = paste(substr(nodb, 1, 3), substr(noda, 1, 3), sep='_')
  opt_price_a = max((filter(node_prices, node==node_a)$price - filter(node_prices, node==node_b)$price), 0)
  # opt_price_b = max(filter(node_prices, node == node_b)$price) - max(filter(node_prices, node == node_a)$price)
  # result = matrix(
  #   c(ftr_path_a, opt_price_a, ftr_path_b, opt_price_b),
  #   nrow=2,
  #   ncol=2,
  #   byrow = TRUE)
  result = c(ftr_path_a, opt_price_a)
  return (result)
}

# for nzdate 2016-12-20
# wide format
ftr_payoffs = mapply(get_ftr_payoff, node_a=paths$V1, node_b=paths$V2)
ftr_payoffs2 = mapply(get_ftr_payoff, node_a=paths$V2, node_b=paths$V1)
ftr_p = t(cbind(ftr_payoffs, ftr_payoffs2))

spot_HAY = dbGetQuery(prices, "select node, nzdate, tp, price
                      from view_finalspot where node = 'HAY2201'
                      and pricetype = 'F' and nzdate > '1999-12-31'")

get_spot_price = function(node){
  start.time = Sys.time()
  spot_price = dbGetQuery(prices, paste("select node, nzdate, tp, price
                                        from view_finalspot where pricetype = 'F' and nzdate > '1999-12-31'
                                        and node = '", node,
                                        "'", sep=""))
  end.time = Sys.time()
  paste(end.time-start.time)
  return(spot_price)
}

current_nodes = mapply(get_spot_price, node=current_nodes)
