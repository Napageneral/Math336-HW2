#a
d_ag = 2.88
d_man = 31.45
d_hh = 30.91

D = matrix(c(d_ag, d_man, d_hh), nrow = 3, ncol = 1)
A = matrix(c(1-0.245, -0.099, -0.433, -0.102, 1-0.291, -0.372, -0.051, -0.279, 1-0.011), nrow = 3, ncol = 3)

optimal_production = solve(A,D)
optimal_production

#b
# The solution is thus x1 = 6875, x2 = 3090, x3 = 5972 [Billion USD]. The solution
# appears reasonable. The service has a strong internal demand and a strong export
# demand, and hence is the largest sector of the economy. The manufacture's internal
# demand is very strong although its export demand is not that large. The optimal production
# level of the manufacture sector is 5972 Billion USD. The agriculture has the
# weakest internal consumption and has limited export demand. The optimal production
# level for agriculture is small at 3090 Billion USD.

#c
