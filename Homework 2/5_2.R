#a
d_ag = 2.88
d_man = 31.45
d_hh = 30.91

D = matrix(c(d_ag, d_man, d_hh), nrow = 3, ncol = 1)
D
A = matrix(c(1-0.245, -0.099, -0.433, -0.102, 1-0.291, -0.372, -0.051, -0.279, 1-0.011), nrow = 3, ncol = 3)
A

optimal_production = solve(A,D)
optimal_production



