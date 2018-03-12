options(scipen=999)
#a
A = matrix(c(9,6,0,0,0), nrow = 1, ncol = 5)
B = matrix(c(0,-6,3,4,0), nrow = 1, ncol = 5)
C = matrix(c(0,0,0,-4,12), nrow = 1, ncol = 5)
D = matrix(c(1,-1,-1,0,0), nrow = 1, ncol = 5)
E = matrix(c(0,0,1,-1,-1), nrow = 1, ncol = 5)

a = rbind(A,B,C,D,E)
b = matrix(c(12,0,0,0,0), nrow =5, ncol =1)

amps = solve(a,b)/1000
amps


#b
Va = amps[2,1]*6*1000
Vb = amps[4,1]*4*1000
Vc = amps[5,1]*3*1000
rbind(Va,Vb,Vc)

#c
P1 = 9*amps[1,1]^2*1000
P2 = 6*amps[2,1]^2*1000
P3 = 3*amps[3,1]^2*1000
P4 = 4*amps[4,1]^2*1000
P5 = 9*amps[5,1]^2*1000
P6 = 3*amps[5,1]^2*1000
rbind(P1,P2,P3,P4,P5,P6)

#d
pTotal = P1+P2+P3+P4+P5+P6
pTotal
work_10min = pTotal*(10*60)
work_10min