g= 9.8
height_init = 3
vel = 900

gpe_init = g*height_init
ke_init = vel^2/2
me = gpe_init+ke_init

height_max = me/g

time = 2*(sqrt(2*height_max/g))

height_max
time