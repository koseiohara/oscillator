import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import matplotlib.patches as patches
import sys

from filein import filein
from namelist import namelist

nml  = sys.argv[1]
#anim = sys.argv[2]
setting = namelist.read(nml)

total_time = setting['time']['simulation_seconds']
dt         = setting['time']['output_delta']
nt         = int(total_time/dt)+1

num_particles = setting['constant']['number_of_particles']
spring_length = setting['constant']['spring_length']

datafile = setting['file']['output_filename']

wall_left  = 0.
wall_right = spring_length*(num_particles+1.)



posits = np.empty([num_particles])
#posits[0] = wall_left
#posits[num_particles+1] = wall_right
#posits_xy = np.empty([2,num_particles])
data = filein(datafile, [num_particles], 4*num_particles, 1, 4, 'little', 1)
time = 0

fig = plt.figure(figsize=[6,3])
ax  = fig.add_subplot(111)

fig.tight_layout()
#ax.set_aspect('equal', adjustable='box')
ax.set_aspect('equal')

ax.set_ylim(-(wall_right-wall_left)*0.23, (wall_right-wall_left)*0.23)

title = ax.set_title('Number of Mass Points : {:0d}'.format(num_particles), fontsize=15)

ims = []
ax.axvline(wall_left , color='black', linewidth=2)
ax.axvline(wall_right, color='black', linewidth=2)
ax.hlines(0., xmin=wall_left, xmax=wall_right, color='black', linewidth=0.5)


for t in range(nt):

    posits[:] = data.fread()

    spring = ax.plot(posits, [0]*(num_particles), color='black', marker='o', markersize=10, linestyle='None')
    clock = ax.text(0.07, 0.94, 'Time : {:5.1f}s'.format(time), va='top', ha='left', transform=ax.transAxes, fontsize=15)

    ims.append(spring+[clock])

    time = time + dt


anim = animation.ArtistAnimation(fig, ims, interval=dt*1.E+3)
anim_name = 'Oscillation{:03}.mp4'.format(num_particles)
anim.save('./anim/{}'.format(anim_name), dpi=350)

