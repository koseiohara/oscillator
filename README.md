# Oscillator

Test Environment : gfortran 14.2.0, Python 3.12.6, matplotlib 3.9.2, numpy 2.1.1, ffmpeg 7.0.2

This is a program to simulate coupled oscillations.
Tested numbers of the mass points are 1, 2, 3, and 10.
Most settings are configured within the namelist.
Specify the file name of the namelist in Makefile.
The program can be compiled by "make".
To execution, enter "make run" in src/.
output/ is needed before the execution.
To plot the result, make directory plot/anim/ and execute "make plot" in src/.
Settings for making an animation are read from the same namelist.

## Namelist
### time
- simulation\_seconds  
  Simulation time (seconds).
- time\_delta  
  Time delta for integration (seconds).
- output\_delta  
  Time delta for output (seconds).

### constant
- number\_of\_particles  
  Number of mass points.
- mass\_of\_particles  
  Mass of the mass points (kg).
  All mass points will have the same mass.
- spring\_constant  
  Spring constant of all springs (N/m).
- spring\_length  
  Length of all springs (m).

### file
- output\_filename  
  File name of the output binary.
  The name should be start from ../output/

