# $Id: Makefile.gnu 31169 2017-03-22 21:38:50Z kcadyper $

# Compiles 

extract_solar_v1.5.0.little_Endian: src/extract_solar.f
        gfortran -frecord-marker=4 -o $@ $?


