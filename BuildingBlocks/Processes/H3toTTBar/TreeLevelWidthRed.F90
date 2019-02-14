double precision function H3toTTBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MT2*(MH32 - 2.D0*MT2 - 2.D0*DBLE(MT**INT(2.D0)))*DBLE(RR32**INT(2.D0)))/(MW2*SB2*SW2)

 H3toTTBarTree = totalAmplitude
end function H3toTTBarTree