double precision function H1toTTBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MT2*(MH12 - 2.D0*MT2 - 2.D0*DBLE(MT**INT(2.D0)))*DBLE(RR12**INT(2.D0)))/(MW2*SB2*SW2)

 H1toTTBarTree = totalAmplitude
end function H1toTTBarTree