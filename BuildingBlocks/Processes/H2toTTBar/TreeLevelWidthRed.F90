double precision function H2toTTBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MT2*(MH22 - 2.D0*MT2 - 2.D0*DBLE(MT**INT(2.D0)))*DBLE(RR22**INT(2.D0)))/(MW2*SB2*SW2)

 H2toTTBarTree = totalAmplitude
end function H2toTTBarTree