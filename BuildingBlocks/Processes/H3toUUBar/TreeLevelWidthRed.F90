double precision function H3toUUBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*(MH32 - 4.D0*MU2)*MU2*DBLE(RR32**INT(2.D0)))/(MW2*SB2*SW2)

 H3toUUBarTree = totalAmplitude
end function H3toUUBarTree