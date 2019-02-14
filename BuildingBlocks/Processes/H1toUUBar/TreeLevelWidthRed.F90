double precision function H1toUUBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*(MH12 - 4.D0*MU2)*MU2*DBLE(RR12**INT(2.D0)))/(MW2*SB2*SW2)

 H1toUUBarTree = totalAmplitude
end function H1toUUBarTree