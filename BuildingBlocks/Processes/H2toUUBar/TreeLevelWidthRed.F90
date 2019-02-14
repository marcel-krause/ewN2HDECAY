double precision function H2toUUBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*(MH22 - 4.D0*MU2)*MU2*DBLE(RR22**INT(2.D0)))/(MW2*SB2*SW2)

 H2toUUBarTree = totalAmplitude
end function H2toUUBarTree