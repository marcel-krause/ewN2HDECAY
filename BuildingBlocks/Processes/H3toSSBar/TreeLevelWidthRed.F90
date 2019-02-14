double precision function H3toSSBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*(MH32 - 4.D0*MS2)*MS2*DBLE(YukS1Quark3**INT(2.D0)))/(MW2*SW2)

 H3toSSBarTree = totalAmplitude
end function H3toSSBarTree