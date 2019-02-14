double precision function H2toSSBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*(MH22 - 4.D0*MS2)*MS2*DBLE(YukS1Quark2**INT(2.D0)))/(MW2*SW2)

 H2toSSBarTree = totalAmplitude
end function H2toSSBarTree