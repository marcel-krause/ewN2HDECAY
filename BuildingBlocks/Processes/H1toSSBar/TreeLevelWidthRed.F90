double precision function H1toSSBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*(MH12 - 4.D0*MS2)*MS2*DBLE(YukS1Quark1**INT(2.D0)))/(MW2*SW2)

 H1toSSBarTree = totalAmplitude
end function H1toSSBarTree