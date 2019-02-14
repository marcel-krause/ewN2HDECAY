double precision function H2toMuMuBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*(MH22 - 4.D0*MM2)*MM2*DBLE(YukS1Lep2**INT(2.D0)))/(MW2*SW2)

 H2toMuMuBarTree = totalAmplitude
end function H2toMuMuBarTree