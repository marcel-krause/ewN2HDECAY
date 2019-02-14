double precision function H3toMuMuBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*(MH32 - 4.D0*MM2)*MM2*DBLE(YukS1Lep3**INT(2.D0)))/(MW2*SW2)

 H3toMuMuBarTree = totalAmplitude
end function H3toMuMuBarTree