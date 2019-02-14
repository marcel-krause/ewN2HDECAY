double precision function H3toTauTauBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*(MH32 - 4.D0*ML2)*ML2*DBLE(YukS1Lep3**INT(2.D0)))/(MW2*SW2)

 H3toTauTauBarTree = totalAmplitude
end function H3toTauTauBarTree