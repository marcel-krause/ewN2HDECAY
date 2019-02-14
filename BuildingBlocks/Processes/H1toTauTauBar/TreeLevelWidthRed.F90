double precision function H1toTauTauBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*(MH12 - 4.D0*ML2)*ML2*DBLE(YukS1Lep1**INT(2.D0)))/(MW2*SW2)

 H1toTauTauBarTree = totalAmplitude
end function H1toTauTauBarTree