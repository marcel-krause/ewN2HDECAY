double precision function H2toTauTauBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*(MH22 - 4.D0*ML2)*ML2*DBLE(YukS1Lep2**INT(2.D0)))/(MW2*SW2)

 H2toTauTauBarTree = totalAmplitude
end function H2toTauTauBarTree