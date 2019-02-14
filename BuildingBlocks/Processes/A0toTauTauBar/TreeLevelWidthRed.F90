double precision function A0toTauTauBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*MA02*ML2*DBLE(YukS2Lep2**INT(2.D0)))/(MW2*SW2)

 A0toTauTauBarTree = totalAmplitude
end function A0toTauTauBarTree