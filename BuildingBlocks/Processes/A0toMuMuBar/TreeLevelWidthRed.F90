double precision function A0toMuMuBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.5D0*EL2*MA02*MM2*DBLE(YukS2Lep2**INT(2.D0)))/(MW2*SW2)

 A0toMuMuBarTree = totalAmplitude
end function A0toMuMuBarTree