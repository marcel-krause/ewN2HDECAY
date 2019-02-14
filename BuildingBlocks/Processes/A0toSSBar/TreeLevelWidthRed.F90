double precision function A0toSSBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MA02*MS2*DBLE(YukS2Quark2**INT(2.D0)))/(MW2*SW2)

 A0toSSBarTree = totalAmplitude
end function A0toSSBarTree