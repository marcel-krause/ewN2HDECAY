double precision function A0toBBBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MA02*MB2*DBLE(YukS2Quark2**INT(2.D0)))/(MW2*SW2)

 A0toBBBarTree = totalAmplitude
end function A0toBBBarTree