double precision function A0toTTBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*CB2*EL2*MT2*(MA02 + 2.D0*MT2 - 2.D0*DBLE(MT**INT(2.D0))))/(MW2*SB2*SW2)

 A0toTTBarTree = totalAmplitude
end function A0toTTBarTree