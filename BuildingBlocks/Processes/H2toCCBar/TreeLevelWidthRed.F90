double precision function H2toCCBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MC2*(-4.D0*MC2 + MH22)*DBLE(RR22**INT(2.D0)))/(MW2*SB2*SW2)

 H2toCCBarTree = totalAmplitude
end function H2toCCBarTree