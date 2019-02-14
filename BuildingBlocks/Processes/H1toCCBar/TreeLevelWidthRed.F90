double precision function H1toCCBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MC2*(-4.D0*MC2 + MH12)*DBLE(RR12**INT(2.D0)))/(MW2*SB2*SW2)

 H1toCCBarTree = totalAmplitude
end function H1toCCBarTree