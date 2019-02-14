double precision function H3toCCBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MC2*(-4.D0*MC2 + MH32)*DBLE(RR32**INT(2.D0)))/(MW2*SB2*SW2)

 H3toCCBarTree = totalAmplitude
end function H3toCCBarTree