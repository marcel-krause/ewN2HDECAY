double precision function H3toA0Z0Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*EL2*(-2.D0*MA02*MH32 - 2.D0*MA02*MZ2 - 2.D0*MH32*MZ2 + DBLE(MA0**INT(4.D0)) + DBLE(MH3**INT(4.D0)) + DB&
  &LE(MZ**INT(4.D0)))* DBLE((CB*RR32 - 1.D0*RR31*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MZ2*SW2)

 H3toA0Z0Tree = totalAmplitude
end function H3toA0Z0Tree