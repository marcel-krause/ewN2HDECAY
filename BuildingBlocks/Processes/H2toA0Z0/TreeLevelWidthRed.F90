double precision function H2toA0Z0Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*EL2*(-2.D0*MA02*MH22 - 2.D0*MA02*MZ2 - 2.D0*MH22*MZ2 + DBLE(MA0**INT(4.D0)) + DBLE(MH2**INT(4.D0)) + DB&
  &LE(MZ**INT(4.D0)))* DBLE((CB*RR22 - 1.D0*RR21*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MZ2*SW2)

 H2toA0Z0Tree = totalAmplitude
end function H2toA0Z0Tree