double precision function A0toH1Z0Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*EL2*(-2.D0*MA02*MH12 - 2.D0*MA02*MZ2 - 2.D0*MH12*MZ2 + DBLE(MA0**INT(4.D0)) + DBLE(MH1**INT(4.D0)) + DB&
  &LE(MZ**INT(4.D0)))* DBLE((CB*RR12 - 1.D0*RR11*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MZ2*SW2)

 A0toH1Z0Tree = totalAmplitude
end function A0toH1Z0Tree