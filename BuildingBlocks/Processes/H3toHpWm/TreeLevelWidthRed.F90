double precision function H3toHpWmTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*EL2*(-2.D0*MH32*MHp2 - 2.D0*MH32*MW2 - 2.D0*MHp2*MW2 + DBLE(MH3**INT(4.D0)) + DBLE(MHp**INT(4.D0)) + DB&
  &LE(MW**INT(4.D0)))* DBLE((CB*RR32 - 1.D0*RR31*SB)**INT(2.D0)))/(MW2*SW2)

 H3toHpWmTree = totalAmplitude
end function H3toHpWmTree