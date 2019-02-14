double precision function H1toHpWmTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*EL2*(-2.D0*MH12*MHp2 - 2.D0*MH12*MW2 - 2.D0*MHp2*MW2 + DBLE(MH1**INT(4.D0)) + DBLE(MHp**INT(4.D0)) + DB&
  &LE(MW**INT(4.D0)))* DBLE((CB*RR12 - 1.D0*RR11*SB)**INT(2.D0)))/(MW2*SW2)

 H1toHpWmTree = totalAmplitude
end function H1toHpWmTree