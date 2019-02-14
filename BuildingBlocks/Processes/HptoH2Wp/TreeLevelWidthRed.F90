double precision function HptoH2WpTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.25D0*EL2*(-2.D0*MH22*MHp2 - 2.D0*MH22*MW2 - 2.D0*MHp2*MW2 + DBLE(MH2**INT(4.D0)) + DBLE(MHp**INT(4.D0)) + DB&
  &LE(MW**INT(4.D0)))* DBLE((CB*RR22 - 1.D0*RR21*SB)**INT(2.D0)))/(MW2*SW2)

 HptoH2WpTree = totalAmplitude
end function HptoH2WpTree