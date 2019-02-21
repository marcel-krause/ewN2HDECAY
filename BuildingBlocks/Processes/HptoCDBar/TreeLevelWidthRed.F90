double precision function HptoCDBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.75D0*EL2*TB2*DBLE(CKM21**INT(2.D0))*((MHp2 - 1.D0*DBLE((MC + MD)**INT(2.D0)))*DBLE((MC/TB - 1.D0*MD*YukS3Qua&
  &rk2)**INT(2.D0)) + (MHp2 - 1.D0*DBLE((MC - 1.D0*MD)**INT(2.D0)))*DBLE((MC/TB + MD*YukS3Quark2)**INT(2.D0)))*DBLE((-1.D0*CB*MC &
  &+ MD*SB*YukS3Quark2)**INT(2.D0))* DBLE((MC - 1.D0*MD*TB*YukS3Quark2)**INT(-2.D0)))/(MW2*SB2*SW2)

 HptoCDBarTree = totalAmplitude
end function HptoCDBarTree