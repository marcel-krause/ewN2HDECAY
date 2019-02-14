double precision function HptoBBarUTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.75D0*EL2*TB2*DBLE(CKM13**INT(2.D0))*((MHp2 - 1.D0*DBLE((MB + MU)**INT(2.D0)))*DBLE((MU/TB - 1.D0*MB*YukS2Qua&
  &rk2)**INT(2.D0)) + (MHp2 - 1.D0*DBLE((MB - 1.D0*MU)**INT(2.D0)))*DBLE((MU/TB + MB*YukS2Quark2)**INT(2.D0)))*DBLE((MU - 1.D0*MB&
  &*TB*YukS2Quark2)**INT(-2.D0))* DBLE((-1.D0*CB*MU + MB*SB*YukS3Quark2)**INT(2.D0)))/(MW2*SB2*SW2)

 HptoBBarUTree = totalAmplitude
end function HptoBBarUTree