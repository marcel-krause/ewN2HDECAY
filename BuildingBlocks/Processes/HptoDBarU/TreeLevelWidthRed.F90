double precision function HptoDBarUTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.75D0*EL2*TB2*DBLE(CKM11**INT(2.D0))*((MHp2 - 1.D0*DBLE((MD + MU)**INT(2.D0)))*DBLE((MU/TB - 1.D0*MD*YukS2Qua&
  &rk2)**INT(2.D0)) + (MHp2 - 1.D0*DBLE((MD - 1.D0*MU)**INT(2.D0)))*DBLE((MU/TB + MD*YukS2Quark2)**INT(2.D0)))*DBLE((MU - 1.D0*MD&
  &*TB*YukS2Quark2)**INT(-2.D0))* DBLE((-1.D0*CB*MU + MD*SB*YukS3Quark2)**INT(2.D0)))/(MW2*SB2*SW2)

 HptoDBarUTree = totalAmplitude
end function HptoDBarUTree