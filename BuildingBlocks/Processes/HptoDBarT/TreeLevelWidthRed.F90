double precision function HptoDBarTTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.75D0*EL2*TB2*DBLE(CKM31**INT(2.D0))*((MHp2 - 1.D0*DBLE((MD + MT)**INT(2.D0)))*DBLE((MT/TB - 1.D0*MD*Y&
  &ukS3Quark2)**INT(2.D0)) + (MHp2 - 1.D0*DBLE((MD - 1.D0*MT)**INT(2.D0)))*DBLE((MT/TB + MD*YukS3Quark2)**INT(2.D0)))* DBL&
  &E((-1.D0*CB*MT + MD*SB*YukS3Quark2)**INT(2.D0))*DBLE((MT - 1.D0*MD*TB*YukS3Quark2)**INT(-2.D0)))/(MW2*SB2*SW2)

 HptoDBarTTree = totalAmplitude
end function HptoDBarTTree