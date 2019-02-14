double precision function HptoBBarTTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.75D0*EL2*TB2*DBLE(CKM33**INT(2.D0))*((MHp2 - 1.D0*DBLE((MB + MT)**INT(2.D0)))*DBLE((MT/TB - 1.D0*MB*Y&
  &ukS3Quark2)**INT(2.D0)) + (MHp2 - 1.D0*DBLE((MB - 1.D0*MT)**INT(2.D0)))*DBLE((MT/TB + MB*YukS3Quark2)**INT(2.D0)))* DBL&
  &E((-1.D0*CB*MT + MB*SB*YukS3Quark2)**INT(2.D0))*DBLE((MT - 1.D0*MB*TB*YukS3Quark2)**INT(-2.D0)))/(MW2*SB2*SW2)

 HptoBBarTTree = totalAmplitude
end function HptoBBarTTree