double precision function HptoBBarCTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.75D0*EL2*TB2*DBLE(CKM23**INT(2.D0))*((MHp2 - 1.D0*DBLE((MB - 1.D0*MC)**INT(2.D0)))*DBLE((MC/TB - 1.D0*MB*Yuk&
  &S3Quark2)**INT(2.D0)) + (MHp2 - 1.D0*DBLE((MB + MC)**INT(2.D0)))*DBLE((MC/TB + MB*YukS3Quark2)**INT(2.D0)))*DBLE((-1.D0*CB*MC &
  &+ MB*SB*YukS3Quark2)**INT(2.D0))* DBLE((MC + MB*TB*YukS3Quark2)**INT(-2.D0)))/(MW2*SB2*SW2)

 HptoBBarCTree = totalAmplitude
end function HptoBBarCTree