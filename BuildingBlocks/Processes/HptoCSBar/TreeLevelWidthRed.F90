double precision function HptoCSBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (0.75D0*EL2*TB2*DBLE(CKM22**INT(2.D0))*((MHp2 - 1.D0*DBLE((MC - 1.D0*MS)**INT(2.D0)))*DBLE((MC/TB - 1.D0*MS*Yuk&
  &S3Quark2)**INT(2.D0)) + (MHp2 - 1.D0*DBLE((MC + MS)**INT(2.D0)))*DBLE((MC/TB + MS*YukS3Quark2)**INT(2.D0)))*DBLE((-1.D0*CB*MC &
  &+ MS*SB*YukS3Quark2)**INT(2.D0))* DBLE((MC + MS*TB*YukS3Quark2)**INT(-2.D0)))/(MW2*SB2*SW2)

 HptoCSBarTree = totalAmplitude
end function HptoCSBarTree