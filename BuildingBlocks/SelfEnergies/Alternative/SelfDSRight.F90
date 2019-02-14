double complex function SelfDSRightAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.03125D0*CKM11*CKMC12*((-0.5D0*EL2*MD*MS*SB2*A0(MU2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*M&
  &S*SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MS*SB2*B0(x, MU2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/ SW&
  &2 + (0.5D0*EL2*MD*MS*MU2*SB2*B0(x, MU2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*SB2*x*B0(x, MU2, MW2)*&
  &DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(2) = (0.03125D0*CKM21*CKMC22*((-0.5D0*EL2*MD*MS*SB2*A0(MC2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*M&
  &S*SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MS*SB2*B0(x, MC2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/ SW&
  &2 + (0.5D0*EL2*MC2*MD*MS*SB2*B0(x, MC2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*SB2*x*B0(x, MC2, MW2)*&
  &DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(3) = (0.03125D0*CKM31*CKMC32*((-0.5D0*EL2*MD*MS*SB2*A0(MT2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*M&
  &S*SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MS*SB2*B0(x, MT2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/ SW&
  &2 + (0.5D0*EL2*MD*MS*MT2*SB2*B0(x, MT2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*SB2*x*B0(x, MT2, MW2)*&
  &DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(4) = (0.03125D0*CKM11*CKMC12*((0.5D0*EL2*MD*MS*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*M&
  &S*SB2*A0(MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MHp2*MS*SB2*B0(x, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)&
  &))/(MW2*SW2) + (0.5D0*EL2*MD*MS*MU2*SB2*B0(x, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*SB2*x*B0(x&
  &, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(5) = (0.03125D0*CKM21*CKMC22*((-0.5D0*EL2*MD*MS*SB2*A0(MC2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*M&
  &S*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MC2*MD*MS*SB2*B0(x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)&
  &))/(MW2*SW2) - (0.5D0*EL2*MD*MHp2*MS*SB2*B0(x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*SB2*x*B0(&
  &x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(6) = (0.03125D0*CKM31*CKMC32*((0.5D0*EL2*MD*MS*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*M&
  &S*SB2*A0(MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MHp2*MS*SB2*B0(x, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)&
  &))/(MW2*SW2) + (0.5D0*EL2*MD*MS*MT2*SB2*B0(x, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*SB2*x*B0(x&
  &, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfDSRightAlter = totalAmplitude
end function SelfDSRightAlter

