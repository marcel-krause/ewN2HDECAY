double complex function DSelfDSRight(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (-0.03125D0*CKM11*CKMC12*DBLE(x**INT(-2.D0))*((-0.5D0*EL2*MD*MS*SB2*A0(MU2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*S&
  &W2) + (0.5D0*EL2*MD*MS*SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MS*SB2*B0(x, MU2, MW2)*DBLE(YukS3Qu&
  &ark1**INT(2.D0)))/ SW2 + (0.5D0*EL2*MD*MS*MU2*SB2*B0(x, MU2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*S&
  &B2*x*B0(x, MU2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM11*CKMC12*((0.5D0*EL2*MD*MS*SB2*B0(x,&
  & MU2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MS*SB2*DB0(x, MU2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 +&
  & (0.5D0*EL2*MD*MS*MU2*SB2*DB0(x, MU2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*SB2*x*DB0(x, MU2, MW2)*D&
  &BLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(2) = (-0.03125D0*CKM21*CKMC22*DBLE(x**INT(-2.D0))*((-0.5D0*EL2*MD*MS*SB2*A0(MC2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*S&
  &W2) + (0.5D0*EL2*MD*MS*SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MS*SB2*B0(x, MC2, MW2)*DBLE(YukS3Qu&
  &ark1**INT(2.D0)))/ SW2 + (0.5D0*EL2*MC2*MD*MS*SB2*B0(x, MC2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*S&
  &B2*x*B0(x, MC2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM21*CKMC22*((0.5D0*EL2*MD*MS*SB2*B0(x,&
  & MC2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MS*SB2*DB0(x, MC2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 +&
  & (0.5D0*EL2*MC2*MD*MS*SB2*DB0(x, MC2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*SB2*x*DB0(x, MC2, MW2)*D&
  &BLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(3) = (-0.03125D0*CKM31*CKMC32*DBLE(x**INT(-2.D0))*((-0.5D0*EL2*MD*MS*SB2*A0(MT2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*S&
  &W2) + (0.5D0*EL2*MD*MS*SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MS*SB2*B0(x, MT2, MW2)*DBLE(YukS3Qu&
  &ark1**INT(2.D0)))/ SW2 + (0.5D0*EL2*MD*MS*MT2*SB2*B0(x, MT2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*S&
  &B2*x*B0(x, MT2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM31*CKMC32*((0.5D0*EL2*MD*MS*SB2*B0(x,&
  & MT2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MS*SB2*DB0(x, MT2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 +&
  & (0.5D0*EL2*MD*MS*MT2*SB2*DB0(x, MT2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*SB2*x*DB0(x, MT2, MW2)*D&
  &BLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(4) = (-0.03125D0*CKM11*CKMC12*DBLE(x**INT(-2.D0))*((0.5D0*EL2*MD*MS*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*S&
  &W2) - (0.5D0*EL2*MD*MS*SB2*A0(MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MHp2*MS*SB2*B0(x, MHp2, MU2)*DBLE(Y&
  &ukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*MU2*SB2*B0(x, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0&
  &*EL2*MD*MS*SB2*x*B0(x, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM11*CKMC12*((0.5D0*EL2*MD&
  &*MS*SB2*B0(x, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MHp2*MS*SB2*DB0(x, MHp2, MU2)*DBLE(YukS3Quark&
  &2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*MU2*SB2*DB0(x, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*&
  &MS*SB2*x*DB0(x, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(5) = (-0.03125D0*CKM21*CKMC22*DBLE(x**INT(-2.D0))*((-0.5D0*EL2*MD*MS*SB2*A0(MC2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*S&
  &W2) + (0.5D0*EL2*MD*MS*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MC2*MD*MS*SB2*B0(x, MC2, MHp2)*DBLE(Y&
  &ukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MHp2*MS*SB2*B0(x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D&
  &0*EL2*MD*MS*SB2*x*B0(x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM21*CKMC22*((0.5D0*EL2*M&
  &D*MS*SB2*B0(x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MC2*MD*MS*SB2*DB0(x, MC2, MHp2)*DBLE(YukS3Quark&
  &2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MHp2*MS*SB2*DB0(x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD&
  &*MS*SB2*x*DB0(x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(6) = (-0.03125D0*CKM31*CKMC32*DBLE(x**INT(-2.D0))*((0.5D0*EL2*MD*MS*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*S&
  &W2) - (0.5D0*EL2*MD*MS*SB2*A0(MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MHp2*MS*SB2*B0(x, MHp2, MT2)*DBLE(Y&
  &ukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*MT2*SB2*B0(x, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0&
  &*EL2*MD*MS*SB2*x*B0(x, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM31*CKMC32*((0.5D0*EL2*MD&
  &*MS*SB2*B0(x, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD*MHp2*MS*SB2*DB0(x, MHp2, MT2)*DBLE(YukS3Quark&
  &2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*MS*MT2*SB2*DB0(x, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD*&
  &MS*SB2*x*DB0(x, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfDSRight = totalAmplitude
end function DSelfDSRight

