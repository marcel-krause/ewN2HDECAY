double complex function DSelfGpGp(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(43)

 amplitudes(1) = 0.D0

 amplitudes(2) = 0.D0

 amplitudes(3) = 0.D0

 amplitudes(4) = 0.D0

 amplitudes(5) = 0.D0

 amplitudes(6) = 0.D0

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

 amplitudes(10) = 0.D0

 amplitudes(11) = (-0.03125D0*EL2*ME2*(-1.D0*B0(x, 0.D0, ME2) + (ME2 - 1.D0*x)*DB0(x, 0.D0, ME2))*DBLE(YukS3Lep1**INT(2.D0)))/(MW&
  &2*PI2*SW2)

 amplitudes(12) = (-0.03125D0*EL2*MM2*(-1.D0*B0(x, 0.D0, MM2) + (MM2 - 1.D0*x)*DB0(x, 0.D0, MM2))*DBLE(YukS3Lep1**INT(2.D0)))/(MW&
  &2*PI2*SW2)

 amplitudes(13) = (-0.03125D0*EL2*ML2*(-1.D0*B0(x, 0.D0, ML2) + (ML2 - 1.D0*x)*DB0(x, 0.D0, ML2))*DBLE(YukS3Lep1**INT(2.D0)))/(MW&
  &2*PI2*SW2)

 amplitudes(14) = (-0.1875D0*CKM11*CKMC11*(B0(x, MD2, MU2)*((-0.5D0*EL2*MU2*SB2)/(MW2*SW2) - (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark1*&
  &*INT(2.D0)))/(MW2*SW2)) + DB0(x, MD2, MU2)*((-2.D0*EL2*MD2*MU2*SB2*YukS3Quark1)/(MW2*SW2) + MD2*((0.5D0*EL2*MU2*SB2)/(MW2*SW2)&
  & + (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)) + (MU2 - 1.D0*x)*((0.5D0*EL2*MU2*SB2)/(MW2*SW2) + (0.5D0*EL2*MD&
  &2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(15) = (-0.1875D0*CKM21*CKMC21*(B0(x, MC2, MD2)*((-0.5D0*EL2*MC2*SB2)/(MW2*SW2) - (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark1*&
  &*INT(2.D0)))/(MW2*SW2)) + DB0(x, MC2, MD2)*((-2.D0*EL2*MC2*MD2*SB2*YukS3Quark1)/(MW2*SW2) + MC2*((0.5D0*EL2*MC2*SB2)/(MW2*SW2)&
  & + (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)) + (MD2 - 1.D0*x)*((0.5D0*EL2*MC2*SB2)/(MW2*SW2) + (0.5D0*EL2*MD&
  &2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(16) = (-0.1875D0*CKM31*CKMC31*(B0(x, MD2, MT2)*((-0.5D0*EL2*MT2*SB2)/(MW2*SW2) - (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark1*&
  &*INT(2.D0)))/(MW2*SW2)) + DB0(x, MD2, MT2)*((-2.D0*EL2*MD2*MT2*SB2*YukS3Quark1)/(MW2*SW2) + MD2*((0.5D0*EL2*MT2*SB2)/(MW2*SW2)&
  & + (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)) + (MT2 - 1.D0*x)*((0.5D0*EL2*MT2*SB2)/(MW2*SW2) + (0.5D0*EL2*MD&
  &2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(17) = (-0.1875D0*CKM12*CKMC12*(B0(x, MS2, MU2)*((-0.5D0*EL2*MU2*SB2)/(MW2*SW2) - (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark1*&
  &*INT(2.D0)))/(MW2*SW2)) + DB0(x, MS2, MU2)*((-2.D0*EL2*MS2*MU2*SB2*YukS3Quark1)/(MW2*SW2) + MS2*((0.5D0*EL2*MU2*SB2)/(MW2*SW2)&
  & + (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)) + (MU2 - 1.D0*x)*((0.5D0*EL2*MU2*SB2)/(MW2*SW2) + (0.5D0*EL2*MS&
  &2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(18) = (-0.1875D0*CKM22*CKMC22*(B0(x, MC2, MS2)*((-0.5D0*EL2*MC2*SB2)/(MW2*SW2) - (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark1*&
  &*INT(2.D0)))/(MW2*SW2)) + DB0(x, MC2, MS2)*((-2.D0*EL2*MC2*MS2*SB2*YukS3Quark1)/(MW2*SW2) + MC2*((0.5D0*EL2*MC2*SB2)/(MW2*SW2)&
  & + (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)) + (MS2 - 1.D0*x)*((0.5D0*EL2*MC2*SB2)/(MW2*SW2) + (0.5D0*EL2*MS&
  &2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(19) = (-0.1875D0*CKM32*CKMC32*(B0(x, MS2, MT2)*((-0.5D0*EL2*MT2*SB2)/(MW2*SW2) - (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark1*&
  &*INT(2.D0)))/(MW2*SW2)) + DB0(x, MS2, MT2)*((-2.D0*EL2*MS2*MT2*SB2*YukS3Quark1)/(MW2*SW2) + MS2*((0.5D0*EL2*MT2*SB2)/(MW2*SW2)&
  & + (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)) + (MT2 - 1.D0*x)*((0.5D0*EL2*MT2*SB2)/(MW2*SW2) + (0.5D0*EL2*MS&
  &2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(20) = (-0.1875D0*CKM13*CKMC13*(B0(x, MB2, MU2)*((-0.5D0*EL2*MU2*SB2)/(MW2*SW2) - (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark1*&
  &*INT(2.D0)))/(MW2*SW2)) + DB0(x, MB2, MU2)*((-2.D0*EL2*MB2*MU2*SB2*YukS3Quark1)/(MW2*SW2) + MB2*((0.5D0*EL2*MU2*SB2)/(MW2*SW2)&
  & + (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)) + (MU2 - 1.D0*x)*((0.5D0*EL2*MU2*SB2)/(MW2*SW2) + (0.5D0*EL2*MB&
  &2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(21) = (-0.1875D0*CKM23*CKMC23*(B0(x, MB2, MC2)*((-0.5D0*EL2*MC2*SB2)/(MW2*SW2) - (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark1*&
  &*INT(2.D0)))/(MW2*SW2)) + DB0(x, MB2, MC2)*((-2.D0*EL2*MB2*MC2*SB2*YukS3Quark1)/(MW2*SW2) + MB2*((0.5D0*EL2*MC2*SB2)/(MW2*SW2)&
  & + (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)) + (MC2 - 1.D0*x)*((0.5D0*EL2*MC2*SB2)/(MW2*SW2) + (0.5D0*EL2*MB&
  &2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(22) = (-0.1875D0*CKM33*CKMC33*(B0(x, MB2, MT2)*((-0.5D0*EL2*MT2*SB2)/(MW2*SW2) - (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark1*&
  &*INT(2.D0)))/(MW2*SW2)) + DB0(x, MB2, MT2)*((-2.D0*EL2*MB2*MT2*SB2*YukS3Quark1)/(MW2*SW2) + MB2*((0.5D0*EL2*MT2*SB2)/(MW2*SW2)&
  & + (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)) + (MT2 - 1.D0*x)*((0.5D0*EL2*MT2*SB2)/(MW2*SW2) + (0.5D0*EL2*MB&
  &2*SB2*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(23) = (0.0625D0*DB0(x, MH12, MW2)*DBLE(CS1S3S3f111**INT(2.D0)))/PI2

 amplitudes(24) = (0.0625D0*DB0(x, MH22, MW2)*DBLE(CS1S3S3f211**INT(2.D0)))/PI2

 amplitudes(25) = (0.0625D0*DB0(x, MH32, MW2)*DBLE(CS1S3S3f311**INT(2.D0)))/PI2

 amplitudes(26) = (0.0625D0*CS1S3S3f112*CS1S3S3f121*DB0(x, MH12, MHp2))/PI2

 amplitudes(27) = (0.0625D0*CS1S3S3f212*CS1S3S3f221*DB0(x, MH22, MHp2))/PI2

 amplitudes(28) = (0.0625D0*CS1S3S3f312*CS1S3S3f321*DB0(x, MH32, MHp2))/PI2

 amplitudes(29) = 0D0

 amplitudes(30) = 0D0

 amplitudes(31) = 0.D0

 amplitudes(32) = (0.015625D0*DB0(x, MA02, MHp2)*DBLE((-1.D0*CB2 - 1.D0*SB2)**INT(2.D0))*DBLE(((-2.D0*CB2*MW*SW)/EL - (2.D0*MW*SB&
  &2*SW)/EL)**INT(2.D0))* DBLE(((-0.25D0*EL2*(-1.D0*MA02 + m12squared/(CB*SB)))/(MW2*SW2) + (0.25D0*EL2*(MA02 - 2.D0*MHp2 + m12sq&
  &uared/(CB*SB)))/(MW2*SW2))**INT(2.D0)))/ PI2

 amplitudes(33) = (0.00390625D0*DB0(x, MW2, MZ2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*(DBLE(CW**INT(4.D0)) - 1.D0*DBLE(SW**IN&
  &T(4.D0)))* DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB2*SW)/EL)**INT(2.D0)))/(CW2*PI2)

 amplitudes(34) = (0.00390625D0*DB0(x, MW2, MZ2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*(DBLE(CW**INT(4.D0)) - 1.D0*DBLE(SW**IN&
  &T(4.D0)))* DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB2*SW)/EL)**INT(2.D0)))/(CW2*PI2)

 amplitudes(35) = (0.0625D0*DB0(x, 0.D0, MW2)*DBLE(EL**INT(4.D0))*DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB2*SW)/EL)**INT(2.D0)))/(&
  &PI2*SW2)

 amplitudes(36) = (0.0625D0*DB0(x, MW2, MZ2)*DBLE(EL**INT(4.D0))*DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB2*SW)/EL)**INT(2.D0)))/(C&
  &W2*PI2)

 amplitudes(37) = (0.0625D0*EL2*(-2.D0*B0(x, 0.D0, MW2) - 2.D0*(MW2 + x)*DB0(x, 0.D0, MW2)))/PI2

 amplitudes(38) = (0.015625D0*EL2*(-2.D0*B0(x, MW2, MZ2) + (-2.D0*MW2 + MZ2 - 2.D0*x)*DB0(x, MW2, MZ2))*DBLE((CW2 - 1.D0*SW2)**IN&
  &T(2.D0)))/(CW2*PI2*SW2)

 amplitudes(39) = (-0.015625D0*EL2*(2.D0*B0(x, MH12, MW2) + (-1.D0*MW2 + 2.D0*(MH12 + x))*DB0(x, MH12, MW2))*DBLE((CA1*CA2*CB + C&
  &A2*SA1*SB)**INT(2.D0)))/(PI2*SW2)

 amplitudes(40) = (-0.015625D0*EL2*(2.D0*B0(x, MH22, MW2) + (-1.D0*MW2 + 2.D0*(MH22 + x))*DB0(x, MH22, MW2))* DBLE((CB*(-1.D0*CA3&
  &*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)**INT(2.D0)))/(PI2*SW2)

 amplitudes(41) = (-0.015625D0*EL2*(2.D0*B0(x, MH32, MW2) + (-1.D0*MW2 + 2.D0*(MH32 + x))*DB0(x, MH32, MW2))* DBLE((CB*(-1.D0*CA1&
  &*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)**INT(2.D0)))/(PI2*SW2)

 amplitudes(42) = (-0.015625D0*EL2*(2.D0*B0(x, MW2, MZ2) + (-1.D0*MW2 + 2.D0*(MZ2 + x))*DB0(x, MW2, MZ2))*DBLE((CB2 + SB2)**INT(2&
  &.D0)))/(PI2*SW2)

 amplitudes(43) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,43
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfGpGp = totalAmplitude
end function DSelfGpGp

