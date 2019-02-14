double complex function SelfGpHpUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(41)

 amplitudes(1) = (-0.03125D0*CS1S1S3S3f1112*A0(MH12))/PI2

 amplitudes(2) = (-0.03125D0*CS1S1S3S3f2212*A0(MH22))/PI2

 amplitudes(3) = (-0.03125D0*CS1S1S3S3f3312*A0(MH32))/PI2

 amplitudes(4) = (-0.03125D0*CS2S2S3S3f1112*A0(MZ2))/PI2

 amplitudes(5) = (-0.03125D0*CS2S2S3S3f2212*A0(MA02))/PI2

 amplitudes(6) = (-0.0625D0*CS3S3S3S3f1121*A0(MW2))/PI2

 amplitudes(7) = (-0.0625D0*CS3S3S3S3f1222*A0(MHp2))/PI2

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

 amplitudes(10) = 0.D0

 amplitudes(11) = (-0.03125D0*EL2*ME2*YukS3Lep1*YukS3Lep2*(A0(ME2) + (ME2 - 1.D0*x)*B0(x, 0.D0, ME2)))/(MW2*PI2*SW2)

 amplitudes(12) = (-0.03125D0*EL2*MM2*YukS3Lep1*YukS3Lep2*(A0(MM2) + (MM2 - 1.D0*x)*B0(x, 0.D0, MM2)))/(MW2*PI2*SW2)

 amplitudes(13) = (-0.03125D0*EL2*ML2*YukS3Lep1*YukS3Lep2*(A0(ML2) + (ML2 - 1.D0*x)*B0(x, 0.D0, ML2)))/(MW2*PI2*SW2)

 amplitudes(14) = (-0.1875D0*CKM11*CKMC11*(((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW&
  &2))*A0(MD2) + ((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MU2) + ((-1.D0*EL2*&
  &MD2*MU2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MD2*((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark&
  &1*YukS3Quark2)/(MW2*SW2)) + (MU2 - 1.D0*x)*((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2&
  &*SW2)))*B0(x, MD2, MU2)))/(PI2*SB2)

 amplitudes(15) = (-0.1875D0*CKM21*CKMC21*(((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW&
  &2))*A0(MC2) + ((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MD2) + ((-1.D0*EL2*&
  &MC2*MD2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MC2*((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark&
  &1*YukS3Quark2)/(MW2*SW2)) + (MD2 - 1.D0*x)*((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2&
  &*SW2)))*B0(x, MC2, MD2)))/(PI2*SB2)

 amplitudes(16) = (-0.1875D0*CKM31*CKMC31*(((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW&
  &2))*A0(MD2) + ((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MT2) + ((-1.D0*EL2*&
  &MD2*MT2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MD2*((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark&
  &1*YukS3Quark2)/(MW2*SW2)) + (MT2 - 1.D0*x)*((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2&
  &*SW2)))*B0(x, MD2, MT2)))/(PI2*SB2)

 amplitudes(17) = (-0.1875D0*CKM12*CKMC12*(((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW&
  &2))*A0(MS2) + ((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MU2) + ((-1.D0*EL2*&
  &MS2*MU2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MS2*((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark&
  &1*YukS3Quark2)/(MW2*SW2)) + (MU2 - 1.D0*x)*((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2&
  &*SW2)))*B0(x, MS2, MU2)))/(PI2*SB2)

 amplitudes(18) = (-0.1875D0*CKM22*CKMC22*(((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW&
  &2))*A0(MC2) + ((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MS2) + ((-1.D0*EL2*&
  &MC2*MS2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MC2*((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark&
  &1*YukS3Quark2)/(MW2*SW2)) + (MS2 - 1.D0*x)*((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2&
  &*SW2)))*B0(x, MC2, MS2)))/(PI2*SB2)

 amplitudes(19) = (-0.1875D0*CKM32*CKMC32*(((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW&
  &2))*A0(MS2) + ((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MT2) + ((-1.D0*EL2*&
  &MS2*MT2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MS2*((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark&
  &1*YukS3Quark2)/(MW2*SW2)) + (MT2 - 1.D0*x)*((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2&
  &*SW2)))*B0(x, MS2, MT2)))/(PI2*SB2)

 amplitudes(20) = (-0.1875D0*CKM13*CKMC13*(((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW&
  &2))*A0(MB2) + ((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MU2) + ((-1.D0*EL2*&
  &MB2*MU2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MB2*((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark&
  &1*YukS3Quark2)/(MW2*SW2)) + (MU2 - 1.D0*x)*((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2&
  &*SW2)))*B0(x, MB2, MU2)))/(PI2*SB2)

 amplitudes(21) = (-0.1875D0*CKM23*CKMC23*(((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW&
  &2))*A0(MB2) + ((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MC2) + ((-1.D0*EL2*&
  &MB2*MC2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MB2*((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark&
  &1*YukS3Quark2)/(MW2*SW2)) + (MC2 - 1.D0*x)*((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2&
  &*SW2)))*B0(x, MB2, MC2)))/(PI2*SB2)

 amplitudes(22) = (-0.1875D0*CKM33*CKMC33*(((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW&
  &2))*A0(MB2) + ((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MT2) + ((-1.D0*EL2*&
  &MB2*MT2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MB2*((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark&
  &1*YukS3Quark2)/(MW2*SW2)) + (MT2 - 1.D0*x)*((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2&
  &*SW2)))*B0(x, MB2, MT2)))/(PI2*SB2)

 amplitudes(23) = (0.0625D0*CS1S3S3f111*CS1S3S3f112*B0(x, MH12, MW2))/PI2

 amplitudes(24) = (0.0625D0*CS1S3S3f211*CS1S3S3f212*B0(x, MH22, MW2))/PI2

 amplitudes(25) = (0.0625D0*CS1S3S3f311*CS1S3S3f312*B0(x, MH32, MW2))/PI2

 amplitudes(26) = (0.0625D0*CS1S3S3f112*CS1S3S3f122*B0(x, MH12, MHp2))/PI2

 amplitudes(27) = (0.0625D0*CS1S3S3f212*CS1S3S3f222*B0(x, MH22, MHp2))/PI2

 amplitudes(28) = (0.0625D0*CS1S3S3f312*CS1S3S3f322*B0(x, MH32, MHp2))/PI2

 amplitudes(29) = 0D0

 amplitudes(30) = 0D0

 amplitudes(31) = 0D0

 amplitudes(32) = 0D0

 amplitudes(33) = 0.D0

 amplitudes(34) = 0.D0

 amplitudes(35) = 0.D0

 amplitudes(36) = 0.D0

 amplitudes(37) = (-0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CA1*CA2*CB + CA2*SA1*SB)*(-1.D0*A0(MH12) + 2.D0*A0(MW2) + (-1.&
  &D0*MW2 + 2.D0*(MH12 + x))*B0(x, MH12, MW2)))/(PI2*SW2)

 amplitudes(38) = (-0.015625D0*EL2*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)* (CB*(-1.D0*CA3&
  &*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(-1.D0*A0(MH22) + 2.D0*A0(MW2) + (-1.D0*MW2 + 2.D0*(MH22 + x))*B0(&
  &x, MH22, MW2)))/(PI2*SW2)

 amplitudes(39) = (-0.015625D0*EL2*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)* (CB*(-1.D0*CA3*SA1&
  &*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)* (-1.D0*A0(MH32) + 2.D0*A0(MW2) + (-1.D0*MW2 + 2.D0*(MH32 + x))*&
  &B0(x, MH32, MW2)))/(PI2*SW2)

 amplitudes(40) = 0.D0

 amplitudes(41) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,41
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfGpHpUsual = totalAmplitude
end function SelfGpHpUsual

