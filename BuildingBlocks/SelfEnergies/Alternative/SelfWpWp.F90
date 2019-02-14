double complex function SelfWpWpAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(145)

 amplitudes(1) = (0.015625D0*EL2*(CA12*CA22 + CA22*SA12)*A0(MH12))/(PI2*SW2)

 amplitudes(2) = (0.015625D0*EL2*A0(MH22)*(DBLE((-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)**INT(2.D0)) + DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3&
  &)**INT(2.D0))))/(PI2*SW2)

 amplitudes(3) = (0.015625D0*EL2*A0(MH32)*(DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0)) + DBLE((-1.D0*CA1*CA3*SA2 + SA1*SA&
  &3)**INT(2.D0))))/(PI2*SW2)

 amplitudes(4) = (0.015625D0*EL2*(CB2 + SB2)*A0(MZ2))/(PI2*SW2)

 amplitudes(5) = (0.015625D0*EL2*(CB2 + SB2)*A0(MA02))/(PI2*SW2)

 amplitudes(6) = (0.03125D0*EL2*(CB2 + SB2)*A0(MW2))/(PI2*SW2)

 amplitudes(7) = (0.03125D0*EL2*(CB2 + SB2)*A0(MHp2))/(PI2*SW2)

 amplitudes(8) = 0.D0

 amplitudes(9) = (0.0625D0*CW2*EL2*(-2.D0*MZ2 + 3.D0*A0(MZ2)))/(PI2*SW2)

 amplitudes(10) = (0.0625D0*EL2*(-2.D0*MW2 + 3.D0*A0(MW2)))/(PI2*SW2)

 amplitudes(11) = (0.0625D0*ME2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*YukS1Lep1*A0(ME2)*DBLE(EL**INT(3.D0))*D&
  &BLE(SW**INT(-3.D0)))/(MH12*MW*PI2)

 amplitudes(12) = (0.0625D0*MM2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*YukS1Lep1*A0(MM2)*DBLE(EL**INT(3.D0))*D&
  &BLE(SW**INT(-3.D0)))/(MH12*MW*PI2)

 amplitudes(13) = (0.0625D0*ML2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*YukS1Lep1*A0(ML2)*DBLE(EL**INT(3.D0))*D&
  &BLE(SW**INT(-3.D0)))/(MH12*MW*PI2)

 amplitudes(14) = (0.0625D0*ME2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB&
  &*SW)/EL)*YukS1Lep2*A0(ME2)* DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH22*MW*PI2)

 amplitudes(15) = (0.0625D0*MM2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB&
  &*SW)/EL)*YukS1Lep2*A0(MM2)* DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH22*MW*PI2)

 amplitudes(16) = (0.0625D0*ML2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB&
  &*SW)/EL)*YukS1Lep2*A0(ML2)* DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH22*MW*PI2)

 amplitudes(17) = (0.0625D0*ME2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*S&
  &B*SW)/EL)*YukS1Lep3*A0(ME2)* DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH32*MW*PI2)

 amplitudes(18) = (0.0625D0*MM2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*S&
  &B*SW)/EL)*YukS1Lep3*A0(MM2)* DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH32*MW*PI2)

 amplitudes(19) = (0.0625D0*ML2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*S&
  &B*SW)/EL)*YukS1Lep3*A0(ML2)* DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH32*MW*PI2)

 amplitudes(20) = (0.1875D0*CA2*MU2*SA1*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MU2)*DBLE(EL**INT(3.D0))*DBL&
  &E(SW**INT(-3.D0)))/(MH12*MW*PI2*SB)

 amplitudes(21) = (0.1875D0*CA2*MC2*SA1*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MC2)*DBLE(EL**INT(3.D0))*DBL&
  &E(SW**INT(-3.D0)))/(MH12*MW*PI2*SB)

 amplitudes(22) = (0.1875D0*CA2*MT2*SA1*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MT2)*DBLE(EL**INT(3.D0))*DBL&
  &E(SW**INT(-3.D0)))/(MH12*MW*PI2*SB)

 amplitudes(23) = (0.1875D0*MU2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(C&
  &A1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)* A0(MU2)*DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH22*MW*PI2*SB)

 amplitudes(24) = (0.1875D0*MC2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(C&
  &A1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)* A0(MC2)*DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH22*MW*PI2*SB)

 amplitudes(25) = (0.1875D0*MT2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(C&
  &A1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)* A0(MT2)*DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH22*MW*PI2*SB)

 amplitudes(26) = (0.1875D0*MU2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(&
  &-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*A0(MU2)*DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH32*MW*PI2*SB)

 amplitudes(27) = (0.1875D0*MC2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(&
  &-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*A0(MC2)*DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH32*MW*PI2*SB)

 amplitudes(28) = (0.1875D0*MT2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(&
  &-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*A0(MT2)*DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH32*MW*PI2*SB)

 amplitudes(29) = (0.1875D0*MD2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*YukS1Quark1*A0(MD2)*DBLE(EL**INT(3.D0))&
  &*DBLE(SW**INT(-3.D0)))/(MH12*MW*PI2)

 amplitudes(30) = (0.1875D0*MS2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*YukS1Quark1*A0(MS2)*DBLE(EL**INT(3.D0))&
  &*DBLE(SW**INT(-3.D0)))/(MH12*MW*PI2)

 amplitudes(31) = (0.1875D0*MB2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*YukS1Quark1*A0(MB2)*DBLE(EL**INT(3.D0))&
  &*DBLE(SW**INT(-3.D0)))/(MH12*MW*PI2)

 amplitudes(32) = (0.1875D0*MD2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB&
  &*SW)/EL)*YukS1Quark2*A0(MD2)* DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH22*MW*PI2)

 amplitudes(33) = (0.1875D0*MS2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB&
  &*SW)/EL)*YukS1Quark2*A0(MS2)* DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH22*MW*PI2)

 amplitudes(34) = (0.1875D0*MB2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB&
  &*SW)/EL)*YukS1Quark2*A0(MB2)* DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH22*MW*PI2)

 amplitudes(35) = (0.1875D0*MD2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*S&
  &B*SW)/EL)*YukS1Quark3*A0(MD2)* DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH32*MW*PI2)

 amplitudes(36) = (0.1875D0*MS2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*S&
  &B*SW)/EL)*YukS1Quark3*A0(MS2)* DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH32*MW*PI2)

 amplitudes(37) = (0.1875D0*MB2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*S&
  &B*SW)/EL)*YukS1Quark3*A0(MB2)* DBLE(EL**INT(3.D0))*DBLE(SW**INT(-3.D0)))/(MH32*MW*PI2)

 amplitudes(38) = (0.015625D0*CS1S1S1f111*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MH12))/(MH12*PI2*SW2)

 amplitudes(39) = (0.015625D0*CS1S1S1f122*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MH22))/(MH12*PI2*SW2)

 amplitudes(40) = (0.015625D0*CS1S1S1f133*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MH32))/(MH12*PI2*SW2)

 amplitudes(41) = (0.015625D0*CS1S1S1f211*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*SB*SW)/EL)*A0(MH12))/ (MH22*PI2*SW2)

 amplitudes(42) = (0.015625D0*CS1S1S1f222*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*SB*SW)/EL)*A0(MH22))/ (MH22*PI2*SW2)

 amplitudes(43) = (0.015625D0*CS1S1S1f233*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*SB*SW)/EL)*A0(MH32))/ (MH22*PI2*SW2)

 amplitudes(44) = (0.015625D0*CS1S1S1f311*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1&
  &.D0*CA1*SA3)*SB*SW)/EL)*A0(MH12))/ (MH32*PI2*SW2)

 amplitudes(45) = (0.015625D0*CS1S1S1f322*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1&
  &.D0*CA1*SA3)*SB*SW)/EL)*A0(MH22))/ (MH32*PI2*SW2)

 amplitudes(46) = (0.015625D0*CS1S1S1f333*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1&
  &.D0*CA1*SA3)*SB*SW)/EL)*A0(MH32))/ (MH32*PI2*SW2)

 amplitudes(47) = (0.015625D0*CS2S2S1f111*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MZ2))/(MH12*PI2*SW2)

 amplitudes(48) = (0.015625D0*CS2S2S1f221*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MA02))/(MH12*PI2*SW2)

 amplitudes(49) = (0.015625D0*CS2S2S1f112*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*SB*SW)/EL)*A0(MZ2))/ (MH22*PI2*SW2)

 amplitudes(50) = (0.015625D0*CS2S2S1f222*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*SB*SW)/EL)*A0(MA02))/ (MH22*PI2*SW2)

 amplitudes(51) = (0.015625D0*CS2S2S1f113*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1&
  &.D0*CA1*SA3)*SB*SW)/EL)*A0(MZ2))/ (MH32*PI2*SW2)

 amplitudes(52) = (0.015625D0*CS2S2S1f223*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1&
  &.D0*CA1*SA3)*SB*SW)/EL)*A0(MA02))/ (MH32*PI2*SW2)

 amplitudes(53) = (0.03125D0*CS1S3S3f111*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MW2))/(MH12*PI2*SW2)

 amplitudes(54) = (0.03125D0*CS1S3S3f122*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MHp2))/(MH12*PI2*SW2)

 amplitudes(55) = (0.03125D0*CS1S3S3f211*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA&
  &1*SA2*SA3)*SB*SW)/EL)*A0(MW2))/ (MH22*PI2*SW2)

 amplitudes(56) = (0.03125D0*CS1S3S3f222*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA&
  &1*SA2*SA3)*SB*SW)/EL)*A0(MHp2))/ (MH22*PI2*SW2)

 amplitudes(57) = (0.03125D0*CS1S3S3f311*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.&
  &D0*CA1*SA3)*SB*SW)/EL)*A0(MW2))/ (MH32*PI2*SW2)

 amplitudes(58) = (0.03125D0*CS1S3S3f322*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.&
  &D0*CA1*SA3)*SB*SW)/EL)*A0(MHp2))/ (MH32*PI2*SW2)

 amplitudes(59) = (0.0078125D0*A0(MZ2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*S&
  &A1*SB*SW)/EL)**INT(2.D0))* DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*PI2)

 amplitudes(60) = (0.0078125D0*A0(MZ2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*&
  &SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)**INT(2.D0))* DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2)

 amplitudes(61) = (0.0078125D0*A0(MZ2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*&
  &SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)**INT(2.D0))* DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2)

 amplitudes(62) = (0.0078125D0*A0(MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*S&
  &A1*SB*SW)/EL)**INT(2.D0)))/(MH12*PI2)

 amplitudes(63) = (0.0078125D0*A0(MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*&
  &SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)**INT(2.D0)))/(MH22*PI2)

 amplitudes(64) = (0.0078125D0*A0(MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*&
  &SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)**INT(2.D0)))/(MH32*PI2)

 amplitudes(65) = (0.0078125D0*A0(MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*S&
  &A1*SB*SW)/EL)**INT(2.D0)))/(MH12*PI2)

 amplitudes(66) = (0.0078125D0*A0(MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*&
  &SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)**INT(2.D0)))/(MH22*PI2)

 amplitudes(67) = (0.0078125D0*A0(MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*&
  &SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)**INT(2.D0)))/(MH32*PI2)

 amplitudes(68) = (0.015625D0*(MZ2 - 2.D0*A0(MZ2))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CA1*CA2*CB*MW*SW)/EL + (2&
  &.D0*CA2*MW*SA1*SB*SW)/EL)**INT(2.D0))* DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*PI2)

 amplitudes(69) = (0.015625D0*(MZ2 - 2.D0*A0(MZ2))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1&
  &.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)**INT(2.D0))* DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH2&
  &2*PI2)

 amplitudes(70) = (0.015625D0*(MZ2 - 2.D0*A0(MZ2))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2&
  & + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)**INT(2.D0))* DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH&
  &32*PI2)

 amplitudes(71) = (0.03125D0*(MW2 - 2.D0*A0(MW2))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.&
  &D0*CA2*MW*SA1*SB*SW)/EL)**INT(2.D0)))/ (MH12*PI2)

 amplitudes(72) = (0.03125D0*(MW2 - 2.D0*A0(MW2))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.&
  &D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)**INT(2.D0)))/(MH22*PI2)

 amplitudes(73) = (0.03125D0*(MW2 - 2.D0*A0(MW2))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 &
  &+ SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)**INT(2.D0)))/(MH32*PI2)

 amplitudes(74) = 0.D0

 amplitudes(75) = 0.D0

 amplitudes(76) = 0.D0

 amplitudes(77) = 0.D0

 amplitudes(78) = 0.D0

 amplitudes(79) = 0.D0

 amplitudes(80) = 0.D0

 amplitudes(81) = 0.D0

 amplitudes(82) = 0.D0

 amplitudes(83) = 0.D0

 amplitudes(84) = 0.D0

 amplitudes(85) = 0.D0

 amplitudes(86) = 0.D0

 amplitudes(87) = 0.D0

 amplitudes(88) = 0.D0

 amplitudes(89) = 0.D0

 amplitudes(90) = 0.D0

 amplitudes(91) = 0.D0

 amplitudes(92) = 0.D0

 amplitudes(93) = 0.D0

 amplitudes(94) = 0.D0

 amplitudes(95) = 0.D0

 amplitudes(96) = 0.D0

 amplitudes(97) = 0.D0

 amplitudes(98) = 0.D0

 amplitudes(99) = 0.D0

 amplitudes(100) = 0.D0

 amplitudes(101) = 0.D0

 amplitudes(102) = 0.D0

 amplitudes(103) = 0.D0

 amplitudes(104) = 0.D0

 amplitudes(105) = (-0.003472222222222222D0*EL2*(-6.D0*ME2*x + (-3.D0*ME2 + 6.D0*x)*A0(ME2) + 3.D0*ME2*x*B0(x, 0.D0, ME2) + 3.D0*&
  &B0(x, 0.D0, ME2)*DBLE(ME**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x, 0.D0, ME2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(106) = 0.D0

 amplitudes(107) = 0.D0

 amplitudes(108) = 0.D0

 amplitudes(109) = (-0.003472222222222222D0*EL2*(-6.D0*MM2*x + (-3.D0*MM2 + 6.D0*x)*A0(MM2) + 3.D0*MM2*x*B0(x, 0.D0, MM2) + 3.D0*&
  &B0(x, 0.D0, MM2)*DBLE(MM**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x, 0.D0, MM2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(110) = 0.D0

 amplitudes(111) = 0.D0

 amplitudes(112) = 0.D0

 amplitudes(113) = (-0.003472222222222222D0*EL2*(-6.D0*ML2*x + (-3.D0*ML2 + 6.D0*x)*A0(ML2) + 3.D0*ML2*x*B0(x, 0.D0, ML2) + 3.D0*&
  &B0(x, 0.D0, ML2)*DBLE(ML**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x, 0.D0, ML2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(114) = (-0.010416666666666666D0*CKM11*CKMC11*EL2*(-6.D0*MD2*x - 6.D0*MU2*x + (-3.D0*MD2 + 3.D0*MU2 + 6.D0*x)*A0(MD2) &
  &+ 3.D0*(MD2 - 1.D0*MU2 + 2.D0*x)*A0(MU2) - 6.D0*MD2*MU2*B0(x, MD2, MU2) + 3.D0*MD2*x*B0(x, MD2, MU2) + 3.D0*MU2*x*B0(x, MD2, M&
  &U2) + 3.D0*B0(x, MD2, MU2)*DBLE(MD**INT(4.D0)) + 3.D0*B0(x, MD2, MU2)*DBLE(MU**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(&
  &x, MD2, MU2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(115) = (-0.010416666666666666D0*CKM21*CKMC21*EL2*(-6.D0*MC2*x - 6.D0*MD2*x + (-3.D0*MC2 + 3.D0*MD2 + 6.D0*x)*A0(MC2) &
  &+ 3.D0*(MC2 - 1.D0*MD2 + 2.D0*x)*A0(MD2) - 6.D0*MC2*MD2*B0(x, MC2, MD2) + 3.D0*MC2*x*B0(x, MC2, MD2) + 3.D0*MD2*x*B0(x, MC2, M&
  &D2) + 3.D0*B0(x, MC2, MD2)*DBLE(MC**INT(4.D0)) + 3.D0*B0(x, MC2, MD2)*DBLE(MD**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(&
  &x, MC2, MD2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(116) = (-0.010416666666666666D0*CKM31*CKMC31*EL2*(-6.D0*MD2*x - 6.D0*MT2*x + (-3.D0*MD2 + 3.D0*MT2 + 6.D0*x)*A0(MD2) &
  &+ 3.D0*(MD2 - 1.D0*MT2 + 2.D0*x)*A0(MT2) - 6.D0*MD2*MT2*B0(x, MD2, MT2) + 3.D0*MD2*x*B0(x, MD2, MT2) + 3.D0*MT2*x*B0(x, MD2, M&
  &T2) + 3.D0*B0(x, MD2, MT2)*DBLE(MD**INT(4.D0)) + 3.D0*B0(x, MD2, MT2)*DBLE(MT**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(&
  &x, MD2, MT2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(117) = (-0.010416666666666666D0*CKM12*CKMC12*EL2*(-6.D0*MS2*x - 6.D0*MU2*x + (-3.D0*MS2 + 3.D0*MU2 + 6.D0*x)*A0(MS2) &
  &+ 3.D0*(MS2 - 1.D0*MU2 + 2.D0*x)*A0(MU2) - 6.D0*MS2*MU2*B0(x, MS2, MU2) + 3.D0*MS2*x*B0(x, MS2, MU2) + 3.D0*MU2*x*B0(x, MS2, M&
  &U2) + 3.D0*B0(x, MS2, MU2)*DBLE(MS**INT(4.D0)) + 3.D0*B0(x, MS2, MU2)*DBLE(MU**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(&
  &x, MS2, MU2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(118) = (-0.010416666666666666D0*CKM22*CKMC22*EL2*(-6.D0*MC2*x - 6.D0*MS2*x + (-3.D0*MC2 + 3.D0*MS2 + 6.D0*x)*A0(MC2) &
  &+ 3.D0*(MC2 - 1.D0*MS2 + 2.D0*x)*A0(MS2) - 6.D0*MC2*MS2*B0(x, MC2, MS2) + 3.D0*MC2*x*B0(x, MC2, MS2) + 3.D0*MS2*x*B0(x, MC2, M&
  &S2) + 3.D0*B0(x, MC2, MS2)*DBLE(MC**INT(4.D0)) + 3.D0*B0(x, MC2, MS2)*DBLE(MS**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(&
  &x, MC2, MS2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(119) = (-0.010416666666666666D0*CKM32*CKMC32*EL2*(-6.D0*MS2*x - 6.D0*MT2*x + (-3.D0*MS2 + 3.D0*MT2 + 6.D0*x)*A0(MS2) &
  &+ 3.D0*(MS2 - 1.D0*MT2 + 2.D0*x)*A0(MT2) - 6.D0*MS2*MT2*B0(x, MS2, MT2) + 3.D0*MS2*x*B0(x, MS2, MT2) + 3.D0*MT2*x*B0(x, MS2, M&
  &T2) + 3.D0*B0(x, MS2, MT2)*DBLE(MS**INT(4.D0)) + 3.D0*B0(x, MS2, MT2)*DBLE(MT**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(&
  &x, MS2, MT2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(120) = (-0.010416666666666666D0*CKM13*CKMC13*EL2*(-6.D0*MB2*x - 6.D0*MU2*x + (-3.D0*MB2 + 3.D0*MU2 + 6.D0*x)*A0(MB2) &
  &+ 3.D0*(MB2 - 1.D0*MU2 + 2.D0*x)*A0(MU2) - 6.D0*MB2*MU2*B0(x, MB2, MU2) + 3.D0*MB2*x*B0(x, MB2, MU2) + 3.D0*MU2*x*B0(x, MB2, M&
  &U2) + 3.D0*B0(x, MB2, MU2)*DBLE(MB**INT(4.D0)) + 3.D0*B0(x, MB2, MU2)*DBLE(MU**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(&
  &x, MB2, MU2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(121) = (-0.010416666666666666D0*CKM23*CKMC23*EL2*(-6.D0*MB2*x - 6.D0*MC2*x + (-3.D0*MB2 + 3.D0*MC2 + 6.D0*x)*A0(MB2) &
  &+ 3.D0*(MB2 - 1.D0*MC2 + 2.D0*x)*A0(MC2) - 6.D0*MB2*MC2*B0(x, MB2, MC2) + 3.D0*MB2*x*B0(x, MB2, MC2) + 3.D0*MC2*x*B0(x, MB2, M&
  &C2) + 3.D0*B0(x, MB2, MC2)*DBLE(MB**INT(4.D0)) + 3.D0*B0(x, MB2, MC2)*DBLE(MC**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(&
  &x, MB2, MC2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(122) = (-0.010416666666666666D0*CKM33*CKMC33*EL2*(-6.D0*MB2*x - 6.D0*MT2*x + (-3.D0*MB2 + 3.D0*MT2 + 6.D0*x)*A0(MB2) &
  &+ 3.D0*(MB2 - 1.D0*MT2 + 2.D0*x)*A0(MT2) - 6.D0*MB2*MT2*B0(x, MB2, MT2) + 3.D0*MB2*x*B0(x, MB2, MT2) + 3.D0*MT2*x*B0(x, MB2, M&
  &T2) + 3.D0*B0(x, MB2, MT2)*DBLE(MB**INT(4.D0)) + 3.D0*B0(x, MB2, MT2)*DBLE(MT**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(&
  &x, MB2, MT2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(123) = (0.001736111111111111D0*EL2*DBLE((CA1*CA2*CB + CA2*SA1*SB)**INT(2.D0))*(-6.D0*MH12*x - 6.D0*MW2*x + 3.D0*(-1.D&
  &0*MH12 + MW2 - 1.D0*x)*A0(MH12) - 3.D0*(-1.D0*MH12 + MW2 + x)*A0(MW2) - 6.D0*MH12*MW2*B0(x, MH12, MW2) - 6.D0*MH12*x*B0(x, MH1&
  &2, MW2) - 6.D0*MW2*x*B0(x, MH12, MW2) + 3.D0*B0(x, MH12, MW2)*DBLE(MH1**INT(4.D0)) + 3.D0*B0(x, MH12, MW2)*DBLE(MW**INT(4.D0))&
  & + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH12, MW2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(124) = (0.001736111111111111D0*EL2*DBLE((CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)**IN&
  &T(2.D0))* (-6.D0*MH22*x - 6.D0*MW2*x + 3.D0*(-1.D0*MH22 + MW2 - 1.D0*x)*A0(MH22) - 3.D0*(-1.D0*MH22 + MW2 + x)*A0(MW2) - 6.D0*&
  &MH22*MW2*B0(x, MH22, MW2) - 6.D0*MH22*x*B0(x, MH22, MW2) - 6.D0*MW2*x*B0(x, MH22, MW2) + 3.D0*B0(x, MH22, MW2)*DBLE(MH2**INT(4&
  &.D0)) + 3.D0*B0(x, MH22, MW2)*DBLE(MW**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH22, MW2)*DBLE(x**INT(2.D0))))/(PI2*&
  &SW2*x)

 amplitudes(125) = (0.001736111111111111D0*EL2*DBLE((CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)**I&
  &NT(2.D0))* (-6.D0*MH32*x - 6.D0*MW2*x + 3.D0*(-1.D0*MH32 + MW2 - 1.D0*x)*A0(MH32) - 3.D0*(-1.D0*MH32 + MW2 + x)*A0(MW2) - 6.D0&
  &*MH32*MW2*B0(x, MH32, MW2) - 6.D0*MH32*x*B0(x, MH32, MW2) - 6.D0*MW2*x*B0(x, MH32, MW2) + 3.D0*B0(x, MH32, MW2)*DBLE(MH3**INT(&
  &4.D0)) + 3.D0*B0(x, MH32, MW2)*DBLE(MW**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH32, MW2)*DBLE(x**INT(2.D0))))/(PI2&
  &*SW2*x)

 amplitudes(126) = (0.001736111111111111D0*EL2*DBLE((CA2*CB*SA1 - 1.D0*CA1*CA2*SB)**INT(2.D0))*(-6.D0*MH12*x - 6.D0*MHp2*x - 3.D0&
  &*(MH12 - 1.D0*MHp2 + x)*A0(MH12) - 3.D0*(-1.D0*MH12 + MHp2 + x)*A0(MHp2) - 6.D0*MH12*MHp2*B0(x, MH12, MHp2) - 6.D0*MH12*x*B0(x&
  &, MH12, MHp2) - 6.D0*MHp2*x*B0(x, MH12, MHp2) + 3.D0*B0(x, MH12, MHp2)*DBLE(MH1**INT(4.D0)) + 3.D0*B0(x, MH12, MHp2)*DBLE(MHp*&
  &*INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH12, MHp2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(127) = (0.001736111111111111D0*EL2*DBLE((CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB&
  &)**INT(2.D0))* (-6.D0*MH22*x - 6.D0*MHp2*x - 3.D0*(MH22 - 1.D0*MHp2 + x)*A0(MH22) - 3.D0*(-1.D0*MH22 + MHp2 + x)*A0(MHp2) - 6.&
  &D0*MH22*MHp2*B0(x, MH22, MHp2) - 6.D0*MH22*x*B0(x, MH22, MHp2) - 6.D0*MHp2*x*B0(x, MH22, MHp2) + 3.D0*B0(x, MH22, MHp2)*DBLE(M&
  &H2**INT(4.D0)) + 3.D0*B0(x, MH22, MHp2)*DBLE(MHp**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH22, MHp2)*DBLE(x**INT(2.&
  &D0))))/(PI2*SW2*x)

 amplitudes(128) = (0.001736111111111111D0*EL2*DBLE((CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*S&
  &B)**INT(2.D0))* (-6.D0*MH32*x - 6.D0*MHp2*x - 3.D0*(MH32 - 1.D0*MHp2 + x)*A0(MH32) - 3.D0*(-1.D0*MH32 + MHp2 + x)*A0(MHp2) - 6&
  &.D0*MH32*MHp2*B0(x, MH32, MHp2) - 6.D0*MH32*x*B0(x, MH32, MHp2) - 6.D0*MHp2*x*B0(x, MH32, MHp2) + 3.D0*B0(x, MH32, MHp2)*DBLE(&
  &MH3**INT(4.D0)) + 3.D0*B0(x, MH32, MHp2)*DBLE(MHp**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH32, MHp2)*DBLE(x**INT(2&
  &.D0))))/(PI2*SW2*x)

 amplitudes(129) = (0.001736111111111111D0*EL2*DBLE((CB2 + SB2)**INT(2.D0))*(-6.D0*MW2*x - 6.D0*MZ2*x - 3.D0*(MW2 - 1.D0*MZ2 + x)&
  &*A0(MW2) + 3.D0*(MW2 - 1.D0*MZ2 - 1.D0*x)*A0(MZ2) - 6.D0*MW2*MZ2*B0(x, MW2, MZ2) - 6.D0*MW2*x*B0(x, MW2, MZ2) - 6.D0*MZ2*x*B0(&
  &x, MW2, MZ2) + 3.D0*B0(x, MW2, MZ2)*DBLE(MW**INT(4.D0)) + 3.D0*B0(x, MW2, MZ2)*DBLE(MZ**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) +&
  & 3.D0*B0(x, MW2, MZ2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(130) = 0.D0

 amplitudes(131) = 0.D0

 amplitudes(132) = (0.001736111111111111D0*EL2*DBLE((CB2 + SB2)**INT(2.D0))*(-6.D0*MA02*x - 6.D0*MHp2*x - 3.D0*(MA02 - 1.D0*MHp2 &
  &+ x)*A0(MA02) - 3.D0*(-1.D0*MA02 + MHp2 + x)*A0(MHp2) - 6.D0*MA02*MHp2*B0(x, MA02, MHp2) - 6.D0*MA02*x*B0(x, MA02, MHp2) - 6.D&
  &0*MHp2*x*B0(x, MA02, MHp2) + 3.D0*B0(x, MA02, MHp2)*DBLE(MA0**INT(4.D0)) + 3.D0*B0(x, MA02, MHp2)*DBLE(MHp**INT(4.D0)) + 2.D0*&
  &DBLE(x**INT(2.D0)) + 3.D0*B0(x, MA02, MHp2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(133) = (-0.001736111111111111D0*EL2*(2.D0*x*(-3.D0*MW2 + x) - 3.D0*(MW2 + x)*A0(MW2) + 3.D0*B0(x, 0.D0, MW2)*DBLE((MW&
  &2 - 1.D0*x)**INT(2.D0))))/(PI2*x)

 amplitudes(134) = (0.001736111111111111D0*CW2*EL2*(6.D0*MW2*x + 6.D0*MZ2*x + 3.D0*(MW2 - 1.D0*MZ2 + x)*A0(MW2) + 3.D0*(-1.D0*MW2&
  & + MZ2 + x)*A0(MZ2) + 6.D0*MW2*MZ2*B0(x, MW2, MZ2) + 6.D0*MW2*x*B0(x, MW2, MZ2) + 6.D0*MZ2*x*B0(x, MW2, MZ2) - 3.D0*B0(x, MW2,&
  & MZ2)*DBLE(MW**INT(4.D0)) - 3.D0*B0(x, MW2, MZ2)*DBLE(MZ**INT(4.D0)) - 2.D0*DBLE(x**INT(2.D0)) - 3.D0*B0(x, MW2, MZ2)*DBLE(x**&
  &INT(2.D0))))/(PI2*SW2*x)

 amplitudes(135) = (-0.001736111111111111D0*EL2*(2.D0*x*(-3.D0*MW2 + x) - 3.D0*(MW2 + x)*A0(MW2) + 3.D0*B0(x, 0.D0, MW2)*DBLE((MW&
  &2 - 1.D0*x)**INT(2.D0))))/(PI2*x)

 amplitudes(136) = (0.001736111111111111D0*CW2*EL2*(6.D0*MW2*x + 6.D0*MZ2*x + 3.D0*(MW2 - 1.D0*MZ2 + x)*A0(MW2) + 3.D0*(-1.D0*MW2&
  & + MZ2 + x)*A0(MZ2) + 6.D0*MW2*MZ2*B0(x, MW2, MZ2) + 6.D0*MW2*x*B0(x, MW2, MZ2) + 6.D0*MZ2*x*B0(x, MW2, MZ2) - 3.D0*B0(x, MW2,&
  & MZ2)*DBLE(MW**INT(4.D0)) - 3.D0*B0(x, MW2, MZ2)*DBLE(MZ**INT(4.D0)) - 2.D0*DBLE(x**INT(2.D0)) - 3.D0*B0(x, MW2, MZ2)*DBLE(x**&
  &INT(2.D0))))/(PI2*SW2*x)

 amplitudes(137) = (-0.003472222222222222D0*EL2*(2.D0*x*(-3.D0*MW2 + x) + 3.D0*(5.D0*MW2 + 11.D0*x)*A0(MW2) + B0(x, 0.D0, MW2)*(4&
  &8.D0*MW2*x - 15.D0*DBLE(MW**INT(4.D0)) + 57.D0*DBLE(x**INT(2.D0)))))/(PI2*x)

 amplitudes(138) = (-0.003472222222222222D0*CW2*EL2*(-6.D0*MW2*x - 6.D0*MZ2*x + 3.D0*(5.D0*MW2 - 5.D0*MZ2 + 11.D0*x)*A0(MW2) + (-&
  &15.D0*MW2 + 15.D0*MZ2 + 33.D0*x)*A0(MZ2) + 30.D0*MW2*MZ2*B0(x, MW2, MZ2) + 48.D0*MW2*x*B0(x, MW2, MZ2) + 48.D0*MZ2*x*B0(x, MW2&
  &, MZ2) - 15.D0*B0(x, MW2, MZ2)*DBLE(MW**INT(4.D0)) - 15.D0*B0(x, MW2, MZ2)*DBLE(MZ**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 57.&
  &D0*B0(x, MW2, MZ2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(139) = (0.015625D0*B0(x, 0.D0, MW2)*DBLE(EL**INT(4.D0))*DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB2*SW)/EL)**INT(2.D0)))&
  &/(PI2*SW2)

 amplitudes(140) = 0.D0

 amplitudes(141) = (0.015625D0*B0(x, MW2, MZ2)*DBLE(EL**INT(4.D0))*DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB2*SW)/EL)**INT(2.D0)))/&
  &(CW2*PI2)

 amplitudes(142) = 0.D0

 amplitudes(143) = (0.015625D0*B0(x, MH12, MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0&
  &*CA2*MW*SA1*SB*SW)/EL)**INT(2.D0)))/PI2

 amplitudes(144) = (0.015625D0*B0(x, MH22, MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0&
  &*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)**INT(2.D0)))/PI2

 amplitudes(145) = (0.015625D0*B0(x, MH32, MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + &
  &SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)**INT(2.D0)))/PI2

  totalAmplitude = (0D0,0D0)
 do j=1,145
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfWpWpAlter = totalAmplitude
end function SelfWpWpAlter

