double complex function SelfGpHpAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(130)

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

 amplitudes(11) = (-0.125D0*CS1S3S3f112*EL*ME2*YukS1Lep1*A0(ME2))/(MH12*MW*PI2*SW)

 amplitudes(12) = (-0.125D0*CS1S3S3f112*EL*MM2*YukS1Lep1*A0(MM2))/(MH12*MW*PI2*SW)

 amplitudes(13) = (-0.125D0*CS1S3S3f112*EL*ML2*YukS1Lep1*A0(ML2))/(MH12*MW*PI2*SW)

 amplitudes(14) = (-0.125D0*CS1S3S3f212*EL*ME2*YukS1Lep2*A0(ME2))/(MH22*MW*PI2*SW)

 amplitudes(15) = (-0.125D0*CS1S3S3f212*EL*MM2*YukS1Lep2*A0(MM2))/(MH22*MW*PI2*SW)

 amplitudes(16) = (-0.125D0*CS1S3S3f212*EL*ML2*YukS1Lep2*A0(ML2))/(MH22*MW*PI2*SW)

 amplitudes(17) = (-0.125D0*CS1S3S3f312*EL*ME2*YukS1Lep3*A0(ME2))/(MH32*MW*PI2*SW)

 amplitudes(18) = (-0.125D0*CS1S3S3f312*EL*MM2*YukS1Lep3*A0(MM2))/(MH32*MW*PI2*SW)

 amplitudes(19) = (-0.125D0*CS1S3S3f312*EL*ML2*YukS1Lep3*A0(ML2))/(MH32*MW*PI2*SW)

 amplitudes(20) = (-0.375D0*CA2*CS1S3S3f112*EL*MU2*SA1*A0(MU2))/(MH12*MW*PI2*SB*SW)

 amplitudes(21) = (-0.375D0*CA2*CS1S3S3f112*EL*MC2*SA1*A0(MC2))/(MH12*MW*PI2*SB*SW)

 amplitudes(22) = (-0.375D0*CA2*CS1S3S3f112*EL*MT2*SA1*A0(MT2))/(MH12*MW*PI2*SB*SW)

 amplitudes(23) = (-0.375D0*CS1S3S3f212*EL*MU2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*A0(MU2))/(MH22*MW*PI2*SB*SW)

 amplitudes(24) = (-0.375D0*CS1S3S3f212*EL*MC2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*A0(MC2))/(MH22*MW*PI2*SB*SW)

 amplitudes(25) = (-0.375D0*CS1S3S3f212*EL*MT2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*A0(MT2))/(MH22*MW*PI2*SB*SW)

 amplitudes(26) = (-0.375D0*CS1S3S3f312*EL*MU2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*A0(MU2))/(MH32*MW*PI2*SB*SW)

 amplitudes(27) = (-0.375D0*CS1S3S3f312*EL*MC2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*A0(MC2))/(MH32*MW*PI2*SB*SW)

 amplitudes(28) = (-0.375D0*CS1S3S3f312*EL*MT2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*A0(MT2))/(MH32*MW*PI2*SB*SW)

 amplitudes(29) = (-0.375D0*CS1S3S3f112*EL*MD2*YukS1Quark1*A0(MD2))/(MH12*MW*PI2*SW)

 amplitudes(30) = (-0.375D0*CS1S3S3f112*EL*MS2*YukS1Quark1*A0(MS2))/(MH12*MW*PI2*SW)

 amplitudes(31) = (-0.375D0*CS1S3S3f112*EL*MB2*YukS1Quark1*A0(MB2))/(MH12*MW*PI2*SW)

 amplitudes(32) = (-0.375D0*CS1S3S3f212*EL*MD2*YukS1Quark2*A0(MD2))/(MH22*MW*PI2*SW)

 amplitudes(33) = (-0.375D0*CS1S3S3f212*EL*MS2*YukS1Quark2*A0(MS2))/(MH22*MW*PI2*SW)

 amplitudes(34) = (-0.375D0*CS1S3S3f212*EL*MB2*YukS1Quark2*A0(MB2))/(MH22*MW*PI2*SW)

 amplitudes(35) = (-0.375D0*CS1S3S3f312*EL*MD2*YukS1Quark3*A0(MD2))/(MH32*MW*PI2*SW)

 amplitudes(36) = (-0.375D0*CS1S3S3f312*EL*MS2*YukS1Quark3*A0(MS2))/(MH32*MW*PI2*SW)

 amplitudes(37) = (-0.375D0*CS1S3S3f312*EL*MB2*YukS1Quark3*A0(MB2))/(MH32*MW*PI2*SW)

 amplitudes(38) = 0D0

 amplitudes(39) = 0D0

 amplitudes(40) = 0D0

 amplitudes(41) = 0D0

 amplitudes(42) = 0D0

 amplitudes(43) = 0D0

 amplitudes(44) = 0D0

 amplitudes(45) = 0D0

 amplitudes(46) = 0D0

 amplitudes(47) = 0D0

 amplitudes(48) = 0D0

 amplitudes(49) = 0D0

 amplitudes(50) = 0D0

 amplitudes(51) = 0D0

 amplitudes(52) = 0D0

 amplitudes(53) = 0D0

 amplitudes(54) = 0D0

 amplitudes(55) = 0D0

 amplitudes(56) = (-0.03125D0*CS1S1S1f111*CS1S3S3f112*A0(MH12))/(MH12*PI2)

 amplitudes(57) = (-0.03125D0*CS1S1S1f122*CS1S3S3f112*A0(MH22))/(MH12*PI2)

 amplitudes(58) = (-0.03125D0*CS1S1S1f133*CS1S3S3f112*A0(MH32))/(MH12*PI2)

 amplitudes(59) = (-0.03125D0*CS1S1S1f211*CS1S3S3f212*A0(MH12))/(MH22*PI2)

 amplitudes(60) = (-0.03125D0*CS1S1S1f222*CS1S3S3f212*A0(MH22))/(MH22*PI2)

 amplitudes(61) = (-0.03125D0*CS1S1S1f233*CS1S3S3f212*A0(MH32))/(MH22*PI2)

 amplitudes(62) = (-0.03125D0*CS1S1S1f311*CS1S3S3f312*A0(MH12))/(MH32*PI2)

 amplitudes(63) = (-0.03125D0*CS1S1S1f322*CS1S3S3f312*A0(MH22))/(MH32*PI2)

 amplitudes(64) = (-0.03125D0*CS1S1S1f333*CS1S3S3f312*A0(MH32))/(MH32*PI2)

 amplitudes(65) = (-0.03125D0*CS1S3S3f112*CS2S2S1f111*A0(MZ2))/(MH12*PI2)

 amplitudes(66) = (-0.03125D0*CS1S3S3f112*CS2S2S1f221*A0(MA02))/(MH12*PI2)

 amplitudes(67) = (-0.03125D0*CS1S3S3f212*CS2S2S1f112*A0(MZ2))/(MH22*PI2)

 amplitudes(68) = (-0.03125D0*CS1S3S3f212*CS2S2S1f222*A0(MA02))/(MH22*PI2)

 amplitudes(69) = (-0.03125D0*CS1S3S3f312*CS2S2S1f113*A0(MZ2))/(MH32*PI2)

 amplitudes(70) = (-0.03125D0*CS1S3S3f312*CS2S2S1f223*A0(MA02))/(MH32*PI2)

 amplitudes(71) = (-0.0625D0*CS1S3S3f111*CS1S3S3f112*A0(MW2))/(MH12*PI2)

 amplitudes(72) = (-0.0625D0*CS1S3S3f112*CS1S3S3f122*A0(MHp2))/(MH12*PI2)

 amplitudes(73) = (-0.0625D0*CS1S3S3f211*CS1S3S3f212*A0(MW2))/(MH22*PI2)

 amplitudes(74) = (-0.0625D0*CS1S3S3f212*CS1S3S3f222*A0(MHp2))/(MH22*PI2)

 amplitudes(75) = (-0.0625D0*CS1S3S3f311*CS1S3S3f312*A0(MW2))/(MH32*PI2)

 amplitudes(76) = (-0.0625D0*CS1S3S3f312*CS1S3S3f322*A0(MHp2))/(MH32*PI2)

 amplitudes(77) = 0D0

 amplitudes(78) = 0D0

 amplitudes(79) = 0D0

 amplitudes(80) = 0D0

 amplitudes(81) = (-0.015625D0*CS1S3S3f112*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MZ2)*DBLE((CW2 + SW2)&
  &**INT(2.D0)))/(CW2*MH12*PI2*SW2)

 amplitudes(82) = (-0.015625D0*CS1S3S3f212*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*&
  &SA1*SA2*SA3)*SB*SW)/EL)*A0(MZ2)* DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2*SW2)

 amplitudes(83) = (-0.015625D0*CS1S3S3f312*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - &
  &1.D0*CA1*SA3)*SB*SW)/EL)*A0(MZ2)* DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2)

 amplitudes(84) = (-0.015625D0*CS1S3S3f112*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MW2))/(MH12*PI2*SW2)

 amplitudes(85) = (-0.015625D0*CS1S3S3f212*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*&
  &SA1*SA2*SA3)*SB*SW)/EL)*A0(MW2))/ (MH22*PI2*SW2)

 amplitudes(86) = (-0.015625D0*CS1S3S3f312*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - &
  &1.D0*CA1*SA3)*SB*SW)/EL)*A0(MW2))/ (MH32*PI2*SW2)

 amplitudes(87) = (-0.015625D0*CS1S3S3f112*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MW2))/(MH12*PI2*SW2)

 amplitudes(88) = (-0.015625D0*CS1S3S3f212*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*&
  &SA1*SA2*SA3)*SB*SW)/EL)*A0(MW2))/ (MH22*PI2*SW2)

 amplitudes(89) = (-0.015625D0*CS1S3S3f312*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - &
  &1.D0*CA1*SA3)*SB*SW)/EL)*A0(MW2))/ (MH32*PI2*SW2)

 amplitudes(90) = 0.D0

 amplitudes(91) = 0.D0

 amplitudes(92) = 0.D0

 amplitudes(93) = 0.D0

 amplitudes(94) = (-0.03125D0*CS1S3S3f112*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*(MZ2 - 2.D0*A0(MZ2))*DBLE&
  &((CW2 + SW2)**INT(2.D0)))/ (CW2*MH12*PI2*SW2)

 amplitudes(95) = (-0.03125D0*CS1S3S3f212*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*SB*SW)/EL)* (MZ2 - 2.D0*A0(MZ2))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2*SW2)

 amplitudes(96) = (-0.03125D0*CS1S3S3f312*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1&
  &.D0*CA1*SA3)*SB*SW)/EL)* (MZ2 - 2.D0*A0(MZ2))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2)

 amplitudes(97) = (-0.0625D0*CS1S3S3f112*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*(MW2 - 2.D0*A0(MW2)))/(MH1&
  &2*PI2*SW2)

 amplitudes(98) = (-0.0625D0*CS1S3S3f212*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA&
  &1*SA2*SA3)*SB*SW)/EL)* (MW2 - 2.D0*A0(MW2)))/(MH22*PI2*SW2)

 amplitudes(99) = (-0.0625D0*CS1S3S3f312*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.&
  &D0*CA1*SA3)*SB*SW)/EL)* (MW2 - 2.D0*A0(MW2)))/(MH32*PI2*SW2)

 amplitudes(100) = (-0.03125D0*EL2*ME2*YukS3Lep1*YukS3Lep2*(A0(ME2) + (ME2 - 1.D0*x)*B0(x, 0.D0, ME2)))/(MW2*PI2*SW2)

 amplitudes(101) = (-0.03125D0*EL2*MM2*YukS3Lep1*YukS3Lep2*(A0(MM2) + (MM2 - 1.D0*x)*B0(x, 0.D0, MM2)))/(MW2*PI2*SW2)

 amplitudes(102) = (-0.03125D0*EL2*ML2*YukS3Lep1*YukS3Lep2*(A0(ML2) + (ML2 - 1.D0*x)*B0(x, 0.D0, ML2)))/(MW2*PI2*SW2)

 amplitudes(103) = (-0.1875D0*CKM11*CKMC11*(((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*S&
  &W2))*A0(MD2) + ((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MU2) + ((-1.D0*EL2&
  &*MD2*MU2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MD2*((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quar&
  &k1*YukS3Quark2)/(MW2*SW2)) + (MU2 - 1.D0*x)*((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW&
  &2*SW2)))*B0(x, MD2, MU2)))/(PI2*SB2)

 amplitudes(104) = (-0.1875D0*CKM21*CKMC21*(((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*S&
  &W2))*A0(MC2) + ((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MD2) + ((-1.D0*EL2&
  &*MC2*MD2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MC2*((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quar&
  &k1*YukS3Quark2)/(MW2*SW2)) + (MD2 - 1.D0*x)*((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW&
  &2*SW2)))*B0(x, MC2, MD2)))/(PI2*SB2)

 amplitudes(105) = (-0.1875D0*CKM31*CKMC31*(((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*S&
  &W2))*A0(MD2) + ((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MT2) + ((-1.D0*EL2&
  &*MD2*MT2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MD2*((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quar&
  &k1*YukS3Quark2)/(MW2*SW2)) + (MT2 - 1.D0*x)*((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*YukS3Quark1*YukS3Quark2)/(MW&
  &2*SW2)))*B0(x, MD2, MT2)))/(PI2*SB2)

 amplitudes(106) = (-0.1875D0*CKM12*CKMC12*(((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*S&
  &W2))*A0(MS2) + ((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MU2) + ((-1.D0*EL2&
  &*MS2*MU2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MS2*((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quar&
  &k1*YukS3Quark2)/(MW2*SW2)) + (MU2 - 1.D0*x)*((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW&
  &2*SW2)))*B0(x, MS2, MU2)))/(PI2*SB2)

 amplitudes(107) = (-0.1875D0*CKM22*CKMC22*(((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*S&
  &W2))*A0(MC2) + ((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MS2) + ((-1.D0*EL2&
  &*MC2*MS2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MC2*((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quar&
  &k1*YukS3Quark2)/(MW2*SW2)) + (MS2 - 1.D0*x)*((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW&
  &2*SW2)))*B0(x, MC2, MS2)))/(PI2*SB2)

 amplitudes(108) = (-0.1875D0*CKM32*CKMC32*(((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*S&
  &W2))*A0(MS2) + ((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MT2) + ((-1.D0*EL2&
  &*MS2*MT2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MS2*((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quar&
  &k1*YukS3Quark2)/(MW2*SW2)) + (MT2 - 1.D0*x)*((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*YukS3Quark1*YukS3Quark2)/(MW&
  &2*SW2)))*B0(x, MS2, MT2)))/(PI2*SB2)

 amplitudes(109) = (-0.1875D0*CKM13*CKMC13*(((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*S&
  &W2))*A0(MB2) + ((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MU2) + ((-1.D0*EL2&
  &*MB2*MU2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MB2*((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quar&
  &k1*YukS3Quark2)/(MW2*SW2)) + (MU2 - 1.D0*x)*((0.5D0*CB*EL2*MU2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW&
  &2*SW2)))*B0(x, MB2, MU2)))/(PI2*SB2)

 amplitudes(110) = (-0.1875D0*CKM23*CKMC23*(((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*S&
  &W2))*A0(MB2) + ((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MC2) + ((-1.D0*EL2&
  &*MB2*MC2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MB2*((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quar&
  &k1*YukS3Quark2)/(MW2*SW2)) + (MC2 - 1.D0*x)*((0.5D0*CB*EL2*MC2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW&
  &2*SW2)))*B0(x, MB2, MC2)))/(PI2*SB2)

 amplitudes(111) = (-0.1875D0*CKM33*CKMC33*(((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*S&
  &W2))*A0(MB2) + ((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW2*SW2))*A0(MT2) + ((-1.D0*EL2&
  &*MB2*MT2*SB*(CB*YukS3Quark1 + SB*YukS3Quark2))/(MW2*SW2) + MB2*((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quar&
  &k1*YukS3Quark2)/(MW2*SW2)) + (MT2 - 1.D0*x)*((0.5D0*CB*EL2*MT2*SB)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*YukS3Quark1*YukS3Quark2)/(MW&
  &2*SW2)))*B0(x, MB2, MT2)))/(PI2*SB2)

 amplitudes(112) = (0.0625D0*CS1S3S3f111*CS1S3S3f112*B0(x, MH12, MW2))/PI2

 amplitudes(113) = (0.0625D0*CS1S3S3f211*CS1S3S3f212*B0(x, MH22, MW2))/PI2

 amplitudes(114) = (0.0625D0*CS1S3S3f311*CS1S3S3f312*B0(x, MH32, MW2))/PI2

 amplitudes(115) = (0.0625D0*CS1S3S3f112*CS1S3S3f122*B0(x, MH12, MHp2))/PI2

 amplitudes(116) = (0.0625D0*CS1S3S3f212*CS1S3S3f222*B0(x, MH22, MHp2))/PI2

 amplitudes(117) = (0.0625D0*CS1S3S3f312*CS1S3S3f322*B0(x, MH32, MHp2))/PI2

 amplitudes(118) = 0D0

 amplitudes(119) = 0D0

 amplitudes(120) = 0D0

 amplitudes(121) = 0D0

 amplitudes(122) = 0.D0

 amplitudes(123) = 0.D0

 amplitudes(124) = 0.D0

 amplitudes(125) = 0.D0

 amplitudes(126) = (-0.015625D0*EL2*(CA2*CB*SA1 - 1.D0*CA1*CA2*SB)*(CA1*CA2*CB + CA2*SA1*SB)*(-1.D0*A0(MH12) + 2.D0*A0(MW2) + (-1&
  &.D0*MW2 + 2.D0*(MH12 + x))*B0(x, MH12, MW2)))/(PI2*SW2)

 amplitudes(127) = (-0.015625D0*EL2*(CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)* (CB*(-1.D0*CA&
  &3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)*(-1.D0*A0(MH22) + 2.D0*A0(MW2) + (-1.D0*MW2 + 2.D0*(MH22 + x))*B0&
  &(x, MH22, MW2)))/(PI2*SW2)

 amplitudes(128) = (-0.015625D0*EL2*(CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)* (CB*(-1.D0*CA3*SA&
  &1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)* (-1.D0*A0(MH32) + 2.D0*A0(MW2) + (-1.D0*MW2 + 2.D0*(MH32 + x))&
  &*B0(x, MH32, MW2)))/(PI2*SW2)

 amplitudes(129) = 0.D0

 amplitudes(130) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,130
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfGpHpAlter = totalAmplitude
end function SelfGpHpAlter

