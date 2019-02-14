double complex function SelfHpHpAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(163)

 amplitudes(1) = (-0.03125D0*CS1S1S3S3f1122*A0(MH12))/PI2

 amplitudes(2) = (-0.03125D0*CS1S1S3S3f2222*A0(MH22))/PI2

 amplitudes(3) = (-0.03125D0*CS1S1S3S3f3322*A0(MH32))/PI2

 amplitudes(4) = (-0.03125D0*CS2S2S3S3f1122*A0(MZ2))/PI2

 amplitudes(5) = (-0.03125D0*CS2S2S3S3f2222*A0(MA02))/PI2

 amplitudes(6) = (-0.0625D0*CS3S3S3S3f2121*A0(MW2))/PI2

 amplitudes(7) = (-0.0625D0*CS3S3S3S3f2222*A0(MHp2))/PI2

 amplitudes(8) = 0.D0

 amplitudes(9) = (-0.03125D0*EL2*(CB2 + SB2)*(MZ2 - 2.D0*A0(MZ2))*DBLE((CW2 - 1.D0*SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(10) = (-0.0625D0*EL2*(CB2 + SB2)*(MW2 - 2.D0*A0(MW2)))/(PI2*SW2)

 amplitudes(11) = (-0.125D0*CS1S3S3f122*EL*ME2*YukS1Lep1*A0(ME2))/(MH12*MW*PI2*SW)

 amplitudes(12) = (-0.125D0*CS1S3S3f122*EL*MM2*YukS1Lep1*A0(MM2))/(MH12*MW*PI2*SW)

 amplitudes(13) = (-0.125D0*CS1S3S3f122*EL*ML2*YukS1Lep1*A0(ML2))/(MH12*MW*PI2*SW)

 amplitudes(14) = (-0.125D0*CS1S3S3f222*EL*ME2*YukS1Lep2*A0(ME2))/(MH22*MW*PI2*SW)

 amplitudes(15) = (-0.125D0*CS1S3S3f222*EL*MM2*YukS1Lep2*A0(MM2))/(MH22*MW*PI2*SW)

 amplitudes(16) = (-0.125D0*CS1S3S3f222*EL*ML2*YukS1Lep2*A0(ML2))/(MH22*MW*PI2*SW)

 amplitudes(17) = (-0.125D0*CS1S3S3f322*EL*ME2*YukS1Lep3*A0(ME2))/(MH32*MW*PI2*SW)

 amplitudes(18) = (-0.125D0*CS1S3S3f322*EL*MM2*YukS1Lep3*A0(MM2))/(MH32*MW*PI2*SW)

 amplitudes(19) = (-0.125D0*CS1S3S3f322*EL*ML2*YukS1Lep3*A0(ML2))/(MH32*MW*PI2*SW)

 amplitudes(20) = (-0.375D0*CA2*CS1S3S3f122*EL*MU2*SA1*A0(MU2))/(MH12*MW*PI2*SB*SW)

 amplitudes(21) = (-0.375D0*CA2*CS1S3S3f122*EL*MC2*SA1*A0(MC2))/(MH12*MW*PI2*SB*SW)

 amplitudes(22) = (-0.375D0*CA2*CS1S3S3f122*EL*MT2*SA1*A0(MT2))/(MH12*MW*PI2*SB*SW)

 amplitudes(23) = (-0.375D0*CS1S3S3f222*EL*MU2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*A0(MU2))/(MH22*MW*PI2*SB*SW)

 amplitudes(24) = (-0.375D0*CS1S3S3f222*EL*MC2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*A0(MC2))/(MH22*MW*PI2*SB*SW)

 amplitudes(25) = (-0.375D0*CS1S3S3f222*EL*MT2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*A0(MT2))/(MH22*MW*PI2*SB*SW)

 amplitudes(26) = (-0.375D0*CS1S3S3f322*EL*MU2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*A0(MU2))/(MH32*MW*PI2*SB*SW)

 amplitudes(27) = (-0.375D0*CS1S3S3f322*EL*MC2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*A0(MC2))/(MH32*MW*PI2*SB*SW)

 amplitudes(28) = (-0.375D0*CS1S3S3f322*EL*MT2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*A0(MT2))/(MH32*MW*PI2*SB*SW)

 amplitudes(29) = (-0.375D0*CS1S3S3f122*EL*MD2*YukS1Quark1*A0(MD2))/(MH12*MW*PI2*SW)

 amplitudes(30) = (-0.375D0*CS1S3S3f122*EL*MS2*YukS1Quark1*A0(MS2))/(MH12*MW*PI2*SW)

 amplitudes(31) = (-0.375D0*CS1S3S3f122*EL*MB2*YukS1Quark1*A0(MB2))/(MH12*MW*PI2*SW)

 amplitudes(32) = (-0.375D0*CS1S3S3f222*EL*MD2*YukS1Quark2*A0(MD2))/(MH22*MW*PI2*SW)

 amplitudes(33) = (-0.375D0*CS1S3S3f222*EL*MS2*YukS1Quark2*A0(MS2))/(MH22*MW*PI2*SW)

 amplitudes(34) = (-0.375D0*CS1S3S3f222*EL*MB2*YukS1Quark2*A0(MB2))/(MH22*MW*PI2*SW)

 amplitudes(35) = (-0.375D0*CS1S3S3f322*EL*MD2*YukS1Quark3*A0(MD2))/(MH32*MW*PI2*SW)

 amplitudes(36) = (-0.375D0*CS1S3S3f322*EL*MS2*YukS1Quark3*A0(MS2))/(MH32*MW*PI2*SW)

 amplitudes(37) = (-0.375D0*CS1S3S3f322*EL*MB2*YukS1Quark3*A0(MB2))/(MH32*MW*PI2*SW)

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

 amplitudes(56) = (-0.03125D0*CS1S1S1f111*CS1S3S3f122*A0(MH12))/(MH12*PI2)

 amplitudes(57) = (-0.03125D0*CS1S1S1f122*CS1S3S3f122*A0(MH22))/(MH12*PI2)

 amplitudes(58) = (-0.03125D0*CS1S1S1f133*CS1S3S3f122*A0(MH32))/(MH12*PI2)

 amplitudes(59) = (-0.03125D0*CS1S1S1f211*CS1S3S3f222*A0(MH12))/(MH22*PI2)

 amplitudes(60) = (-0.03125D0*CS1S1S1f222*CS1S3S3f222*A0(MH22))/(MH22*PI2)

 amplitudes(61) = (-0.03125D0*CS1S1S1f233*CS1S3S3f222*A0(MH32))/(MH22*PI2)

 amplitudes(62) = (-0.03125D0*CS1S1S1f311*CS1S3S3f322*A0(MH12))/(MH32*PI2)

 amplitudes(63) = (-0.03125D0*CS1S1S1f322*CS1S3S3f322*A0(MH22))/(MH32*PI2)

 amplitudes(64) = (-0.03125D0*CS1S1S1f333*CS1S3S3f322*A0(MH32))/(MH32*PI2)

 amplitudes(65) = (-0.03125D0*CS1S3S3f122*CS2S2S1f111*A0(MZ2))/(MH12*PI2)

 amplitudes(66) = (-0.03125D0*CS1S3S3f122*CS2S2S1f221*A0(MA02))/(MH12*PI2)

 amplitudes(67) = (-0.03125D0*CS1S3S3f222*CS2S2S1f112*A0(MZ2))/(MH22*PI2)

 amplitudes(68) = (-0.03125D0*CS1S3S3f222*CS2S2S1f222*A0(MA02))/(MH22*PI2)

 amplitudes(69) = (-0.03125D0*CS1S3S3f322*CS2S2S1f113*A0(MZ2))/(MH32*PI2)

 amplitudes(70) = (-0.03125D0*CS1S3S3f322*CS2S2S1f223*A0(MA02))/(MH32*PI2)

 amplitudes(71) = (-0.0625D0*CS1S3S3f111*CS1S3S3f122*A0(MW2))/(MH12*PI2)

 amplitudes(72) = (-0.0625D0*A0(MHp2)*DBLE(CS1S3S3f122**INT(2.D0)))/(MH12*PI2)

 amplitudes(73) = (-0.0625D0*CS1S3S3f211*CS1S3S3f222*A0(MW2))/(MH22*PI2)

 amplitudes(74) = (-0.0625D0*A0(MHp2)*DBLE(CS1S3S3f222**INT(2.D0)))/(MH22*PI2)

 amplitudes(75) = (-0.0625D0*CS1S3S3f311*CS1S3S3f322*A0(MW2))/(MH32*PI2)

 amplitudes(76) = (-0.0625D0*A0(MHp2)*DBLE(CS1S3S3f322**INT(2.D0)))/(MH32*PI2)

 amplitudes(77) = 0D0

 amplitudes(78) = 0D0

 amplitudes(79) = 0D0

 amplitudes(80) = 0D0

 amplitudes(81) = (-0.015625D0*CS1S3S3f122*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MZ2)*DBLE((CW2 + SW2)&
  &**INT(2.D0)))/(CW2*MH12*PI2*SW2)

 amplitudes(82) = (-0.015625D0*CS1S3S3f222*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*&
  &SA1*SA2*SA3)*SB*SW)/EL)*A0(MZ2)* DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2*SW2)

 amplitudes(83) = (-0.015625D0*CS1S3S3f322*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - &
  &1.D0*CA1*SA3)*SB*SW)/EL)*A0(MZ2)* DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2)

 amplitudes(84) = (-0.015625D0*CS1S3S3f122*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MW2))/(MH12*PI2*SW2)

 amplitudes(85) = (-0.015625D0*CS1S3S3f222*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*&
  &SA1*SA2*SA3)*SB*SW)/EL)*A0(MW2))/ (MH22*PI2*SW2)

 amplitudes(86) = (-0.015625D0*CS1S3S3f322*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - &
  &1.D0*CA1*SA3)*SB*SW)/EL)*A0(MW2))/ (MH32*PI2*SW2)

 amplitudes(87) = (-0.015625D0*CS1S3S3f122*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*A0(MW2))/(MH12*PI2*SW2)

 amplitudes(88) = (-0.015625D0*CS1S3S3f222*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*&
  &SA1*SA2*SA3)*SB*SW)/EL)*A0(MW2))/ (MH22*PI2*SW2)

 amplitudes(89) = (-0.015625D0*CS1S3S3f322*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - &
  &1.D0*CA1*SA3)*SB*SW)/EL)*A0(MW2))/ (MH32*PI2*SW2)

 amplitudes(90) = 0D0

 amplitudes(91) = 0D0

 amplitudes(92) = 0D0

 amplitudes(93) = 0D0

 amplitudes(94) = (-0.03125D0*CS1S3S3f122*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*(MZ2 - 2.D0*A0(MZ2))*DBLE&
  &((CW2 + SW2)**INT(2.D0)))/ (CW2*MH12*PI2*SW2)

 amplitudes(95) = (-0.03125D0*CS1S3S3f222*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*S&
  &A1*SA2*SA3)*SB*SW)/EL)* (MZ2 - 2.D0*A0(MZ2))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH22*PI2*SW2)

 amplitudes(96) = (-0.03125D0*CS1S3S3f322*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1&
  &.D0*CA1*SA3)*SB*SW)/EL)* (MZ2 - 2.D0*A0(MZ2))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2*SW2)

 amplitudes(97) = (-0.0625D0*CS1S3S3f122*EL2*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)*(MW2 - 2.D0*A0(MW2)))/(MH1&
  &2*PI2*SW2)

 amplitudes(98) = (-0.0625D0*CS1S3S3f222*EL2*((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA&
  &1*SA2*SA3)*SB*SW)/EL)* (MW2 - 2.D0*A0(MW2)))/(MH22*PI2*SW2)

 amplitudes(99) = (-0.0625D0*CS1S3S3f322*EL2*((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.&
  &D0*CA1*SA3)*SB*SW)/EL)* (MW2 - 2.D0*A0(MW2)))/(MH32*PI2*SW2)

 amplitudes(100) = 0.D0

 amplitudes(101) = 0.D0

 amplitudes(102) = 0.D0

 amplitudes(103) = 0.D0

 amplitudes(104) = 0.D0

 amplitudes(105) = 0.D0

 amplitudes(106) = 0.D0

 amplitudes(107) = 0.D0

 amplitudes(108) = 0.D0

 amplitudes(109) = 0.D0

 amplitudes(110) = 0.D0

 amplitudes(111) = 0.D0

 amplitudes(112) = 0.D0

 amplitudes(113) = 0.D0

 amplitudes(114) = 0.D0

 amplitudes(115) = 0.D0

 amplitudes(116) = 0.D0

 amplitudes(117) = 0.D0

 amplitudes(118) = 0.D0

 amplitudes(119) = 0.D0

 amplitudes(120) = 0.D0

 amplitudes(121) = 0.D0

 amplitudes(122) = 0.D0

 amplitudes(123) = 0.D0

 amplitudes(124) = 0.D0

 amplitudes(125) = 0.D0

 amplitudes(126) = 0.D0

 amplitudes(127) = 0.D0

 amplitudes(128) = 0.D0

 amplitudes(129) = 0.D0

 amplitudes(130) = 0.D0

 amplitudes(131) = (-0.03125D0*EL2*ME2*(A0(ME2) + (ME2 - 1.D0*x)*B0(x, 0.D0, ME2))*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(132) = (-0.03125D0*EL2*MM2*(A0(MM2) + (MM2 - 1.D0*x)*B0(x, 0.D0, MM2))*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(133) = (-0.03125D0*EL2*ML2*(A0(ML2) + (ML2 - 1.D0*x)*B0(x, 0.D0, ML2))*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(134) = (-0.1875D0*CKM11*CKMC11*(A0(MD2)*((0.5D0*CB2*EL2*MU2)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark2**INT(2.D&
  &0)))/(MW2*SW2)) + A0(MU2)*((0.5D0*CB2*EL2*MU2)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)) + B0(x,&
  & MD2, MU2)*((-2.D0*CB*EL2*MD2*MU2*SB*YukS3Quark2)/(MW2*SW2) + MD2*((0.5D0*CB2*EL2*MU2)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*DBLE(Yuk&
  &S3Quark2**INT(2.D0)))/(MW2*SW2)) + (MU2 - 1.D0*x)*((0.5D0*CB2*EL2*MU2)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark2**INT(2.&
  &D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(135) = (-0.1875D0*CKM21*CKMC21*(A0(MC2)*((0.5D0*CB2*EL2*MC2)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark2**INT(2.D&
  &0)))/(MW2*SW2)) + A0(MD2)*((0.5D0*CB2*EL2*MC2)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)) + B0(x,&
  & MC2, MD2)*((-2.D0*CB*EL2*MC2*MD2*SB*YukS3Quark2)/(MW2*SW2) + MC2*((0.5D0*CB2*EL2*MC2)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*DBLE(Yuk&
  &S3Quark2**INT(2.D0)))/(MW2*SW2)) + (MD2 - 1.D0*x)*((0.5D0*CB2*EL2*MC2)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark2**INT(2.&
  &D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(136) = (-0.1875D0*CKM31*CKMC31*(A0(MD2)*((0.5D0*CB2*EL2*MT2)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark2**INT(2.D&
  &0)))/(MW2*SW2)) + A0(MT2)*((0.5D0*CB2*EL2*MT2)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)) + B0(x,&
  & MD2, MT2)*((-2.D0*CB*EL2*MD2*MT2*SB*YukS3Quark2)/(MW2*SW2) + MD2*((0.5D0*CB2*EL2*MT2)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*DBLE(Yuk&
  &S3Quark2**INT(2.D0)))/(MW2*SW2)) + (MT2 - 1.D0*x)*((0.5D0*CB2*EL2*MT2)/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*DBLE(YukS3Quark2**INT(2.&
  &D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(137) = (-0.1875D0*CKM12*CKMC12*(A0(MS2)*((0.5D0*CB2*EL2*MU2)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark2**INT(2.D&
  &0)))/(MW2*SW2)) + A0(MU2)*((0.5D0*CB2*EL2*MU2)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)) + B0(x,&
  & MS2, MU2)*((-2.D0*CB*EL2*MS2*MU2*SB*YukS3Quark2)/(MW2*SW2) + MS2*((0.5D0*CB2*EL2*MU2)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*DBLE(Yuk&
  &S3Quark2**INT(2.D0)))/(MW2*SW2)) + (MU2 - 1.D0*x)*((0.5D0*CB2*EL2*MU2)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark2**INT(2.&
  &D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(138) = (-0.1875D0*CKM22*CKMC22*(A0(MC2)*((0.5D0*CB2*EL2*MC2)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark2**INT(2.D&
  &0)))/(MW2*SW2)) + A0(MS2)*((0.5D0*CB2*EL2*MC2)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)) + B0(x,&
  & MC2, MS2)*((-2.D0*CB*EL2*MC2*MS2*SB*YukS3Quark2)/(MW2*SW2) + MC2*((0.5D0*CB2*EL2*MC2)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*DBLE(Yuk&
  &S3Quark2**INT(2.D0)))/(MW2*SW2)) + (MS2 - 1.D0*x)*((0.5D0*CB2*EL2*MC2)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark2**INT(2.&
  &D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(139) = (-0.1875D0*CKM32*CKMC32*(A0(MS2)*((0.5D0*CB2*EL2*MT2)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark2**INT(2.D&
  &0)))/(MW2*SW2)) + A0(MT2)*((0.5D0*CB2*EL2*MT2)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)) + B0(x,&
  & MS2, MT2)*((-2.D0*CB*EL2*MS2*MT2*SB*YukS3Quark2)/(MW2*SW2) + MS2*((0.5D0*CB2*EL2*MT2)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*DBLE(Yuk&
  &S3Quark2**INT(2.D0)))/(MW2*SW2)) + (MT2 - 1.D0*x)*((0.5D0*CB2*EL2*MT2)/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*DBLE(YukS3Quark2**INT(2.&
  &D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(140) = (-0.1875D0*CKM13*CKMC13*(A0(MB2)*((0.5D0*CB2*EL2*MU2)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark2**INT(2.D&
  &0)))/(MW2*SW2)) + A0(MU2)*((0.5D0*CB2*EL2*MU2)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)) + B0(x,&
  & MB2, MU2)*((-2.D0*CB*EL2*MB2*MU2*SB*YukS3Quark2)/(MW2*SW2) + MB2*((0.5D0*CB2*EL2*MU2)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*DBLE(Yuk&
  &S3Quark2**INT(2.D0)))/(MW2*SW2)) + (MU2 - 1.D0*x)*((0.5D0*CB2*EL2*MU2)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark2**INT(2.&
  &D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(141) = (-0.1875D0*CKM23*CKMC23*(A0(MB2)*((0.5D0*CB2*EL2*MC2)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark2**INT(2.D&
  &0)))/(MW2*SW2)) + A0(MC2)*((0.5D0*CB2*EL2*MC2)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)) + B0(x,&
  & MB2, MC2)*((-2.D0*CB*EL2*MB2*MC2*SB*YukS3Quark2)/(MW2*SW2) + MB2*((0.5D0*CB2*EL2*MC2)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*DBLE(Yuk&
  &S3Quark2**INT(2.D0)))/(MW2*SW2)) + (MC2 - 1.D0*x)*((0.5D0*CB2*EL2*MC2)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark2**INT(2.&
  &D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(142) = (-0.1875D0*CKM33*CKMC33*(A0(MB2)*((0.5D0*CB2*EL2*MT2)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark2**INT(2.D&
  &0)))/(MW2*SW2)) + A0(MT2)*((0.5D0*CB2*EL2*MT2)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)) + B0(x,&
  & MB2, MT2)*((-2.D0*CB*EL2*MB2*MT2*SB*YukS3Quark2)/(MW2*SW2) + MB2*((0.5D0*CB2*EL2*MT2)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*DBLE(Yuk&
  &S3Quark2**INT(2.D0)))/(MW2*SW2)) + (MT2 - 1.D0*x)*((0.5D0*CB2*EL2*MT2)/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*DBLE(YukS3Quark2**INT(2.&
  &D0)))/(MW2*SW2)))))/(PI2*SB2)

 amplitudes(143) = (0.0625D0*CS1S3S3f112*CS1S3S3f121*B0(x, MH12, MW2))/PI2

 amplitudes(144) = (0.0625D0*CS1S3S3f212*CS1S3S3f221*B0(x, MH22, MW2))/PI2

 amplitudes(145) = (0.0625D0*CS1S3S3f312*CS1S3S3f321*B0(x, MH32, MW2))/PI2

 amplitudes(146) = (0.0625D0*B0(x, MH12, MHp2)*DBLE(CS1S3S3f122**INT(2.D0)))/PI2

 amplitudes(147) = (0.0625D0*B0(x, MH22, MHp2)*DBLE(CS1S3S3f222**INT(2.D0)))/PI2

 amplitudes(148) = (0.0625D0*B0(x, MH32, MHp2)*DBLE(CS1S3S3f322**INT(2.D0)))/PI2

 amplitudes(149) = 0.D0

 amplitudes(150) = (0.015625D0*B0(x, MA02, MW2)*DBLE((-1.D0*CB2 - 1.D0*SB2)**INT(2.D0))*DBLE(((-2.D0*CB2*MW*SW)/EL - (2.D0*MW*SB2&
  &*SW)/EL)**INT(2.D0))* DBLE(((-0.25D0*EL2*(-1.D0*MA02 + m12squared/(CB*SB)))/(MW2*SW2) + (0.25D0*EL2*(MA02 - 2.D0*MHp2 + m12squ&
  &ared/(CB*SB)))/(MW2*SW2))**INT(2.D0)))/ PI2

 amplitudes(151) = 0D0

 amplitudes(152) = 0D0

 amplitudes(153) = 0.D0

 amplitudes(154) = 0.D0

 amplitudes(155) = 0.D0

 amplitudes(156) = 0.D0

 amplitudes(157) = (0.0625D0*EL2*(A0(MHp2) - 2.D0*(MHp2 + x)*B0(x, 0.D0, MHp2)))/PI2

 amplitudes(158) = (-0.015625D0*EL2*(-1.D0*A0(MHp2) + 2.D0*A0(MZ2) + (-1.D0*MZ2 + 2.D0*(MHp2 + x))*B0(x, MHp2, MZ2))*DBLE((CW2 - &
  &1.D0*SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(159) = (-0.015625D0*EL2*(-1.D0*A0(MH12) + 2.D0*A0(MW2) + (-1.D0*MW2 + 2.D0*(MH12 + x))*B0(x, MH12, MW2))*DBLE((CA2*CB&
  &*SA1 - 1.D0*CA1*CA2*SB)**INT(2.D0)))/ (PI2*SW2)

 amplitudes(160) = (-0.015625D0*EL2*(-1.D0*A0(MH22) + 2.D0*A0(MW2) + (-1.D0*MW2 + 2.D0*(MH22 + x))*B0(x, MH22, MW2))* DBLE((CB*(C&
  &A1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)**INT(2.D0)))/(PI2*SW2)

 amplitudes(161) = (-0.015625D0*EL2*(-1.D0*A0(MH32) + 2.D0*A0(MW2) + (-1.D0*MW2 + 2.D0*(MH32 + x))*B0(x, MH32, MW2))* DBLE((CB*(-&
  &1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB)**INT(2.D0)))/(PI2*SW2)

 amplitudes(162) = 0.D0

 amplitudes(163) = (-0.015625D0*EL2*(-1.D0*A0(MA02) + 2.D0*A0(MW2) + (-1.D0*MW2 + 2.D0*(MA02 + x))*B0(x, MA02, MW2))*DBLE((CB2 + &
  &SB2)**INT(2.D0)))/(PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,163
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfHpHpAlter = totalAmplitude
end function SelfHpHpAlter

