double complex function SelfDDScalarAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(136)

 amplitudes(1) = (0.0625D0*EL2*ME2*YukS1Lep1*YukS1Quark1*A0(ME2))/(MH12*MW2*PI2*SW2)

 amplitudes(2) = (0.0625D0*EL2*MM2*YukS1Lep1*YukS1Quark1*A0(MM2))/(MH12*MW2*PI2*SW2)

 amplitudes(3) = (0.0625D0*EL2*ML2*YukS1Lep1*YukS1Quark1*A0(ML2))/(MH12*MW2*PI2*SW2)

 amplitudes(4) = (0.0625D0*EL2*ME2*YukS1Lep2*YukS1Quark2*A0(ME2))/(MH22*MW2*PI2*SW2)

 amplitudes(5) = (0.0625D0*EL2*MM2*YukS1Lep2*YukS1Quark2*A0(MM2))/(MH22*MW2*PI2*SW2)

 amplitudes(6) = (0.0625D0*EL2*ML2*YukS1Lep2*YukS1Quark2*A0(ML2))/(MH22*MW2*PI2*SW2)

 amplitudes(7) = (0.0625D0*EL2*ME2*YukS1Lep3*YukS1Quark3*A0(ME2))/(MH32*MW2*PI2*SW2)

 amplitudes(8) = (0.0625D0*EL2*MM2*YukS1Lep3*YukS1Quark3*A0(MM2))/(MH32*MW2*PI2*SW2)

 amplitudes(9) = (0.0625D0*EL2*ML2*YukS1Lep3*YukS1Quark3*A0(ML2))/(MH32*MW2*PI2*SW2)

 amplitudes(10) = (0.1875D0*CA2*EL2*MU2*SA1*YukS1Quark1*A0(MU2))/(MH12*MW2*PI2*SB*SW2)

 amplitudes(11) = (0.1875D0*CA2*EL2*MC2*SA1*YukS1Quark1*A0(MC2))/(MH12*MW2*PI2*SB*SW2)

 amplitudes(12) = (0.1875D0*CA2*EL2*MT2*SA1*YukS1Quark1*A0(MT2))/(MH12*MW2*PI2*SB*SW2)

 amplitudes(13) = (0.1875D0*EL2*MU2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*YukS1Quark2*A0(MU2))/(MH22*MW2*PI2*SB*SW2)

 amplitudes(14) = (0.1875D0*EL2*MC2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*YukS1Quark2*A0(MC2))/(MH22*MW2*PI2*SB*SW2)

 amplitudes(15) = (0.1875D0*EL2*MT2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*YukS1Quark2*A0(MT2))/(MH22*MW2*PI2*SB*SW2)

 amplitudes(16) = (-0.1875D0*EL2*MU2*(CA3*SA1*SA2 + CA1*SA3)*YukS1Quark3*A0(MU2))/(MH32*MW2*PI2*SB*SW2)

 amplitudes(17) = (-0.1875D0*EL2*MC2*(CA3*SA1*SA2 + CA1*SA3)*YukS1Quark3*A0(MC2))/(MH32*MW2*PI2*SB*SW2)

 amplitudes(18) = (-0.1875D0*EL2*MT2*(CA3*SA1*SA2 + CA1*SA3)*YukS1Quark3*A0(MT2))/(MH32*MW2*PI2*SB*SW2)

 amplitudes(19) = (0.1875D0*EL2*MD2*A0(MD2)*DBLE(YukS1Quark1**INT(2.D0)))/(MH12*MW2*PI2*SW2)

 amplitudes(20) = (0.1875D0*EL2*MS2*A0(MS2)*DBLE(YukS1Quark1**INT(2.D0)))/(MH12*MW2*PI2*SW2)

 amplitudes(21) = (0.1875D0*EL2*MB2*A0(MB2)*DBLE(YukS1Quark1**INT(2.D0)))/(MH12*MW2*PI2*SW2)

 amplitudes(22) = (0.1875D0*EL2*MD2*A0(MD2)*DBLE(YukS1Quark2**INT(2.D0)))/(MH22*MW2*PI2*SW2)

 amplitudes(23) = (0.1875D0*EL2*MS2*A0(MS2)*DBLE(YukS1Quark2**INT(2.D0)))/(MH22*MW2*PI2*SW2)

 amplitudes(24) = (0.1875D0*EL2*MB2*A0(MB2)*DBLE(YukS1Quark2**INT(2.D0)))/(MH22*MW2*PI2*SW2)

 amplitudes(25) = (0.1875D0*EL2*MD2*A0(MD2)*DBLE(YukS1Quark3**INT(2.D0)))/(MH32*MW2*PI2*SW2)

 amplitudes(26) = (0.1875D0*EL2*MS2*A0(MS2)*DBLE(YukS1Quark3**INT(2.D0)))/(MH32*MW2*PI2*SW2)

 amplitudes(27) = (0.1875D0*EL2*MB2*A0(MB2)*DBLE(YukS1Quark3**INT(2.D0)))/(MH32*MW2*PI2*SW2)

 amplitudes(28) = 0D0

 amplitudes(29) = 0D0

 amplitudes(30) = 0D0

 amplitudes(31) = 0D0

 amplitudes(32) = 0D0

 amplitudes(33) = 0D0

 amplitudes(34) = 0D0

 amplitudes(35) = 0D0

 amplitudes(36) = 0D0

 amplitudes(37) = 0D0

 amplitudes(38) = 0D0

 amplitudes(39) = 0D0

 amplitudes(40) = 0D0

 amplitudes(41) = 0D0

 amplitudes(42) = 0D0

 amplitudes(43) = 0D0

 amplitudes(44) = 0D0

 amplitudes(45) = 0D0

 amplitudes(46) = (0.015625D0*CS1S1S1f111*EL*YukS1Quark1*A0(MH12))/(MH12*MW*PI2*SW)

 amplitudes(47) = (0.015625D0*CS1S1S1f122*EL*YukS1Quark1*A0(MH22))/(MH12*MW*PI2*SW)

 amplitudes(48) = (0.015625D0*CS1S1S1f133*EL*YukS1Quark1*A0(MH32))/(MH12*MW*PI2*SW)

 amplitudes(49) = (0.015625D0*CS1S1S1f211*EL*YukS1Quark2*A0(MH12))/(MH22*MW*PI2*SW)

 amplitudes(50) = (0.015625D0*CS1S1S1f222*EL*YukS1Quark2*A0(MH22))/(MH22*MW*PI2*SW)

 amplitudes(51) = (0.015625D0*CS1S1S1f233*EL*YukS1Quark2*A0(MH32))/(MH22*MW*PI2*SW)

 amplitudes(52) = (0.015625D0*CS1S1S1f311*EL*YukS1Quark3*A0(MH12))/(MH32*MW*PI2*SW)

 amplitudes(53) = (0.015625D0*CS1S1S1f322*EL*YukS1Quark3*A0(MH22))/(MH32*MW*PI2*SW)

 amplitudes(54) = (0.015625D0*CS1S1S1f333*EL*YukS1Quark3*A0(MH32))/(MH32*MW*PI2*SW)

 amplitudes(55) = (0.015625D0*CS2S2S1f111*EL*YukS1Quark1*A0(MZ2))/(MH12*MW*PI2*SW)

 amplitudes(56) = (0.015625D0*CS2S2S1f221*EL*YukS1Quark1*A0(MA02))/(MH12*MW*PI2*SW)

 amplitudes(57) = (0.015625D0*CS2S2S1f112*EL*YukS1Quark2*A0(MZ2))/(MH22*MW*PI2*SW)

 amplitudes(58) = (0.015625D0*CS2S2S1f222*EL*YukS1Quark2*A0(MA02))/(MH22*MW*PI2*SW)

 amplitudes(59) = (0.015625D0*CS2S2S1f113*EL*YukS1Quark3*A0(MZ2))/(MH32*MW*PI2*SW)

 amplitudes(60) = (0.015625D0*CS2S2S1f223*EL*YukS1Quark3*A0(MA02))/(MH32*MW*PI2*SW)

 amplitudes(61) = (0.03125D0*CS1S3S3f111*EL*YukS1Quark1*A0(MW2))/(MH12*MW*PI2*SW)

 amplitudes(62) = (0.03125D0*CS1S3S3f122*EL*YukS1Quark1*A0(MHp2))/(MH12*MW*PI2*SW)

 amplitudes(63) = (0.03125D0*CS1S3S3f211*EL*YukS1Quark2*A0(MW2))/(MH22*MW*PI2*SW)

 amplitudes(64) = (0.03125D0*CS1S3S3f222*EL*YukS1Quark2*A0(MHp2))/(MH22*MW*PI2*SW)

 amplitudes(65) = (0.03125D0*CS1S3S3f311*EL*YukS1Quark3*A0(MW2))/(MH32*MW*PI2*SW)

 amplitudes(66) = (0.03125D0*CS1S3S3f322*EL*YukS1Quark3*A0(MHp2))/(MH32*MW*PI2*SW)

 amplitudes(67) = 0D0

 amplitudes(68) = 0D0

 amplitudes(69) = 0D0

 amplitudes(70) = 0D0

 amplitudes(71) = (0.015625D0*CA2*EL2*(CA1*CB + SA1*SB)*YukS1Quark1*A0(MZ2)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*PI2*SW2)

 amplitudes(72) = (-0.015625D0*EL2*(CA3*CB*SA1 + CA1*CB*SA2*SA3 - 1.D0*CA1*CA3*SB + SA1*SA2*SA3*SB)*YukS1Quark2*A0(MZ2)*DBLE((CW2&
  & + SW2)**INT(2.D0)))/ (CW2*MH22*PI2*SW2)

 amplitudes(73) = (-0.015625D0*EL2*(CA1*CA3*CB*SA2 - 1.D0*CB*SA1*SA3 + CA3*SA1*SA2*SB + CA1*SA3*SB)*YukS1Quark3*A0(MZ2)*DBLE((CW2&
  & + SW2)**INT(2.D0)))/ (CW2*MH32*PI2*SW2)

 amplitudes(74) = (0.015625D0*CA2*EL2*(CA1*CB + SA1*SB)*YukS1Quark1*A0(MW2))/(MH12*PI2*SW2)

 amplitudes(75) = (-0.015625D0*EL2*(CA3*CB*SA1 + CA1*CB*SA2*SA3 - 1.D0*CA1*CA3*SB + SA1*SA2*SA3*SB)*YukS1Quark2*A0(MW2))/(MH22*PI&
  &2*SW2)

 amplitudes(76) = (-0.015625D0*EL2*(CA1*CA3*CB*SA2 - 1.D0*CB*SA1*SA3 + CA3*SA1*SA2*SB + CA1*SA3*SB)*YukS1Quark3*A0(MW2))/(MH32*PI&
  &2*SW2)

 amplitudes(77) = (0.015625D0*CA2*EL2*(CA1*CB + SA1*SB)*YukS1Quark1*A0(MW2))/(MH12*PI2*SW2)

 amplitudes(78) = (-0.015625D0*EL2*(CA3*CB*SA1 + CA1*CB*SA2*SA3 - 1.D0*CA1*CA3*SB + SA1*SA2*SA3*SB)*YukS1Quark2*A0(MW2))/(MH22*PI&
  &2*SW2)

 amplitudes(79) = (-0.015625D0*EL2*(CA1*CA3*CB*SA2 - 1.D0*CB*SA1*SA3 + CA3*SA1*SA2*SB + CA1*SA3*SB)*YukS1Quark3*A0(MW2))/(MH32*PI&
  &2*SW2)

 amplitudes(80) = (0.015625D0*EL2*(CB2 + SB2)*YukS2Quark1*A0(MW2)*(DiracGamma(6.D0) - 1.D0*DiracGamma(7.D0)))/(MZ2*PI2*SW2)

 amplitudes(81) = 0.D0

 amplitudes(82) = (-0.015625D0*EL2*(CB2 + SB2)*YukS2Quark1*A0(MW2)*(DiracGamma(6.D0) - 1.D0*DiracGamma(7.D0)))/(MZ2*PI2*SW2)

 amplitudes(83) = 0.D0

 amplitudes(84) = (0.03125D0*CA2*EL2*(CA1*CB + SA1*SB)*YukS1Quark1*(MZ2 - 2.D0*A0(MZ2))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*MH12*P&
  &I2*SW2)

 amplitudes(85) = (-0.03125D0*EL2*(CA3*CB*SA1 + CA1*CB*SA2*SA3 - 1.D0*CA1*CA3*SB + SA1*SA2*SA3*SB)*YukS1Quark2*(MZ2 - 2.D0*A0(MZ2&
  &))*DBLE((CW2 + SW2)**INT(2.D0)))/ (CW2*MH22*PI2*SW2)

 amplitudes(86) = (-0.03125D0*EL2*(CA1*CA3*CB*SA2 - 1.D0*CB*SA1*SA3 + CA3*SA1*SA2*SB + CA1*SA3*SB)*YukS1Quark3*(MZ2 - 2.D0*A0(MZ2&
  &))*DBLE((CW2 + SW2)**INT(2.D0)))/ (CW2*MH32*PI2*SW2)

 amplitudes(87) = (0.0625D0*CA2*EL2*(CA1*CB + SA1*SB)*YukS1Quark1*(MW2 - 2.D0*A0(MW2)))/(MH12*PI2*SW2)

 amplitudes(88) = (-0.0625D0*EL2*(CA3*CB*SA1 + CA1*CB*SA2*SA3 - 1.D0*CA1*CA3*SB + SA1*SA2*SA3*SB)*YukS1Quark2*(MW2 - 2.D0*A0(MW2)&
  &))/(MH22*PI2*SW2)

 amplitudes(89) = (-0.0625D0*EL2*(CA1*CA3*CB*SA2 - 1.D0*CB*SA1*SA3 + CA3*SA1*SA2*SB + CA1*SA3*SB)*YukS1Quark3*(MW2 - 2.D0*A0(MW2)&
  &))/(MH32*PI2*SW2)

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

 amplitudes(121) = (0.015625D0*EL2*MD2*B0(x, MD2, MH12)*DBLE(YukS1Quark1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(122) = (0.015625D0*EL2*MD2*B0(x, MD2, MH22)*DBLE(YukS1Quark2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(123) = (0.015625D0*EL2*MD2*B0(x, MD2, MH32)*DBLE(YukS1Quark3**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(124) = (-0.015625D0*EL2*MD2*B0(x, MD2, MZ2)*DBLE(YukS2Quark1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(125) = (-0.015625D0*EL2*MD2*B0(x, MA02, MD2)*DBLE(YukS2Quark2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(126) = (-0.03125D0*CKM11*CKMC11*EL2*MU2*YukS3Quark1*B0(x, MU2, MW2))/(MW2*PI2*SW2)

 amplitudes(127) = (-0.03125D0*CKM21*CKMC21*EL2*MC2*YukS3Quark1*B0(x, MC2, MW2))/(MW2*PI2*SW2)

 amplitudes(128) = (-0.03125D0*CKM31*CKMC31*EL2*MT2*YukS3Quark1*B0(x, MT2, MW2))/(MW2*PI2*SW2)

 amplitudes(129) = (-0.03125D0*CB*CKM11*CKMC11*EL2*MU2*YukS3Quark2*B0(x, MHp2, MU2))/(MW2*PI2*SB*SW2)

 amplitudes(130) = (-0.03125D0*CB*CKM21*CKMC21*EL2*MC2*YukS3Quark2*B0(x, MC2, MHp2))/(MW2*PI2*SB*SW2)

 amplitudes(131) = (-0.03125D0*CB*CKM31*CKMC31*EL2*MT2*YukS3Quark2*B0(x, MHp2, MT2))/(MW2*PI2*SB*SW2)

 amplitudes(132) = (-0.013888888888888888D0*EL2*(-1.D0 + 2.D0*B0(x, 0.D0, MD2)))/PI2

 amplitudes(133) = (0.006944444444444444D0*EL2*(3.D0*CW2 + SW2)*(-1.D0 + 2.D0*B0(x, MD2, MZ2)))/(CW2*PI2)

 amplitudes(134) = 0.D0

 amplitudes(135) = 0.D0

 amplitudes(136) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,136
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfDDScalarAlter = totalAmplitude
end function SelfDDScalarAlter

