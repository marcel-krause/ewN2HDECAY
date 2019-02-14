double complex function SelfUULeftAlter(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(136)

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

 amplitudes(11) = 0.D0

 amplitudes(12) = 0.D0

 amplitudes(13) = 0.D0

 amplitudes(14) = 0.D0

 amplitudes(15) = 0.D0

 amplitudes(16) = 0.D0

 amplitudes(17) = 0.D0

 amplitudes(18) = 0.D0

 amplitudes(19) = 0.D0

 amplitudes(20) = 0.D0

 amplitudes(21) = 0.D0

 amplitudes(22) = 0.D0

 amplitudes(23) = 0.D0

 amplitudes(24) = 0.D0

 amplitudes(25) = 0.D0

 amplitudes(26) = 0.D0

 amplitudes(27) = 0.D0

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

 amplitudes(46) = 0.D0

 amplitudes(47) = 0.D0

 amplitudes(48) = 0.D0

 amplitudes(49) = 0.D0

 amplitudes(50) = 0.D0

 amplitudes(51) = 0.D0

 amplitudes(52) = 0.D0

 amplitudes(53) = 0.D0

 amplitudes(54) = 0.D0

 amplitudes(55) = 0.D0

 amplitudes(56) = 0.D0

 amplitudes(57) = 0.D0

 amplitudes(58) = 0.D0

 amplitudes(59) = 0.D0

 amplitudes(60) = 0.D0

 amplitudes(61) = 0.D0

 amplitudes(62) = 0.D0

 amplitudes(63) = 0.D0

 amplitudes(64) = 0.D0

 amplitudes(65) = 0.D0

 amplitudes(66) = 0.D0

 amplitudes(67) = 0D0

 amplitudes(68) = 0D0

 amplitudes(69) = 0D0

 amplitudes(70) = 0D0

 amplitudes(71) = 0.D0

 amplitudes(72) = 0.D0

 amplitudes(73) = 0.D0

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

 amplitudes(121) = (0.0078125D0*CA22*EL2*MU2*SA12*(A0(MH12) - 1.D0*A0(MU2) - 1.D0*MH12*B0(x, MH12, MU2) + MU2*B0(x, MH12, MU2) + &
  &x*B0(x, MH12, MU2)))/ (MW2*PI2*SB2*SW2*x)

 amplitudes(122) = (0.0078125D0*EL2*MU2*(A0(MH22) - 1.D0*A0(MU2) - 1.D0*MH22*B0(x, MH22, MU2) + MU2*B0(x, MH22, MU2) + x*B0(x, MH&
  &22, MU2))* DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2*x)

 amplitudes(123) = (0.0078125D0*EL2*MU2*(A0(MH32) - 1.D0*A0(MU2) - 1.D0*MH32*B0(x, MH32, MU2) + MU2*B0(x, MH32, MU2) + x*B0(x, MH&
  &32, MU2))* DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2*x)

 amplitudes(124) = (0.0078125D0*EL2*MU2*(-1.D0*A0(MU2) + A0(MZ2) + MU2*B0(x, MU2, MZ2) - 1.D0*MZ2*B0(x, MU2, MZ2) + x*B0(x, MU2, &
  &MZ2)))/(MW2*PI2*SW2*x)

 amplitudes(125) = (0.0078125D0*CB2*EL2*MU2*(A0(MA02) - 1.D0*A0(MU2) - 1.D0*MA02*B0(x, MA02, MU2) + MU2*B0(x, MA02, MU2) + x*B0(x&
  &, MA02, MU2)))/ (MW2*PI2*SB2*SW2*x)

 amplitudes(126) = (0.03125D0*CKM11*CKMC11*((-0.5D0*EL2*MD2*SB2*A0(MD2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD2*&
  &SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*SB2*B0(x, MD2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (&
  &0.5D0*EL2*MD2*SB2*x*B0(x, MD2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MD2, MW2)*DBLE(MD**INT(4.D0&
  &))*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(127) = (0.03125D0*CKM12*CKMC12*((-0.5D0*EL2*MS2*SB2*A0(MS2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MS2*&
  &SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MS2*SB2*B0(x, MS2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (&
  &0.5D0*EL2*MS2*SB2*x*B0(x, MS2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MS2, MW2)*DBLE(MS**INT(4.D0&
  &))*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(128) = (0.03125D0*CKM13*CKMC13*((-0.5D0*EL2*MB2*SB2*A0(MB2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*&
  &SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*SB2*B0(x, MB2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (&
  &0.5D0*EL2*MB2*SB2*x*B0(x, MB2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MB2, MW2)*DBLE(MB**INT(4.D0&
  &))*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(129) = (0.03125D0*CKM11*CKMC11*((-0.5D0*EL2*MD2*SB2*A0(MD2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD2*&
  &SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*MHp2*SB2*B0(x, MD2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/&
  & (MW2*SW2) + (0.5D0*EL2*MD2*SB2*x*B0(x, MD2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MD2, MHp2)*D&
  &BLE(MD**INT(4.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(130) = (0.03125D0*CKM12*CKMC12*((0.5D0*EL2*MS2*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MS2*&
  &SB2*A0(MS2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MHp2*MS2*SB2*B0(x, MHp2, MS2)*DBLE(YukS3Quark2**INT(2.D0)))/ &
  &(MW2*SW2) + (0.5D0*EL2*MS2*SB2*x*B0(x, MHp2, MS2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MHp2, MS2)*DB&
  &LE(MS**INT(4.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(131) = (0.03125D0*CKM13*CKMC13*((-0.5D0*EL2*MB2*SB2*A0(MB2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*&
  &SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*MHp2*SB2*B0(x, MB2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/&
  & (MW2*SW2) + (0.5D0*EL2*MB2*SB2*x*B0(x, MB2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MB2, MHp2)*D&
  &BLE(MB**INT(4.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(132) = (0.027777777777777776D0*EL2*(-1.D0*x - 1.D0*A0(MU2) + MU2*B0(x, 0.D0, MU2) + x*B0(x, 0.D0, MU2)))/(PI2*x)

 amplitudes(133) = (0.001736111111111111D0*EL2*(6.D0*CW2*SW2*x + 6.D0*CW2*SW2*A0(MU2) - 6.D0*CW2*SW2*A0(MZ2) - 6.D0*CW2*MU2*SW2*B&
  &0(x, MU2, MZ2) + 6.D0*CW2*MZ2*SW2*B0(x, MU2, MZ2) - 6.D0*CW2*SW2*x*B0(x, MU2, MZ2) - 9.D0*x*DBLE(CW**INT(4.D0)) - 9.D0*A0(MU2)&
  &*DBLE(CW**INT(4.D0)) + 9.D0*A0(MZ2)*DBLE(CW**INT(4.D0)) + 9.D0*MU2*B0(x, MU2, MZ2)*DBLE(CW**INT(4.D0)) - 9.D0*MZ2*B0(x, MU2, M&
  &Z2)*DBLE(CW**INT(4.D0)) + 9.D0*x*B0(x, MU2, MZ2)*DBLE(CW**INT(4.D0)) - 1.D0*x*DBLE(SW**INT(4.D0)) - 1.D0*A0(MU2)*DBLE(SW**INT(&
  &4.D0)) + A0(MZ2)*DBLE(SW**INT(4.D0)) + MU2*B0(x, MU2, MZ2)*DBLE(SW**INT(4.D0)) - 1.D0*MZ2*B0(x, MU2, MZ2)*DBLE(SW**INT(4.D0)) &
  &+ x*B0(x, MU2, MZ2)*DBLE(SW**INT(4.D0))))/ (CW2*PI2*SW2*x)

 amplitudes(134) = (0.03125D0*CKM11*CKMC11*EL2*(-1.D0*x - 1.D0*A0(MD2) + A0(MW2) + MD2*B0(x, MD2, MW2) - 1.D0*MW2*B0(x, MD2, MW2)&
  & + x*B0(x, MD2, MW2)))/(PI2*SW2*x)

 amplitudes(135) = (0.03125D0*CKM12*CKMC12*EL2*(-1.D0*x - 1.D0*A0(MS2) + A0(MW2) + MS2*B0(x, MS2, MW2) - 1.D0*MW2*B0(x, MS2, MW2)&
  & + x*B0(x, MS2, MW2)))/(PI2*SW2*x)

 amplitudes(136) = (0.03125D0*CKM13*CKMC13*EL2*(-1.D0*x - 1.D0*A0(MB2) + A0(MW2) + MB2*B0(x, MB2, MW2) - 1.D0*MW2*B0(x, MB2, MW2)&
  & + x*B0(x, MB2, MW2)))/(PI2*SW2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,136
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfUULeftAlter = totalAmplitude
end function SelfUULeftAlter

