double complex function DSelfDDRight(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(16)

 amplitudes(1) = (0.0078125D0*EL2*MD2*(B0(x, MD2, MH12) + MD2*DB0(x, MD2, MH12) - 1.D0*MH12*DB0(x, MD2, MH12) + x*DB0(x, MD2, MH1&
  &2))* DBLE(YukS1Quark1**INT(2.D0)))/(MW2*PI2*SW2*x) - (0.0078125D0*EL2*MD2*(-1.D0*A0(MD2) + A0(MH12) + MD2*B0(x, MD2, MH12) - 1&
  &.D0*MH12*B0(x, MD2, MH12) + x*B0(x, MD2, MH12))*DBLE(x**INT(-2.D0))* DBLE(YukS1Quark1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(2) = (0.0078125D0*EL2*MD2*(B0(x, MD2, MH22) + MD2*DB0(x, MD2, MH22) - 1.D0*MH22*DB0(x, MD2, MH22) + x*DB0(x, MD2, MH2&
  &2))* DBLE(YukS1Quark2**INT(2.D0)))/(MW2*PI2*SW2*x) - (0.0078125D0*EL2*MD2*(-1.D0*A0(MD2) + A0(MH22) + MD2*B0(x, MD2, MH22) - 1&
  &.D0*MH22*B0(x, MD2, MH22) + x*B0(x, MD2, MH22))*DBLE(x**INT(-2.D0))* DBLE(YukS1Quark2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(3) = (0.0078125D0*EL2*MD2*(B0(x, MD2, MH32) + MD2*DB0(x, MD2, MH32) - 1.D0*MH32*DB0(x, MD2, MH32) + x*DB0(x, MD2, MH3&
  &2))* DBLE(YukS1Quark3**INT(2.D0)))/(MW2*PI2*SW2*x) - (0.0078125D0*EL2*MD2*(-1.D0*A0(MD2) + A0(MH32) + MD2*B0(x, MD2, MH32) - 1&
  &.D0*MH32*B0(x, MD2, MH32) + x*B0(x, MD2, MH32))*DBLE(x**INT(-2.D0))* DBLE(YukS1Quark3**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(4) = (0.0078125D0*EL2*MD2*(B0(x, MD2, MZ2) + MD2*DB0(x, MD2, MZ2) - 1.D0*MZ2*DB0(x, MD2, MZ2) + x*DB0(x, MD2, MZ2))*D&
  &BLE(YukS2Quark1**INT(2.D0)))/ (MW2*PI2*SW2*x) - (0.0078125D0*EL2*MD2*(-1.D0*A0(MD2) + A0(MZ2) + MD2*B0(x, MD2, MZ2) - 1.D0*MZ2&
  &*B0(x, MD2, MZ2) + x*B0(x, MD2, MZ2))* DBLE(x**INT(-2.D0))*DBLE(YukS2Quark1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(5) = (0.0078125D0*EL2*MD2*(B0(x, MA02, MD2) - 1.D0*MA02*DB0(x, MA02, MD2) + MD2*DB0(x, MA02, MD2) + x*DB0(x, MA02, MD&
  &2))* DBLE(YukS2Quark2**INT(2.D0)))/(MW2*PI2*SW2*x) - (0.0078125D0*EL2*MD2*(A0(MA02) - 1.D0*A0(MD2) - 1.D0*MA02*B0(x, MA02, MD2&
  &) + MD2*B0(x, MA02, MD2) + x*B0(x, MA02, MD2))*DBLE(x**INT(-2.D0))* DBLE(YukS2Quark2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(6) = (-0.03125D0*CKM11*CKMC11*DBLE(x**INT(-2.D0))*((-0.5D0*EL2*MD2*SB2*A0(MU2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2&
  &) + (0.5D0*EL2*MD2*SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*SB2*B0(x, MU2, MW2)*DBLE(YukS3Quark1**&
  &INT(2.D0)))/SW2 + (0.5D0*EL2*MD2*MU2*SB2*B0(x, MU2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*x*B0(x, &
  &MU2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM11*CKMC11*((0.5D0*EL2*MD2*SB2*B0(x, MU2, MW2)*DB&
  &LE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*SB2*DB0(x, MU2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (0.5D0*EL2*MD2&
  &*MU2*SB2*DB0(x, MU2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*x*DB0(x, MU2, MW2)*DBLE(YukS3Quark1**IN&
  &T(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(7) = (-0.03125D0*CKM21*CKMC21*DBLE(x**INT(-2.D0))*((-0.5D0*EL2*MD2*SB2*A0(MC2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2&
  &) + (0.5D0*EL2*MD2*SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*SB2*B0(x, MC2, MW2)*DBLE(YukS3Quark1**&
  &INT(2.D0)))/SW2 + (0.5D0*EL2*MC2*MD2*SB2*B0(x, MC2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*x*B0(x, &
  &MC2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM21*CKMC21*((0.5D0*EL2*MD2*SB2*B0(x, MC2, MW2)*DB&
  &LE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*SB2*DB0(x, MC2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (0.5D0*EL2*MC2&
  &*MD2*SB2*DB0(x, MC2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*x*DB0(x, MC2, MW2)*DBLE(YukS3Quark1**IN&
  &T(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(8) = (-0.03125D0*CKM31*CKMC31*DBLE(x**INT(-2.D0))*((-0.5D0*EL2*MD2*SB2*A0(MT2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2&
  &) + (0.5D0*EL2*MD2*SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*SB2*B0(x, MT2, MW2)*DBLE(YukS3Quark1**&
  &INT(2.D0)))/SW2 + (0.5D0*EL2*MD2*MT2*SB2*B0(x, MT2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*x*B0(x, &
  &MT2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM31*CKMC31*((0.5D0*EL2*MD2*SB2*B0(x, MT2, MW2)*DB&
  &LE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*SB2*DB0(x, MT2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (0.5D0*EL2*MD2&
  &*MT2*SB2*DB0(x, MT2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*x*DB0(x, MT2, MW2)*DBLE(YukS3Quark1**IN&
  &T(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(9) = (-0.03125D0*CKM11*CKMC11*DBLE(x**INT(-2.D0))*((0.5D0*EL2*MD2*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2&
  &) - (0.5D0*EL2*MD2*SB2*A0(MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*MHp2*SB2*B0(x, MHp2, MU2)*DBLE(YukS3Qu&
  &ark2**INT(2.D0)))/ (MW2*SW2) + (0.5D0*EL2*MD2*MU2*SB2*B0(x, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD&
  &2*SB2*x*B0(x, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM11*CKMC11*((0.5D0*EL2*MD2*SB2*B0(&
  &x, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*MHp2*SB2*DB0(x, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0))&
  &)/(MW2*SW2) + (0.5D0*EL2*MD2*MU2*SB2*DB0(x, MHp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*x*DB0(x, M&
  &Hp2, MU2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(10) = (-0.03125D0*CKM21*CKMC21*DBLE(x**INT(-2.D0))*((-0.5D0*EL2*MD2*SB2*A0(MC2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW&
  &2) + (0.5D0*EL2*MD2*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MC2*MD2*SB2*B0(x, MC2, MHp2)*DBLE(YukS3Q&
  &uark2**INT(2.D0)))/ (MW2*SW2) - (0.5D0*EL2*MD2*MHp2*SB2*B0(x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*&
  &MD2*SB2*x*B0(x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM21*CKMC21*((0.5D0*EL2*MD2*SB2*B&
  &0(x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MC2*MD2*SB2*DB0(x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)&
  &))/(MW2*SW2) - (0.5D0*EL2*MD2*MHp2*SB2*DB0(x, MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*x*DB0(x,&
  & MC2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(11) = (-0.03125D0*CKM31*CKMC31*DBLE(x**INT(-2.D0))*((0.5D0*EL2*MD2*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW&
  &2) - (0.5D0*EL2*MD2*SB2*A0(MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*MHp2*SB2*B0(x, MHp2, MT2)*DBLE(YukS3Q&
  &uark2**INT(2.D0)))/ (MW2*SW2) + (0.5D0*EL2*MD2*MT2*SB2*B0(x, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*M&
  &D2*SB2*x*B0(x, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM31*CKMC31*((0.5D0*EL2*MD2*SB2*B0&
  &(x, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*MHp2*SB2*DB0(x, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)&
  &))/(MW2*SW2) + (0.5D0*EL2*MD2*MT2*SB2*DB0(x, MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*x*DB0(x, &
  &MHp2, MT2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(12) = (0.006944444444444444D0*EL2*(-1.D0 + B0(x, 0.D0, MD2) + MD2*DB0(x, 0.D0, MD2) + x*DB0(x, 0.D0, MD2)))/(PI2*x) -&
  & (0.006944444444444444D0*EL2*(-1.D0*x - 1.D0*A0(MD2) + MD2*B0(x, 0.D0, MD2) + x*B0(x, 0.D0, MD2))*DBLE(x**INT(-2.D0)))/PI2

 amplitudes(13) = (0.001736111111111111D0*EL2*(-4.D0*DBLE(SW**INT(4.D0)) + 4.D0*B0(x, MD2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*MD2*DB&
  &0(x, MD2, MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*MZ2*DB0(x, MD2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*x*DB0(x, MD2, MZ2)*DBLE(SW**INT(4.D0&
  &))))/(CW2*PI2*SW2*x) - (0.001736111111111111D0*EL2*(-4.D0*x*DBLE(SW**INT(4.D0)) - 4.D0*A0(MD2)*DBLE(SW**INT(4.D0)) + 4.D0*A0(M&
  &Z2)*DBLE(SW**INT(4.D0)) + 4.D0*MD2*B0(x, MD2, MZ2)*DBLE(SW**INT(4.D0)) - 4.D0*MZ2*B0(x, MD2, MZ2)*DBLE(SW**INT(4.D0)) + 4.D0*x&
  &*B0(x, MD2, MZ2)*DBLE(SW**INT(4.D0)))* DBLE(x**INT(-2.D0)))/(CW2*PI2*SW2)

 amplitudes(14) = 0.D0

 amplitudes(15) = 0.D0

 amplitudes(16) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,16
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfDDRight = totalAmplitude
end function DSelfDDRight

