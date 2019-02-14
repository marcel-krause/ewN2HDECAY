double complex function SelfDDLeftUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(16)

 amplitudes(1) = (0.0078125D0*EL2*MD2*(-1.D0*A0(MD2) + A0(MH12) + MD2*B0(x, MD2, MH12) - 1.D0*MH12*B0(x, MD2, MH12) + x*B0(x, MD2&
  &, MH12))* DBLE(YukS1Quark1**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(2) = (0.0078125D0*EL2*MD2*(-1.D0*A0(MD2) + A0(MH22) + MD2*B0(x, MD2, MH22) - 1.D0*MH22*B0(x, MD2, MH22) + x*B0(x, MD2&
  &, MH22))* DBLE(YukS1Quark2**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(3) = (0.0078125D0*EL2*MD2*(-1.D0*A0(MD2) + A0(MH32) + MD2*B0(x, MD2, MH32) - 1.D0*MH32*B0(x, MD2, MH32) + x*B0(x, MD2&
  &, MH32))* DBLE(YukS1Quark3**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(4) = (0.0078125D0*EL2*MD2*(-1.D0*A0(MD2) + A0(MZ2) + MD2*B0(x, MD2, MZ2) - 1.D0*MZ2*B0(x, MD2, MZ2) + x*B0(x, MD2, MZ&
  &2))* DBLE(YukS2Quark1**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(5) = (0.0078125D0*EL2*MD2*(A0(MA02) - 1.D0*A0(MD2) - 1.D0*MA02*B0(x, MA02, MD2) + MD2*B0(x, MA02, MD2) + x*B0(x, MA02&
  &, MD2))* DBLE(YukS2Quark2**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(6) = (0.03125D0*CKM11*CKMC11*((-0.5D0*EL2*MU2*SB2*A0(MU2))/(MW2*SW2) + (0.5D0*EL2*MU2*SB2*A0(MW2))/(MW2*SW2) - (0.5D0&
  &*EL2*MU2*SB2*B0(x, MU2, MW2))/SW2 + (0.5D0*EL2*MU2*SB2*x*B0(x, MU2, MW2))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MU2, MW2)*DBLE(MU**&
  &INT(4.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(7) = (0.03125D0*CKM21*CKMC21*((-0.5D0*EL2*MC2*SB2*A0(MC2))/(MW2*SW2) + (0.5D0*EL2*MC2*SB2*A0(MW2))/(MW2*SW2) - (0.5D0&
  &*EL2*MC2*SB2*B0(x, MC2, MW2))/SW2 + (0.5D0*EL2*MC2*SB2*x*B0(x, MC2, MW2))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MC2, MW2)*DBLE(MC**&
  &INT(4.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(8) = (0.03125D0*CKM31*CKMC31*((-0.5D0*EL2*MT2*SB2*A0(MT2))/(MW2*SW2) + (0.5D0*EL2*MT2*SB2*A0(MW2))/(MW2*SW2) - (0.5D0&
  &*EL2*MT2*SB2*B0(x, MT2, MW2))/SW2 + (0.5D0*EL2*MT2*SB2*x*B0(x, MT2, MW2))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MT2, MW2)*DBLE(MT**&
  &INT(4.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(9) = (0.03125D0*CKM11*CKMC11*((0.5D0*CB2*EL2*MU2*A0(MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MU2*A0(MU2))/(MW2*SW2) - (0.5D0&
  &*CB2*EL2*MHp2*MU2*B0(x, MHp2, MU2))/(MW2*SW2) + (0.5D0*CB2*EL2*MU2*x*B0(x, MHp2, MU2))/(MW2*SW2) + (0.5D0*CB2*EL2*B0(x, MHp2, &
  &MU2)*DBLE(MU**INT(4.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(10) = (0.03125D0*CKM21*CKMC21*((-0.5D0*CB2*EL2*MC2*A0(MC2))/(MW2*SW2) + (0.5D0*CB2*EL2*MC2*A0(MHp2))/(MW2*SW2) - (0.5&
  &D0*CB2*EL2*MC2*MHp2*B0(x, MC2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*MC2*x*B0(x, MC2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*B0(x, MC2,&
  & MHp2)*DBLE(MC**INT(4.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(11) = (0.03125D0*CKM31*CKMC31*((0.5D0*CB2*EL2*MT2*A0(MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MT2*A0(MT2))/(MW2*SW2) - (0.5D&
  &0*CB2*EL2*MHp2*MT2*B0(x, MHp2, MT2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT2*x*B0(x, MHp2, MT2))/(MW2*SW2) + (0.5D0*CB2*EL2*B0(x, MHp2,&
  & MT2)*DBLE(MT**INT(4.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(12) = (0.006944444444444444D0*EL2*(-1.D0*x - 1.D0*A0(MD2) + MD2*B0(x, 0.D0, MD2) + x*B0(x, 0.D0, MD2)))/(PI2*x)

 amplitudes(13) = (0.001736111111111111D0*EL2*(-6.D0*CW2*SW2*x - 6.D0*CW2*SW2*A0(MD2) + 6.D0*CW2*SW2*A0(MZ2) + 6.D0*CW2*MD2*SW2*B&
  &0(x, MD2, MZ2) - 6.D0*CW2*MZ2*SW2*B0(x, MD2, MZ2) + 6.D0*CW2*SW2*x*B0(x, MD2, MZ2) - 9.D0*x*DBLE(CW**INT(4.D0)) - 9.D0*A0(MD2)&
  &*DBLE(CW**INT(4.D0)) + 9.D0*A0(MZ2)*DBLE(CW**INT(4.D0)) + 9.D0*MD2*B0(x, MD2, MZ2)*DBLE(CW**INT(4.D0)) - 9.D0*MZ2*B0(x, MD2, M&
  &Z2)*DBLE(CW**INT(4.D0)) + 9.D0*x*B0(x, MD2, MZ2)*DBLE(CW**INT(4.D0)) - 1.D0*x*DBLE(SW**INT(4.D0)) - 1.D0*A0(MD2)*DBLE(SW**INT(&
  &4.D0)) + A0(MZ2)*DBLE(SW**INT(4.D0)) + MD2*B0(x, MD2, MZ2)*DBLE(SW**INT(4.D0)) - 1.D0*MZ2*B0(x, MD2, MZ2)*DBLE(SW**INT(4.D0)) &
  &+ x*B0(x, MD2, MZ2)*DBLE(SW**INT(4.D0))))/ (CW2*PI2*SW2*x)

 amplitudes(14) = (0.03125D0*CKM11*CKMC11*EL2*(-1.D0*x - 1.D0*A0(MU2) + A0(MW2) + MU2*B0(x, MU2, MW2) - 1.D0*MW2*B0(x, MU2, MW2) &
  &+ x*B0(x, MU2, MW2)))/(PI2*SW2*x)

 amplitudes(15) = (0.03125D0*CKM21*CKMC21*EL2*(-1.D0*x - 1.D0*A0(MC2) + A0(MW2) + MC2*B0(x, MC2, MW2) - 1.D0*MW2*B0(x, MC2, MW2) &
  &+ x*B0(x, MC2, MW2)))/(PI2*SW2*x)

 amplitudes(16) = (0.03125D0*CKM31*CKMC31*EL2*(-1.D0*x - 1.D0*A0(MT2) + A0(MW2) + MT2*B0(x, MT2, MW2) - 1.D0*MW2*B0(x, MT2, MW2) &
  &+ x*B0(x, MT2, MW2)))/(PI2*SW2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,16
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfDDLeftUsual = totalAmplitude
end function SelfDDLeftUsual

