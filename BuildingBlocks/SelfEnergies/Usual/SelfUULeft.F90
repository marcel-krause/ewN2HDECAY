double complex function SelfUULeftUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(16)

 amplitudes(1) = (0.0078125D0*CA22*EL2*MU2*SA12*(A0(MH12) - 1.D0*A0(MU2) - 1.D0*MH12*B0(x, MH12, MU2) + MU2*B0(x, MH12, MU2) + x*&
  &B0(x, MH12, MU2)))/ (MW2*PI2*SB2*SW2*x)

 amplitudes(2) = (0.0078125D0*EL2*MU2*(A0(MH22) - 1.D0*A0(MU2) - 1.D0*MH22*B0(x, MH22, MU2) + MU2*B0(x, MH22, MU2) + x*B0(x, MH22&
  &, MU2))* DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2*x)

 amplitudes(3) = (0.0078125D0*EL2*MU2*(A0(MH32) - 1.D0*A0(MU2) - 1.D0*MH32*B0(x, MH32, MU2) + MU2*B0(x, MH32, MU2) + x*B0(x, MH32&
  &, MU2))* DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2*x)

 amplitudes(4) = (0.0078125D0*EL2*MU2*(-1.D0*A0(MU2) + A0(MZ2) + MU2*B0(x, MU2, MZ2) - 1.D0*MZ2*B0(x, MU2, MZ2) + x*B0(x, MU2, MZ&
  &2)))/(MW2*PI2*SW2*x)

 amplitudes(5) = (0.0078125D0*CB2*EL2*MU2*(A0(MA02) - 1.D0*A0(MU2) - 1.D0*MA02*B0(x, MA02, MU2) + MU2*B0(x, MA02, MU2) + x*B0(x, &
  &MA02, MU2)))/ (MW2*PI2*SB2*SW2*x)

 amplitudes(6) = (0.03125D0*CKM11*CKMC11*((-0.5D0*EL2*MD2*SB2*A0(MD2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD2*SB&
  &2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*SB2*B0(x, MD2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (0.&
  &5D0*EL2*MD2*SB2*x*B0(x, MD2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MD2, MW2)*DBLE(MD**INT(4.D0))&
  &*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(7) = (0.03125D0*CKM12*CKMC12*((-0.5D0*EL2*MS2*SB2*A0(MS2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MS2*SB&
  &2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MS2*SB2*B0(x, MS2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (0.&
  &5D0*EL2*MS2*SB2*x*B0(x, MS2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MS2, MW2)*DBLE(MS**INT(4.D0))&
  &*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(8) = (0.03125D0*CKM13*CKMC13*((-0.5D0*EL2*MB2*SB2*A0(MB2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*SB&
  &2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*SB2*B0(x, MB2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (0.&
  &5D0*EL2*MB2*SB2*x*B0(x, MB2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MB2, MW2)*DBLE(MB**INT(4.D0))&
  &*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(9) = (0.03125D0*CKM11*CKMC11*((-0.5D0*EL2*MD2*SB2*A0(MD2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD2*SB&
  &2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*MHp2*SB2*B0(x, MD2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/ (&
  &MW2*SW2) + (0.5D0*EL2*MD2*SB2*x*B0(x, MD2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MD2, MHp2)*DBL&
  &E(MD**INT(4.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(10) = (0.03125D0*CKM12*CKMC12*((0.5D0*EL2*MS2*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MS2*S&
  &B2*A0(MS2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MHp2*MS2*SB2*B0(x, MHp2, MS2)*DBLE(YukS3Quark2**INT(2.D0)))/ (&
  &MW2*SW2) + (0.5D0*EL2*MS2*SB2*x*B0(x, MHp2, MS2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MHp2, MS2)*DBL&
  &E(MS**INT(4.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(11) = (0.03125D0*CKM13*CKMC13*((-0.5D0*EL2*MB2*SB2*A0(MB2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*S&
  &B2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*MHp2*SB2*B0(x, MB2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/ &
  &(MW2*SW2) + (0.5D0*EL2*MB2*SB2*x*B0(x, MB2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MB2, MHp2)*DB&
  &LE(MB**INT(4.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(12) = (0.027777777777777776D0*EL2*(-1.D0*x - 1.D0*A0(MU2) + MU2*B0(x, 0.D0, MU2) + x*B0(x, 0.D0, MU2)))/(PI2*x)

 amplitudes(13) = (0.001736111111111111D0*EL2*(6.D0*CW2*SW2*x + 6.D0*CW2*SW2*A0(MU2) - 6.D0*CW2*SW2*A0(MZ2) - 6.D0*CW2*MU2*SW2*B0&
  &(x, MU2, MZ2) + 6.D0*CW2*MZ2*SW2*B0(x, MU2, MZ2) - 6.D0*CW2*SW2*x*B0(x, MU2, MZ2) - 9.D0*x*DBLE(CW**INT(4.D0)) - 9.D0*A0(MU2)*&
  &DBLE(CW**INT(4.D0)) + 9.D0*A0(MZ2)*DBLE(CW**INT(4.D0)) + 9.D0*MU2*B0(x, MU2, MZ2)*DBLE(CW**INT(4.D0)) - 9.D0*MZ2*B0(x, MU2, MZ&
  &2)*DBLE(CW**INT(4.D0)) + 9.D0*x*B0(x, MU2, MZ2)*DBLE(CW**INT(4.D0)) - 1.D0*x*DBLE(SW**INT(4.D0)) - 1.D0*A0(MU2)*DBLE(SW**INT(4&
  &.D0)) + A0(MZ2)*DBLE(SW**INT(4.D0)) + MU2*B0(x, MU2, MZ2)*DBLE(SW**INT(4.D0)) - 1.D0*MZ2*B0(x, MU2, MZ2)*DBLE(SW**INT(4.D0)) +&
  & x*B0(x, MU2, MZ2)*DBLE(SW**INT(4.D0))))/ (CW2*PI2*SW2*x)

 amplitudes(14) = (0.03125D0*CKM11*CKMC11*EL2*(-1.D0*x - 1.D0*A0(MD2) + A0(MW2) + MD2*B0(x, MD2, MW2) - 1.D0*MW2*B0(x, MD2, MW2) &
  &+ x*B0(x, MD2, MW2)))/(PI2*SW2*x)

 amplitudes(15) = (0.03125D0*CKM12*CKMC12*EL2*(-1.D0*x - 1.D0*A0(MS2) + A0(MW2) + MS2*B0(x, MS2, MW2) - 1.D0*MW2*B0(x, MS2, MW2) &
  &+ x*B0(x, MS2, MW2)))/(PI2*SW2*x)

 amplitudes(16) = (0.03125D0*CKM13*CKMC13*EL2*(-1.D0*x - 1.D0*A0(MB2) + A0(MW2) + MB2*B0(x, MB2, MW2) - 1.D0*MW2*B0(x, MB2, MW2) &
  &+ x*B0(x, MB2, MW2)))/(PI2*SW2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,16
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfUULeftUsual = totalAmplitude
end function SelfUULeftUsual

