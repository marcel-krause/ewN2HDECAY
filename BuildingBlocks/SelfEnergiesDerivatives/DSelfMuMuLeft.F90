double complex function DSelfMuMuLeft(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(12)

 amplitudes(1) = (0.0078125D0*EL2*MM2*(B0(x, MH12, MM2) - 1.D0*MH12*DB0(x, MH12, MM2) + MM2*DB0(x, MH12, MM2) + x*DB0(x, MH12, MM&
  &2))* DBLE(YukS1Lep1**INT(2.D0)))/(MW2*PI2*SW2*x) - (0.0078125D0*EL2*MM2*(A0(MH12) - 1.D0*A0(MM2) - 1.D0*MH12*B0(x, MH12, MM2) &
  &+ MM2*B0(x, MH12, MM2) + x*B0(x, MH12, MM2))*DBLE(x**INT(-2.D0))* DBLE(YukS1Lep1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(2) = (0.0078125D0*EL2*MM2*(B0(x, MH22, MM2) - 1.D0*MH22*DB0(x, MH22, MM2) + MM2*DB0(x, MH22, MM2) + x*DB0(x, MH22, MM&
  &2))* DBLE(YukS1Lep2**INT(2.D0)))/(MW2*PI2*SW2*x) - (0.0078125D0*EL2*MM2*(A0(MH22) - 1.D0*A0(MM2) - 1.D0*MH22*B0(x, MH22, MM2) &
  &+ MM2*B0(x, MH22, MM2) + x*B0(x, MH22, MM2))*DBLE(x**INT(-2.D0))* DBLE(YukS1Lep2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(3) = (0.0078125D0*EL2*MM2*(B0(x, MH32, MM2) - 1.D0*MH32*DB0(x, MH32, MM2) + MM2*DB0(x, MH32, MM2) + x*DB0(x, MH32, MM&
  &2))* DBLE(YukS1Lep3**INT(2.D0)))/(MW2*PI2*SW2*x) - (0.0078125D0*EL2*MM2*(A0(MH32) - 1.D0*A0(MM2) - 1.D0*MH32*B0(x, MH32, MM2) &
  &+ MM2*B0(x, MH32, MM2) + x*B0(x, MH32, MM2))*DBLE(x**INT(-2.D0))* DBLE(YukS1Lep3**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(4) = (0.0078125D0*EL2*MM2*(B0(x, MM2, MZ2) + MM2*DB0(x, MM2, MZ2) - 1.D0*MZ2*DB0(x, MM2, MZ2) + x*DB0(x, MM2, MZ2))*D&
  &BLE(YukS2Lep1**INT(2.D0)))/ (MW2*PI2*SW2*x) - (0.0078125D0*EL2*MM2*(-1.D0*A0(MM2) + A0(MZ2) + MM2*B0(x, MM2, MZ2) - 1.D0*MZ2*B&
  &0(x, MM2, MZ2) + x*B0(x, MM2, MZ2))* DBLE(x**INT(-2.D0))*DBLE(YukS2Lep1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(5) = (0.0078125D0*EL2*MM2*(B0(x, MA02, MM2) - 1.D0*MA02*DB0(x, MA02, MM2) + MM2*DB0(x, MA02, MM2) + x*DB0(x, MA02, MM&
  &2))* DBLE(YukS2Lep2**INT(2.D0)))/(MW2*PI2*SW2*x) - (0.0078125D0*EL2*MM2*(A0(MA02) - 1.D0*A0(MM2) - 1.D0*MA02*B0(x, MA02, MM2) &
  &+ MM2*B0(x, MA02, MM2) + x*B0(x, MA02, MM2))*DBLE(x**INT(-2.D0))* DBLE(YukS2Lep2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(6) = 0.D0

 amplitudes(7) = 0.D0

 amplitudes(8) = (0.0625D0*EL2*(-1.D0 + B0(x, 0.D0, MM2) + MM2*DB0(x, 0.D0, MM2) + x*DB0(x, 0.D0, MM2)))/(PI2*x) - (0.0625D0*EL2*&
  &(-1.D0*x - 1.D0*A0(MM2) + MM2*B0(x, 0.D0, MM2) + x*B0(x, 0.D0, MM2))*DBLE(x**INT(-2.D0)))/PI2

 amplitudes(9) = (0.015625D0*EL2*(2.D0*CW2*SW2 - 2.D0*CW2*SW2*B0(x, MM2, MZ2) - 2.D0*CW2*MM2*SW2*DB0(x, MM2, MZ2) + 2.D0*CW2*MZ2*&
  &SW2*DB0(x, MM2, MZ2) - 2.D0*CW2*SW2*x*DB0(x, MM2, MZ2) - 1.D0*DBLE(CW**INT(4.D0)) + B0(x, MM2, MZ2)*DBLE(CW**INT(4.D0)) + MM2*&
  &DB0(x, MM2, MZ2)*DBLE(CW**INT(4.D0)) - 1.D0*MZ2*DB0(x, MM2, MZ2)*DBLE(CW**INT(4.D0)) + x*DB0(x, MM2, MZ2)*DBLE(CW**INT(4.D0)) &
  &- 1.D0*DBLE(SW**INT(4.D0)) + B0(x, MM2, MZ2)*DBLE(SW**INT(4.D0)) + MM2*DB0(x, MM2, MZ2)*DBLE(SW**INT(4.D0)) - 1.D0*MZ2*DB0(x, &
  &MM2, MZ2)*DBLE(SW**INT(4.D0)) + x*DB0(x, MM2, MZ2)*DBLE(SW**INT(4.D0))))/(CW2*PI2*SW2*x) - (0.015625D0*EL2*(2.D0*CW2*SW2*x + 2&
  &.D0*CW2*SW2*A0(MM2) - 2.D0*CW2*SW2*A0(MZ2) - 2.D0*CW2*MM2*SW2*B0(x, MM2, MZ2) + 2.D0*CW2*MZ2*SW2*B0(x, MM2, MZ2) - 2.D0*CW2*SW&
  &2*x*B0(x, MM2, MZ2) - 1.D0*x*DBLE(CW**INT(4.D0)) - 1.D0*A0(MM2)*DBLE(CW**INT(4.D0)) + A0(MZ2)*DBLE(CW**INT(4.D0)) + MM2*B0(x, &
  &MM2, MZ2)*DBLE(CW**INT(4.D0)) - 1.D0*MZ2*B0(x, MM2, MZ2)*DBLE(CW**INT(4.D0)) + x*B0(x, MM2, MZ2)*DBLE(CW**INT(4.D0)) - 1.D0*x*&
  &DBLE(SW**INT(4.D0)) - 1.D0*A0(MM2)*DBLE(SW**INT(4.D0)) + A0(MZ2)*DBLE(SW**INT(4.D0)) + MM2*B0(x, MM2, MZ2)*DBLE(SW**INT(4.D0))&
  & - 1.D0*MZ2*B0(x, MM2, MZ2)*DBLE(SW**INT(4.D0)) + x*B0(x, MM2, MZ2)*DBLE(SW**INT(4.D0)))* DBLE(x**INT(-2.D0)))/(CW2*PI2*SW2)

 amplitudes(10) = 0.D0

 amplitudes(11) = (0.03125D0*EL2*(-1.D0 + B0(x, 0.D0, MW2) - 1.D0*MW2*DB0(x, 0.D0, MW2) + x*DB0(x, 0.D0, MW2)))/(PI2*SW2*x) - (0.&
  &03125D0*EL2*(-1.D0*x + A0(MW2) - 1.D0*MW2*B0(x, 0.D0, MW2) + x*B0(x, 0.D0, MW2))*DBLE(x**INT(-2.D0)))/(PI2*SW2)

 amplitudes(12) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,12
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfMuMuLeft = totalAmplitude
end function DSelfMuMuLeft

