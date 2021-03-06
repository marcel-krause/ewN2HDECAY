double complex function SelfMuMuLeftUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(12)

 amplitudes(1) = (0.0078125D0*EL2*MM2*(A0(MH12) - 1.D0*A0(MM2) - 1.D0*MH12*B0(x, MH12, MM2) + MM2*B0(x, MH12, MM2) + x*B0(x, MH12&
  &, MM2))* DBLE(YukS1Lep1**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(2) = (0.0078125D0*EL2*MM2*(A0(MH22) - 1.D0*A0(MM2) - 1.D0*MH22*B0(x, MH22, MM2) + MM2*B0(x, MH22, MM2) + x*B0(x, MH22&
  &, MM2))* DBLE(YukS1Lep2**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(3) = (0.0078125D0*EL2*MM2*(A0(MH32) - 1.D0*A0(MM2) - 1.D0*MH32*B0(x, MH32, MM2) + MM2*B0(x, MH32, MM2) + x*B0(x, MH32&
  &, MM2))* DBLE(YukS1Lep3**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(4) = (0.0078125D0*EL2*MM2*(-1.D0*A0(MM2) + A0(MZ2) + MM2*B0(x, MM2, MZ2) - 1.D0*MZ2*B0(x, MM2, MZ2) + x*B0(x, MM2, MZ&
  &2))*DBLE(YukS2Lep1**INT(2.D0)))/ (MW2*PI2*SW2*x)

 amplitudes(5) = (0.0078125D0*EL2*MM2*(A0(MA02) - 1.D0*A0(MM2) - 1.D0*MA02*B0(x, MA02, MM2) + MM2*B0(x, MA02, MM2) + x*B0(x, MA02&
  &, MM2))* DBLE(YukS2Lep2**INT(2.D0)))/(MW2*PI2*SW2*x)

 amplitudes(6) = 0.D0

 amplitudes(7) = 0.D0

 amplitudes(8) = (0.0625D0*EL2*(-1.D0*x - 1.D0*A0(MM2) + MM2*B0(x, 0.D0, MM2) + x*B0(x, 0.D0, MM2)))/(PI2*x)

 amplitudes(9) = (0.015625D0*EL2*(2.D0*CW2*SW2*x + 2.D0*CW2*SW2*A0(MM2) - 2.D0*CW2*SW2*A0(MZ2) - 2.D0*CW2*MM2*SW2*B0(x, MM2, MZ2)&
  & + 2.D0*CW2*MZ2*SW2*B0(x, MM2, MZ2) - 2.D0*CW2*SW2*x*B0(x, MM2, MZ2) - 1.D0*x*DBLE(CW**INT(4.D0)) - 1.D0*A0(MM2)*DBLE(CW**INT(&
  &4.D0)) + A0(MZ2)*DBLE(CW**INT(4.D0)) + MM2*B0(x, MM2, MZ2)*DBLE(CW**INT(4.D0)) - 1.D0*MZ2*B0(x, MM2, MZ2)*DBLE(CW**INT(4.D0)) &
  &+ x*B0(x, MM2, MZ2)*DBLE(CW**INT(4.D0)) - 1.D0*x*DBLE(SW**INT(4.D0)) - 1.D0*A0(MM2)*DBLE(SW**INT(4.D0)) + A0(MZ2)*DBLE(SW**INT&
  &(4.D0)) + MM2*B0(x, MM2, MZ2)*DBLE(SW**INT(4.D0)) - 1.D0*MZ2*B0(x, MM2, MZ2)*DBLE(SW**INT(4.D0)) + x*B0(x, MM2, MZ2)*DBLE(SW**&
  &INT(4.D0))))/ (CW2*PI2*SW2*x)

 amplitudes(10) = 0.D0

 amplitudes(11) = (0.03125D0*EL2*(-1.D0*x + A0(MW2) - 1.D0*MW2*B0(x, 0.D0, MW2) + x*B0(x, 0.D0, MW2)))/(PI2*SW2*x)

 amplitudes(12) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,12
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfMuMuLeftUsual = totalAmplitude
end function SelfMuMuLeftUsual

