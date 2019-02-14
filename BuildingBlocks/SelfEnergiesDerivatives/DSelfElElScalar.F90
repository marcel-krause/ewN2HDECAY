double complex function DSelfElElScalar(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(12)

 amplitudes(1) = (0.015625D0*EL2*ME2*DB0(x, ME2, MH12)*DBLE(YukS1Lep1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(2) = (0.015625D0*EL2*ME2*DB0(x, ME2, MH22)*DBLE(YukS1Lep2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(3) = (0.015625D0*EL2*ME2*DB0(x, ME2, MH32)*DBLE(YukS1Lep3**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(4) = (-0.015625D0*EL2*ME2*DB0(x, ME2, MZ2)*DBLE(YukS2Lep1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(5) = (-0.015625D0*EL2*ME2*DB0(x, MA02, ME2)*DBLE(YukS2Lep2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(6) = 0.D0

 amplitudes(7) = 0.D0

 amplitudes(8) = (-0.25D0*EL2*DB0(x, 0.D0, ME2))/PI2

 amplitudes(9) = (0.125D0*EL2*(CW2 - 1.D0*SW2)*DB0(x, ME2, MZ2))/(CW2*PI2)

 amplitudes(10) = 0.D0

 amplitudes(11) = 0.D0

 amplitudes(12) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,12
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfElElScalar = totalAmplitude
end function DSelfElElScalar

