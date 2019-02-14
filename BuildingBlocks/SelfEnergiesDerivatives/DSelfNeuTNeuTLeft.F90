double complex function DSelfNeuTNeuTLeft(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(6)

 amplitudes(1) = (-0.03125D0*EL2*ML2*DB1(x, ML2, MW2)*DBLE(YukS3Lep1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(2) = (-0.03125D0*EL2*ML2*DB1(x, ML2, MHp2)*DBLE(YukS3Lep2**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(3) = (-0.03125D0*EL2*DB1(x, 0.D0, MZ2)*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(4) = 0.D0

 amplitudes(5) = 0.D0

 amplitudes(6) = (-0.0625D0*EL2*DB1(x, ML2, MW2))/(PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,6
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfNeuTNeuTLeft = totalAmplitude
end function DSelfNeuTNeuTLeft

