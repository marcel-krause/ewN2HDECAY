double complex function SelfNeuTNeuTRightUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(6)

 amplitudes(1) = 0.D0

 amplitudes(2) = 0.D0

 amplitudes(3) = 0.D0

 amplitudes(4) = 0.D0

 amplitudes(5) = 0.D0

 amplitudes(6) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,6
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfNeuTNeuTRightUsual = totalAmplitude
end function SelfNeuTNeuTRightUsual

