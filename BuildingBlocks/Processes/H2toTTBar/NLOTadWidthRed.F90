double complex function H2toTTBarTad()
 use constants
 implicit none
#include "looptools.h"
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(1)

 amplitudes(1) = (0D0,0D0)

  totalAmplitude = (0D0,0D0)
 do j=1,1
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do

 H2toTTBarTad = totalAmplitude
end function H2toTTBarTad