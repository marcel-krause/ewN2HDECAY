double complex function SelfAALight(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(8)

 amplitudes(1) = (0.027777777777777776D0*EL2*(6.D0*ME2 - 1.D0*x - 6.D0*A0(ME2) + 3.D0*(2.D0*ME2 + x)*B0(x, ME2, ME2)))/PI2

 amplitudes(2) = (0.027777777777777776D0*EL2*(6.D0*MM2 - 1.D0*x - 6.D0*A0(MM2) + 3.D0*(2.D0*MM2 + x)*B0(x, MM2, MM2)))/PI2

 amplitudes(3) = (0.027777777777777776D0*EL2*(6.D0*ML2 - 1.D0*x - 6.D0*A0(ML2) + 3.D0*(2.D0*ML2 + x)*B0(x, ML2, ML2)))/PI2

 amplitudes(4) = (0.037037037037037035D0*EL2*(6.D0*MU2 - 1.D0*x - 6.D0*A0(MU2) + 3.D0*(2.D0*MU2 + x)*B0(x, MU2, MU2)))/PI2

 amplitudes(5) = (0.037037037037037035D0*EL2*(6.D0*MC2 - 1.D0*x - 6.D0*A0(MC2) + 3.D0*(2.D0*MC2 + x)*B0(x, MC2, MC2)))/PI2

 amplitudes(6) = (0.009259259259259259D0*EL2*(6.D0*MD2 - 1.D0*x - 6.D0*A0(MD2) + 3.D0*(2.D0*MD2 + x)*B0(x, MD2, MD2)))/PI2

 amplitudes(7) = (0.009259259259259259D0*EL2*(6.D0*MS2 - 1.D0*x - 6.D0*A0(MS2) + 3.D0*(2.D0*MS2 + x)*B0(x, MS2, MS2)))/PI2

 amplitudes(8) = (0.009259259259259259D0*EL2*(6.D0*MB2 - 1.D0*x - 6.D0*A0(MB2) + 3.D0*(2.D0*MB2 + x)*B0(x, MB2, MB2)))/PI2

  totalAmplitude = (0D0,0D0)
 do j=1,8
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfAALight = totalAmplitude
end function SelfAALight

