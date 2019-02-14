double complex function SelfH1H2Add(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 double complex :: totalAmplitude

 totalAmplitude = (-0.03125D0*EL2*(0.5D0*(-1.D0*MH12 - 1.D0*MH22) + x)*(-1.D0*CA2*(-1.D0*CB*SA1 + CA1*SB)*(SA2*SA3*(-1.D0*CB*SA1 &
  &+ CA1*SB) + CA3*(CA1*CB + SA1*SB))* B0(x, MA02, MZ2) + 2.D0*CW2*(-1.D0*CA2*(-1.D0*CB*SA1 + CA1*SB)*(SA2*SA3*(-1.D0*CB*SA1 + CA&
  &1*SB) + CA3*(CA1*CB + SA1*SB))*B0(x, MHp2, MW2) + CA2*(CA1*CB + SA1*SB)*(CA3*(-1.D0*CB*SA1 + CA1*SB) - 1.D0*SA2*SA3*(CA1*CB + &
  &SA1*SB))*B0(x, MW2, MW2)) + CA2*(CA1*CB + SA1*SB)*(CA3*(-1.D0*CB*SA1 + CA1*SB) - 1.D0*SA2*SA3*(CA1*CB + SA1*SB))*B0(x, MZ2, MZ&
  &2)))/(CW2*PI2*SW2)

 SelfH1H2Add = totalAmplitude
end function SelfH1H2Add

