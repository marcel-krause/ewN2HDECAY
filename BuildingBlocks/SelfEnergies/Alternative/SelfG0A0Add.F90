double complex function SelfG0A0Add(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 double complex :: totalAmplitude

 totalAmplitude = (-0.03125D0*EL2*(-0.5D0*MA02 + x)*(-1.D0*CA22*(-1.D0*CB*SA1 + CA1*SB)*(CA1*CB + SA1*SB)*B0(x, MH12, MZ2) + (SA2&
  &*SA3*(-1.D0*CB*SA1 + CA1*SB) + CA3*(CA1*CB + SA1*SB))*(CA3*(-1.D0*CB*SA1 + CA1*SB) - 1.D0*SA2*SA3*(CA1*CB + SA1*SB))*B0(x, MH2&
  &2, MZ2) + (-1.D0*SA3*(-1.D0*CB*SA1 + CA1*SB) - 1.D0*CA3*SA2*(CA1*CB + SA1*SB))*(CA3*SA2*(-1.D0*CB*SA1 + CA1*SB) - 1.D0*SA3*(CA&
  &1*CB + SA1*SB))* B0(x, MH32, MZ2)))/(CW2*PI2*SW2)

 SelfG0A0Add = totalAmplitude
end function SelfG0A0Add

