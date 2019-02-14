double complex function SelfAZ0Usual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(21)

 amplitudes(1) = (0.0625D0*EL2*(CB2 + SB2)*(CW2 - 1.D0*SW2)*A0(MW2))/(CW*PI2*SW)

 amplitudes(2) = (0.0625D0*EL2*(CB2 + SB2)*(CW2 - 1.D0*SW2)*A0(MHp2))/(CW*PI2*SW)

 amplitudes(3) = (0.125D0*CW*EL2*(-2.D0*MW2 + 3.D0*A0(MW2)))/(PI2*SW)

 amplitudes(4) = (0.006944444444444444D0*EL2*(CW2 - 3.D0*SW2)*(6.D0*ME2 - 1.D0*x - 6.D0*A0(ME2) + 3.D0*(2.D0*ME2 + x)*B0(x, ME2, &
  &ME2)))/(CW*PI2*SW)

 amplitudes(5) = (0.006944444444444444D0*EL2*(CW2 - 3.D0*SW2)*(6.D0*MM2 - 1.D0*x - 6.D0*A0(MM2) + 3.D0*(2.D0*MM2 + x)*B0(x, MM2, &
  &MM2)))/(CW*PI2*SW)

 amplitudes(6) = (0.006944444444444444D0*EL2*(CW2 - 3.D0*SW2)*(6.D0*ML2 - 1.D0*x - 6.D0*A0(ML2) + 3.D0*(2.D0*ML2 + x)*B0(x, ML2, &
  &ML2)))/(CW*PI2*SW)

 amplitudes(7) = (0.004629629629629629D0*EL2*(3.D0*CW2 - 5.D0*SW2)*(6.D0*MU2 - 1.D0*x - 6.D0*A0(MU2) + 3.D0*(2.D0*MU2 + x)*B0(x, &
  &MU2, MU2)))/(CW*PI2*SW)

 amplitudes(8) = (0.004629629629629629D0*EL2*(3.D0*CW2 - 5.D0*SW2)*(6.D0*MC2 - 1.D0*x - 6.D0*A0(MC2) + 3.D0*(2.D0*MC2 + x)*B0(x, &
  &MC2, MC2)))/(CW*PI2*SW)

 amplitudes(9) = (0.004629629629629629D0*EL2*(3.D0*CW2 - 5.D0*SW2)*(6.D0*MT2 - 1.D0*x - 6.D0*A0(MT2) + 3.D0*(2.D0*MT2 + x)*B0(x, &
  &MT2, MT2)))/(CW*PI2*SW)

 amplitudes(10) = (0.0023148148148148147D0*EL2*(3.D0*CW2 - 1.D0*SW2)*(6.D0*MD2 - 1.D0*x - 6.D0*A0(MD2) + 3.D0*(2.D0*MD2 + x)*B0(x&
  &, MD2, MD2)))/(CW*PI2*SW)

 amplitudes(11) = (0.0023148148148148147D0*EL2*(3.D0*CW2 - 1.D0*SW2)*(6.D0*MS2 - 1.D0*x - 6.D0*A0(MS2) + 3.D0*(2.D0*MS2 + x)*B0(x&
  &, MS2, MS2)))/(CW*PI2*SW)

 amplitudes(12) = (0.0023148148148148147D0*EL2*(3.D0*CW2 - 1.D0*SW2)*(6.D0*MB2 - 1.D0*x - 6.D0*A0(MB2) + 3.D0*(2.D0*MB2 + x)*B0(x&
  &, MB2, MB2)))/(CW*PI2*SW)

 amplitudes(13) = (-0.003472222222222222D0*EL2*(CW2 - 1.D0*SW2)*(12.D0*MW2 - 2.D0*x + 6.D0*A0(MW2) + 3.D0*(4.D0*MW2 - 1.D0*x)*B0(&
  &x, MW2, MW2)))/(CW*PI2*SW)

 amplitudes(14) = (0.003472222222222222D0*EL2*(CW2 - 1.D0*SW2)*(2.D0*(-6.D0*MHp2 + x) - 6.D0*A0(MHp2) + 3.D0*(-4.D0*MHp2 + x)*B0(&
  &x, MHp2, MHp2)))/(CW*PI2*SW)

 amplitudes(15) = (0.001736111111111111D0*CW*EL2*(12.D0*MW2 - 2.D0*x + 6.D0*A0(MW2) + 3.D0*(4.D0*MW2 - 1.D0*x)*B0(x, MW2, MW2)))/&
  &(PI2*SW)

 amplitudes(16) = (0.001736111111111111D0*CW*EL2*(12.D0*MW2 - 2.D0*x + 6.D0*A0(MW2) + 3.D0*(4.D0*MW2 - 1.D0*x)*B0(x, MW2, MW2)))/&
  &(PI2*SW)

 amplitudes(17) = (-0.003472222222222222D0*CW*EL2*(2.D0*(-6.D0*MW2 + x) + 66.D0*A0(MW2) + (96.D0*MW2 + 57.D0*x)*B0(x, MW2, MW2)))&
  &/(PI2*SW)

 amplitudes(18) = (-0.015625D0*B0(x, MW2, MW2)*DBLE(EL**INT(4.D0))*DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB2*SW)/EL)**INT(2.D0)))/&
  &(CW*PI2*SW)

 amplitudes(19) = 0.D0

 amplitudes(20) = (-0.015625D0*B0(x, MW2, MW2)*DBLE(EL**INT(4.D0))*DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB2*SW)/EL)**INT(2.D0)))/&
  &(CW*PI2*SW)

 amplitudes(21) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,21
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfAZ0Usual = totalAmplitude
end function SelfAZ0Usual

