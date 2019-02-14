double complex function SelfAZ0UsualZeroMom(x)
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

 amplitudes(4) = (0.041666666666666664D0*EL2*(CW2 - 3.D0*SW2)*(-1.D0*A0(ME2) + ME2*(1.D0 + B0(0.D0, ME2, ME2))))/(CW*PI2*SW)

 amplitudes(5) = (0.041666666666666664D0*EL2*(CW2 - 3.D0*SW2)*(-1.D0*A0(MM2) + MM2*(1.D0 + B0(0.D0, MM2, MM2))))/(CW*PI2*SW)

 amplitudes(6) = (0.041666666666666664D0*EL2*(CW2 - 3.D0*SW2)*(-1.D0*A0(ML2) + ML2*(1.D0 + B0(0.D0, ML2, ML2))))/(CW*PI2*SW)

 amplitudes(7) = (0.027777777777777776D0*EL2*(3.D0*CW2 - 5.D0*SW2)*(-1.D0*A0(MU2) + MU2*(1.D0 + B0(0.D0, MU2, MU2))))/(CW*PI2*SW)

 amplitudes(8) = (0.027777777777777776D0*EL2*(3.D0*CW2 - 5.D0*SW2)*(-1.D0*A0(MC2) + MC2*(1.D0 + B0(0.D0, MC2, MC2))))/(CW*PI2*SW)

 amplitudes(9) = (0.027777777777777776D0*EL2*(3.D0*CW2 - 5.D0*SW2)*(-1.D0*A0(MT2) + MT2*(1.D0 + B0(0.D0, MT2, MT2))))/(CW*PI2*SW)

 amplitudes(10) = (0.013888888888888888D0*EL2*(3.D0*CW2 - 1.D0*SW2)*(-1.D0*A0(MD2) + MD2*(1.D0 + B0(0.D0, MD2, MD2))))/(CW*PI2*SW&
  &)

 amplitudes(11) = (0.013888888888888888D0*EL2*(3.D0*CW2 - 1.D0*SW2)*(-1.D0*A0(MS2) + MS2*(1.D0 + B0(0.D0, MS2, MS2))))/(CW*PI2*SW&
  &)

 amplitudes(12) = (0.013888888888888888D0*EL2*(3.D0*CW2 - 1.D0*SW2)*(-1.D0*A0(MB2) + MB2*(1.D0 + B0(0.D0, MB2, MB2))))/(CW*PI2*SW&
  &)

 amplitudes(13) = (-0.020833333333333332D0*EL2*(CW2 - 1.D0*SW2)*(A0(MW2) + 2.D0*MW2*(1.D0 + B0(0.D0, MW2, MW2))))/(CW*PI2*SW)

 amplitudes(14) = (-0.020833333333333332D0*EL2*(CW2 - 1.D0*SW2)*(A0(MHp2) + 2.D0*MHp2*(1.D0 + B0(0.D0, MHp2, MHp2))))/(CW*PI2*SW)

 amplitudes(15) = (0.010416666666666666D0*CW*EL2*(A0(MW2) + 2.D0*MW2*(1.D0 + B0(0.D0, MW2, MW2))))/(PI2*SW)

 amplitudes(16) = (0.010416666666666666D0*CW*EL2*(A0(MW2) + 2.D0*MW2*(1.D0 + B0(0.D0, MW2, MW2))))/(PI2*SW)

 amplitudes(17) = (-0.020833333333333332D0*CW*EL2*(11.D0*A0(MW2) + 2.D0*MW2*(-1.D0 + 8.D0*B0(0.D0, MW2, MW2))))/(PI2*SW)

 amplitudes(18) = (-0.015625D0*B0(0.D0, MW2, MW2)*DBLE(EL**INT(4.D0))*DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB2*SW)/EL)**INT(2.D0)&
  &))/(CW*PI2*SW)

 amplitudes(19) = 0.D0

 amplitudes(20) = (-0.015625D0*B0(0.D0, MW2, MW2)*DBLE(EL**INT(4.D0))*DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB2*SW)/EL)**INT(2.D0)&
  &))/(CW*PI2*SW)

 amplitudes(21) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,21
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfAZ0UsualZeroMom = totalAmplitude
end function SelfAZ0UsualZeroMom

