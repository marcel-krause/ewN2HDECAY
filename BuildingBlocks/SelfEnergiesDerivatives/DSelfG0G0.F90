double complex function DSelfG0G0(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(37)

 amplitudes(1) = 0.D0

 amplitudes(2) = 0.D0

 amplitudes(3) = 0.D0

 amplitudes(4) = 0.D0

 amplitudes(5) = 0.D0

 amplitudes(6) = 0.D0

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

 amplitudes(10) = (-0.03125D0*EL2*ME2*(-1.D0*B0(x, ME2, ME2) - 1.D0*x*DB0(x, ME2, ME2))*DBLE(YukS2Lep1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(11) = (-0.03125D0*EL2*MM2*(-1.D0*B0(x, MM2, MM2) - 1.D0*x*DB0(x, MM2, MM2))*DBLE(YukS2Lep1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(12) = (-0.03125D0*EL2*ML2*(-1.D0*B0(x, ML2, ML2) - 1.D0*x*DB0(x, ML2, ML2))*DBLE(YukS2Lep1**INT(2.D0)))/(MW2*PI2*SW2)

 amplitudes(13) = (-0.09375D0*EL2*MU2*(-1.D0*B0(x, MU2, MU2) - 1.D0*x*DB0(x, MU2, MU2)))/(MW2*PI2*SW2)

 amplitudes(14) = (-0.09375D0*EL2*MC2*(-1.D0*B0(x, MC2, MC2) - 1.D0*x*DB0(x, MC2, MC2)))/(MW2*PI2*SW2)

 amplitudes(15) = (-0.09375D0*EL2*MT2*(-1.D0*B0(x, MT2, MT2) - 1.D0*x*DB0(x, MT2, MT2)))/(MW2*PI2*SW2)

 amplitudes(16) = (-0.09375D0*EL2*MD2*(-1.D0*B0(x, MD2, MD2) - 1.D0*x*DB0(x, MD2, MD2))*DBLE(YukS2Quark1**INT(2.D0)))/(MW2*PI2*SW&
  &2)

 amplitudes(17) = (-0.09375D0*EL2*MS2*(-1.D0*B0(x, MS2, MS2) - 1.D0*x*DB0(x, MS2, MS2))*DBLE(YukS2Quark1**INT(2.D0)))/(MW2*PI2*SW&
  &2)

 amplitudes(18) = (-0.09375D0*EL2*MB2*(-1.D0*B0(x, MB2, MB2) - 1.D0*x*DB0(x, MB2, MB2))*DBLE(YukS2Quark1**INT(2.D0)))/(MW2*PI2*SW&
  &2)

 amplitudes(19) = (0.0625D0*DB0(x, MH12, MZ2)*DBLE(CS2S2S1f111**INT(2.D0)))/PI2

 amplitudes(20) = (0.0625D0*DB0(x, MH22, MZ2)*DBLE(CS2S2S1f112**INT(2.D0)))/PI2

 amplitudes(21) = (0.0625D0*DB0(x, MH32, MZ2)*DBLE(CS2S2S1f113**INT(2.D0)))/PI2

 amplitudes(22) = (0.0625D0*DB0(x, MA02, MH12)*DBLE(CS2S2S1f121**INT(2.D0)))/PI2

 amplitudes(23) = (0.0625D0*DB0(x, MA02, MH22)*DBLE(CS2S2S1f122**INT(2.D0)))/PI2

 amplitudes(24) = (0.0625D0*DB0(x, MA02, MH32)*DBLE(CS2S2S1f123**INT(2.D0)))/PI2

 amplitudes(25) = 0D0

 amplitudes(26) = 0D0

 amplitudes(27) = 0.D0

 amplitudes(28) = 0.D0

 amplitudes(29) = (0.00390625D0*DB0(x, MW2, MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB&
  &2*SW)/EL)**INT(2.D0)))/PI2

 amplitudes(30) = (0.00390625D0*DB0(x, MW2, MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB&
  &2*SW)/EL)**INT(2.D0)))/PI2

 amplitudes(31) = (-0.015625D0*EL2*(2.D0*B0(x, MH12, MZ2) + (-1.D0*MZ2 + 2.D0*(MH12 + x))*DB0(x, MH12, MZ2))*DBLE((CA1*CA2*CB + C&
  &A2*SA1*SB)**INT(2.D0))* DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(32) = (-0.015625D0*EL2*(2.D0*B0(x, MH22, MZ2) + (-1.D0*MZ2 + 2.D0*(MH22 + x))*DB0(x, MH22, MZ2))* DBLE((CB*(-1.D0*CA3&
  &*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(33) = (-0.015625D0*EL2*(2.D0*B0(x, MH32, MZ2) + (-1.D0*MZ2 + 2.D0*(MH32 + x))*DB0(x, MH32, MZ2))* DBLE((CB*(-1.D0*CA1&
  &*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)**INT(2.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(34) = (-0.015625D0*EL2*(2.D0*B0(x, MW2, MW2) + (MW2 + 2.D0*x)*DB0(x, MW2, MW2))*DBLE((CB2 + SB2)**INT(2.D0)))/(PI2*SW&
  &2)

 amplitudes(35) = 0.D0

 amplitudes(36) = (-0.015625D0*EL2*(2.D0*B0(x, MW2, MW2) + (MW2 + 2.D0*x)*DB0(x, MW2, MW2))*DBLE((CB2 + SB2)**INT(2.D0)))/(PI2*SW&
  &2)

 amplitudes(37) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,37
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfG0G0 = totalAmplitude
end function DSelfG0G0

