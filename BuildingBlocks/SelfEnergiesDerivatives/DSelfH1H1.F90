double complex function DSelfH1H1(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(42)

 amplitudes(1) = 0.D0

 amplitudes(2) = 0.D0

 amplitudes(3) = 0.D0

 amplitudes(4) = 0.D0

 amplitudes(5) = 0.D0

 amplitudes(6) = 0.D0

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

 amplitudes(10) = (-0.03125D0*EL2*ME2*(-1.D0*B0(x, ME2, ME2) + (4.D0*ME2 - 1.D0*x)*DB0(x, ME2, ME2))*DBLE(YukS1Lep1**INT(2.D0)))/&
  &(MW2*PI2*SW2)

 amplitudes(11) = (-0.03125D0*EL2*MM2*(-1.D0*B0(x, MM2, MM2) + (4.D0*MM2 - 1.D0*x)*DB0(x, MM2, MM2))*DBLE(YukS1Lep1**INT(2.D0)))/&
  &(MW2*PI2*SW2)

 amplitudes(12) = (-0.03125D0*EL2*ML2*(-1.D0*B0(x, ML2, ML2) + (4.D0*ML2 - 1.D0*x)*DB0(x, ML2, ML2))*DBLE(YukS1Lep1**INT(2.D0)))/&
  &(MW2*PI2*SW2)

 amplitudes(13) = (-0.09375D0*CA22*EL2*MU2*SA12*(-1.D0*B0(x, MU2, MU2) + (4.D0*MU2 - 1.D0*x)*DB0(x, MU2, MU2)))/(MW2*PI2*SB2*SW2)

 amplitudes(14) = (-0.09375D0*CA22*EL2*MC2*SA12*(-1.D0*B0(x, MC2, MC2) + (4.D0*MC2 - 1.D0*x)*DB0(x, MC2, MC2)))/(MW2*PI2*SB2*SW2)

 amplitudes(15) = (-0.09375D0*CA22*EL2*MT2*SA12*(-1.D0*B0(x, MT2, MT2) + (4.D0*MT2 - 1.D0*x)*DB0(x, MT2, MT2)))/(MW2*PI2*SB2*SW2)

 amplitudes(16) = (-0.09375D0*EL2*MD2*(-1.D0*B0(x, MD2, MD2) + (4.D0*MD2 - 1.D0*x)*DB0(x, MD2, MD2))*DBLE(YukS1Quark1**INT(2.D0))&
  &)/(MW2*PI2*SW2)

 amplitudes(17) = (-0.09375D0*EL2*MS2*(-1.D0*B0(x, MS2, MS2) + (4.D0*MS2 - 1.D0*x)*DB0(x, MS2, MS2))*DBLE(YukS1Quark1**INT(2.D0))&
  &)/(MW2*PI2*SW2)

 amplitudes(18) = (-0.09375D0*EL2*MB2*(-1.D0*B0(x, MB2, MB2) + (4.D0*MB2 - 1.D0*x)*DB0(x, MB2, MB2))*DBLE(YukS1Quark1**INT(2.D0))&
  &)/(MW2*PI2*SW2)

 amplitudes(19) = (0.03125D0*DB0(x, MH12, MH12)*DBLE(CS1S1S1f111**INT(2.D0)))/PI2

 amplitudes(20) = (0.03125D0*DB0(x, MH22, MH22)*DBLE(CS1S1S1f122**INT(2.D0)))/PI2

 amplitudes(21) = (0.0625D0*DB0(x, MH12, MH22)*DBLE(CS1S1S1f112**INT(2.D0)))/PI2

 amplitudes(22) = (0.03125D0*DB0(x, MH32, MH32)*DBLE(CS1S1S1f133**INT(2.D0)))/PI2

 amplitudes(23) = (0.0625D0*DB0(x, MH12, MH32)*DBLE(CS1S1S1f113**INT(2.D0)))/PI2

 amplitudes(24) = (0.0625D0*DB0(x, MH22, MH32)*DBLE(CS1S1S1f123**INT(2.D0)))/PI2

 amplitudes(25) = (0.03125D0*DB0(x, MZ2, MZ2)*DBLE(CS2S2S1f111**INT(2.D0)))/PI2

 amplitudes(26) = (0.03125D0*DB0(x, MA02, MA02)*DBLE(CS2S2S1f221**INT(2.D0)))/PI2

 amplitudes(27) = (0.0625D0*DB0(x, MA02, MZ2)*DBLE(CS2S2S1f121**INT(2.D0)))/PI2

 amplitudes(28) = (0.0625D0*DB0(x, MW2, MW2)*DBLE(CS1S3S3f111**INT(2.D0)))/PI2

 amplitudes(29) = (0.0625D0*DB0(x, MHp2, MHp2)*DBLE(CS1S3S3f122**INT(2.D0)))/PI2

 amplitudes(30) = (0.0625D0*CS1S3S3f112*CS1S3S3f121*DB0(x, MHp2, MW2))/PI2

 amplitudes(31) = (0.0625D0*CS1S3S3f112*CS1S3S3f121*DB0(x, MHp2, MW2))/PI2

 amplitudes(32) = (-0.00390625D0*DB0(x, MZ2, MZ2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CA1*&
  &CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)**INT(2.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2

 amplitudes(33) = (-0.00390625D0*DB0(x, MW2, MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.&
  &D0*CA2*MW*SA1*SB*SW)/EL)**INT(2.D0)))/ PI2

 amplitudes(34) = (-0.00390625D0*DB0(x, MW2, MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.&
  &D0*CA2*MW*SA1*SB*SW)/EL)**INT(2.D0)))/ PI2

 amplitudes(35) = (0.03125D0*DB0(x, MZ2, MZ2)*DBLE(CW**INT(-4.D0))*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CA1*CA2*&
  &CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*SW)/EL)**INT(2.D0))*DBLE((CW2 + SW2)**INT(4.D0)))/PI2

 amplitudes(36) = (0.0625D0*DB0(x, MW2, MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA&
  &2*MW*SA1*SB*SW)/EL)**INT(2.D0)))/PI2

 amplitudes(37) = (-0.015625D0*EL2*(2.D0*B0(x, MZ2, MZ2) + (MZ2 + 2.D0*x)*DB0(x, MZ2, MZ2))*DBLE((CA1*CA2*CB + CA2*SA1*SB)**INT(2&
  &.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/ (CW2*PI2*SW2)

 amplitudes(38) = (-0.015625D0*EL2*(2.D0*B0(x, MA02, MZ2) + (-1.D0*MZ2 + 2.D0*(MA02 + x))*DB0(x, MA02, MZ2))*DBLE((CA2*CB*SA1 - 1&
  &.D0*CA1*CA2*SB)**INT(2.D0))* DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2*SW2)

 amplitudes(39) = (-0.015625D0*EL2*(2.D0*B0(x, MW2, MW2) + (MW2 + 2.D0*x)*DB0(x, MW2, MW2))*DBLE((CA1*CA2*CB + CA2*SA1*SB)**INT(2&
  &.D0)))/(PI2*SW2)

 amplitudes(40) = (-0.015625D0*EL2*(2.D0*B0(x, MHp2, MW2) + (-1.D0*MW2 + 2.D0*(MHp2 + x))*DB0(x, MHp2, MW2))*DBLE((CA2*CB*SA1 - 1&
  &.D0*CA1*CA2*SB)**INT(2.D0)))/(PI2*SW2)

 amplitudes(41) = (-0.015625D0*EL2*(2.D0*B0(x, MW2, MW2) + (MW2 + 2.D0*x)*DB0(x, MW2, MW2))*DBLE((CA1*CA2*CB + CA2*SA1*SB)**INT(2&
  &.D0)))/(PI2*SW2)

 amplitudes(42) = (-0.015625D0*EL2*(2.D0*B0(x, MHp2, MW2) + (-1.D0*MW2 + 2.D0*(MHp2 + x))*DB0(x, MHp2, MW2))*DBLE((CA2*CB*SA1 - 1&
  &.D0*CA1*CA2*SB)**INT(2.D0)))/(PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,42
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfH1H1 = totalAmplitude
end function DSelfH1H1

