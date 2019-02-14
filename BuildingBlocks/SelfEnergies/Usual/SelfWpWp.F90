double complex function SelfWpWpUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(51)

 amplitudes(1) = (0.015625D0*EL2*(CA12*CA22 + CA22*SA12)*A0(MH12))/(PI2*SW2)

 amplitudes(2) = (0.015625D0*EL2*A0(MH22)*(DBLE((-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)**INT(2.D0)) + DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3&
  &)**INT(2.D0))))/(PI2*SW2)

 amplitudes(3) = (0.015625D0*EL2*A0(MH32)*(DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0)) + DBLE((-1.D0*CA1*CA3*SA2 + SA1*SA&
  &3)**INT(2.D0))))/(PI2*SW2)

 amplitudes(4) = (0.015625D0*EL2*(CB2 + SB2)*A0(MZ2))/(PI2*SW2)

 amplitudes(5) = (0.015625D0*EL2*(CB2 + SB2)*A0(MA02))/(PI2*SW2)

 amplitudes(6) = (0.03125D0*EL2*(CB2 + SB2)*A0(MW2))/(PI2*SW2)

 amplitudes(7) = (0.03125D0*EL2*(CB2 + SB2)*A0(MHp2))/(PI2*SW2)

 amplitudes(8) = 0.D0

 amplitudes(9) = (0.0625D0*CW2*EL2*(-2.D0*MZ2 + 3.D0*A0(MZ2)))/(PI2*SW2)

 amplitudes(10) = (0.0625D0*EL2*(-2.D0*MW2 + 3.D0*A0(MW2)))/(PI2*SW2)

 amplitudes(11) = (-0.003472222222222222D0*EL2*(-6.D0*ME2*x + (-3.D0*ME2 + 6.D0*x)*A0(ME2) + 3.D0*ME2*x*B0(x, 0.D0, ME2) + 3.D0*B&
  &0(x, 0.D0, ME2)*DBLE(ME**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x, 0.D0, ME2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(12) = 0.D0

 amplitudes(13) = 0.D0

 amplitudes(14) = 0.D0

 amplitudes(15) = (-0.003472222222222222D0*EL2*(-6.D0*MM2*x + (-3.D0*MM2 + 6.D0*x)*A0(MM2) + 3.D0*MM2*x*B0(x, 0.D0, MM2) + 3.D0*B&
  &0(x, 0.D0, MM2)*DBLE(MM**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x, 0.D0, MM2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(16) = 0.D0

 amplitudes(17) = 0.D0

 amplitudes(18) = 0.D0

 amplitudes(19) = (-0.003472222222222222D0*EL2*(-6.D0*ML2*x + (-3.D0*ML2 + 6.D0*x)*A0(ML2) + 3.D0*ML2*x*B0(x, 0.D0, ML2) + 3.D0*B&
  &0(x, 0.D0, ML2)*DBLE(ML**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x, 0.D0, ML2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(20) = (-0.010416666666666666D0*CKM11*CKMC11*EL2*(-6.D0*MD2*x - 6.D0*MU2*x + (-3.D0*MD2 + 3.D0*MU2 + 6.D0*x)*A0(MD2) +&
  & 3.D0*(MD2 - 1.D0*MU2 + 2.D0*x)*A0(MU2) - 6.D0*MD2*MU2*B0(x, MD2, MU2) + 3.D0*MD2*x*B0(x, MD2, MU2) + 3.D0*MU2*x*B0(x, MD2, MU&
  &2) + 3.D0*B0(x, MD2, MU2)*DBLE(MD**INT(4.D0)) + 3.D0*B0(x, MD2, MU2)*DBLE(MU**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MD2, MU2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(21) = (-0.010416666666666666D0*CKM21*CKMC21*EL2*(-6.D0*MC2*x - 6.D0*MD2*x + (-3.D0*MC2 + 3.D0*MD2 + 6.D0*x)*A0(MC2) +&
  & 3.D0*(MC2 - 1.D0*MD2 + 2.D0*x)*A0(MD2) - 6.D0*MC2*MD2*B0(x, MC2, MD2) + 3.D0*MC2*x*B0(x, MC2, MD2) + 3.D0*MD2*x*B0(x, MC2, MD&
  &2) + 3.D0*B0(x, MC2, MD2)*DBLE(MC**INT(4.D0)) + 3.D0*B0(x, MC2, MD2)*DBLE(MD**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MC2, MD2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(22) = (-0.010416666666666666D0*CKM31*CKMC31*EL2*(-6.D0*MD2*x - 6.D0*MT2*x + (-3.D0*MD2 + 3.D0*MT2 + 6.D0*x)*A0(MD2) +&
  & 3.D0*(MD2 - 1.D0*MT2 + 2.D0*x)*A0(MT2) - 6.D0*MD2*MT2*B0(x, MD2, MT2) + 3.D0*MD2*x*B0(x, MD2, MT2) + 3.D0*MT2*x*B0(x, MD2, MT&
  &2) + 3.D0*B0(x, MD2, MT2)*DBLE(MD**INT(4.D0)) + 3.D0*B0(x, MD2, MT2)*DBLE(MT**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MD2, MT2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(23) = (-0.010416666666666666D0*CKM12*CKMC12*EL2*(-6.D0*MS2*x - 6.D0*MU2*x + (-3.D0*MS2 + 3.D0*MU2 + 6.D0*x)*A0(MS2) +&
  & 3.D0*(MS2 - 1.D0*MU2 + 2.D0*x)*A0(MU2) - 6.D0*MS2*MU2*B0(x, MS2, MU2) + 3.D0*MS2*x*B0(x, MS2, MU2) + 3.D0*MU2*x*B0(x, MS2, MU&
  &2) + 3.D0*B0(x, MS2, MU2)*DBLE(MS**INT(4.D0)) + 3.D0*B0(x, MS2, MU2)*DBLE(MU**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MS2, MU2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(24) = (-0.010416666666666666D0*CKM22*CKMC22*EL2*(-6.D0*MC2*x - 6.D0*MS2*x + (-3.D0*MC2 + 3.D0*MS2 + 6.D0*x)*A0(MC2) +&
  & 3.D0*(MC2 - 1.D0*MS2 + 2.D0*x)*A0(MS2) - 6.D0*MC2*MS2*B0(x, MC2, MS2) + 3.D0*MC2*x*B0(x, MC2, MS2) + 3.D0*MS2*x*B0(x, MC2, MS&
  &2) + 3.D0*B0(x, MC2, MS2)*DBLE(MC**INT(4.D0)) + 3.D0*B0(x, MC2, MS2)*DBLE(MS**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MC2, MS2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(25) = (-0.010416666666666666D0*CKM32*CKMC32*EL2*(-6.D0*MS2*x - 6.D0*MT2*x + (-3.D0*MS2 + 3.D0*MT2 + 6.D0*x)*A0(MS2) +&
  & 3.D0*(MS2 - 1.D0*MT2 + 2.D0*x)*A0(MT2) - 6.D0*MS2*MT2*B0(x, MS2, MT2) + 3.D0*MS2*x*B0(x, MS2, MT2) + 3.D0*MT2*x*B0(x, MS2, MT&
  &2) + 3.D0*B0(x, MS2, MT2)*DBLE(MS**INT(4.D0)) + 3.D0*B0(x, MS2, MT2)*DBLE(MT**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MS2, MT2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(26) = (-0.010416666666666666D0*CKM13*CKMC13*EL2*(-6.D0*MB2*x - 6.D0*MU2*x + (-3.D0*MB2 + 3.D0*MU2 + 6.D0*x)*A0(MB2) +&
  & 3.D0*(MB2 - 1.D0*MU2 + 2.D0*x)*A0(MU2) - 6.D0*MB2*MU2*B0(x, MB2, MU2) + 3.D0*MB2*x*B0(x, MB2, MU2) + 3.D0*MU2*x*B0(x, MB2, MU&
  &2) + 3.D0*B0(x, MB2, MU2)*DBLE(MB**INT(4.D0)) + 3.D0*B0(x, MB2, MU2)*DBLE(MU**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MB2, MU2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(27) = (-0.010416666666666666D0*CKM23*CKMC23*EL2*(-6.D0*MB2*x - 6.D0*MC2*x + (-3.D0*MB2 + 3.D0*MC2 + 6.D0*x)*A0(MB2) +&
  & 3.D0*(MB2 - 1.D0*MC2 + 2.D0*x)*A0(MC2) - 6.D0*MB2*MC2*B0(x, MB2, MC2) + 3.D0*MB2*x*B0(x, MB2, MC2) + 3.D0*MC2*x*B0(x, MB2, MC&
  &2) + 3.D0*B0(x, MB2, MC2)*DBLE(MB**INT(4.D0)) + 3.D0*B0(x, MB2, MC2)*DBLE(MC**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MB2, MC2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(28) = (-0.010416666666666666D0*CKM33*CKMC33*EL2*(-6.D0*MB2*x - 6.D0*MT2*x + (-3.D0*MB2 + 3.D0*MT2 + 6.D0*x)*A0(MB2) +&
  & 3.D0*(MB2 - 1.D0*MT2 + 2.D0*x)*A0(MT2) - 6.D0*MB2*MT2*B0(x, MB2, MT2) + 3.D0*MB2*x*B0(x, MB2, MT2) + 3.D0*MT2*x*B0(x, MB2, MT&
  &2) + 3.D0*B0(x, MB2, MT2)*DBLE(MB**INT(4.D0)) + 3.D0*B0(x, MB2, MT2)*DBLE(MT**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) - 6.D0*B0(x&
  &, MB2, MT2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(29) = (0.001736111111111111D0*EL2*DBLE((CA1*CA2*CB + CA2*SA1*SB)**INT(2.D0))*(-6.D0*MH12*x - 6.D0*MW2*x + 3.D0*(-1.D0&
  &*MH12 + MW2 - 1.D0*x)*A0(MH12) - 3.D0*(-1.D0*MH12 + MW2 + x)*A0(MW2) - 6.D0*MH12*MW2*B0(x, MH12, MW2) - 6.D0*MH12*x*B0(x, MH12&
  &, MW2) - 6.D0*MW2*x*B0(x, MH12, MW2) + 3.D0*B0(x, MH12, MW2)*DBLE(MH1**INT(4.D0)) + 3.D0*B0(x, MH12, MW2)*DBLE(MW**INT(4.D0)) &
  &+ 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH12, MW2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(30) = (0.001736111111111111D0*EL2*DBLE((CB*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + (CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB)**INT&
  &(2.D0))* (-6.D0*MH22*x - 6.D0*MW2*x + 3.D0*(-1.D0*MH22 + MW2 - 1.D0*x)*A0(MH22) - 3.D0*(-1.D0*MH22 + MW2 + x)*A0(MW2) - 6.D0*M&
  &H22*MW2*B0(x, MH22, MW2) - 6.D0*MH22*x*B0(x, MH22, MW2) - 6.D0*MW2*x*B0(x, MH22, MW2) + 3.D0*B0(x, MH22, MW2)*DBLE(MH2**INT(4.&
  &D0)) + 3.D0*B0(x, MH22, MW2)*DBLE(MW**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH22, MW2)*DBLE(x**INT(2.D0))))/(PI2*S&
  &W2*x)

 amplitudes(31) = (0.001736111111111111D0*EL2*DBLE((CB*(-1.D0*CA1*CA3*SA2 + SA1*SA3) + (-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB)**IN&
  &T(2.D0))* (-6.D0*MH32*x - 6.D0*MW2*x + 3.D0*(-1.D0*MH32 + MW2 - 1.D0*x)*A0(MH32) - 3.D0*(-1.D0*MH32 + MW2 + x)*A0(MW2) - 6.D0*&
  &MH32*MW2*B0(x, MH32, MW2) - 6.D0*MH32*x*B0(x, MH32, MW2) - 6.D0*MW2*x*B0(x, MH32, MW2) + 3.D0*B0(x, MH32, MW2)*DBLE(MH3**INT(4&
  &.D0)) + 3.D0*B0(x, MH32, MW2)*DBLE(MW**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH32, MW2)*DBLE(x**INT(2.D0))))/(PI2*&
  &SW2*x)

 amplitudes(32) = (0.001736111111111111D0*EL2*DBLE((CA2*CB*SA1 - 1.D0*CA1*CA2*SB)**INT(2.D0))*(-6.D0*MH12*x - 6.D0*MHp2*x - 3.D0*&
  &(MH12 - 1.D0*MHp2 + x)*A0(MH12) - 3.D0*(-1.D0*MH12 + MHp2 + x)*A0(MHp2) - 6.D0*MH12*MHp2*B0(x, MH12, MHp2) - 6.D0*MH12*x*B0(x,&
  & MH12, MHp2) - 6.D0*MHp2*x*B0(x, MH12, MHp2) + 3.D0*B0(x, MH12, MHp2)*DBLE(MH1**INT(4.D0)) + 3.D0*B0(x, MH12, MHp2)*DBLE(MHp**&
  &INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH12, MHp2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(33) = (0.001736111111111111D0*EL2*DBLE((CB*(CA1*CA3 - 1.D0*SA1*SA2*SA3) - 1.D0*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SB)&
  &**INT(2.D0))* (-6.D0*MH22*x - 6.D0*MHp2*x - 3.D0*(MH22 - 1.D0*MHp2 + x)*A0(MH22) - 3.D0*(-1.D0*MH22 + MHp2 + x)*A0(MHp2) - 6.D&
  &0*MH22*MHp2*B0(x, MH22, MHp2) - 6.D0*MH22*x*B0(x, MH22, MHp2) - 6.D0*MHp2*x*B0(x, MH22, MHp2) + 3.D0*B0(x, MH22, MHp2)*DBLE(MH&
  &2**INT(4.D0)) + 3.D0*B0(x, MH22, MHp2)*DBLE(MHp**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH22, MHp2)*DBLE(x**INT(2.D&
  &0))))/(PI2*SW2*x)

 amplitudes(34) = (0.001736111111111111D0*EL2*DBLE((CB*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) - 1.D0*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SB&
  &)**INT(2.D0))* (-6.D0*MH32*x - 6.D0*MHp2*x - 3.D0*(MH32 - 1.D0*MHp2 + x)*A0(MH32) - 3.D0*(-1.D0*MH32 + MHp2 + x)*A0(MHp2) - 6.&
  &D0*MH32*MHp2*B0(x, MH32, MHp2) - 6.D0*MH32*x*B0(x, MH32, MHp2) - 6.D0*MHp2*x*B0(x, MH32, MHp2) + 3.D0*B0(x, MH32, MHp2)*DBLE(M&
  &H3**INT(4.D0)) + 3.D0*B0(x, MH32, MHp2)*DBLE(MHp**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 3.D0*B0(x, MH32, MHp2)*DBLE(x**INT(2.&
  &D0))))/(PI2*SW2*x)

 amplitudes(35) = (0.001736111111111111D0*EL2*DBLE((CB2 + SB2)**INT(2.D0))*(-6.D0*MW2*x - 6.D0*MZ2*x - 3.D0*(MW2 - 1.D0*MZ2 + x)*&
  &A0(MW2) + 3.D0*(MW2 - 1.D0*MZ2 - 1.D0*x)*A0(MZ2) - 6.D0*MW2*MZ2*B0(x, MW2, MZ2) - 6.D0*MW2*x*B0(x, MW2, MZ2) - 6.D0*MZ2*x*B0(x&
  &, MW2, MZ2) + 3.D0*B0(x, MW2, MZ2)*DBLE(MW**INT(4.D0)) + 3.D0*B0(x, MW2, MZ2)*DBLE(MZ**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + &
  &3.D0*B0(x, MW2, MZ2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(36) = 0.D0

 amplitudes(37) = 0.D0

 amplitudes(38) = (0.001736111111111111D0*EL2*DBLE((CB2 + SB2)**INT(2.D0))*(-6.D0*MA02*x - 6.D0*MHp2*x - 3.D0*(MA02 - 1.D0*MHp2 +&
  & x)*A0(MA02) - 3.D0*(-1.D0*MA02 + MHp2 + x)*A0(MHp2) - 6.D0*MA02*MHp2*B0(x, MA02, MHp2) - 6.D0*MA02*x*B0(x, MA02, MHp2) - 6.D0&
  &*MHp2*x*B0(x, MA02, MHp2) + 3.D0*B0(x, MA02, MHp2)*DBLE(MA0**INT(4.D0)) + 3.D0*B0(x, MA02, MHp2)*DBLE(MHp**INT(4.D0)) + 2.D0*D&
  &BLE(x**INT(2.D0)) + 3.D0*B0(x, MA02, MHp2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(39) = (-0.001736111111111111D0*EL2*(2.D0*x*(-3.D0*MW2 + x) - 3.D0*(MW2 + x)*A0(MW2) + 3.D0*B0(x, 0.D0, MW2)*DBLE((MW2&
  & - 1.D0*x)**INT(2.D0))))/(PI2*x)

 amplitudes(40) = (0.001736111111111111D0*CW2*EL2*(6.D0*MW2*x + 6.D0*MZ2*x + 3.D0*(MW2 - 1.D0*MZ2 + x)*A0(MW2) + 3.D0*(-1.D0*MW2 &
  &+ MZ2 + x)*A0(MZ2) + 6.D0*MW2*MZ2*B0(x, MW2, MZ2) + 6.D0*MW2*x*B0(x, MW2, MZ2) + 6.D0*MZ2*x*B0(x, MW2, MZ2) - 3.D0*B0(x, MW2, &
  &MZ2)*DBLE(MW**INT(4.D0)) - 3.D0*B0(x, MW2, MZ2)*DBLE(MZ**INT(4.D0)) - 2.D0*DBLE(x**INT(2.D0)) - 3.D0*B0(x, MW2, MZ2)*DBLE(x**I&
  &NT(2.D0))))/(PI2*SW2*x)

 amplitudes(41) = (-0.001736111111111111D0*EL2*(2.D0*x*(-3.D0*MW2 + x) - 3.D0*(MW2 + x)*A0(MW2) + 3.D0*B0(x, 0.D0, MW2)*DBLE((MW2&
  & - 1.D0*x)**INT(2.D0))))/(PI2*x)

 amplitudes(42) = (0.001736111111111111D0*CW2*EL2*(6.D0*MW2*x + 6.D0*MZ2*x + 3.D0*(MW2 - 1.D0*MZ2 + x)*A0(MW2) + 3.D0*(-1.D0*MW2 &
  &+ MZ2 + x)*A0(MZ2) + 6.D0*MW2*MZ2*B0(x, MW2, MZ2) + 6.D0*MW2*x*B0(x, MW2, MZ2) + 6.D0*MZ2*x*B0(x, MW2, MZ2) - 3.D0*B0(x, MW2, &
  &MZ2)*DBLE(MW**INT(4.D0)) - 3.D0*B0(x, MW2, MZ2)*DBLE(MZ**INT(4.D0)) - 2.D0*DBLE(x**INT(2.D0)) - 3.D0*B0(x, MW2, MZ2)*DBLE(x**I&
  &NT(2.D0))))/(PI2*SW2*x)

 amplitudes(43) = (-0.003472222222222222D0*EL2*(2.D0*x*(-3.D0*MW2 + x) + 3.D0*(5.D0*MW2 + 11.D0*x)*A0(MW2) + B0(x, 0.D0, MW2)*(48&
  &.D0*MW2*x - 15.D0*DBLE(MW**INT(4.D0)) + 57.D0*DBLE(x**INT(2.D0)))))/(PI2*x)

 amplitudes(44) = (-0.003472222222222222D0*CW2*EL2*(-6.D0*MW2*x - 6.D0*MZ2*x + 3.D0*(5.D0*MW2 - 5.D0*MZ2 + 11.D0*x)*A0(MW2) + (-1&
  &5.D0*MW2 + 15.D0*MZ2 + 33.D0*x)*A0(MZ2) + 30.D0*MW2*MZ2*B0(x, MW2, MZ2) + 48.D0*MW2*x*B0(x, MW2, MZ2) + 48.D0*MZ2*x*B0(x, MW2,&
  & MZ2) - 15.D0*B0(x, MW2, MZ2)*DBLE(MW**INT(4.D0)) - 15.D0*B0(x, MW2, MZ2)*DBLE(MZ**INT(4.D0)) + 2.D0*DBLE(x**INT(2.D0)) + 57.D&
  &0*B0(x, MW2, MZ2)*DBLE(x**INT(2.D0))))/(PI2*SW2*x)

 amplitudes(45) = (0.015625D0*B0(x, 0.D0, MW2)*DBLE(EL**INT(4.D0))*DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB2*SW)/EL)**INT(2.D0)))/&
  &(PI2*SW2)

 amplitudes(46) = 0.D0

 amplitudes(47) = (0.015625D0*B0(x, MW2, MZ2)*DBLE(EL**INT(4.D0))*DBLE(((2.D0*CB2*MW*SW)/EL + (2.D0*MW*SB2*SW)/EL)**INT(2.D0)))/(&
  &CW2*PI2)

 amplitudes(48) = 0.D0

 amplitudes(49) = (0.015625D0*B0(x, MH12, MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))*DBLE(((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*&
  &CA2*MW*SA1*SB*SW)/EL)**INT(2.D0)))/PI2

 amplitudes(50) = (0.015625D0*B0(x, MH22, MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*&
  &CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)**INT(2.D0)))/PI2

 amplitudes(51) = (0.015625D0*B0(x, MH32, MW2)*DBLE(EL**INT(4.D0))*DBLE(SW**INT(-4.D0))* DBLE(((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + S&
  &A1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)**INT(2.D0)))/PI2

  totalAmplitude = (0D0,0D0)
 do j=1,51
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfWpWpUsual = totalAmplitude
end function SelfWpWpUsual

