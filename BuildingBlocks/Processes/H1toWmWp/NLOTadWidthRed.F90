double complex function H1toWmWpTad()
 use constants
 implicit none
#include "looptools.h"
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(63)

 amplitudes(1) = (-0.0625D0*ME2*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*YukS1Lep1*A0(ME2)*DBLE(EL**INT(4.D0))* (3.D0 - (1.D0*&
  &MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/(MH12*PI2)

 amplitudes(2) = (-0.0625D0*MM2*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*YukS1Lep1*A0(MM2)*DBLE(EL**INT(4.D0))* (3.D0 - (1.D0*&
  &MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/(MH12*PI2)

 amplitudes(3) = (-0.0625D0*ML2*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*YukS1Lep1*A0(ML2)*DBLE(EL**INT(4.D0))* (3.D0 - (1.D0*&
  &MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/(MH12*PI2)

 amplitudes(4) = (-0.0625D0*ME2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*(CB*RR11 + RR&
  &12*SB)*YukS1Lep2*A0(ME2)* DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE&
  &(SW**INT(-4.D0)))/(MH22*PI2)

 amplitudes(5) = (-0.0625D0*MM2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*(CB*RR11 + RR&
  &12*SB)*YukS1Lep2*A0(MM2)* DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE&
  &(SW**INT(-4.D0)))/(MH22*PI2)

 amplitudes(6) = (-0.0625D0*ML2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*(CB*RR11 + RR&
  &12*SB)*YukS1Lep2*A0(ML2)* DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE&
  &(SW**INT(-4.D0)))/(MH22*PI2)

 amplitudes(7) = (-0.0625D0*ME2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*(CB*RR11 + R&
  &R12*SB)*YukS1Lep3*A0(ME2)* DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBL&
  &E(SW**INT(-4.D0)))/(MH32*PI2)

 amplitudes(8) = (-0.0625D0*MM2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*(CB*RR11 + R&
  &R12*SB)*YukS1Lep3*A0(MM2)* DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBL&
  &E(SW**INT(-4.D0)))/(MH32*PI2)

 amplitudes(9) = (-0.0625D0*ML2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*(CB*RR11 + R&
  &R12*SB)*YukS1Lep3*A0(ML2)* DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBL&
  &E(SW**INT(-4.D0)))/(MH32*PI2)

 amplitudes(10) = (-0.1875D0*CA2*MU2*SA1*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*A0(MU2)*DBLE(EL**INT(4.D0))* (3.D0 - (1.D0*M&
  &H12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/(MH12*PI2*SB)

 amplitudes(11) = (-0.1875D0*CA2*MC2*SA1*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*A0(MC2)*DBLE(EL**INT(4.D0))* (3.D0 - (1.D0*M&
  &H12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/(MH12*PI2*SB)

 amplitudes(12) = (-0.1875D0*CA2*MT2*SA1*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*A0(MT2)*DBLE(EL**INT(4.D0))* (3.D0 - (1.D0*M&
  &H12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/(MH12*PI2*SB)

 amplitudes(13) = (-0.1875D0*MU2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.&
  &D0*SA1*SA2*SA3))* (CB*RR11 + RR12*SB)*A0(MU2)*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(M&
  &W**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/ (MH22*PI2*SB)

 amplitudes(14) = (-0.1875D0*MC2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.&
  &D0*SA1*SA2*SA3))* (CB*RR11 + RR12*SB)*A0(MC2)*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(M&
  &W**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/ (MH22*PI2*SB)

 amplitudes(15) = (-0.1875D0*MT2*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.&
  &D0*SA1*SA2*SA3))* (CB*RR11 + RR12*SB)*A0(MT2)*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(M&
  &W**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/ (MH22*PI2*SB)

 amplitudes(16) = (-0.1875D0*MU2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*&
  &CA1*CA3*SA2 + SA1*SA3))* (CB*RR11 + RR12*SB)*A0(MU2)*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))&
  &*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/ (MH32*PI2*SB)

 amplitudes(17) = (-0.1875D0*MC2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*&
  &CA1*CA3*SA2 + SA1*SA3))* (CB*RR11 + RR12*SB)*A0(MC2)*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))&
  &*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/ (MH32*PI2*SB)

 amplitudes(18) = (-0.1875D0*MT2*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*&
  &CA1*CA3*SA2 + SA1*SA3))* (CB*RR11 + RR12*SB)*A0(MT2)*DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))&
  &*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/ (MH32*PI2*SB)

 amplitudes(19) = (-0.1875D0*MD2*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*YukS1Quark1*A0(MD2)*DBLE(EL**INT(4.D0))* (3.D0 - (1.&
  &D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/(MH12*PI2)

 amplitudes(20) = (-0.1875D0*MS2*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*YukS1Quark1*A0(MS2)*DBLE(EL**INT(4.D0))* (3.D0 - (1.&
  &D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/(MH12*PI2)

 amplitudes(21) = (-0.1875D0*MB2*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*YukS1Quark1*A0(MB2)*DBLE(EL**INT(4.D0))* (3.D0 - (1.&
  &D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-4.D0)))/(MH12*PI2)

 amplitudes(22) = (-0.1875D0*MD2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*(CB*RR11 + R&
  &R12*SB)*YukS1Quark2*A0(MD2)* DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*D&
  &BLE(SW**INT(-4.D0)))/(MH22*PI2)

 amplitudes(23) = (-0.1875D0*MS2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*(CB*RR11 + R&
  &R12*SB)*YukS1Quark2*A0(MS2)* DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*D&
  &BLE(SW**INT(-4.D0)))/(MH22*PI2)

 amplitudes(24) = (-0.1875D0*MB2*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*(CB*RR11 + R&
  &R12*SB)*YukS1Quark2*A0(MB2)* DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*D&
  &BLE(SW**INT(-4.D0)))/(MH22*PI2)

 amplitudes(25) = (-0.1875D0*MD2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*(CB*RR11 + &
  &RR12*SB)*YukS1Quark3*A0(MD2)* DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*&
  &DBLE(SW**INT(-4.D0)))/(MH32*PI2)

 amplitudes(26) = (-0.1875D0*MS2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*(CB*RR11 + &
  &RR12*SB)*YukS1Quark3*A0(MS2)* DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*&
  &DBLE(SW**INT(-4.D0)))/(MH32*PI2)

 amplitudes(27) = (-0.1875D0*MB2*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*(CB*RR11 + &
  &RR12*SB)*YukS1Quark3*A0(MB2)* DBLE(EL**INT(4.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*&
  &DBLE(SW**INT(-4.D0)))/(MH32*PI2)

 amplitudes(28) = (-0.015625D0*CS1S1S1f111*MW*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*A0(MH12)*DBLE(EL**INT(3.D0))* (3.D0 - (&
  &1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-3.D0)))/(MH12*PI2)

 amplitudes(29) = (-0.015625D0*CS1S1S1f122*MW*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*A0(MH22)*DBLE(EL**INT(3.D0))* (3.D0 - (&
  &1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-3.D0)))/(MH12*PI2)

 amplitudes(30) = (-0.015625D0*CS1S1S1f133*MW*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*A0(MH32)*DBLE(EL**INT(3.D0))* (3.D0 - (&
  &1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-3.D0)))/(MH12*PI2)

 amplitudes(31) = (-0.015625D0*CS1S1S1f211*MW*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))&
  &*(CB*RR11 + RR12*SB)*A0(MH12)* DBLE(EL**INT(3.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))&
  &*DBLE(SW**INT(-3.D0)))/(MH22*PI2)

 amplitudes(32) = (-0.015625D0*CS1S1S1f222*MW*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))&
  &*(CB*RR11 + RR12*SB)*A0(MH22)* DBLE(EL**INT(3.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))&
  &*DBLE(SW**INT(-3.D0)))/(MH22*PI2)

 amplitudes(33) = (-0.015625D0*CS1S1S1f233*MW*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))&
  &*(CB*RR11 + RR12*SB)*A0(MH32)* DBLE(EL**INT(3.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))&
  &*DBLE(SW**INT(-3.D0)))/(MH22*PI2)

 amplitudes(34) = (-0.015625D0*CS1S1S1f311*MW*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)&
  &)*(CB*RR11 + RR12*SB)*A0(MH12)* DBLE(EL**INT(3.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0))&
  &)*DBLE(SW**INT(-3.D0)))/(MH32*PI2)

 amplitudes(35) = (-0.015625D0*CS1S1S1f322*MW*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)&
  &)*(CB*RR11 + RR12*SB)*A0(MH22)* DBLE(EL**INT(3.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0))&
  &)*DBLE(SW**INT(-3.D0)))/(MH32*PI2)

 amplitudes(36) = (-0.015625D0*CS1S1S1f333*MW*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)&
  &)*(CB*RR11 + RR12*SB)*A0(MH32)* DBLE(EL**INT(3.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0))&
  &)*DBLE(SW**INT(-3.D0)))/(MH32*PI2)

 amplitudes(37) = (-0.015625D0*CS2S2S1f111*MW*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*A0(GaugeXiZ*MZ2)*DBLE(EL**INT(3.D0))* (&
  &3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-3.D0)))/(MH12*PI2)

 amplitudes(38) = (-0.015625D0*CS2S2S1f221*MW*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*A0(MA02)*DBLE(EL**INT(3.D0))* (3.D0 - (&
  &1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-3.D0)))/(MH12*PI2)

 amplitudes(39) = (-0.015625D0*CS2S2S1f112*MW*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))&
  &*(CB*RR11 + RR12*SB)* A0(GaugeXiZ*MZ2)*DBLE(EL**INT(3.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(&
  &-4.D0)))*DBLE(SW**INT(-3.D0)))/(MH22*PI2)

 amplitudes(40) = (-0.015625D0*CS2S2S1f222*MW*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))&
  &*(CB*RR11 + RR12*SB)*A0(MA02)* DBLE(EL**INT(3.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))&
  &*DBLE(SW**INT(-3.D0)))/(MH22*PI2)

 amplitudes(41) = (-0.015625D0*CS2S2S1f113*MW*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)&
  &)*(CB*RR11 + RR12*SB)* A0(GaugeXiZ*MZ2)*DBLE(EL**INT(3.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT&
  &(-4.D0)))*DBLE(SW**INT(-3.D0)))/(MH32*PI2)

 amplitudes(42) = (-0.015625D0*CS2S2S1f223*MW*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3)&
  &)*(CB*RR11 + RR12*SB)*A0(MA02)* DBLE(EL**INT(3.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0))&
  &)*DBLE(SW**INT(-3.D0)))/(MH32*PI2)

 amplitudes(43) = (-0.03125D0*CS1S3S3f111*MW*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*A0(GaugeXiW*MW2)*DBLE(EL**INT(3.D0))* (3&
  &.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-3.D0)))/(MH12*PI2)

 amplitudes(44) = (-0.03125D0*CS1S3S3f122*MW*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*A0(MHp2)*DBLE(EL**INT(3.D0))* (3.D0 - (1&
  &.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-3.D0)))/(MH12*PI2)

 amplitudes(45) = (-0.03125D0*CS1S3S3f211*MW*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*&
  &(CB*RR11 + RR12*SB)* A0(GaugeXiW*MW2)*DBLE(EL**INT(3.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-&
  &4.D0)))*DBLE(SW**INT(-3.D0)))/(MH22*PI2)

 amplitudes(46) = (-0.03125D0*CS1S3S3f222*MW*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*&
  &(CB*RR11 + RR12*SB)*A0(MHp2)* DBLE(EL**INT(3.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*&
  &DBLE(SW**INT(-3.D0)))/(MH22*PI2)

 amplitudes(47) = (-0.03125D0*CS1S3S3f311*MW*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))&
  &*(CB*RR11 + RR12*SB)* A0(GaugeXiW*MW2)*DBLE(EL**INT(3.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(&
  &-4.D0)))*DBLE(SW**INT(-3.D0)))/(MH32*PI2)

 amplitudes(48) = (-0.03125D0*CS1S3S3f322*MW*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))&
  &*(CB*RR11 + RR12*SB)*A0(MHp2)* DBLE(EL**INT(3.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))&
  &*DBLE(SW**INT(-3.D0)))/(MH32*PI2)

 amplitudes(49) = (-0.0078125D0*MW*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*&
  &SW)/EL)*A0(MZ2)* DBLE(EL**INT(5.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(&
  &-5.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/ (CW2*MH12*PI2)

 amplitudes(50) = (-0.0078125D0*MW*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*(CB*RR11 +&
  & RR12*SB)* ((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*A0(MZ2)*D&
  &BLE(EL**INT(5.D0))* (3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-5.D0))*DBLE((CW2&
  & + SW2)**INT(2.D0)))/(CW2*MH22*PI2)

 amplitudes(51) = (-0.0078125D0*MW*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*(CB*RR11 &
  &+ RR12*SB)* ((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*A0(MZ2)&
  &*DBLE(EL**INT(5.D0))* (3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-5.D0))*DBLE((C&
  &W2 + SW2)**INT(2.D0)))/(CW2*MH32*PI2)

 amplitudes(52) = (-0.0078125D0*MW*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*&
  &SW)/EL)*A0(MW2)* DBLE(EL**INT(5.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(&
  &-5.D0)))/(MH12*PI2)

 amplitudes(53) = (-0.0078125D0*MW*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*(CB*RR11 +&
  & RR12*SB)* ((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*A0(MW2)*D&
  &BLE(EL**INT(5.D0))* (3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-5.D0)))/(MH22*PI&
  &2)

 amplitudes(54) = (-0.0078125D0*MW*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*(CB*RR11 &
  &+ RR12*SB)* ((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*A0(MW2)&
  &*DBLE(EL**INT(5.D0))* (3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-5.D0)))/(MH32*&
  &PI2)

 amplitudes(55) = (-0.0078125D0*MW*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*&
  &SW)/EL)*A0(MW2)* DBLE(EL**INT(5.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(&
  &-5.D0)))/(MH12*PI2)

 amplitudes(56) = (-0.0078125D0*MW*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*(CB*RR11 +&
  & RR12*SB)* ((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*A0(MW2)*D&
  &BLE(EL**INT(5.D0))* (3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-5.D0)))/(MH22*PI&
  &2)

 amplitudes(57) = (-0.0078125D0*MW*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*(CB*RR11 &
  &+ RR12*SB)* ((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*A0(MW2)&
  &*DBLE(EL**INT(5.D0))* (3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE(SW**INT(-5.D0)))/(MH32*&
  &PI2)

 amplitudes(58) = (-0.0078125D0*MW*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*&
  &SW)/EL)* ((2.D0*MZ2)/MH12 - (4.D0*A0(MZ2))/MH12)*DBLE(EL**INT(5.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBL&
  &E(MW**INT(-4.D0)))* DBLE(SW**INT(-5.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/(CW2*PI2)

 amplitudes(59) = (-0.0078125D0*MW*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*(CB*RR11 +&
  & RR12*SB)* ((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*((2.D0*MZ&
  &2)/MH22 - (4.D0*A0(MZ2))/MH22)* DBLE(EL**INT(5.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0))&
  &)*DBLE(SW**INT(-5.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/ (CW2*PI2)

 amplitudes(60) = (-0.0078125D0*MW*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*(CB*RR11 &
  &+ RR12*SB)* ((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*((2.D0*&
  &MZ2)/MH32 - (4.D0*A0(MZ2))/MH32)* DBLE(EL**INT(5.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0&
  &)))*DBLE(SW**INT(-5.D0))*DBLE((CW2 + SW2)**INT(2.D0)))/ (CW2*PI2)

 amplitudes(61) = (-0.015625D0*MW*(CA12*CA22 + CA22*SA12)*(CB*RR11 + RR12*SB)*((2.D0*CA1*CA2*CB*MW*SW)/EL + (2.D0*CA2*MW*SA1*SB*S&
  &W)/EL)* ((2.D0*MW2)/MH12 - (4.D0*A0(MW2))/MH12)*DBLE(EL**INT(5.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE&
  &(MW**INT(-4.D0)))* DBLE(SW**INT(-5.D0)))/PI2

 amplitudes(62) = (-0.015625D0*MW*(CA1*CA2*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3) + CA2*SA1*(CA1*CA3 - 1.D0*SA1*SA2*SA3))*(CB*RR11 + &
  &RR12*SB)* ((2.D0*CB*MW*(-1.D0*CA3*SA1 - 1.D0*CA1*SA2*SA3)*SW)/EL + (2.D0*MW*(CA1*CA3 - 1.D0*SA1*SA2*SA3)*SB*SW)/EL)*((2.D0*MW2&
  &)/MH22 - (4.D0*A0(MW2))/MH22)* DBLE(EL**INT(5.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)))&
  &*DBLE(SW**INT(-5.D0)))/PI2

 amplitudes(63) = (-0.015625D0*MW*(CA2*SA1*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3) + CA1*CA2*(-1.D0*CA1*CA3*SA2 + SA1*SA3))*(CB*RR11 +&
  & RR12*SB)* ((2.D0*CB*MW*(-1.D0*CA1*CA3*SA2 + SA1*SA3)*SW)/EL + (2.D0*MW*(-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)*SB*SW)/EL)*((2.D0*M&
  &W2)/MH32 - (4.D0*A0(MW2))/MH32)* DBLE(EL**INT(5.D0))*(3.D0 - (1.D0*MH12)/MW2 + 0.25D0*DBLE(MH1**INT(4.D0))*DBLE(MW**INT(-4.D0)&
  &))*DBLE(SW**INT(-5.D0)))/PI2

  totalAmplitude = (0D0,0D0)
 do j=1,63
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do

 H1toWmWpTad = totalAmplitude
end function H1toWmWpTad