# IATTC_MSE

IATTC's MSE code for tropical tunas in the EPO

Tricks:

- The EM's control file should come from a converged run's control_new

- The R0 in EM's control file should be slightly larger than the MLE (+0.25 for bigeye) and should be fixed at phase 2 to increase the convergence rate of the EM in the MSE

- The par folder should include few files: CLEAN.BAT, R_devs.csv, seeds.csv, and ss.exe
