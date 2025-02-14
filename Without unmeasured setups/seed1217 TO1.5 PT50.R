# PT50 IO0.5-------------------
# Specific
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "Specific"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.005,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO0.5_S.RData")

rm(list = ls())
gc()

# System Clustered
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "System Clustered"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.005,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO0.5_SC.RData")

rm(list = ls())
gc()

# Center
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "Center"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.005,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO0.5_C.RData")

rm(list = ls())
gc()

# Broad
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "Broad"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.005,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO0.5_B.RData")

rm(list = ls())
gc()

# UShape
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "UShape"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.005,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO0.5_US.RData")

rm(list = ls())
gc()

# PT50 IO5-----------------------------
# Specific
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "Specific"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.05,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO5_S.RData")

rm(list = ls())
gc()

# System Clustered
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "System Clustered"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.05,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO5_SC.RData")

rm(list = ls())
gc()

# Center
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "Center"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.05,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO5_C.RData")

rm(list = ls())
gc()

# Broad
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "Broad"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.05,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO5_B.RData")

rm(list = ls())
gc()

# UShape
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "UShape"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.05,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO5_US.RData")

rm(list = ls())
gc()

# PT50 IO10----------------------------
# Specific
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "Specific"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.1,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO10_S.RData")

rm(list = ls())
gc()

# System Clustered
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "System Clustered"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.1,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO10_SC.RData")

rm(list = ls())
gc()

# Center
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "Center"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.1,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO10_C.RData")

rm(list = ls())
gc()

# Broad
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "Broad"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.1,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO10_B.RData")

rm(list = ls())
gc()

# UShape
source("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Code/Function.R")
set.seed(1217)
D_input <- "UShape"
sim_num_input <- 500
Y_num_input <- 20
X_num_input <- 50
sample_num_input <- 10000
TO_input <- rep(1.5,Y_num_input)
Tpre_input <- 0.5
UCT_input <- c(rep(1,Y_num_input))
UCT_input_un <- c(rep(1.2,Y_num_input))
Oinci_input <-  c(rep(0.1,Y_num_input))
CO_effect_input <- rep(1.3,Y_num_input)

Association_and_PSmodel_S <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Specific")
Association_and_PSmodel_SC <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "System Clustered")
Association_and_PSmodel_C <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Center")
Association_and_PSmodel_B <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "Broad")
Association_and_PSmodel_US <- ASSO_PSM_Function(Y_num = Y_num_input,X_num = X_num_input,D = "UShape")

# UCO_input
UCO_input <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinput in 1:Y_num_input) {
  UCO_input[i_ucoinput] <- rep(1, Y_num_input)
}# without unmeasured

# with unmeasured confounders
UCO_input_un <- data.frame(matrix(ncol = 0, nrow = Y_num_input))
for (i_ucoinputun in 1:Y_num_input) {
  UCO_input_un[i_ucoinputun] <- rep(1, Y_num_input)
}
for (i_UCOun in 1:Y_num_input) {
  UCO_input_un[,i_UCOun][i_UCOun] <- 1.2
}

Time <- system.time({
  Result_list1 <- Simulation_Function(D = D_input, Y_num = Y_num_input,X_num = X_num_input,sample_num = sample_num_input,
                                      sim_num = sim_num_input, CO_effect = CO_effect_input,
                                      TO = TO_input, Tpre = Tpre_input, Oinci = Oinci_input,
                                      UCT = UCT_input, UCO = UCO_input)
  CT1 <- Coverage_Table_Function(Result_list1)
  Result_form <- Result_Form_function_allY(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_notabs <- Result_Form_function_allY_notabs(Result_list = Result_list1,Coverage_table = CT1)
  Result_form_MAE <- Result_Form_function_allY_MAE(Result_list = Result_list1,Coverage_table = CT1)
  Avery_form_function(Result_form)
  Avery_form_function_notabs(Result_form_notabs)
  Avery_form_function_MAE(Result_form_MAE)
})
save.image("D:/Learning epi-master/Research Project/Code/Code and Result 2025/Result_500_new/seed1217 TO1.5 PT50IO10_US.RData")

rm(list = ls())
gc()
