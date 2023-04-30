admissionsProbs <- function() 
{
  # rename to make more readable
  ucb <- UCBAdmissions
  # total number of applicants
  total <- sum(UCBAdmissions)
  p_admit <- sum(ucb[1, , ]) / total
  p_admitGivenFemale <- sum(ucb[1, 2, ]) / sum(ucb[, 2, ])
  p_admitGivenFemaleAndDeptB <- ucb[1,2,2] / sum(ucb[,2,2])
  p_admitGivenFemaleAndDeptC <- ucb[1, 2, 3] / sum(ucb[, 2, 3])
  p_deptCGivenFemale <- sum(ucb[, 2, 3]) / sum(ucb[,2,]) 
  p_admitGivenFemaleandDeptBorC <- sum(ucb[1, 2, 2:3]) / sum(ucb[, 2, 2:3])
  p_femaleGivenAdmit <- sum(ucb[1,2,]) / sum(ucb[1,,])
  p_femaleAndAdmit <- sum(ucb[1,2,]) / total
  p_DeptCorD <- sum(ucb[,,3:4]) / total
  return (c(p_admit,p_admitGivenFemale, p_admitGivenFemaleAndDeptB,
              p_admitGivenFemaleAndDeptC,p_deptCGivenFemale,
              p_admitGivenFemaleandDeptBorC, p_femaleGivenAdmit,
              p_femaleAndAdmit,p_DeptCorD))
}

print(admissionsProbs())