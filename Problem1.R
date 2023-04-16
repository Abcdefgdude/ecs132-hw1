# Accessing UCBAdmissions
# UCBAdmissions[Admit(1)/Reject(2),Male(1)/Female(2),Dept(1-6)]

admissionsProbs <- function() {
  totalAdmissions <- sum(UCBAdmissions)
  # P(admit) = 1755/4526 
  # (totalAdmits / totalAdmissions)
  p_admit <- sum(UCBAdmissions[1,,]) / totalAdmissions
  
  # P(female and admit) = 557/4526 
  # (total admitted females / totalAdmissions)
  p_admitandFemale <- sum(UCBAdmissions[1,2,]) / totalAdmissions
  
  # P(admit | female) == P(female & admit) / P(female)
  # = 557/1835
  p_female <- sum(UCBAdmissions[,2,]) / sum(UCBAdmissions[,1,] + UCBAdmissions[,2,])
  p_admitGivenFemale <- p_admitandFemale / p_female
  
  # P(female | admit) == P(female & admit) / P(admit)
  # = 557 / 1755
  p_femaleGivenAdmit <- p_admitandFemale / p_admit
  
  # P(admit | female and Dept. B) == P(admit & female & dept b) / P(female & dept b)
  # = 17 / 25
  p_femaleandDeptB <- sum(UCBAdmissions[,2,2]) / totalAdmissions
  p_admitandFemaleandDeptB <- sum(UCBAdmissions[1,2,2]) / totalAdmissions
  p_admitGivenFemaleandDeptB <- p_admitandFemaleandDeptB / p_femaleandDeptB
  
  # P(admit | female and Dept. C) == P(admit & female & dept c) / P(female & dept c)
  # = 202 / 593
  p_femaleandDeptC <- sum(UCBAdmissions[,2,3]) / totalAdmissions
  p_admitandFemaleandDeptC <- sum(UCBAdmissions[1,2,3]) / totalAdmissions
  p_admitGivenFemaleandDeptC <- p_admitandFemaleandDeptC / p_femaleandDeptC
  
  # P(Dept. C | female) == P(dept c & female) / P(female)
  # = 593 / 1835
  p_deptCandFemale <- sum(UCBAdmissions[,2,3]) / totalAdmissions
  p_deptCGivenFemale <- p_deptCandFemale / p_female
  
  # P(Dept C. or Dept. D) = P(Dept C) + P(Dept D)
  # = (918 + 792) / 4526
  p_deptCorD <- sum(UCBAdmissions[,,3] + UCBAdmissions[,,4]) / totalAdmissions
  
  # P(admit | female and (Dept B. or Dept. C))
  # == P(admit and female and (Dept B. or Dept C)) / P(female and (Dept B. or Dept. C))
  # == P(admit and female and Dept B) + P(admit and female and Dept C) / P(female and Dept B) + P(female and Dept C)
  # (17 + 202) / (25 + 593)
  p_femaleandDeptBorC <- sum(UCBAdmissions[,2,2] + UCBAdmissions[,2,3]) / totalAdmissions
  p_admitandFemaleandDeptBorC <- sum(UCBAdmissions[1,2,2] + UCBAdmissions[1,2,3]) / totalAdmissions
  p_admitGivenFemaleandDeptBorC <- p_admitandFemaleandDeptBorC / p_femaleandDeptBorC
  
  res <- list(p_admit, p_admitGivenFemale, 
              p_admitGivenFemaleandDeptB, p_admitGivenFemaleandDeptC, 
              p_deptCGivenFemale, p_admitGivenFemaleandDeptBorC, 
              p_admitandFemale, p_deptCorD)
  
  return(res)
}

UCBAdmitProbs <- admissionsProbs()
print(UCBAdmitProbs)
