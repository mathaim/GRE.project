DroppingGRERequirementDataset_1_ <- read_excel("DroppingGRERequirementDataset (1).xlsx")
DroppingGRERequirementDataset_1_$Institution_Name <- DroppingGRERequirementDataset_1_$Institution
sum(table(DroppingGRERequirementDataset_1_$Institution_Name))

inner <- merge(gss, DroppingGRERequirementDataset_1_, by = "Institution_Name", all = F)
onlygss <- merge(gss, DroppingGRERequirementDataset_1_, by = "Institution_Name", all.x = T)

school <- unique(inner$Institution_Name)
count(unique(inner$Institution_Name))
count(unique(onlygss$Institution_Name))

r <- unique(inner$Institution_Name)
print(r)

for (i in r) {
  onlygss1 <- subset(onlygss, onlygss$Institution_Name != i)
}
View(onlygss1)

onlygss3 <- unite(onlygss1, "InstitutionName_gsscode", c("Institution_Name", "gss_code"), sep = "-")

onlygss2 <- table(onlygss3$InstitutionName_gsscode)
onlygss2 <- as.data.frame(onlygss2)
onlygss2 <- separate(onlygss2, Var1, c("Institution_Name", "gss_code"), sep = "-")