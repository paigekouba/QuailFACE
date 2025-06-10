caret::findLinearCombos(model.matrix(totmass ~ Anet + herbivory + Plot, final_df.1))

skimr::skim(final_df.1)

dplyr::count(final_df.1, Plot)

boxplot(Anet ~ Plot, data = final_df.1)
boxplot(Anet ~ herbivory, data = final_df.1)

print(dplyr::count(final_df.1, herbivory, Plot),
      n = Inf)
